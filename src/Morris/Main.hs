{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
module Main where

import Morris.Board

import Graphics.QML
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

import Paths_hsqml_demo_morris

newtype IdBoard = IdBoard (IntMap (Player, Position))

newIdBoard :: IdBoard
newIdBoard =
    IdBoard IntMap.empty

boardToId :: Board -> IdBoard
boardToId board =
    IdBoard $ fst $ foldr (\pl mn ->
        foldr (\ps (m,n) ->
            (IntMap.insert n (pl,ps) m,n+1)) mn $
        getPlayerPieces pl board) (IntMap.empty,0) [Red,Black]

findPieceId :: Position -> IdBoard -> Int
findPieceId pos (IdBoard m) =
    IntMap.foldrWithKey (\k' (_,pos') k -> if pos == pos' then k' else k) 0 m

playFirstActionId :: Player -> FirstAction -> IdBoard -> IdBoard
playFirstActionId player (Place p) (IdBoard m) =
    let i = if IntMap.null m then 0 else 1 + (fst $ IntMap.findMax m)
    in IdBoard $ IntMap.insert i (player,p) m
playFirstActionId player (Move p p') ib@(IdBoard m) =
    let i = findPieceId p ib
    in IdBoard $ IntMap.insert i (player,p') m

playSecondActionId :: SecondAction -> IdBoard -> IdBoard
playSecondActionId (Take pos) ib@(IdBoard m) =
    let i = findPieceId pos ib
    in IdBoard $ IntMap.delete i m

playActionId :: Player -> Action -> IdBoard -> IdBoard
playActionId player (FirstAction act) = playFirstActionId player act
playActionId _ (SecondAction act) = playSecondActionId act

moveToActions :: Move -> [Action]
moveToActions (FullMove act1 Nothing) =
    [FirstAction act1]
moveToActions (FullMove act1 (Just act2)) =
    [FirstAction act1,SecondAction act2]

actionToPositions :: Action -> [Position]
actionToPositions (FirstAction (Place p1))   = [p1]
actionToPositions (FirstAction (Move p1 p2)) = [p1,p2]
actionToPositions (SecondAction (Take p1))   = [p1]

moveToPositions :: Move -> [Position]
moveToPositions = concatMap actionToPositions . moveToActions

instance Marshal Player where
    type MarshalMode Player c d = ModeBidi c
    marshaller = bidiMarshaller from to
        where from txt = case T.unpack txt of
                  "red" -> Red
                  _     -> Black
              to plyr = T.pack $ case plyr of
                  Red   -> "red"
                  Black -> "black"

instance Marshal Position where
    type MarshalMode Position c d = ModeBidi c
    marshaller = bidiMarshaller Position (\(Position i) -> i)

data GameObj = GameObj {
    gameBoard :: Board,
    gameIdBoard :: IdBoard,
    gameActions :: [Position],
    gameBias :: Map Board Float,
    gamePrev :: Maybe GameObj}
    deriving Typeable

data PieceObj = PieceObj {
    piecePlayer :: Player,
    piecePrevPos :: Maybe Position,
    pieceCurrPos :: Maybe Position}
    deriving (Typeable, Show)

instance DefaultClass PieceObj where
    classMembers = [
        defPropertyConst "player" $ return . piecePlayer,
        defPropertyConst "currPos" $ return . pieceCurrPos,
        defPropertyConst "prevPos" $ return . piecePrevPos]

instance Marshal PieceObj where
    type MarshalMode PieceObj c d = ModeObjFrom PieceObj c
    marshaller = fromMarshaller fromObjRef

getPlayer :: GameObj -> IO Player
getPlayer = return . getBoardNextPlayer . gameBoard

getPieces :: GameObj -> IO [ObjRef PieceObj]
getPieces gs =
    let (IdBoard ib) = gameIdBoard $ fromMaybe gs $ gamePrev gs
        (IdBoard ib') = gameIdBoard gs
        merge _ (plyr,p) (_,p') = Just $ PieceObj plyr (Just p) (Just p')
        oldMap = fmap $ \(plyr,p) -> PieceObj plyr (Just p) Nothing
        newMap = fmap $ \(plyr,p') -> PieceObj plyr Nothing (Just p')
        ps = IntMap.elems $ IntMap.mergeWithKey merge oldMap newMap ib ib'
    in mapM newObjectDC ps
 
getTargets :: GameObj -> IO [Position]
getTargets gs =
    let actions = gameActions gs
        posOff  = length actions
    in return $ nub $ map (!! posOff) $
        filter (isPrefixOf actions) $ map moveToPositions $
        legalMoves $ gameBoard gs

getActions :: GameObj -> IO [Position]
getActions = return . gameActions

startAI :: ObjRef GameObj -> Int -> IO ()
startAI gsRef d = fmap (const ()) $ forkIO $ do
    let gs = fromObjRef gsRef
        ps = maybe [] moveToPositions $ aiMove d (gameBias gs) $ gameBoard gs
    evaluate $ force ps
    fireSignal (Proxy :: Proxy AIReady) gsRef ps

selectTarget :: GameObj -> Int -> IO (ObjRef GameObj)
selectTarget gs i =
    let actions' = gameActions gs ++ [Position i]
        board = gameBoard gs
        iboard = gameIdBoard gs
        player = getBoardNextPlayer board
        moves = legalMoves board
        maybeMove = fmap fst $ find ((actions' ==) . snd) $
            zip moves $ map moveToPositions moves
        board' = maybe board (\m -> playMove m board) maybeMove
        actionLists = concatMap (tail . inits . moveToActions) moves
        actionTargets = map (\as ->
            (last as, concatMap actionToPositions as)) actionLists
        maybeAction = fmap fst $ find ((actions' ==) . snd) actionTargets
        iboard' = maybe iboard (\a -> playActionId player a iboard) maybeAction
    in newObjectDC $ case maybeMove of
        Just move -> gs {
            gameBoard = board',
            gameIdBoard = iboard',
            gameActions = [],
            gameBias = {-Map.map (*0.9) $-} Map.alter (\x ->
                Just $ fromMaybe 0 x - 0.5) board' $ gameBias gs,
            gamePrev = Just gs}
        Nothing -> gs {
            gameIdBoard = iboard',
            gameActions = actions',
            gamePrev = Just gs}

data AIReady deriving Typeable

instance SignalKeyClass AIReady where
    type SignalParams AIReady = [Position] -> IO ()

instance DefaultClass GameObj where
    classMembers = [
        defPropertyRO "player" getPlayer,
        defPropertyRO "pieces" getPieces,
        defPropertyRO "targets" getTargets,
        defPropertyRO "actions" getActions,
        defMethod "startAI" startAI,
        defSignal "aiReady" (Proxy :: Proxy AIReady),
        defMethod "selectTarget" selectTarget]

instance Marshal GameObj where
    type MarshalMode GameObj c d = ModeObjFrom GameObj c
    marshaller = fromMarshaller fromObjRef

createGame :: ObjRef () -> IO (ObjRef GameObj)
createGame _ =
    newObjectDC $ GameObj newBoard newIdBoard [] Map.empty Nothing

main :: IO ()
main = do
    clazz <- newClass [defMethod "createGame" createGame]
    ctx <- newObject clazz ()
    qml <- getDataFileName "morris.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qml,
        contextObject = Just $ anyObjRef ctx}

