{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
module Main where

import Morris.Board

import Graphics.QML
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Maybe
import Data.List
import Data.Tagged
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

getIdBoardCount :: IdBoard -> Int
getIdBoardCount (IdBoard m) =
    if IntMap.null m then 0 else (1+) $ fst $ IntMap.findMax m

getIdBoardPiece :: Int -> IdBoard -> Maybe (Player,Position)
getIdBoardPiece i (IdBoard m) =
    IntMap.lookup i m

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

data PosListObj = PosListObj [Position] deriving Typeable

posListElem :: PosListObj -> Int -> IO Int
posListElem (PosListObj posList) idx =
    return $ (\(Position i) -> i) $ posList !! idx

posListCount :: PosListObj -> IO Int
posListCount (PosListObj posList) =
    return $ length posList

instance Object PosListObj where
    classDef = defClass [
        defMethod "elem" posListElem,
        defPropertyRO "count" posListCount]

instance Marshal PosListObj where
    type MarshalMode PosListObj = ValObjToOnly PosListObj
    marshaller = objSimpleMarshaller

data GameObj = GameObj {
    gameBoard :: Board,
    gameIdBoard :: IdBoard,
    gameActions :: [Position],
    gameBias :: Map Board Float}
    deriving Typeable

getPlayer :: GameObj -> IO String
getPlayer gs =
    return $ case getBoardNextPlayer $ gameBoard gs of
        Red   -> "red"
        Black -> "black"
 
getTargets :: GameObj -> IO (ObjRef PosListObj)
getTargets gs =
    let actions = gameActions gs
        posOff  = length actions
    in newObject $ PosListObj $ nub $ map (!! posOff) $
        filter (isPrefixOf actions) $ map moveToPositions $
        legalMoves $ gameBoard gs

getActions :: GameObj -> IO (ObjRef PosListObj)
getActions gs =
    newObject $ PosListObj $ gameActions gs

startAI :: ObjRef GameObj -> Int -> IO ()
startAI gsRef d = fmap (const ()) $ forkIO $ do
    let gs = fromObjRef gsRef
        ps = maybe [] moveToPositions $ aiMove d (gameBias gs) $ gameBoard gs
    evaluate $ force ps
    posObj <- newObject $ PosListObj ps
    fireSignal (Tagged gsRef :: Tagged AIReady (ObjRef GameObj)) posObj

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
    in newObject $ case maybeMove of
        Just move -> gs {
            gameBoard = board',
            gameIdBoard = iboard',
            gameActions = [],
            gameBias = {-Map.map (*0.9) $-} Map.alter (\x ->
                Just $ fromMaybe 0 x - 0.5) board' $ gameBias gs}
        Nothing -> gs {
            gameIdBoard = iboard',
            gameActions = actions'}

getIndexCount :: GameObj -> IO Int
getIndexCount gs =
    return $ getIdBoardCount $ gameIdBoard gs

getPlayerAtIndex :: GameObj -> Int -> IO String
getPlayerAtIndex gs i =
    return $ case getIdBoardPiece i $ gameIdBoard gs of
        Nothing -> "none"
        Just (Red,_) -> "red"
        Just (Black,_) -> "black"

getPositionAtIndex :: GameObj -> Int -> IO Int
getPositionAtIndex gs i =
    return $ case getIdBoardPiece i $ gameIdBoard gs of
        Nothing -> -1
        Just (_,Position pos) -> pos

data AIReady deriving Typeable

instance SignalKey AIReady where
    type SignalParams AIReady = ObjRef PosListObj -> IO ()

instance Object GameObj where
    classDef = defClass [
        defPropertyRO "player" getPlayer,
        defPropertyRO "targets" getTargets,
        defPropertyRO "actions" getActions,
        defMethod "startAI" startAI,
        defSignal (Tagged "aiReady" :: Tagged AIReady String),
        defMethod "selectTarget" selectTarget,
        defPropertyRO "indexCount" getIndexCount,
        defMethod "idxPlayer" getPlayerAtIndex,
        defMethod "idxPosition" getPositionAtIndex]

instance Marshal GameObj where
    type MarshalMode GameObj = ValObjToOnly GameObj
    marshaller = objSimpleMarshaller

createGame :: ObjRef MainObj -> IO (ObjRef GameObj)
createGame _ =
    newObject $ GameObj newBoard newIdBoard [] Map.empty

data MainObj = MainObj deriving Typeable

instance Object MainObj where
    classDef = defClass [
        defMethod "createGame" createGame]

main :: IO ()
main = do
    ctx <- newObject $ MainObj
    qml <- getDataFileName "morris.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qml,
        initialWindowState = ShowWindowWithTitle "HsQML Morris",
        contextObject = Just $ anyObjRef ctx}

