var game = null;
var redAI = 0;
var blackAI = 2;
var aiMove = [];

function updateTargets()
{
    targetView.model = game.targets;
}

function clearTargets()
{
    targetView.model = [];
}

function updatePieces()
{
    pieceView.model = game.pieces;
    var actions = game.actions.length;
    if (actions == 0 || actions == 2) {
        anim.restart();
    }
    else {
        animFinished();
    }
}

function setupGame()
{
    game = createGame();
    updateTargets();
    updatePieces();
}

function selectTarget(i)
{
    game = game.selectTarget(i);
    clearTargets();
    updatePieces();
}

function animFinished()
{
    var d = isPlayerAI(game.player);
    if (d > 0) {
        if (0 == aiMove.length) {
            game.aiReady.connect(aiReadyCallback);
            game.startAI(d);
        }
        else {
            selectTarget(aiMove.pop());
        }
    }
    else {
        updateTargets();
    }
}

function aiReadyCallback(move)
{
    if (move.length == 0) {
        return;
    }
    for (var j=move.length-1; j>=0; j--) {
        aiMove.push(move[j]);
    }
    animFinished();
}

function isPlayerAI(p)
{
    switch (p) {
        case 'red': return redAI;
        case 'black': return blackAI;
    }
}

function posIdToX(i)
{
    switch (i) {
        case 0: case 9: case 21: return 40;
        case 3: case 10: case 18: return 80;
        case 6: case 11: case 15: return 120;
        case 1: case 4: case 7: case 16: case 19: case 22: return 160;
        case 8: case 12: case 17: return 200;
        case 5: case 13: case 20: return 240;
        case 2: case 14: case 23: return 280;
    }
}

function posIdToY(i)
{
    switch (i) {
        case 0: case 1: case 2: return 40;
        case 3: case 4: case 5: return 80;
        case 6: case 7: case 8: return 120;
        case 9: case 10: case 11: case 12: case 13: case 14: return 160;
        case 15: case 16: case 17: return 200;
        case 18: case 19: case 20: return 240;
        case 21: case 22: case 23: return 280;
    }
}
