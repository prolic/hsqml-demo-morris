import QtQuick 2.0
import "morris.js" as Morris

Image {
    id: board;
    source: "board.svg";
    width: 320; height: 320;

    property real pw : parent!=null?parent.width:width;
    property real ph : parent!=null?parent.height:height;

    transform: [
        Scale {
            id: scale; xScale: yScale;
            yScale: Math.min(board.pw/board.width,board.ph/board.height);},
        Translate {
            x: (board.pw-board.width*scale.xScale)/2;
            y: (board.ph-board.height*scale.yScale)/2;}]

    property real t;

    NumberAnimation on t {
        id: anim;
        running: false;
        from: 0;
        to: 1;
        duration: 300;
    }

    Connections {
        target: anim;
        onRunningChanged: {
            if (!anim.running) {
                Morris.animFinished();
            }
        }
    }

    Repeater {
        id: pieceView
        model: [];
        Image {
            source: modelData.player == "red" ? "red.svg" : "black.svg";
            opacity: ((modelData.currPos!=null?1:0)*board.t)+((modelData.prevPos!=null?1:0)*(1-board.t));
            x: (Morris.posIdToX(modelData.currPos!=null?modelData.currPos:modelData.prevPos)*board.t)+(Morris.posIdToX(modelData.prevPos!=null?modelData.prevPos:modelData.currPos)*(1-board.t))-width/2;
            y: (Morris.posIdToY(modelData.currPos!=null?modelData.currPos:modelData.prevPos)*board.t)+(Morris.posIdToY(modelData.prevPos!=null?modelData.prevPos:modelData.currPos)*(1-board.t))-height/2;
        }
    }

    Repeater {
        id: targetView
        model: [];
        Image {
            source: "target.svg";
            x: Morris.posIdToX(modelData)-width/2;
            y: Morris.posIdToY(modelData)-height/2;
            MouseArea {
                anchors.fill: parent;
                onClicked: Morris.selectTarget(modelData);
            }
        }
    }

    Component.onCompleted: {
        Morris.setupGame();
    }
}
