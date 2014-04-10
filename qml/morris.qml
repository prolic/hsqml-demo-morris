import QtQuick 2.0
import "morris.js" as Morris

Image {
    id: board;
    source: "board.svg";
    fillMode: Image.PreserveAspectFit;
    anchors.fill: parent;

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
            source: modelData.red ? "red.svg" : "black.svg";
            opacity: (modelData.endVis*board.t)+(modelData.startVis*(1-board.t));
            x: (Morris.posIdToX(modelData.endIdx)*board.t)+(Morris.posIdToX(modelData.startIdx)*(1-board.t))-width/2;
            y: (Morris.posIdToY(modelData.endIdx)*board.t)+(Morris.posIdToY(modelData.startIdx)*(1-board.t))-height/2;
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
