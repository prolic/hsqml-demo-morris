import Qt 4.7
import "morris.js" as Morris

Image {
    id: board;
    source: "board.svg";
    fillMode: Image.PreserveAspectFit;
    anchors.fill: parent;

    property real t;

    SequentialAnimation on t {
        id: anim;
        NumberAnimation {
            from: 0;
            to: 1;
            duration: 300;
        }
        ScriptAction {
            script: Morris.animFinished();
        }
    }

    Repeater {
        id: pieceView
        model: [];
        Image {
            source: modelData.red ? "red.svg" : "black.svg";
            opacity: (modelData.endVis*t)+(modelData.startVis*(1-t));
            x: (Morris.posIdToX(modelData.endIdx)*t)+(Morris.posIdToX(modelData.startIdx)*(1-t))-width/2;
            y: (Morris.posIdToY(modelData.endIdx)*t)+(Morris.posIdToY(modelData.startIdx)*(1-t))-height/2;
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
