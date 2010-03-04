

function init_selection() {
  var savedSelection;
  var selectionUtil = createSelectionUtil(window);
 
  if (selectionUtil.saveRestoreSupported) {
    saveSelection = function() {
      if (savedSelection) {
	selectionUtil.removeMarkers(savedSelection);
      }
      savedSelection = selectionUtil.saveSelection();
    }
 
    restoreSelection = function() {
      if (savedSelection) {
	selectionUtil.restoreSelection(savedSelection);
	savedSelection = null;
      }
    }
  }
}

var ws;

function initws() {

  ws = new WebSocket("ws://localhost:1234/websession");

  ws.onopen = function() {
    // Web Socket is connected. You can send data by send() method.
    debug("connected...");
    //txText();
  };

  txText = function() {
    var text = $("#editor").html();
    console.log(text);
    ws.send(text);       
    return true;
  };

  ws.onmessage = function (evt) {
    var data = evt.data;
    //$("#editor").html(data);
    //     $("#editor").focus();
    $("#editor").html('!' + $("#editor").html());
    //$("#editor").focus();
    restoreSelection();
  };

  ws.onclose = function() {
    debug(" socket closed");
  };
} 


function debug(str) {
  console.log(str);
}

$('#editor').focus(function() {
    //restoreSelection();
  });

$('#editor').blur(function() {
    //saveSelection();
  });


$('#editor').keyup(function() {
    
    saveSelection(); 
    //$("#editor").html('!' + $("#editor").html());
    //$("#editor").focus();
    //restoreSelection();
    txText();

    //var data = $("#editor").html();
    //var text = $("#editor").text();
    //captureSelection();
    //$("#editor").text("x" + text);
    //$("#editor").focus();
    //updateCursor();
    //document.setSelection(editable);
//     var selObj = window.getSelection();
//     var selRange = selObj.getRangeAt(0);
//     cursorPos =  findNode(selObj.anchorNode.parentNode.childNodes, selObj.anchorNode) + selObj.anchorOffset;
//     console.log(cursorPos)
});
