


// TODO: Op generation must take into account the cursor
// so that diff between aaaa and aaa will delete the correct
// character

var sync = false;

function initws() {

  var ws = new WebSocket("ws://localhost:1234/websession");

  ws.onopen = function() {
    debug("connected.");
  };
  
  txText = function(data) {
    sync = true;
    ws.send(data);
  };
  
  ws.onmessage = function (evt) {
    debug("received: " + evt.data); 
    var data = JSON.parse(evt.data);
    // If the data contains an operation, apply it to
    // the document
    if (data.op) {
      var doc = editor.text();
      var newDoc = apply_op(doc, data.op);
      editor.text(newDoc);
    }
    sync = false;
  };

  ws.onclose = function() {
    debug("socket closed");
  }
  
}

var ops = [];
var localOpQueue = [];
var lastText = "";
var docLength = 0;
var cursorPos = 0;
var editor = $("#editor");

function createInsert(text, position) {
  return [{retain: position},
	  {insert: text},
	  {retain: docLength - position}];
      
}

function pushOp(op) {
  ops.push(op);
  debug(JSON.stringify(ops));
}


function initState() {
  // Create the first operation from the initial state
  var text = editor.text();
  var op = createInsert(text, 0);
  pushOp(op);
  lastText = text;
}

function createDiff(firstString, secondString) {
  // Very naive insertion doc op creation, only supports
  // contiguous diff
  var newCursor = 0;
  var beforeDiffLength = 0;
  var afterDiffLength = 0;
  var diff = "";
  // The state machine
  var state = "before";
  for (var lastCursor = 0; lastCursor < firstString.length; ++lastCursor, ++newCursor) {
    if (firstString[lastCursor] != secondString[newCursor]) {
      state = "during";
      for (; newCursor < secondString.length && (firstString[lastCursor] != secondString[newCursor]);
	   ++newCursor) {
	diff += secondString[newCursor];
      }
      state = "after";
    } 
    if (state == "before") {
      ++beforeDiffLength;
    } else if (state == "after") {
      ++afterDiffLength;
    } 
  }
  // Anything remaining
  if ((lastCursor == firstString.length) &&
      (newCursor < secondString.length)) {
    for (; newCursor < secondString.length; ++newCursor) {
	diff += secondString[newCursor];
    }
  }

  return [beforeDiffLength, diff, afterDiffLength];
}

function createOp(lastText, newText) {
  if (lastText.length == newText.length) {
    if (lastText != newText) {
      error('lastTex != newText');
    }
    return [];
  } else if (lastText.length < newText.length) {
    // insert
    var diff = createDiff(lastText, newText);
    return [{ret : diff[0]},
	    {ins : diff[1]},
	    {ret : diff[2]}];
    
  } else {
    // delete
    var diff = createDiff(newText, lastText);
    return [{ret : diff[0]},
	    {del : diff[1]},
	    {ret : diff[2]}];
  }
}

// Apply an operation on an existing document (a string)
function apply_op(doc, op) {
  var cursor = 0;
  var newDoc = "";
  for (var i = 0; i < op.length; ++i) {
    var component = op[i];
    if (component.ret) {
      newDoc = newDoc + doc.substring(cursor, component.ret);
      cursor = cursor + component.ret;
    } else if (component.ins) {
      newDoc = newDoc + component.ins;
    } else if (component.del) {
      cursor = cursor + component.del.length;
    }
  }
  return newDoc;
}

function debug(str) {
  $("#console").append(str);
  $("#console").append('<br>');
}

function error(str) {
  $("#errors").append(str);
  $("#errors").append('<br>');
}

editor.focus(function() {
  });

editor.keydown(function(event) {
    event.preventDefault();
    var newOp = createOp(lastText, editor.text());
    if (sync == false) {
      
      if (newOp.length > 0) {
	debug(JSON.stringify(newOp));
	txText(JSON.stringify(newOp));
	lastText = editor.text();
      }
    } else {
      localOpQueue.push(newOp);
    }
  });

editor.keyup(function(event) {
    event.preventDefault();

  });

$(document).ready(function() {
    initState();
    initws();
  });
