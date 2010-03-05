
var ops = [];
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
    // Anything remaining
    if ((lastCursor == firstString.length - 1) &&
	(newCursor < secondString.length - 1)) {
      ++newCursor;
      for (; newCursor < secondString.length; ++newCursor) {
	diff += secondString[newCursor];
      }
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

editor.keyup(function(event) {
    var newOp = createOp(lastText, editor.text());
    debug(JSON.stringify(newOp));
    lastText = editor.text();
  });

$(document).ready(function() {
    initState();
  });
