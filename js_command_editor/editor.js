


// TODO: Op generation must take into account the cursor
// so that diff between aaaa and aaa will delete the correct
// character

function initws() {

  var ws = new WebSocket("ws://localhost:1234/websession");

  ws.onopen = function() {
    debug("connected.");
  };
  
  txText = function(data) {
    ws.send(data);
  };
  
  ws.onmessage = function (evt) {
    debug("received: " + evt.data); 
    var data = JSON.parse(evt.data);

    if (data.transformed) {
      transforming = false;

      // The outoing ops are replaced with the transformed
      // version of the client ops
      debug('cdash: ' + JSON.stringify(data.transformed.cdash));
      debug('sdash: ' + JSON.stringify(data.transformed.sdash));

      ops = ops.concat(data.transformed.sdash);
      outgoingOps = data.transformed.cdash;
      var doc = get_doc();
      $("#output").text(doc);
      editor.text(doc);

      // TODO: Update for composite transforms
      serverVersion = serverVersion + 2;

      // And now try and send the outgoing ops again?
      sendOutgoing();
    }

    if (data.ack) {
      var appliedOps = data.ack.applied_ops;
      serverVersion = data.ack.serverVersion;
      var newOutgoingOps = [];
      for (var i = appliedOps; i < outgoingOps.length; ++i) {
	newOutgoingOps[i - appliedOps] = outgoingOps[i];
      }
      outgoingOps = newOutgoingOps;
      acknowledged = true;
      sendOutgoing();
    }

    // If the data contains an operation, apply it to
    // the document
    if (data.ops) {
      var appliedOps = data.ops.applied_ops;

      if (outgoingOps.length == 0) {
	serverVersion = data.ops.serverVersion;
	clientVersion = serverVersion;

	// TODO: Sync can be lost here between editor and ops result
	ops = ops.concat(appliedOps);
	var doc = get_doc();
	$("#output").text(doc);
	editor.text(doc);
      } else {

	debug("transforming");
	var serverOps = data.ops.applied_ops;
	txText(JSON.stringify({transform : 
	      {outgoingOps : outgoingOps, serverOps : serverOps}}));
	transforming = true;
      }
    }

    
  };

  ws.onclose = function() {
    debug("socket closed");
  }
  
}

var ops = [];
var localOpQueue = [];
var outgoingOps = [];

var editor = $("#editor");
var clientVersion = 0;
var serverVersion = 0;
var acknowledged = true;
var transforming = false;

function get_doc() {
  debug('--------');
  debug('ops: ' + JSON.stringify(ops));
  debug('outgoing: ' + JSON.stringify(outgoingOps));
  debug('--------');

  var doc = "";
  for (var i = 0; i < ops.length; ++i) {
    doc = apply_op(doc, ops[i]);
  }

  return doc;
}

function pushOp(op) {

  // If the server is at the same version,
  // send the next ops. Otherwise queue
  // the outgoing ops
  outgoingOps.push(op);

  // Apply on the client side
  ops.push(op);
  ++clientVersion;
  $("#output").text(get_doc());
  
  if (acknowledged) {
    sendOutgoing();
  }

}

function sendOutgoing() {
  if (outgoingOps.length > 0) {
    var message = {};
    message.clientVersion = clientVersion;
    message.serverVersion = serverVersion;
    message.ops = outgoingOps;
    acknowledged = false;
    txText(JSON.stringify({client_ops: message}));
  }
}


// Apply an operation on an existing document (a string)
function apply_op(doc, op) {
  var cursor = 0;
  var newDoc = "";
  for (var i = 0; i < op.length; ++i) {
    var component = op[i];
    if (component.ret) {
      newDoc = newDoc + doc.substring(cursor, cursor + component.ret);
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

var os = 0;
var oe = 0;
var cursor = {};
function updateCursor() {
  var o = $("#editor")[0];
  var t = o.value, s = getSelectionStart(o), e = getSelectionEnd(o);


  if (s == os && e == oe) return;

  cursor.caretPos = s;
  cursor.maxLength = o.getAttribute('maxLength');
  cursor.textLength = t.length;
  cursor.availLength = o.getAttribute('maxLength') - t.length;
  cursor.afterCaret = t.substring(s).replace(/ /g, '\xa0') || '\xa0';
  cursor.beforeCaret = t.substring(0, s).replace(/ /g, '\xa0') || '\xa0';
  cursor.selectedText = t.substring(s, e).replace(/ /g, '\xa0') || '\xa0';
  cursor.rightbox = t.substring(s).replace(/ /g, '\xa0') || '\xa0';
  cursor.leftbox = t.substring(0, s).replace(/ /g, '\xa0') || '\xa0';
  $("#cursor").text(JSON.stringify(cursor));
  os = s;
  oe = e;
}

function getSelectionStart(o) {
	if (o.createTextRange) {
		var r = document.selection.createRange().duplicate()
		r.moveEnd('character', o.value.length)
		if (r.text == '') return o.value.length
		return o.value.lastIndexOf(r.text)
	} else return o.selectionStart
}
 
function getSelectionEnd(o) {
	if (o.createTextRange) {
		var r = document.selection.createRange().duplicate()
		r.moveStart('character', -o.value.length)
		return r.text.length
	} else return o.selectionEnd
}

editor.focus(function() {
    editor.removeClass('not-editing');
    editor.addClass('editing');
  });

editor.blur(function() {
    editor.addClass('not-editing');
    editor.removeClass('editing');
  });

editor.keydown(function(event) {
    // Reject (for now) events that occur during transformation
    if (transforming) {
      return;
    }

    updateCursor();

    // Arrow keys and apple key
    if ((event.keyCode == 37) || 
	(event.keyCode == 38) ||
	(event.keyCode == 39) ||
	(event.keyCode == 40) || 
	(event.keyCode == 91)) {
      return;
    }

    var printable =  ((event.keyCode >= 32) || 
		      (event.keyCode == 13) ||
		      (event.keyCode == 8));
    if (!printable) {
      return;
    }
	
    var stringFromCharCode =  String.fromCharCode(event.keyCode);
    var before;
    var middle;
    var docOp = [];
    if (event.keyCode == 8) {
      if (cursor.caretPos > 0) {
	if (cursor.caretPos > 1) {
	  docOp = docOp.concat([{ret : cursor.caretPos - 1}]);
	}
	docOp = docOp.concat({del : cursor.beforeCaret[cursor.beforeCaret.length - 1]});
      }
    } else {
      if (cursor.caretPos > 0) {
	docOp = docOp.concat([{ret: cursor.caretPos}]);
      }
      docOp = docOp.concat([{ins: stringFromCharCode}]);
    }
    if (cursor.textLength - cursor.caretPos > 0) {
      after = cursor.textLength - cursor.caretPos;
      docOp = docOp.concat([{ret: after}]);
    }

    
    pushOp(docOp);
    
  });


editor.keyup(function(event) {
    updateCursor();
  });

editor.keypress(function(event) {
    updateCursor();
  });

editor.mouseup(function(event) {
    updateCursor();
  });

editor.mousedown(function(event) {
    updateCursor();
  });

editor.click(function(event) {
    updateCursor();
  });


$(document).ready(function() {
    initws();
  });

