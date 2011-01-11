/**
 * @fileoverview Demonstration for AJAX aspects of EWS.
 * @author Simon Wright (simon@pushface.org)
 *
 * Copyright (C) Simon Wright <simon@pushface.org>
 *
 * This unit is free software; you can redistribute it and/or modify
 * it as you wish. This unit is distributed in the hope that it will
 * be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * This script is used in the AJAX demo for EWS.
 *
 * It relies on the utility HttpInteraction.js and is a demo for it.
 */

/**
 * Retrieve the state of all widgets when the page is (re)loaded.
 *
 * Expects a text/xml result:
 *
 * state
 *   time-format   (iso|us|european|locale)
 *   forward-light (false|true)
 *   aft-light     (false|true)
 *   lamp          (starboard: false|true)
 *   lamp          (port: false|true)
 */
var stateRequest = new OneshotHttpInteraction
  ("state.xml",
   null,
   function (r) {
     var x = r.responseXML;
     var value = x.getElementsByTagName("time-format")[0].firstChild.nodeValue;
     for (var o = document.fTimeFormat.format.options, i = 0;
	  i < o.length;
	  i++) {
       o[i].selected = (o[i].value == value);
     }
     value = x.getElementsByTagName("forward-light")[0].firstChild.nodeValue;
     for (var o = document.lights.forward, i = 0;
	  i < o.length;
	  i++) {
       o[i].checked = (o[i].value == value);
     }
     value = x.getElementsByTagName("aft-light")[0].firstChild.nodeValue;
     for (var o = document.lights.aft, i = 0;
	  i < o.length;
	  i++) {
       o[i].checked = (o[i].value == value);
     }
     var lamps = x.getElementsByTagName("lamp");
     for (var c = document.lamps.lamp, i = 0; i < c.length; i++) {
       c[i].checked = lamps[i].firstChild.nodeValue == "true";
     }
   });

/**
 * Get 'ajaxTime' every second.
 *
 * Expects a text/plain result, which it pastes into the document at
 * the element identified as 'timeDisplay'.
 */
var timeRequest = new CyclicHttpInteraction
  ("ajaxTime",
   function (r) {
    document.getElementById("timeDisplay").innerHTML = r.responseText;
   },
   1000);

/**
 * A generalised action request.
 */
var postChange = new OneshotHttpInteraction
  ("aChange",
   null,
   function (r) { });

/**
 * Procedure to simplify setting up radiobuttons.
 * @param buttons the set of buttons: eg, document.formName.sharedButtonName.
 * @param name    the property name that is passed to postChange.
 */
function setUpRadioButtons(buttons, name) {
  for (var i = 0; i < buttons.length; i++) {
    buttons[i].onclick = new Function("postChange.start('"
				      + name
				      + "="
				      + buttons[i].value
				      + "');");
  };
}

/**
 * Assign event handlers and begin fetching.
 */
window.onload = function () {

  // Cyclic requests.
  stateRequest.start();
  timeRequest.start();

  // Time format input.
  document.fTimeFormat.format.onchange = function () {
    for (var o = document.fTimeFormat.format.options, i = 0;
	 i < o.length;
	 i++) {
      if (o[i].selected) {
	postChange.start("format=" +  o[i].value);
	break;
      }
    }
  };

  //  Radiobutton input.
  setUpRadioButtons(document.lights.forward, "forward-light");
  setUpRadioButtons(document.lights.aft, "aft-light");

  //  Checkbox input.
  for (var c = document.lamps.lamp, i = 0; i < c.length; i++) {
    // This is especially hairy. The "function () {}" style doesn't
    // work, because we don't have access to the context, (or perhaps
    // it's that the variables are still in the context, but their
    // values have changed?), so we have to construct a function using
    // the Function(arg, arg, body) scheme. Need to keep careful track
    // of those single/double quotes!
    //
    // I suppose that if you wanted to name the buttons you could
    // either name them individually (could cause grief on retrieving
    // state from server) or, as in old-style input processing, put
    // the name-to-be-passed in .value.
    c[i].onclick = new Function (
      "postChange.start('lamp="
	+ i
	+ "&checked=' + document.lamps.lamp["
	+ i
	+ "].checked);"
    );
  }

  //  File upload.
  document.fileInput.send.onclick = function () {
    if (document.fileInput.datafile.value) {
      document.fileInput.submit();
    } else {
      return 0;
    }
  };

};
