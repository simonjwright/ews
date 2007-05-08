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
 *
 * $RCSfile$
 * $Revision$
 * $Date$
 * $Author$
 */

/**
 * Retrieve the state of all widgets when the page is (re)loaded.
 *
 * Expects a text/xml result:
 *
 * state
 *   time-format
 */
var stateRequest = new OneshotHttpInteraction 
  ("state.xml",
   null,
   function (r) {
    var x = r.responseXML;
    var value = x.getElementsByTagName("time-format")[0].firstChild.nodeValue;
    for (o = document.fTimeFormat.format.options, i = 0;
	 i < o.length;
	 i++) {
      o[i].selected = (o[i].value == value);
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
 * Assign event handlers and begin fetching.
 */
window.onload = function () {
  stateRequest.start();
  timeRequest.start();
  document.fTimeFormat.format.onchange = function() {
    for (o = document.fTimeFormat.format.options, i = 0;
	 i < o.length;
	 i++) {
      if (o[i].selected) {
	postChange.start("format=" +  o[i].value);
	break;
      }
    }
  };
  document.fileInput.send.onclick = function() {
    alert("foo " + document.fileInput.datafile.value);
    if (document.fileInput.datafile.value) {
      alert("document.fileInput.datafile.value=" 
	    + document.fileInput.datafile.value);
      document.fileInput.submit();
    } else {
      return 0;
    }
  };
};
