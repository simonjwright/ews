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
 * Create an HTTP interaction object which GETs 'ajaxTime' every
 * second. It expects a text/plain result, which it pastes into the
 * document at the element identified as 'timeDisplay'. 
 */
var timeRequest = new CyclicHttpInteraction
  ("ajaxTime",
   function (r) {
    document.getElementById("timeDisplay").innerHTML = r.responseText;
   },
   1000);

/**
 * Assign event handlers and begin fetching.
 */
window.onload = function () {
  timeRequest.start();              
};
