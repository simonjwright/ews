/*
 * Copyright (C) Simon Wright <simon@pushface.org>
 *
 * This unit is free software; you can redistribute it and/or modify
 * it as you wish. This unit is distributed in the hope that it will
 * be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * This script is used in the AJAX demo for EWS.
 * 
 * It relies on the zXml library from http://www.nczonline.net/downloads/.
 *
 * $RCSfile$
 * $Revision$
 * $Date$
 * $Author$
 */

var oXmlHttp = null;
var iInterval = 1000;

function getTime() {

  if (!oXmlHttp) {
    oXmlHttp = zXmlHttp.createRequest();
  } else if (oXmlHttp.readyState != 0) {
    oXmlHttp.abort();
  }    
  
  oXmlHttp.open("get", "ajaxTime", true);
  oXmlHttp.onreadystatechange = function () {               
    
    if (oXmlHttp.readyState == 4) {
      if (oXmlHttp.status == 200) {
	
	var eTimeDisplay = document.getElementById("timeDisplay");
	eTimeDisplay.innerHTML = oXmlHttp.responseText;

	setTimeout(getTime, iInterval);
             
      } else {
	alert("An error occurred: "+ oXmlHttp.statusText);
      }                    
    }
  };    
  
  oXmlHttp.send(null);       
  
}

//if Ajax is enabled, assign event handlers and begin fetching
window.onload = function () {
  if (zXmlHttp.isSupported()) {
    getTime();              
  } else {
    alert("zXmlHttp.isSupported(): false");
  }
};
