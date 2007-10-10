/**
 * @fileoverview Contains utility classes to support AJAX communications.
 * @author Simon Wright (simon@pushface.org)
 * Copyright (C) Simon Wright <simon@pushface.org>
 *
 * This package is free software; you can redistribute it and/or
 * modify it under terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version. This package is distributed in the
 * hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE. See the GNU General Public License for more details. You
 * should have received a copy of the GNU General Public License
 * distributed with this package; see file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 * $RCSfile$
 * $Revision$
 * $Date$
 * $Author$
 */

/**
 * When you call an HttpInteraction's start(), it sends the 'query'
 * using GET; when the response arrives, calls 'handler'(request) --
 * you can get its responseText or responseXML as appropriate. If
 * 'interval' is non-zero, repeats every 'interval' milliseconds.
 * @class This is an abstract base class for HTTP interactioons.
 * @constructor
 */
function HttpInteraction(query, handler, interval) {
  this.query = query;
  this.handler = handler;
  this.interval = interval;
  this.intervalTimer = null;
  this.request = null;
}

HttpInteraction.prototype.start = function() {
  /* preserve the current object in the closure for the nested function */
  var object = this;
  /* override the query, if any was supplied */
  if (arguments.length > 0) {
    object.query = arguments[0];
  }
  /* the actual work is done in a nested function with the actual
   * HttpInteraction object in its closure, because when we get called
   * after the timeout 'this' is the Window */ 
  object.run = function () {
    if (!object.request) {
      /* try to create the HttpRequest, coping with various
       * browsers/versions */
      if (window.XMLHttpRequest) {
	object.request = new XMLHttpRequest();
      } else if (window.ActiveXObject) {
	object.request = new ActiveXObject("Microsoft.XMLHTTP");
      } else {
	alert("unable to create XMLHTTP request");
      }
    } else if (object.request.readyState != 0) {
      object.request.abort();
    }
    object.request.onreadystatechange = function () {
      if (object.request.readyState == 4) {
	if (object.request.status >= 200 && object.request.status < 300) {
	  if (object.handler) {
	    object.handler(object.request);
	  }
	  if (object.interval && !object.intervalTimer) {
	    object.intervalTimer = setInterval(object.run, object.interval);
	  }
	} else {
	  if (object.intervalTimer) {
	    clearInterval(object.intervalTimer);
	    object.intervalTimer = null;
	  }
	  alert("An HttpInteraction error occurred: "
		+ object.request.statusText
		+ "\nRefresh page to restart");
	}                    
      }
    };
    object.open();
  }
  object.run();
}


/**
 * @class OneshotHttpInteraction
 * @extends HttpInteraction
 * @constructor
 * @returns A new OneShotHttpInteraction.
 */
function OneshotHttpInteraction(url, query, handler) {
  this.url = url;
  this.query = query;
  this.handler = handler;
  this.interval = 0;
  this.request = null;
}

OneshotHttpInteraction.prototype = new HttpInteraction();

OneshotHttpInteraction.prototype.open = function () {
  this.request.open("post", this.url, true);
  this.request.send(this.query);
}


/**
 * @class CyclicHttpInteraction
 * @extends HttpInteraction
 * @constructor
 * @returns A new CyclicHttpInteraction.
 */
  function CyclicHttpInteraction(url, handler, interval) {
  this.url = url;
  this.handler = handler;
  this.interval = interval;
  this.request = null;
}

CyclicHttpInteraction.prototype = new HttpInteraction();

CyclicHttpInteraction.prototype.open = function () {
  this.request.open("get", this.url, true);
  this.request.send(null);
}
