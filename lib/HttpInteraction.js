/**
 * @fileoverview Contains utility classes to support AJAX communications.
 * @author Simon Wright (simon@pushface.org)
 * Copyright (C) Simon Wright 2006-2022, <simon@pushface.org>
 *
 *  This package is free software; you can redistribute it and/or
 *  modify it under terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 3, or
 *  (at your option) any later version.  It is distributed in the
 *  hope that it will be useful, but WITHOUT ANY WARRANTY; without
 *  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 *  PARTICULAR PURPOSE.
 *
 *  As a special exception under Section 7 of GPL version 3, you are
 *  granted additional permissions described in the GCC Runtime
 *  Library Exception, version 3.1, as published by the Free Software
 *  Foundation.
 *
 *  You should have received a copy of the GNU General Public License
 *  and a copy of the GCC Runtime Library Exception along with this
 *  program; see the files COPYING3 and COPYING.RUNTIME respectively.
 *  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * Abstract base class to manage HTTP interactions for AJAX.
 *
 * When you call an HttpInteraction's start(), it sets up the request
 * and calls the overridden open(), whose task is to open the request
 * and initiate it by calling send() (if there is a query,
 * send(query); when the response arrives, calls 'handler'(request) --
 * you can get its responseText or responseXML as appropriate. If
 * 'interval' is non-null, repeats every 'interval' milliseconds.
 *
 * @param url      The URL to which the request is to be sent.
 * @param query    The query which is to be sent, if any.
 * @param handler  The handler to be called when the response arrives,
 *                 with parameter the completed request.
 * @param interval If non-null, the interval in milliseconds between
 *                 repeat requests.
 * @class          This is an abstract base class for HTTP interactions.
 * @constructor.
 * @returns        A new HttpInteraction.
 */
function HttpInteraction(url, query, handler, interval) {
  this.url = url;
  this.query = query;
  this.handler = handler;
  this.interval = interval;
  this.intervalTimer = null;
  this.request = null;
}

/**
 * Initiate the transaction.
 *
 * Preserves the current object and creates a nested function run() to
 * do the work of the transaction. run() uses the preserved object
 * instead of 'this' because, if we get called after the timeout,
 * 'this' is the Window.
 *
 * @param   overriding query (optional).
 * @returns void.
 */
HttpInteraction.prototype.start = function() {
  /* Preserve the current object in the closure for the nested function */
  var object = this;
  /* Override the query, if any was supplied */
  if (arguments.length > 0) {
    object.query = arguments[0];
  }
  /* The actual work is done in this nested function with the actual
   * HttpInteraction object in its closure, because when we get called
   * after the timeout 'this' is the Window */
  object.run = function () {
    if (!object.request) {
      /* Try to create the HttpRequest, coping with various
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
    /* open() is to be overridden by concrete extensions, and is
     * required to open the request (using "post" or "get" as
     * appropriate) and send the query (or null, if none). */
    object.open();
  }
  object.run();
}

HttpInteraction.prototype.open = function() {
    alert("HttpInteraction.open() not overridden.");
}


/**
 * Creates a new HttpInteraction which sends the request once.
 *
 * This class is to be used to perform updates, and therefore uses
 * POST. You can use a single URL and encode the actual update in the
 * query, or for some use cases ("quit", perhaps) you could use a null
 * query.
 *
 * @param url      The URL to which the request is to be sent.
 * @param query    The query which is to be sent, if any.
 * @param handler  The handler to be called when the response arrives,
 *                 with parameter the completed request.
 * @class OneshotHttpInteraction.
 * @extends HttpInteraction.
 * @constructor.
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
 * Creates a new HttpInteraction which sends the request repeatedly.
 *
 * This class is to be used to perform status requests, and therefore
 * uses GET. It doesn't allow you to pass a separate query - where
 * would it come from?
 *
 * @param url      The URL to which the request is to be sent.
 * @param handler  The handler to be called when the response arrives,
 *                 with parameter the completed request.
 * @param interval The interval in milliseconds between repeat requests.
 * @class CyclicHttpInteraction.
 * @extends HttpInteraction.
 * @constructor.
 * @returns A new CyclicHttpInteraction.
 */
  function CyclicHttpInteraction(url, handler, interval) {
  this.url = url;
  this.query = null;
  this.handler = handler;
  this.interval = interval;
  this.request = null;
}

CyclicHttpInteraction.prototype = new HttpInteraction();

CyclicHttpInteraction.prototype.open = function () {
  this.request.open("get", this.url, true);
  this.request.send(null);
}
