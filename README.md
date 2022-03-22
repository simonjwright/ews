# Embedded Web Server #

## Introduction ##

EWS is a web server construction kit, designed for embedded applications using the GNAT Ada compiler.

The project is hosted on [Github](https://github.com/simonjwright/ews).

## Prerequisites ##

EWS requires GNAT (GNAT GPL 2012 or later, FSF GCC 4.8.0 or later) and XML/Ada. GNAT GPL 2013 (and, hopefully, later) includes XML/Ada; earlier versions supply XML/Ada as source which has to be built and installed.

In the top-level directory, `make` will build the library, and `make demo` will create a server `doc/ews_demo`, which when executed will listen on port 8080 and respond with the web in the `doc/` directory.

The package can be installed with the compiler by `make install` (more likely, `sudo make install`). You can install in an alternative place by setting `prefix`, for example

    $ make install prefix=~/local/ews

## Documentation ##

The facilities available in EWS, and the code for the demonstration, are described in `doc/ews.pdf`, which is derived from `doc/ews.w`. As well as being the document source, `ews.w` also acts as the source code using the [Literate Programming](http://www.literateprogramming.com/) facilities of [nuweb.py](https://github.com/simonjwright/nuweb.py).

## Demonstration ##

If you're seeing this page via the demonstration (`ews_demo`), you can view a page with [AJAX](https://en.wikipedia.org/wiki/Ajax_\(programming\)) content [here](ajax.html).
