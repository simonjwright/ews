# Embedded Web Server #

## Introduction ##

EWS is a web server construction kit, designed for embedded applications using the GNAT Ada compiler.

The project is hosted on [Github](https://github.com/simonjwright/ews).

## Building with Alire ##

See the [Alire documentation](https://alire.ada.dev/docs/#introduction).

After you've installed Alire, start a small test program by saying
```
alr init --bin ews_test
cd ews_test
alr with ews
```

This generates a project (GPR) file at the top level and a source file `src/ews_test.adb`.

Create a directory `html` and in it `index.html` containing (for example)
```
<html>
<body>
<font color="red">Hello world!</font>
</body>
</html>
```

The contents of `html/` need to be converted to Ada source code in `src/`, which will form the static content of the pages served by `ews_test`. This can be done as part of the Alire build process; to do this, edit `alire.toml` to add the lines
```
[[actions]]
type = "pre-build"
command = ["ews_generator", "--input-dir", "html", "--output-dir", "src"]
```

Edit the contents of `src/ews_test.adb` to contain
```
with EWS_Htdocs;
with EWS.Server;
procedure EWS_Test is
begin
   EWS.Server.Serve (Using_Port => 8088,
                     Tracing    => True);
end EWS_Test;
```

and build and run with
```
alr run
```

Now, in your browser, open `localhost:8088` (possibly, `127.0.0.1:8088`) to see your exciting web page!

To see a worked example of dynamic content, check out the document [ews.pdf](https://github.com/simonjwright/ews/blob/master/demonstrator/ews.pdf).

## Building outside Alire ##

EWS requires GNAT and XML/Ada, and uses Ada 2012 features.

In the top-level directory, `make` will build the library, and `make demo` will create a server `doc/ews_demo`, which when executed will listen on port 8080 and respond with the web in the `doc/` directory.

The package can be installed with the compiler by `make install` (more likely, `sudo make install`). You can install in an alternative place by setting `prefix`, for example

    $ make install prefix=~/local/ews

## Dynamic web ##

The facilities available in EWS, and the code for a demonstration, are described in `demonstrator/ews.pdf`, which is derived from `demonstrator/ews.w`. As well as being the document source, `ews.w` also acts as the source code using the [Literate Programming](http://www.literateprogramming.com/) facilities of [nuweb.py](https://github.com/simonjwright/nuweb.py).

### Demonstration ###

If you're seeing this page via the demonstration (`ews_demo`), you can view a page with [AJAX](https://en.wikipedia.org/wiki/Ajax_\(programming\)) content [here](ajax.html).
