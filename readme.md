Koka: a function-oriented language with effect inference
--------------------------------------------------------

 To build Koka from source you need to install:

  * The [Haskell platform](http://www.haskell.org/platform) (known to work with 2013.2.0.0 or later).
  * libncurses5-dev (on Linux)
  * node.js (optional)

After installing the above tools, go to the Koka directory and type:

    > cabal update && cabal install

This will install Koka locally for the current user.

Starting out
------------

After running `koka`, the Koka interpreter will start:

    __          _
    | |        | |
    | | __ ___ | | __ __ _
    | |/ // _ \| |/ // _` | welcome to the koka interpreter
    |   <| (_) |   <| (_| | version 0.5.0-dev (debug), Apr  8 2013
    |_|\_\\___/|_|\_\\__,_| type :? for help

    loading: std/core

Now you can test some expressions:

    > println("hi koka")
    hi koka

    > :t "hi"
    string

    > :t println("hi")
    console ()

Or load a demo:

    > :l demo/collatz
    compile: lib/demo/collatz.kk
    check  : demo/collatz
    modules:
      demo/collatz

    > main()
    Collatz(27) took 111 steps.

Or a browser based demo:

    > :l demo/dom/starfield
    loading: demo/dom/starfield
    loading: sys/dom
    loading: sys/dom/types
    loading: sys/dom/document
    loading: sys/dom/html/window
    loading: sys/dom/html/htmlElement
    loading: sys/dom/html/htmlCanvasElement
    loading: sys/dom/html/canvasRenderingContext2d
    loading: sys/dom/css/cssStyleDeclaration
    loading: demo/dom/microsoftLogo
    loading: sys/dom/html/htmlTableElement
    loading: sys/dom/html/htmlTableRowElement
    loading: sys/dom/html/htmlTableDataCellElement
    modules:
      demo/dom/starfield

    > main()

And quit the interpreter:

    > :q

    Before the effect one believes in different causes than one does after the effect.
     -- Friedrich Nietzsche

Have fun!
  Daan Leijen
  Lars Petersen


Development
-----------

For development we recommend:

  * The excellent [SublimeText](http://www.sublimetext.com) text editor. There is a full Koka and Haskell
    language mode for SublimeText (run `jake sublime` to install the Koka mode on your system).

To generate a browsable haddock documentation run:

    cabal haddock --executable --hyperlink-source
