Koch/Julia fractal generator
================================
Entry to the 2014 Functional Programming competition

About the program:
This program does two separate things
	1. Generates Koch snowflake fractals with variable angles and base shapes
	2. Generates simply coloured Julia set fractals.
You can run both of these functions by following the instructions on screen after starting the main function
of the program.

--------
Installation:

The program uses a very nice graphics library called Gloss:
http://gloss.ouroborus.net/

You need to install it by executing:

cabal update
cabal install gloss-examples

This should download and install all the necessary dependencies.
If you get this error -

gloss-raster-1.8.1.2 failed during the building phase. The exception was:
ExitFailure 1

- then try installing llvm:
sudo apt-get install llvm

and do the cabal install again.

-------------
To run it, you need to compile it with:

ghc -threaded --make main.hs

and run:
./main

The koch function won't work when you start it from emacs/ghci or using runghc from terminal, freeglut
will complain about missing fonts.

------------
Controls:

The Koch generator is controlled using arrow keys, numbers 1-3 and some character keys, specific instructions
will be displayed on screen when you run the generator.
