# Common-Lisp

## Big Number Library

This library is used to do arithmetic on huge numbers (consisting of thousands
of digits). So far the library can do all basic arithmetic, except for division
isn't fully functional yet. You can only divide wholly and will not get the
fractional portion. These functions are being rewritten at the moment.

To see the library in action, I recommend using the factorial program. Put in
1000! and see the huge number that comes out, accurate to every digit. If that's
not enough, try 5000!. You may have to wait a while for that one...

## Gnuplot-Out

gnuplot-out.lisp is a simple program that will print an ordered pair into a
file. This program is meant to be ran in a recursive function within the main
program. For example: you put a bunch of points in a list, then call this
function ordered pair by ordered pair.

## Land of Lisp Games:

* attack-of-the-robots.lisp

* dod-v1.lisp

* evolution.lisp

* graph-util.lisp

* orc-battle.lisp

* wizard-adventure.lisp

* wumpus.lisp

Also, those programs should be ran using CLISP. Although it only matters with
wumpus.lisp, they were written for CLISP and you may run into problems if you
don't use it.

## Langton's Ant

langtons-ant.lisp is a simulation of Langton's Ant, a mathematical problem where
an ant is located on a board filled with sqaures. You place the ant in any space
and you can color in any space (you have to do this manually for now). On a
white space, the ant turns 90 degrees to the right, and moves a space causing
the white space it was just on to become black. On a black space, the ant turns
90 degress to the left, and moves a space causing the black space it was just on
to become white. The fun part is that you will notice it's very random. Run the
program for 100,000 steps and view the blacklist. You will notice the ant starts
to go off in 1 direction (usually diagonally in one of the 4 quadrants). This is
the magic of Langton's Ant.

## Lisp Problems

lisp-problems.lisp is a bunch of problems from a _99 Lisp Problems_ webpage. It
does, however, contain one of my favorite functions. This function was used to
solve the combinations problem (it's called combination). Although extremely
messy, it gets the job done very well.

## Polynomials

polynomials.lisp was an attempt to create a program that could solve
polynomials. However, most of the program is a failure as of now. It can factor
wholly, multiply polynomials, and simplify them.
