# Common-Lisp

Some of the programs in this repo are not originally written by me. The following were games (or programs) that we taught in the amazing book The Land of Lisp:

attack-of-the-robots.lisp

dod-v1.lisp

evolution.lisp

graph-util.lisp

orc-battle.lisp

wizard-adventure.lisp

wumpus.lisp

Also, those programs should be ran using CLISP. Although it only matters with wumpus.lisp, they were written for CLISP and you may run into problems if you don't use it.

langtons-ant.lisp is a simulation of Langton's Ant, a mathematical concept where an ant is located on a board filled with sqaures. You place the ant in any space and you can color in any space (you have to do this manually for now). On a white space, the ant turns 90 degrees to the right, and moves a space causing the white space it was just on to become black. On a black space, the ant turns 90 degress to the left, and moves a space causing the black space it was just on to become white. The fun part is that you will notice it's very random. Run the program for 100,000 steps and view the blacklist. You will notice the ant starts to go off in 1 direction (usually diagonally in one of the 4 quadrants). This is the magic of Langton's Ant. 

gnuplot-out.lisp is a simple program that will print an ordered pair into a file. This program is meant to be ran in a recursive function within the main program. For example: you put a bunch of points in a list, then call this function ordered pair by ordered pair.

big-number.lisp is a program that will do huge factorials (like 1000!). It uses a weird multiplication method that requires all the values to be in lists. Luckily, it is written in LISP, the best listing language! You can go up to 3000! and it takes about 14 minutes. I was too impatient to try anything bigger.