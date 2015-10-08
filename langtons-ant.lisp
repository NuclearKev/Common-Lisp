;;This program is Free Software! Do with it as you wish!
;;I'm sure you can run and edit this program without using
;;non-free software, so lets just say it's licensed under
;;the GPLv3 and above.

;;This program is a simulation of the class mathematical
;;idea of Langton's Ant. There is now support for Gnuplot in the works!

;;I've updated this to use complex coordinates for the location of the ant.
;;This makes everything more efficient and easier to understand. Enjoy.
;;Also note if you see a value in the *black-spaces* list that is
;;just a single number, this means the imaginary part is zero, or
;; in other words, y = 0.
;;For exmaple: *black-list* = '(#C(1 1) #C(2 3) 3), this means we have
;;3 points, (1,1), (2,3), and (3, 0). Enjoy.

;;You'd want to change this to where ever your gnuplot-out.lisp is
(load "/home/kevin/development/lisp/Common-Lisp/gnuplot-out.lisp")

(defparameter *black-spaces* '()) ;contains all blacked coordinates
(defparameter *steps* nil)
;;in order to make the move function smaller I needed to make this global
(defparameter *direction* nil) 

;;Checks to see if the current position is a black one
(defun check-black(l)
  (when (member l *black-spaces*)
    'nil
    't))

;;The sole purpose of this function is to make the code more readable
(defun add-black(l)
  (push l *black-spaces*))

;;The sole purpose of this function is to make the code more readable
(defun remove-black(l)
   (setf *black-spaces* (remove l *black-spaces*)))

;;Made keeping track of the steps easier
(defun steps()
  (if (equal *steps* 0)
      'nil
      't))

;;When on a black space, sets new direction and new space
(defun black-move(l)
  (remove-black l)
  (case *direction*
    ((up)    (setf *direction* 'left)
             (- l 1))
    ((down)  (setf *direction* 'right)
             (+ l 1))
    ((right) (setf *direction* 'up)
             (+ l #C(0 1)))
    ((left)  (setf *direction* 'down)
             (- l #C(0 1)))))

;;When on a white space, sets new direction and new space
(defun white-move(l)
  (add-black l)
  (case *direction*
    ((up)    (setf *direction* 'right)
             (+ l 1))
    ((down)  (setf *direction* 'left)
             (- l 1))
    ((right) (setf *direction* 'down)
             (- l #C(0 1)))
    ((left)  (setf *direction* 'up)
             (+ l #C(0 1)))))

;;Main movement function
;;Checks to see if the space is black, and chooses which function to call
;;Then passes the new postion back into itself
(defun move(l)
  (let ((is-black (check-black l)))
    (setf *steps* (- *steps* 1))
    (if (steps)
	(if is-black
	    (move (black-move l))
	    (move (white-move l)))
	'done)))

;;enter the x and y coordinates you'd like to start at
;;enter 'b for black, 'w for white (or anything but 'b)
;;enter 'up, 'right, 'down, or 'left for direction
;;enter the number of steps you'd like the ant to make
(defun start(x y b-or-w direction number-of-steps)
  (let ((c (complex x y)))
    (setf *steps* number-of-steps)
    (setf *black-spaces* '()) ;just in case you run this after you ran it before
    (setf *direction* direction)
    (if (eq 'b b-or-w)
	(progn (add-black c)
	       (move c))
	(move c))))

;;You want to pass *black-spaces* to this.
;;The reason this takes an argument is because you chop *black-spaces* up and
;;you don't want to destroy *black-spaces*.
;;The "write-to-file" function is found in the gnuplot-out.lisp program
;;Please note: You *must* run (new-file "foo.out") before running this function.
(defun graph-lang(blacklist)
  (if (null blacklist)
      'all-done
      (let ((c (car blacklist)))
	(let ((x (realpart c))
	      (y (imagpart c)))
	  (write-to-file "langton.out" x y)
	  (graph-lang (cdr blacklist))))))
      
