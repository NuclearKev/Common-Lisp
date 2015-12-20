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

;;When on a black space, sets new direction and new space
(defun black-move(l dir)
  (remove-black l)
  (case dir
    ((up)    (- l 1))
    ((down)  (+ l 1))
    ((right) (+ l #C(0 1)))
    ((left)  (- l #C(0 1)))))

;;When on a white space, sets new direction and new space
(defun white-move(l dir)
  (add-black l)
  (case dir
    ((up)    (+ l 1))
    ((down)  (- l 1))
    ((right) (- l #C(0 1)))
    ((left)  (+ l #C(0 1)))))

(defun direction-changer (dir is-black)
  (if is-black
      (case dir
	((up)    'left)
	((down)  'right)
	((right) 'up)
	((left)  'down))
      (case dir
	((up)    'right)
	((down)  'left)
	((right) 'down)
	((left)  'up))))

;;Main movement function
;;Checks to see if the space is black, and chooses which function to call
;;Then passes the new postion back into itself
(defun move(l dir steps)		;l is current position
  (let ((is-black (check-black l)) (next-steps (- steps 1)))
    (if (equal 0 steps)
	'done
	(if is-black
	    (move (black-move l dir) (direction-changer dir t) next-steps)
	    (move (white-move l dir) (direction-changer dir nil) next-steps)))))


;;enter the x and y coordinates you'd like to start at
;;enter 'b for black, 'w for white (or anything but 'b)
;;enter 'up, 'right, 'down, or 'left for direction
;;enter the number of steps you'd like the ant to make
(defun start(x y b-or-w direction number-of-steps)
  (let ((c (complex x y)))
    (setf *black-spaces* '()) ;just in case you run this after you ran it before
    (if (eq 'b b-or-w)
	(add-black c))
    (move c direction number-of-steps)))

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
      
