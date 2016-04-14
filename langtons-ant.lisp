;; This program is Free Software! Do with it as you wish!
;; I'm sure you can run and edit this program without using
;; non-free software, so lets just say it's licensed under
;; the GPLv3 and above.

;; This program is a simulation of the class mathematical
;; idea of Langton's Ant.

;; Status: Everything works, include Gnuplot graphing. However, I wish to
;; eventually include the ability to place black space on the grid in the
;; beginning in order to get different outcomes. 

;; I've updated this to use complex coordinates for the location of the ant.
;; This makes everything more efficient and easier to understand. Enjoy.
;; Also note if you see a value in the *black-spaces* list that is
;; just a single number, this means the imaginary part is zero, or
;; in other words, y = 0.
;; For exmaple: black-spaces = '(#C(1 1) #C(2 3) 3), this means we have
;; 3 points, (1,1), (2,3), and (3, 0). Enjoy.

;; You'd want to change this to where ever your gnuplot-out.lisp is
(load "/home/kdb/code/common-lisp/gnuplot-out.lisp")

;; Checks to see if the current position is a black one
(defun check-black (cur-pos black-spaces)
  (when (member cur-pos black-spaces)
    nil))

;; The sole purpose of this function is to make the code more readable
(defun add-black (cur-pos black-spaces)
  (cons cur-pos black-spaces))

;; The sole purpose of this function is to make the code more readable
(defun remove-black (cur-pos black-spaces)
  (remove cur-pos black-spaces))

(defun position-changer (cur-pos dir is-black)
  (if is-black
      (case dir
	((up)    (- cur-pos 1))
	((down)  (+ cur-pos 1))
	((right) (+ cur-pos #C(0 1)))
	((left)  (- cur-pos #C(0 1))))
      (case dir				
	((up)    (+ cur-pos 1))
	((down)  (- cur-pos 1))
	((right) (- cur-pos #C(0 1)))
	((left)  (+ cur-pos #C(0 1))))))

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

;; Main movement function
;; Checks to see if the space is black, and chooses which function to call
;; Then passes the new postion back into itself
(defun move (cur-pos dir steps black-spaces)		;l is current position
  (let ((is-black (check-black cur-pos black-spaces))
	(next-steps (- steps 1)))
    (if (equal 0 steps)
	black-spaces
	(if is-black
	    (move (position-changer cur-pos dir is-black)
		  (direction-changer dir t) next-steps
		  (remove-black cur-pos black-spaces))
	    (move (position-changer cur-pos dir is-black)
		  (direction-changer dir nil) next-steps
		  (add-black cur-pos black-spaces))))))


;; enter the x and y coordinates you'd like to start at
;; enter 'b for black, 'w for white (or anything but 'b)
;; enter 'up, 'right, 'down, or 'left for direction
;; enter the number of steps you'd like the ant to make
(defun start (x y b-or-w direction number-of-steps)
  (let ((c (complex x y)))
    (if (eq 'b b-or-w)
	(move c direction number-of-steps (list c))
	(move c direction number-of-steps '()))))

;; You want to pass the start function to this.
;; The "write-to-file" function is found in the gnuplot-out.lisp program
;; Please note: You *must* run (new-file "langton.out") before running this
;; function.
(defun graph-lang (blacklist)
  (if (null blacklist)
      'all-done
      (let ((c (car blacklist)))
	(let ((x (realpart c))
	      (y (imagpart c)))
	  (write-to-file "langton.out" x y)
	  (graph-lang (cdr blacklist))))))

