;;This program is Free Software! Do with it as you wish!
;;I'm sure you can run and edit this program without using
;;non-free software, so lets just say it's licensed under
;;the GPLv3 and above.

;;This program is a simulation of the class mathematical
;;idea of Langton's Ant. There is not GUI, but you can
;;put the values found in the *black-spaces* list into
;;a spreadsheet or calculator and see what you get.

;;Please note that within the *black-spaces* list there
;;will be a bunch of numbers that *don't* look like
;;coordinates. This is normal, each pair of numbers
;;starting from the beginning are coordinates.
;;For example '(0 0 1 2 3 4), (0,0) is a coordinate of
;;a black space; so if (1,2) and (3,4). Enjoy.

(defparameter *black-spaces* '()) ;contains all blacked coordinates
(defparameter *steps* nil) 
(defparameter *index* 0) ;used in the remove-black function

;;takes the first element, compares to x, if it's not equal
;;the those coordinates are out so we pass that same list
;;back, but without the first ordered pair (cddr blacklist).
;;if it is equal to x, then check for y, if not send the
;;list minus that ordered pair. If yes, pass true!
(defun check-black(x y blacklist)
  (cond ((equal x (car blacklist))
	 (cond ((equal y (cadr blacklist))
		't)
	       (t
		(check-black x y (cddr blacklist)))))
	((equal nil (car blacklist))
	 'nil)
	(t
	 (check-black x y (cddr blacklist)))))

;;basically the check-black function but keeps track of the index
;;this makes it easy to remove coordinates
(defun remove-black(x y blacklist)
  (cond ((equal x (car blacklist))
	 (if (equal y (cadr blacklist))
	     (progn (setf *black-spaces*
		   (remove y
			   (remove x *black-spaces* :start *index* :end (+ *index* 1))
			   :start *index* :end (+ *index* 1)))
		    (setf *index* 0))
	     (progn (setf *index* (+ *index* 2))
		    (remove-black x y (cddr blacklist)))))
	(t
	 (progn (setf *index* (+ *index* 2))
		(remove-black x y (cddr blacklist))))))

(defun add-black(x y)
  (push y *black-spaces*)
  (push x *black-spaces*))

;;made keeping track of the steps easier
(defun steps()
  (if (equal *steps* 0)
      'nil
      't))

;;moving logic, the power of cond!
(defun move(x y direction)
  (setf *steps* (- *steps* 1))
  (if (steps)
      (progn (cond ((eq direction 'up)
		    (if (check-black x y *black-spaces*) 
			(progn (remove-black x y *black-spaces*) ;black
			       (move (- x 1) y 'left))
			(progn (add-black x y) ;white
			       (move (+ x 1) y 'right))))
		   ((eq direction 'right)
		    (if (check-black x y *black-spaces*) 
			(progn (remove-black x y *black-spaces*) ;black
			       (move x (+ y 1) 'up))
			(progn (add-black x y) ;white
			       (move x (- y 1) 'down))))
		   ((eq direction 'left)
		    (if (check-black x y *black-spaces*)
			(progn (remove-black x y *black-spaces*) ;black
			       (move x (- y 1) 'down))
			(progn (add-black x y) ;white
			       (move x (+ y 1) 'up))))
		   ((eq direction 'down)
		    (if (check-black x y *black-spaces*)
			(progn (remove-black x y *black-spaces*) ;black
			       (move (+ x 1) y 'right))
			(progn (add-black x y)
			       (move (- x 1) y 'left))))))
      'done))

;;enter the x and y coordinates you'd like to start at
;;enter 'b for black, 'w for white (or anything but 'b)
;;enter 'up, 'right, 'down, or 'left for direction
;;enter the number of steps you'd like the ant to make
(defun start(x y b-or-w direction number-of-steps)
  (setf *steps* number-of-steps)
  (setf *black-spaces* '()) ;just in case you run this after you ran it before
  (if (eq 'b b-or-w)
      (add-black x y)
      (move x y direction)))
