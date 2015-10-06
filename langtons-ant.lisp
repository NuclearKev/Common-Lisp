;;This program is Free Software! Do with it as you wish!
;;I'm sure you can run and edit this program without using
;;non-free software, so lets just say it's licensed under
;;the GPLv3 and above.

;;This program is a simulation of the class mathematical
;;idea of Langton's Ant. There is not GUI, but you can
;;put the values found in the *black-spaces* list into
;;a spreadsheet or calculator and see what you get.

;;I've updated this to use complex coordinates for the location of the ant.
;;This makes everything more efficient and easier to understand. Enjoy.
;;Also note if you see a value in the *black-spaces* list that is
;;just a single number, this means the imaginary part is zero, or
;; in other words, y = 0.
;;For exmaple: *black-list* = '(#C(1 1) #C(2 3) 3), this means we have
;;3 points, (1,1), (2,3), and (3, 0). Enjoy.

(defparameter *black-spaces* '()) ;contains all blacked coordinates
(defparameter *steps* nil) 

;;Checks to see if the current position is a black one
(defun check-black(l)
  (when (member l *black-spaces*)
    'nil
    't))

;;The sole purpose of this is to make the code more readable
(defun add-black(l)
  (push l *black-spaces*))

;;Made keeping track of the steps easier
(defun steps()
  (if (equal *steps* 0)
      'nil
      't))

;;Movement logic function
;;Very messy, working on making it look better!
(defun move(l direction)
  (setf *steps* (- *steps* 1))
  (if (steps)
      (cond ((eq direction 'up)
	     (if (check-black l) 
		 (progn (setf *black-spaces* (remove l *black-spaces*)) ;black
			(move (- l 1) 'left))
		 (progn (add-black l) ;white
			(move (+ l 1) 'right))))
	    ((eq direction 'right)
	     (if (check-black l) 
		 (progn (setf *black-spaces* (remove l *black-spaces*)) ;black
			(move (+ l #C(0 1)) 'up))
		 (progn (add-black l) ;white
			(move (- l #C(0 1)) 'down))))
	    ((eq direction 'left)
	     (if (check-black l)
		 (progn (setf *black-spaces* (remove l *black-spaces*)) ;black
			(move (- l #C(0 1)) 'down))
		 (progn (add-black l) ;white
			(move (+ l #C(0 1)) 'up))))
	    ((eq direction 'down)
	     (if (check-black l)
		 (progn (setf *black-spaces* (remove l *black-spaces*)) ;black
			(move (+ l 1) 'right))
		 (progn (add-black l)
			(move (- l 1) 'left)))))
      'done))

;;enter the x and y coordinates you'd like to start at
;;enter 'b for black, 'w for white (or anything but 'b)
;;enter 'up, 'right, 'down, or 'left for direction
;;enter the number of steps you'd like the ant to make
(defun start(x y b-or-w direction number-of-steps)
  (let ((c (complex x y)))
    (setf *steps* number-of-steps)
    (setf *black-spaces* '()) ;just in case you run this after you ran it before
    (if (eq 'b b-or-w)
	(progn (add-black c)
	       (move c direction))
	(move c direction))))

