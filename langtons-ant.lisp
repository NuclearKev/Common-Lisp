(defparameter *black-spaces* '())
(defparameter *steps* nil)
(defparameter *index* 0)

(defun check-black(x y blacklist)
  (cond ((equal x (car blacklist))
	 (progn (cond ((equal y (cadr blacklist))
		       't)
		      (t
		       (check-black x y (cddr blacklist))))))
	((equal nil (car blacklist))
	 'nil)
	(t
	 (check-black x y (cddr blacklist)))))

;does not replace *black-spaces* with the new list!
(defun remove-black(x y blacklist)
  (cond ((equal x (car blacklist))
	 (progn (cond ((equal y (cadr blacklist))
		       (progn (setf *black-spaces*
				    (remove y
					    (remove x *black-spaces* :start *index* :end (+ *index* 1))
					    :start *index* :end (+ *index* 1)))
			      (setf *index* 0)))
		      (t
		       (progn (setf *index* (+ *index* 2))
			      (check-black x y (cddr blacklist)))))))
	(t
	 (progn (setf *index* (+ *index* 2))
		(check-black x y (cddr blacklist))))))

(defun add-black(x y)
  (push y *black-spaces*)
  (push x *black-spaces*))

(defun steps()
  (if (equal *steps* 0)
      'nil
      't))

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

(defun start(x y b-or-w direction number-of-steps)
  (setf *steps* number-of-steps)
  (if (eq 'b b-or-w)
      (add-black x y)
      (move x y direction))
  (move x y direction))
