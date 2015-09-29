;;This program is Free Software under the GNU GPLv3.0 and above

;;This program is *not* finished
;;Things to be added:
;;factorial function
;;start function

(defparameter *result* '()) ;global so you can see it easier 

;;corrects digits if it is greater than 10
;;this keeps it so that each element is 1 digit
(defun digit-correct(y n)
  (if (= n (length y))
      (setf y y)
      (progn (if (< 9 (nth n y))
		 (if (< (+ 1 n) (length y))
		     (progn (setf (nth (+ n 1) y) 
				  (+ (nth (+ n 1) y) (floor (nth n y) 10)))
			    (setf (nth n y) (mod (nth n y) 10))
			    (setf y y))
		     (progn (setf y (cons (floor (nth n y) 10) (reverse y)))
			    (setf y (reverse y))
			    (setf (nth n y) (mod (nth n y) 10))
			    (setf y y)))
		 (digit-correct y (+ 1 n)))
	     (digit-correct y (+ 1 n)))))

;;used to increment one of the numbers
(defun increment(x)
  (setf x (reverse x))
  (setf (nth 0 x) (+ 1 (nth 0 x)))
  (setf x (digit-correct x 0))
  (setf x (reverse x)))

;;compares each element in a list
;;if x is bigger pass nil, if y bigger pass t
(defun compare-digits(x y n)
  (if (= n (length x))
      'nil
      (cond ((> (nth n x) (nth n y))
	     'nil)
	    ((< (nth n x) (nth n y))
	     't)
	    (t
	     (compare-digits x y (+ 1 n))))))

;;compares the lengths of the lists and calls compare-digits if they are of
;;equal length
(defun compare(x y)
  (cond ((> (length x) (length y))
	 'nil)
	((< (length x) (length y))
	 't)
	(t
	 (compare-digits x y 0))))

;;adds zeros the front and back of the list being multiplied
;;this prevents errors like: "NIL is not a number"
(defun pad-with-zeros(x n)
  (if (> n 0)
      (progn (setf x (cons 0 x))
	     (setf x (cons 0 (reverse x)))
	     (setf x (reverse x))
	     (pad-with-zeros x (- n 1)))
      (setf x x)))

;;removes the added zeros so we have the original list back
(defun remove-zeros(x n)
    (if (> n 0)
      (progn (setf x (cdr x))
	     (setf x (cdr (reverse x)))
	     (setf x (reverse x))
	     (remove-zeros x (- n 1)))
      (setf x x)))

;;actually does the multiplication, *will be explained more*
(defun multiply-loop(x y n i)
  (if (= n (- (length x) (- (length y) 1)))
      't
      (progn (if (= 1 (length y))
		 (setf *result* (cons (* (nth n x) (nth i y)) *result*))
		 (setf *result* (cons 
		  (+ (* (nth n x) (nth (- i 1) y)) (* (nth (+ 1 n) x) (nth i y)))
		  *result*)))
	     (multiply-loop x y (+ 1 n) i))))

;;must be called before the multiply-loop function (this is a setup function) 
(defun multiply(x y)
  (setf *result* '())
  (setf x (pad-with-zeros x (- (length y) 1)))
  (setf y (reverse y))
  (multiply-loop x y 0 (- (length y) 1))
  (setf *result* (reverse (digit-correct *result* 0))))

;;computes the factorial of the number x (in list form)
(defun factorial(x y z)
  (if (compare x y)
      'done
      (progn (setf z (multiply z y))
	     (princ z)
	     (factorial x (increment y)  z))))
