;;This program is Free Software under the GNU GPLv3.0 and above

;;This program will take the factorial of any number you give it.
;;It stores each digit of the number in a list so that you can deal with huge
;;numbers. 
;;In the comments, I use the terms MSD and LSD, which means, Most Significant 
;;Digit and Least Significant Digit.

;;BUG: You can go up to 99 perfectly, however, after that you will have zeros in
;;the beginning of the result list.

(defparameter *result* '()) ;this will be phased out soon enough

(defun digit-correct-middle-of-list(y n)
  (setf (nth (+ n 1) y) (+ (nth (+ n 1) y) (floor (nth n y) 10))) ;takes the MSD
  (setf (nth n y) (mod (nth n y) 10)) ;takes the LSD
  y)

(defun digit-correct-end-of-list(y n)
  (setf y (cons (floor (nth n y) 10) (reverse y))) ;takes MSD
  (setf y (reverse y))
  (setf (nth n y) (mod (nth n y) 10)) ;takes LSD
  y)

(defun correction-main(y n)
  (if (< 9 (nth n y))
      (if (< (+ 1 n) (length y))
	  (setf y (digit-correct-middle-of-list y n))
	  (setf y (digit-correct-end-of-list y n)))
      y))


;;Corrects digits if it is greater than 10.
;;This keeps it so that each element is 1 digit.
(defun digit-correct(y n)
  (if (equal n (length y))
      y
      (digit-correct (correction-main y n) (+ 1 n))))

;;Converts a regular whole number into a listed version.
;;For example, 123 -> '(1 2 3)
(defun convert-to-list(x temp-list)
  (if (> x 0)
      (progn (setf temp-list (cons (mod x 10) temp-list)) ;passes them in 1 LSD at a time
	     (convert-to-list (floor x 10) temp-list)) ;cuts off the current LSD, loops again
      temp-list))

;;Used to increment one of the numbers
(defun increment(x)
  (setf x (reverse x))
  (setf (nth 0 x) (+ 1 (nth 0 x)))
  (setf x (digit-correct x 0))
  (setf x (reverse x)))

;;Compares each element in a list
;;If x is bigger pass nil, if y bigger pass t
(defun compare-digits(x y n)
  (if (equal n (length x))
      'nil
      (cond ((> (nth n x) (nth n y))
	     'nil)
	    ((< (nth n x) (nth n y))
	     't)
	    (t
	     (compare-digits x y (+ 1 n))))))

;;Compares the lengths of the lists and calls compare-digits if they are of
;;equal length.
(defun compare(x y)
  (cond ((> (length x) (length y))
	 'nil)
	((< (length x) (length y))
	 't)
	(t
	 (compare-digits x y 0))))

;;Adds zeros the front and back of the list being multiplied
;;This prevents errors like: "NIL is not a number"
;;The number of zeros added: (length y) - 1
(defun pad-with-zeros(x n)
  (if (> n 0)
      (progn (setf x (cons 0 x))
	     (setf x (cons 0 (reverse x)))
	     (setf x (reverse x))
	     (pad-with-zeros x (- n 1)))
      x))

;;Removes the added zeros so we have the original list back
(defun remove-zeros(x n)
    (if (> n 0)
      (progn (setf x (cdr x))
	     (setf x (cdr (reverse x)))
	     (setf x (reverse x))
	     (remove-zeros x (- n 1)))
      x))

;;Actually does the multiplication.
;;We take number 'x' and pad it with zeros to start.
;;Then we take number 'y' and reverse it.
;;Now it takes these and multiples down the line.
;;For example, x = '(1 2 3) y = '(4 5)
;;Pad zeros: '(0 1 2 3 0)
;;Reverse y: '(5 4)
;;Multiply:
;;          '(0 1 2 3 0)
;;          '(5 4)
;;            0 4 <- add these together then added to the *result* list
;;
;;          '(0 1 2 3 0)
;;            '(5 4)
;;              5 8
;;
;;          '(0 1 2 3 0)
;;              '(5 4)
;;               10 12 <- these will get fixed in the digit-correct function later
;;
;;          '(0 1 2 3 0)
;;                '(5 4)
;;                  15 0
;;
;;In *result* will be '(15 22 13 4) *note that it's backwards
;;In the next function (multiply) it passes *result* into digit-correct where it is
;;fixed. This is why it's backwards (because digit-correct needs them backwards)
;;You then have '(5 3 5 5) in *result*, it gets reversed to have '(5 5 3 5) YAY!

;; (defun multiply-more-digits(x y n i temp-buffer)
;;   (if (equal i (length y))
;;       temp-buffer
;;       (multiply-more-digits x y (+ 1 n) (+ 1 i) 
;; 			    (+ temp-buffer (* (nth n x) (nth (- i 1) y))))))

(defun multiply-loop(x y n i)
  (if (equal n (- (length x) (- (length y) 1)))
      't
      (progn (if (equal 1 (length y))
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
      z
      (progn (setf z (multiply z y))
	     (factorial x (increment y)  z))))

(defun take-factorial(f)
  (setf f (convert-to-list f '()))
  (factorial f '(2) '(1)))
