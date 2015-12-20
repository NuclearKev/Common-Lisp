;;This program is Free Software under the GNU GPLv3.0 and above

;;This program will take the factorial of any number you give it.
;;It stores each digit of the number in a list so that you can deal with huge
;;numbers. 
;;In the comments, I use the terms MSD and LSD, which means, Most Significant 
;;Digit and Least Significant Digit.

;;There still may be a few changes that I'll add to make this more Lispy, but
;;for now, try doing 1000!; it has ~2500 digits in it and is awesome!

(defparameter *result* '()) ;makes life easier

;; Fix any digits that are over 9.
;; This function is quite complicated; however, with some thought, it can be
;; figured out.
(defun digit-fixer (org-list)
  (let ((cur-elem (car org-list)) (rest-of-list (cdr org-list)))
    (let ((end-of-list (null rest-of-list)))
      (if (> cur-elem 9)
	  (let ((msd (floor cur-elem 10)) (lsd (mod cur-elem 10)))
	    (if end-of-list
		(list lsd msd)
		(let ((new-next-elem (+ msd (car rest-of-list))))
		  (cons lsd (digit-fixer (cons new-next-elem (cdr rest-of-list)))))))
	  (if end-of-list
	      (list cur-elem)
	      (cons cur-elem (digit-fixer rest-of-list)))))))

;;Converts a regular whole number into a listed version.
;;For example, 123 -> '(1 2 3)
(defun convert-to-list (number)
  (if (> number 0)
      (append (convert-to-list (floor number 10)) (list (mod number 10)))
      nil))

;;Used to increment one of the numbers
(defun increment(x)
  (setf x (reverse x))
  (setf (nth 0 x) (+ 1 (nth 0 x)))
  (setf x (digit-fixer x))
  (setf x (reverse x)))

;;Compares each element in a list
;;If x is bigger pass nil, if y bigger pass t
(defun compare-digits(x y n)
  (let ((nth-x (nth n x)) (nth-y (nth n y)))
    (if (equal n (length x))
	nil
	(cond ((> nth-x nth-y)
	       nil)
	      ((< nth-x nth-y)
	       t)
	      (t
	       (compare-digits x y (+ 1 n)))))))

;;Compares the lengths of the lists and calls compare-digits if they are of
;;equal length.
(defun compare(x y)
  (let ((len-x (length x)) (len-y (length y)))
    (cond ((> len-x len-y)
	   nil)
	  ((< len-x len-y)
	   t)
	  (t
	   (compare-digits x y 0)))))

;;Adds zeros the front and back of the list being multiplied
;;This prevents errors like: "NIL is not a number"
;;The number of zeros added: (length y) - 1
(defun pad-with-zeros(x n)
  (if (> n 0)
      (progn (push 0 x)
	     (setf x (reverse x))
	     (push 0 x)
	     (pad-with-zeros (reverse x) (- n 1)))
      x))

;;Removes the added zeros so we have the original list back
(defun remove-zeros(x n)
    (if (> n 0)
      (progn (setf x (cdr x))
	     (setf x (cdr (reverse x)))
	     (remove-zeros (reverse x) (- n 1)))
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

;;This function multiplies the current digits in position 'n' and adds them into
;;the temp-buffer, which is then returned back to multiply-loop
(defun multiply-digits(x y n i temp-buffer)
  (if (equal i (length y))
      temp-buffer
      (multiply-digits x y (+ 1 n) (+ 1 i) 
			    (+ temp-buffer (* (nth n x) (nth i y))))))

;;This function basically conses the result list with the output from the
;;multiply-digits function, then calls itself with a new position (n)
(defun multiply-loop(x y n i)
  (if (equal n (- (length x) (- (length y) 1)))
      't
      (progn (setf *result* (cons (multiply-digits x y n 0 0) *result*))
	     (multiply-loop x y (+ 1 n) i))))

;;must be called before the multiply-loop function (this is a setup function) 
(defun multiply(x y)
  (setf *result* '())
  (setf x (pad-with-zeros x (- (length y) 1)))
  (multiply-loop x (reverse y) 0 (- (length y) 1))
  (setf *result* (reverse (digit-correct *result* 0))))

;;computes the factorial of the number x (in list form)
(defun factorial(x y z)
  (if (compare x y)
      z
      (factorial x (increment y)  (multiply z y))))

(defun take-factorial(f)
  (factorial (convert-to-list f) '(2) '(1)))
