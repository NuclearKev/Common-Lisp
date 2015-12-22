;;This program is Free Software under the GNU GPLv3.0 and above

;; This is a library that handles really really big numbers. You can use it
;; to add, subtract, and multiply them. To see it in action, check out my
;; other programs factorial.lisp or fibonacci.lisp.

;; Maybe one day I'll just some macros to use the standard *, +, - symbols.



;;; General ;;;

;;Converts a regular whole number into a listed version.
;;For example, 123 -> '(1 2 3)
(defun convert-to-list (number)
  (if (> number 0)
      (append (convert-to-list (floor number 10)) (list (mod number 10)))
      nil))

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

;;Compares each element in a list
;;If the first number is bigger pass nil, if second number is bigger pass t
(defun compare-digits(fir-num sec-num pos) ;pos is the current position
  (let ((nth-fir-num (nth pos fir-num)) (nth-sec-num (nth pos sec-num)))
    (if (equal pos (length fir-num))
	nil
	(cond ((> nth-fir-num nth-sec-num)
	       nil)
	      ((< nth-fir-num nth-sec-num)
	       t)
	      (t
	       (compare-digits fir-num sec-num (+ 1 pos)))))))


;;Compares the lengths of the lists and calls compare-digits if they are of
;;equal length.
(defun compare(fir-num sec-num)
  (let ((len-fir-num (length fir-num)) (len-sec-num (length sec-num)))
    (cond ((> len-fir-num len-sec-num)
	   nil)
	  ((< len-fir-num len-sec-num)
	   t)
	  (t
	   (compare-digits fir-num sec-num 0)))))


;;; Multiplication ;;;

;;Adds zeros the front and back of the list being multiplied
;;This prevents errors like: "NIL is not a number"
;;The number of zeros added: 2((length y) - 1)
(defun pad-with-zeros(num-list n)	;n is a loop variable
  (if (> n 0)
      (pad-with-zeros (cons 0 (reverse num-list)) (- n 1))
      num-list))


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
(defun multiply-digits(fir-num sec-num n i temp-buffer) ;n is current position, i is a loop variable
  (if (equal i (length sec-num))
      temp-buffer
      (multiply-digits fir-num sec-num (+ 1 n) (+ 1 i) 
			    (+ temp-buffer (* (nth n fir-num) (nth i sec-num))))))


;;This function basically conses the result list with the output from the
;;multiply-digits function, then calls itself with a new position (n)
(defun multiply-loop(fir-num sec-num n i)
  (if (equal n (- (length fir-num) (- (length sec-num) 1)))
      nil
      (cons (multiply-digits fir-num sec-num n 0 0) (multiply-loop fir-num sec-num (+ 1 n) i))))


;;must be called before the multiply-loop function (this is a setup function) 
(defun multiply(fir-num sec-num)
  (let ((pad-fir-num (pad-with-zeros fir-num (* 2 (- (length sec-num) 1)))))
    (reverse (digit-fixer (reverse (multiply-loop pad-fir-num (reverse sec-num) 0 (- (length sec-num) 1)))))))



;;; Addition ;;;

;; This function is also used in subtraction
;; Pads the front of the smaller number with zeros so we can do addition easier.
;; For example: 123 + 12, would be 123 + 012
(defun pad-front (list-to-pad num-zeros)
  (if (equal 0 num-zeros)
      list-to-pad
      (cons 0 (pad-front list-to-pad (- num-zeros 1)))))

(defun add-loop (fir-num sec-num)
  (if (null fir-num)			;both go null at the same time
      nil
      (let ((fir-digit (car fir-num)) (sec-digit (car sec-num)))
	(cons (+ fir-digit sec-digit) (add-loop (cdr fir-num) (cdr sec-num))))))

;; Adds digit by digit
(defun addition (fir-num sec-num)	;where fir- and sec-num are list numbers
  (let ((len-fir (length fir-num)) (len-sec (length sec-num)))
    (let ((pad-sec-num (pad-front sec-num (- len-fir len-sec))))
      (reverse
       (digit-fixer (add-loop (reverse fir-num) (reverse pad-sec-num)))))))

;;; Subtraction ;;;
(defun carry-fix (number-list)
  (if (null number-list)
      nil
      (let ((cur-digit (car number-list)) (carried-digit (cadr number-list))) ;carried-digit may or may not be carried
	(if (> 0 cur-digit)
	    (let ((temp-list (list (+ cur-digit 10) (- carried-digit 1))))
	      (let ((new-number-list (append temp-list (cddr number-list))))
		(cons (car new-number-list) (carry-fix (cdr new-number-list)))))
	    (cons cur-digit (carry-fix (cdr number-list)))))))

(defun subtract-loop (fir-num sec-num)
  (if (null fir-num)			;they are null at the same time
      nil
      (let ((fir-digit (car fir-num)) (sec-digit (car sec-num)))
	(cons (- fir-digit sec-digit) (subtract-loop (cdr fir-num) (cdr sec-num))))))
  
(defun subtract (fir-num sec-num)
  (let ((len-fir (length fir-num)) (len-sec (length sec-num)))
    (let ((pad-sec-num (pad-front sec-num (- len-fir len-sec))))
      (reverse
       (carry-fix (subtract-loop (reverse fir-num) (reverse pad-sec-num)))))))
