;;This program is Free Software under the GNU GPLv3.0 and above

;;This program will take the factorial of any number you give it.
;;It stores each digit of the number in a list so that you can deal with huge
;;numbers. 
;;In the comments, I use the terms MSD and LSD, which means, Most Significant 
;;Digit and Least Significant Digit.

;;There still may be a few changes that I'll add to make this more Lispy, but
;;for now, try doing 1000!; it has ~2500 digits in it and is awesome!

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
(defun increment(number-list)
  (let ((reversed-list (reverse number-list)))
    (let ((elem-to-increment (car reversed-list)) (rest-of-list (cdr reversed-list)))
      (reverse (digit-fixer (cons (+ elem-to-increment 1) rest-of-list))))))
  

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
  

;;computes the factorial of the number fir-num (in list form)
(defun take-factorial(fir-num sec-num z)
  (if (compare fir-num sec-num)
      z
      (take-factorial fir-num (increment sec-num) (multiply z sec-num))))


(defun factorial(f)
  (take-factorial (convert-to-list f) '(2) '(1)))
