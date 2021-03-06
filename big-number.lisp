;;This program is Free Software under the GNU GPLv3.0 and above

;; This is a library that handles really really big numbers. You can use it
;; to add, subtract, multiply, and divide them. To see it in action, check out
;; my other programs factorial.lisp or fibonacci.lisp.

;; Maybe one day I'll add some macros to use the standard *, +, - symbols.

;;; General ;;;

;;Converts a regular whole number into a listed version.
;;For example, 123 -> '(1 2 3)
(defun convert-to-list (number)
  (when (> number 0)
    (append (convert-to-list (floor number 10))  `(,(mod number 10)))))


;; Fix any digits that are over 9.
;; This function is quite complicated; however, with some thought, it can be
;; figured out.
(defun digit-fixer (org-list)
  (let ((cur-elem (car org-list))
				(rest-of-list (cdr org-list)))
    (let ((end-of-list (null rest-of-list)))
      (cond ((> cur-elem 9)
						 (let ((msd (floor cur-elem 10))
									 (lsd (mod cur-elem 10)))
							 (if end-of-list
									 `(,lsd ,msd)
									 (let ((new-next-elem (+ msd (car rest-of-list))))
										 (cons lsd (digit-fixer
																(cons new-next-elem (cdr rest-of-list))))))))
						(end-of-list
						 `(,cur-elem))
						(t
						 (cons cur-elem (digit-fixer rest-of-list)))))))

;; Compares each element in a list
;; If the first number is bigger pass nil, if second number is bigger pass t
(defun compare-digits (fir-num sec-num pos) ;pos is the current position
  (let ((nth-fir-num (nth pos fir-num))
				(nth-sec-num (nth pos sec-num)))
    (unless (equal pos (length fir-num))
      (cond ((> nth-fir-num nth-sec-num)
						 nil)
						((< nth-fir-num nth-sec-num)
						 t)
						(t
						 (compare-digits fir-num sec-num (+ 1 pos)))))))


;; Compares the lengths of the lists and calls compare-digits if they are of
;; equal length.
(defun compare (fir-num sec-num)
  (let ((len-fir-num (length fir-num))
				(len-sec-num (length sec-num)))
    (cond ((> len-fir-num len-sec-num)
					 nil)
					((< len-fir-num len-sec-num)
					 t)
					(t
					 (compare-digits fir-num sec-num 0)))))


;;; Multiplication ;;;

;; Adds zeros the front and back of the list being multiplied
;; This prevents errors like: "NIL is not a number"
;; The number of zeros added: 2((length y) - 1)
(defun pad-with-zeros (num-list n)	;n is a loop variable
  (cond ((> n 0)
				 (pad-with-zeros (cons 0 (reverse num-list)) (- n 1)))
				(t
				 num-list)))


;; Actually does the multiplication.
;; We take number 'x' and pad it with zeros to start.
;; Then we take number 'y' and reverse it.
;; Now it takes these and multiples down the line.
;; For example, x = '(1 2 3) y = '(4 5)
;; Pad zeros: '(0 1 2 3 0)
;; Reverse y: '(5 4)
;; Multiply:
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
;; In *result* will be '(15 22 13 4) *note that it's backwards
;; In the next function (multiply) it passes *result* into digit-correct where
;; it is fixed. This is why it's backwards (because digit-correct needs them
;; backwards). You then have '(5 3 5 5) in *result*, it gets reversed to have
;; '(5 5 3 5) YAY!

;; This function multiplies the current digits in position 'n' and adds them
;; into the temp-buffer, which is then returned back to multiply-loop
(defun multiply-digits (fir-num sec-num n i temp-buffer) ;n is current position, i is a loop variable
  (if (equal i (length sec-num))
      temp-buffer
      (multiply-digits fir-num sec-num (+ 1 n) (+ 1 i)
											 (+ temp-buffer
													(* (nth n fir-num) (nth i sec-num))))))


;; This function basically conses the result list with the output from the
;; multiply-digits function, then calls itself with a new position (n)
(defun multiply-loop (fir-num sec-num n i)
  (unless (equal n (- (length fir-num) (- (length sec-num) 1)))
    (cons (multiply-digits fir-num sec-num n 0 0)
					(multiply-loop fir-num sec-num (+ 1 n) i))))


;;must be called before the multiply-loop function (this is a setup function)
(defun multiply (fir-num sec-num)
  (let ((pad-fir-num (pad-with-zeros fir-num (* 2 (- (length sec-num) 1)))))
    (reverse
     (digit-fixer (reverse
									 (multiply-loop pad-fir-num
																	(reverse sec-num) 0
																	(- (length sec-num) 1)))))))



;;; Addition ;;;

;; This function is also used in subtraction
;; Pads the front of the smaller number with zeros so we can do addition easier.
;; For example: 123 + 12, would be 123 + 012
(defun pad-front (list-to-pad num-zeros)
  (if (equal 0 num-zeros)
      list-to-pad
      (cons 0 (pad-front list-to-pad (- num-zeros 1)))))

(defun add-loop (fir-num sec-num)
  (unless (null fir-num)			;both go null at the same time
    (let ((fir-digit (car fir-num))
					(sec-digit (car sec-num)))
      (cons (+ fir-digit sec-digit) (add-loop (cdr fir-num) (cdr sec-num))))))

;; Adds digit by digit
(defun addition (fir-num sec-num)	;where fir- and sec-num are list numbers
  (let* ((len-fir (length fir-num))
				 (len-sec (length sec-num))
				 (pad-sec-num (pad-front sec-num (- len-fir len-sec))))
		(reverse
		 (digit-fixer (add-loop (reverse fir-num) (reverse pad-sec-num))))))

;;; Subtraction ;;;

;; This function does all the carrying. It works like this:
;; Subtract-loop would do '(1 1 1) - '(9 9) = '(1 -8 -8)
;; Then if the element is negative we add 10 to it and subtract 1 from
;; the element before it; like so:
;; '(1 -8 -8) => '(1 -9 2) => '(0 1 2)
(defun carry-fix (number-list)					;number-list is reversed from orginal
  (unless (null number-list)
    (let ((cur-digit (car number-list))
					(carried-digit (cadr number-list))) ;carried-digit may or may not be carried
      (if (> 0 cur-digit)
					(let* ((temp-list `(,(+ cur-digit 10) ,(- carried-digit 1)))
								 (new-number-list (append temp-list (cddr number-list))))
						(cons (car new-number-list) (carry-fix (cdr new-number-list))))
					(cons cur-digit (carry-fix (cdr number-list)))))))

;; Since the carry-fix leaves leading zeros, we must remove them!
(defun remove-leading-zeros (number-list)
  (let ((poss-zero (car number-list)))	;an element that might possibly be a zero
    (if (equal 0 poss-zero)
				(remove-leading-zeros (cdr number-list))
				number-list)))

;; Subtracts the numbers, digit by digit. It obviously doesn't do the carrying
(defun subtract-loop (fir-num sec-num)
  (unless (null fir-num)			;they are null at the same time
    (let ((fir-digit (car fir-num))
					(sec-digit (car sec-num)))
      (cons (- fir-digit sec-digit)
						(subtract-loop (cdr fir-num) (cdr sec-num))))))

(defun subtract (fir-num sec-num)
  (if (equal fir-num sec-num)
      '(0)
      (let* ((len-fir (length fir-num))
						 (len-sec (length sec-num))
						 (pad-sec-num (pad-front sec-num (- len-fir len-sec))))
				(remove-leading-zeros (reverse
															 (carry-fix
																(subtract-loop
																 (reverse fir-num) (reverse pad-sec-num))))))))

;;; Division ;;;

;; After one day of hard work, I have what I wanted! Although still ugly, I have
;; yet to rewrite these for efficiency.

;; Used to place the decimal in the end
(defun insert-at (symbol orig-list pos)	;pos is where placed
  (if (equal 0 pos)
      (cons symbol orig-list)
      (cons (car orig-list) (insert-at symbol (cdr orig-list) (- pos 1)))))

;; Does the actual division with remainders
(defun divide-loop (cur-num divisor multiple past-multiple)
	(let ((cur-guess (multiply divisor multiple)))
		(cond ((compare cur-guess cur-num)	;is cur-guess is smaller keep going!
					 (divide-loop cur-num divisor (addition multiple '(1)) multiple))
					((equal cur-guess cur-num)		;if equal we good!
					 `(,multiple))
					(t														;if bigger, go back to the last one
					 (let ((remainder (subtract cur-num
																			(multiply divisor past-multiple))))
						 (list past-multiple remainder))))))

;; Although large, this function is fairly easy to understand.. Thanks cond!
;; I plan on rewritting this function to look better

;; More or less, this function looks at the current digit in the dividend and
;; sees if it is big enough to divide, zero, or whatever. It then does the
;; appropriate logic to correctly divide.
(defun divide-compare (dividend divisor cur-num d i)
	(let ((big-or-small (compare cur-num divisor))
				(is-zero (equal '(0) cur-num)))
		(cond ((equal d i)										;d # of decimal places please
					 `(,i))
					((null dividend)								;pad zeros basically
					 (if big-or-small
							 (append '(0) (divide-compare
														 dividend divisor (append cur-num '(0)) d (+ i 1)))
							 (let* ((div-n-rem (divide-loop cur-num divisor '(1) '(1)))
											(div (car div-n-rem))
											(remainder (cadr div-n-rem)))
								 (if (null remainder)
										 (append div `(,(+ i 1)))
										 (append div
														 (divide-compare
															(cdr dividend) divisor
															(append remainder '(0)) d (+ 1 i)))))))
					((equal '(E) dividend)					;at the end, but not decimal yet
					 (cond (is-zero
									'(0 -1))
								 (big-or-small
									(divide-compare dividend divisor (append cur-num '(0)) d i))
								 (t
									(let* ((div-n-rem (divide-loop cur-num divisor '(1) '(1)))
												 (div (car div-n-rem))
												 (remainder (cadr div-n-rem)))
										(if (null remainder)
												(append div '(-1))
												(append div
																(divide-compare
																 (cdr dividend) divisor
																 (append remainder '(0)) d (+ 1 i))))))))
					 (is-zero
						(append '(0)
										(divide-compare (cdr dividend) divisor
																		`(,(car dividend)) d i)))
					 (big-or-small			 ;if cur-num is smaller, get more number
						(divide-compare
						 (cdr dividend) divisor (append cur-num `(,(car dividend))) d i))
					 (t															;if cur-num is bigger, divide!
						(let* ((div-n-rem (divide-loop cur-num divisor '(1) '(1)))
									 (div (car div-n-rem))
									 (remainder (cadr div-n-rem)))
							(if (null remainder)
									(append div
													(divide-compare (cdr dividend) divisor
																					`(,(car dividend)) d i))
									(append div
													(divide-compare (cdr dividend) divisor
																					(append remainder
																									`(,(car dividend))) d i))))))))

;; You can even choose how many decimal places you want! How wonderful!
;; Based on the decimal value and how many decimals were used, it finds were to
;; put the 'D symbol.
(defun divide (dividend divisor &optional (decimal-places 5))
	(let* ((end-dividend (reverse (cons 'E (reverse dividend))))
				 (no-deci (divide-compare (cdr end-dividend) divisor
																	`(,(car end-dividend)) decimal-places -1))
				 (rev-no-deci (reverse no-deci))
				 (deci-pos (car rev-no-deci))
				 (remove-deci-pos (reverse (cdr rev-no-deci))))
		(if (equal -1 deci-pos)
				remove-deci-pos
				(insert-at 'D remove-deci-pos (- (length remove-deci-pos) deci-pos)))))
