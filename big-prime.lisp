;; This program is Free Software under the GNU GPLv3 and above.

;; Ever wanted to know what the 1,000th prime number is? Here's your answer!
;; This program will find prime number based on their position with respect to 2
;; This means that 2 is 1, 3 is 2, 5 is 3, 7 is 4, p is n. Enjoy.

;; Keep in mind, for the most part using the standard number type is much quicker
;; than using this program. However, if were to be looking for a prime number with
;; thousands of digits, you could use this. It would just take forever!

;; It isn't written the best but it is 12 times more efficient than the original!

(load "/home/kevin/development/lisp/Common-Lisp/big-number.lisp")

(defun mod-divide-loop (dividend divisor cur-div remainder)
  (if (null divisor)
      remainder
      (let ((rest-of-div (cdr divisor)) (no-zero-cur-div (remove-leading-zeros cur-div)))
	(if (compare dividend no-zero-cur-div)	;t when cur-div is bigger
	    (let ((quotient (find-quotient dividend no-zero-cur-div '(1))))
	      (let ((new-remainder (subtract no-zero-cur-div (multiply dividend quotient))))
		(mod-divide-loop dividend rest-of-div
				 (append new-remainder (list (cadr divisor))) new-remainder)))
	    (if (null rest-of-div)
		(if (or (equal dividend no-zero-cur-div) (null no-zero-cur-div))
		    '(0)
		    no-zero-cur-div)
		(mod-divide-loop dividend rest-of-div (append cur-div (list (cadr divisor))) remainder))))))
	    

(defun mod-divide (dividend divisor)
  (if (equal dividend divisor)
      '(1)
      (mod-divide-loop dividend divisor (list (car divisor)) '(0))))

(defun prime-finder (list-number factor limit)
  (if (or (equal factor limit) (compare limit factor))
      t
      (if (equal '(0) (mod-divide factor list-number))
	  nil
	  (let ((new-limit (divide factor list-number)))
	    (if (equal factor new-limit)
		t
		(prime-finder list-number (addition factor '(2)) new-limit))))))

(defun big-prime-loop (cur-num cur-prime-num pos l) ;l is a loop variable
  (cond ((equal pos l)
	 cur-prime-num)
	((equal '(0) (mod-divide '(2) cur-num))
	 (big-prime-loop (addition cur-num '(1)) cur-prime-num pos l))
	((prime-finder cur-num '(3) cur-num)
	 (big-prime-loop (addition cur-num '(1)) cur-num pos (+ l 1))) ;only increment loop when it IS prime
	(t
	 (big-prime-loop (addition cur-num '(1)) cur-prime-num pos l))))

(defun big-prime (position)
  (cond ((equal 1 position)
	 '(2))
	((equal 2 position)
	 '(3))
	(t
	 (big-prime-loop '(6) '(5) position 3))))
