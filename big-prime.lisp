;; This program is Free Software under the GNU GPLv3 and above.

;; Ever wanted to know what the 1,000th prime number is? Here's your answer!
;; This program will find prime number based on their position with respect to 2
;; This means that 2 is 1, 3 is 2, 5 is 3, 7 is 4, p is n. Enjoy.

;; Keep in mind, for the most part using the standard number type is much quicker
;; than using this program. However, if were to be looking for a prime number with
;; thousands of digits, you could use this. It would just take forever!

;; It isn't written the best but it is 12 times more efficient than the original!
;; In order to make the program much faster, I will be adding a global list that
;; that contains some primes in which you can start at. For example, if you wanted
;; to find the 1,000,500th prime, you could start a 1,000,000 and work the extra
;; 500. (I probably won't add this feature)

(load "/home/kevin/development/lisp/Common-Lisp/big-number.lisp")

;; This function just returns the remainder of the division. How it works is
;; rather complex.
;; Say we are taking '(1 2 0) / '(3).
;; We grab the first digit from the divisor and compare it to the dividend.
;; If the dividend is bigger, we can't do anything yet so we cons the next
;; digit with the first and compare again. Like so:
;; '(3) > '(1) => '(3) < '(1 2)
;; Once we have the second case, we use the find-quotient function to find
;; the best possible quotient for the given numbers. Then we multiply, then
;; subtract and this becomes our new remainder.
;; At the end of the function there are some precautionary bits. These
;; protect against weird cases where at the end of the division the
;; remainder is equal to the dividend or if the remainder is 0 before
;; we find the quotient.
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
	    
;; To avoid confusing and making the above function more complex, if they are
;; the same, it's 1.
(defun mod-divide (dividend divisor)
  (if (equal dividend divisor)
      '(1)
      (mod-divide-loop dividend divisor (list (car divisor)) '(0))))

;; Crunch through some numbers to see if the number is prime. The limit variable
;; is nice because it cuts down on the amount of work needed to be done. What it
;; does is takes the number divided by a number, say 3. This means that it isn't
;; divisible by 3; but it also means that it isn't divisible by list-number / 3
;; rounded up. We do this until the limit value is equal to or over the number
;; we are dividing by.
(defun prime-finder (list-number factor limit)
  (if (or (equal factor limit) (compare limit factor))
      t
      (if (equal '(0) (mod-divide factor list-number))
	  nil
	  (let ((new-limit (divide factor list-number)))
	    (if (equal factor new-limit)
		t
		(prime-finder list-number (addition factor '(2)) new-limit))))))

;; If we have counted "pos" amounts of primes, we have made it to the prime we want!
;; We check to see if the number is divisible by 2, if so, we already know it isn't
;; a prime. However, if it isn't divisible by 2, we can conclude that it isn't
;; divisible by ANY even number. This is why we pass a 3 and the prime-finder
;; function increments by 2.
(defun big-prime-loop (cur-num cur-prime-num pos l) ;l is a loop variable
  (cond ((equal pos l)
	 cur-prime-num)
	((equal '(0) (mod-divide '(2) cur-num))
	 (big-prime-loop (addition cur-num '(1)) cur-prime-num pos l))
	((prime-finder cur-num '(3) cur-num)
	 (big-prime-loop (addition cur-num '(1)) cur-num pos (+ l 1))) ;only increment loop when it IS prime
	(t
	 (big-prime-loop (addition cur-num '(1)) cur-prime-num pos l))))

;; To work with the new method of prime-finding, we cannot have 2 or 3 go into
;; big-prime-loop. This explains the use of the cond.
(defun big-prime (position)
  (cond ((equal 1 position)
	 '(2))
	((equal 2 position)
	 '(3))
	(t
	 (big-prime-loop '(6) '(5) position 3))))
