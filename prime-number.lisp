;; This program is Free Software under the GNU GPLv3 and above.

;; Ever wanted to know what the 1,000th prime number is? Here's your answer!
;; This program will find prime number based on their position with respect to 2
;; This means that 2 is 1, 3 is 2, 5 is 3, 7 is 4, p is n. Enjoy.

;; This program is much much faster than the big-prime program because it uses
;; the regular integer type and not the special list number type. Enjoy.

(defun prime-finder (number factor limit)
  (cond ((or (equal limit factor) (> factor limit))
	 t)
	((equal 0 (mod number factor))
	 nil)
	(t
	 (let ((new-limit (floor number factor)))
	   (prime-finder number (+ 2 factor) new-limit)))))
	       
(defun prime-loop (cur-num cur-prime-num pos l) ;l is the number of counted primes
  (let ((next-cur-num (+ 1 cur-num)))
    (cond ((equal pos l)
	   cur-prime-num)
	  ((equal 0 (mod cur-num 2))
	   (prime-loop next-cur-num cur-prime-num pos l))
	  ((prime-finder cur-num 3 cur-num)
	   (prime-loop next-cur-num cur-num pos (+ 1 l)))
	  (t
	   (prime-loop next-cur-num cur-prime-num pos l)))))
	  

(defun prime (position)
  (cond ((equal 1 position)
	 2)
	((equal 2 position)
	 3)
	(t
	 (prime-loop 6 5 position 3))))
	 
    
