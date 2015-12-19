;; 1
(defun last-in-list (list)
  (car
   (reverse list)))

;; 2
(defun last-2-in-list (list)
  (let ((reversed-list (reverse list)))
    (list (cadr reversed-list)
	  (car reversed-list))))
    
;; 3
(defun element-at (list location)
  (if (equal 0 location)
      (car list)
      (element-at (cdr list) (- location 1))))

;; 4
(defun length-of-list (list)
  (if (null list)
      0
      (1+ (length-of-list (rest list)))))

;; 5
(defun reverse-a-list (list rest-of-list)
  (if (null list)
      rest-of-list
      (reverse-a-list (rest list)
		      (cons (first list) rest-of-list))))
      
;; 6
(defun palindrome-logic (list reversed-list)
  (if (null list)
      't
      (if (equal (car list) (car reversed-list))
	  (palindrome-logic (cdr list) (cdr reversed-list))
	  'nil)))

(defun palindrome (list)
  (palindrome-logic list (reverse list)))
       
;; 7
(defun flatten (orig-list)
  (if (null orig-list)
      nil
      (let ((elem (car orig-list)) (rest-of-list (cdr orig-list)))
	(if (listp elem)
	    (append (flatten elem) (flatten rest-of-list))
	    (append (cons elem nil) (flatten rest-of-list))))))

;; 8
(defun compress (orig-list)
  (if (null orig-list)
      nil
      (let ((current (car orig-list)) (rest-of-list (cdr orig-list)))
	(if (equal current (car rest-of-list))
	    (compress rest-of-list)
	    (append (cons current nil) (compress rest-of-list))))))

;; 9
(defun pack (orig-list)
  (if (null orig-list)
      nil
      (let ((fir-elem (car orig-list)) (sec-elem (cadr orig-list)))
	(if (listp fir-elem)
	    (if (equal (car fir-elem) sec-elem)
		(pack
		 (cons
		  (cons sec-elem fir-elem) (cddr orig-list))) ;fir-elem is a list here
		(cons fir-elem (pack (cdr orig-list))))
	    (if (equal fir-elem sec-elem)
		(pack
		 (cons (list fir-elem sec-elem) (cddr orig-list))) ;jump 2 elements when passing list
		(pack
		 (cons (list fir-elem) (cdr orig-list))))))))

;; 10
(defun encode-logic (L)
  (if (null L)
      nil
      (let ((elem (car L)))		;elem is a list
	(cons
	 (list (length elem) (car elem)) (encode-logic (cdr L))))))
  
(defun encode (L)
  (encode-logic (pack L)))		;pack is from problem 9

;; 11
(defun encode-mod-logic (L)
  (if (null L)
      nil
      (let ((elem (car L)))
	(let ((len (length elem)))	;2 lexical variables?!?!? Far out!
	  (if (equal 1 len)
	      (cons (car elem) (encode-mod-logic (cdr L)))
	      (cons
	       (list (length elem) (car elem)) (encode-mod-logic (cdr L))))))))

(defun encode-mod (L)
  (encode-mod-logic (pack L)))		;remember pack?

;; 12
(defun list-maker (number-of-elements element)
  (if (equal 0 number-of-elements)
      nil
      (append (list element) (list-maker (- number-of-elements 1) element))))

(defun decode (orig-list)
  (if (null orig-list)
      nil
      (let ((sublist (car orig-list)))
	(if (listp sublist)
	    (let ((number (car sublist)) (elem (cadr sublist)))
	      (append (list-maker number elem) (decode (cdr orig-list))))
	    (append (list sublist) (decode (cdr orig-list))))))) ;sublist isn't a list here

;; 13
(defun count-n-list (element rest-list number-of-elements)
  (if (equal element (car rest-list))
      (count-n-list element (cdr rest-list) (+ 1 number-of-elements))
      (cons (list number-of-elements element) rest-list)))

(defun encode-direct (orig-list)
  (if (null orig-list)
      nil
      (let ((elem (car orig-list)))
	(if (equal elem (cadr orig-list))
	    (let ((new-list (count-n-list elem (cdr orig-list) 1)))
	      (cons (car new-list) (encode-direct (cdr new-list))))
	    (append (list elem) (encode-direct (cdr orig-list)))))))

;; 14
(defun dupli (L)
  (if (null L)
      nil
      (let ((element-to-duplicate (car L)))
	(append
	 (list element-to-duplicate element-to-duplicate) (dupli (cdr L))))))

;; 15
(defun repli (orig-list number-of-repeats)
  (if (null orig-list)
      nil
      (let ((elem (car orig-list)))
	(append (list-maker number-of-repeats elem) (repli (cdr orig-list) number-of-repeats))))) ;list-maker from #12

;; 16
(defun drop (orig-list pos)
  (let ((elem (car orig-list)) (rest-of-list (cdr orig-list)))
    (if (equal pos 0)
	rest-of-list
	(append (list elem) (drop rest-of-list (- pos 1))))))

;; 17 (sorta)
(defun split (orig-list pos)
  (if (equal pos 0)
      (list orig-list)
      (cons (car orig-list) (split (cdr orig-list) (- pos 1)))))

;; 18
(defun slice (orig-list I K)		;I & K are the starting & stopping positions, respectively
  (if (equal I 0)
      (if (equal K 0)
	  nil
	  (cons (car orig-list) (slice (cdr orig-list) 0 (- K 1))))
      (slice (cdr orig-list) (- I 1) K)))

;; 19
(defun rotate-loop (orig-list shift-num)
  (if (equal 0 shift-num)
      orig-list
      (rotate-loop (append (cdr orig-list) (list (car orig-list))) (- shift-num 1))))

(defun rotate (orig-list shift-num)	
  (if (< shift-num 0)
      (rotate-loop orig-list (+ (length orig-list) shift-num)) ;rotates to the right
      (rotate-loop orig-list shift-num)))		       ;rotates to the left

;; 20
(defun remove-at (orig-list pos)	;pos is the element's position you wish to remove
  (if (equal 0 pos)
      (cdr orig-list)
      (cons (car orig-list) (remove-at (cdr orig-list) (- pos 1)))))

;; 21
(defun insert-at (symbol orig-list pos)	;pos is the position in which you wish to insert a element
  (if (equal 0 pos)
      (cons symbol orig-list)
      (cons (car orig-list) (insert-at symbol (cdr orig-list) (- pos 1)))))

;; 22
(defun range-loop (I K)
  (if (equal I K)
      (list K)
      (cons I (range-loop (+ I 1) K))))

(defun range (I K)			;I and K are the limits of the range
  (if (< I K)
      (range-loop I K)
      (reverse (range-loop K I))))

;; 23
(defun rnd-select (orig-list number-of-elements)
  (if (equal 0 number-of-elements)
      nil
      (let ((rand-pos (random (length orig-list))))
	(cons (element-at orig-list rand-pos)
	      (rnd-select (remove-at orig-list rand-pos) (- number-of-elements 1))))))

;; 24
(defun lotto-select (N M)		;N & M are the number of numbers and limit of numbers, respectively
  (if (equal 0 N)
      nil
      (cons (random M) (lotto-select (- N 1) M))))

;; 25
(defun rnd-permu (orig-list)
  (rnd-select orig-list (length orig-list))) ;doi?

;; 26
;; Since this is the hardest problem done so far, the function(s) is/are more
;; complicated. In order to help with this confusion, I will explain how it works.
;; Assume we start off with a list of '(a b c d) and we want groups of 3. The
;; combination functions chops it up so that fir = '(a) and sec = '(b c d). This
;; is to keep the combo-loop function from going crazy with nil. The size is
;; obviously the size of the groups you want. pos is the current position of
;; the list. In our case, '(a) has a position of 0, but '(a b) has a position of
;; 1. This is so that we remove a certain element each time as you will see later.
;; 
;; We check to see if sec is null, this will tell us when we are at the end of a
;; element group. For example, if fir = '(a b d), sec = '(), we are dont with the
;; '(a b) case. Note that the only way we get '(a b d) is when sec is null AND the
;; size of fir is equal to the group size. This is so we don't miss out on the last
;; group in an element case. If fir is of size, we then can start making each group.
;; At this point in time, fir IS a group, thus, it is the first to be cons'ed. We
;; then remove the last element, move the first element of sec into fir, and chop
;; off the first element of sec. This is run until sec is nil.
;; For example, if we have fir = '(a b c), sec = '(d): fir is cons'ed and the next
;; loop fir = '(a b d) sec = '(). This means we will return (list fir) and it will
;; end this element case.
;;
;; To change element cases (or if fir is not of size) we shift is the car of sec
;; and cdr sec. We then call the function again. However we append this to the
;; recursion of next-fir. This is so we can go from fir='(a) to '(b) and repeat.
(defun combo-loop (fir sec size pos)
  (if (null sec)
      (if (equal (length fir) size)	;to get the last group
	  (list fir)
	  nil)
      (if (equal (length fir) size)
	  (let ((new-fir (append (remove-at fir pos) (list (car sec))))
		(new-sec (cdr sec)))
	    (cons fir (combo-loop new-fir new-sec size pos)))
	  (let ((shift-in-fir (append fir (list (car sec)))) (shift-in-sec (cdr sec)))
	    (let ((next-fir (remove-at shift-in-fir pos)))
	      (append (combo-loop shift-in-fir shift-in-sec size (+ pos 1))
		      (combo-loop next-fir shift-in-sec size pos)))))))

(defun combination (group-size org-list)
  (combo-loop (list (car org-list)) (cdr org-list) group-size 0))

;; 31
(defun prime-loop (number factor)
  (if (equal number factor)
      t
      (if (equal 0 (mod number factor))
	  nil
	  (prime-loop number (+ factor 1)))))

(defun is-prime (number)
  (prime-loop number 2))

;; 32
(defun gcb-loop (fir-num sec-num cur-divisor g-divisor)
  (let ((new-cur-divisor (+ cur-divisor 1)))
    (if (and (equal 0 (mod fir-num cur-divisor)) (equal 0 (mod sec-num cur-divisor)))
	(if (or (equal fir-num cur-divisor) (equal sec-num cur-divisor))
	    cur-divisor
	    (if (> cur-divisor g-divisor)
		(gcb-loop fir-num sec-num new-cur-divisor cur-divisor) ;current divisor becomes the greatest
		(gcb-loop fir-num sec-num new-cur-divisor g-divisor)))
	(if (or (equal fir-num cur-divisor) (equal sec-num cur-divisor))
	    g-divisor
	    (gcb-loop fir-num sec-num new-cur-divisor g-divisor)))))

(defun my-gcd (fir-num sec-num)
  (gcb-loop fir-num sec-num 2 1))

;; 33
(defun coprime (fir-num sec-num)
  (if (equal 1 (my-gcd fir-num sec-num))
      t
      nil))

;; 34
(defun phi-loop (m r)
  (if (equal m r)
      0
      (if (coprime m r)
	  (1+ (phi-loop m (+ r 1)))
	  (phi-loop m (+ r 1)))))

(defun totient-phi (m)			;m is the upper limit
  (if (equal 1 m)
      1
      (phi-loop m 1)))

;; 35
(defun prime-factors-loop (number divisor)
  (if (is-prime number)
      (list number)
      (if (equal 0 (mod number divisor))
	  (let ((new-number (/ number divisor)))
	    (append (list divisor) (prime-factors-loop new-number 2)))
	  (prime-factors-loop number (+ divisor 1)))))

(defun prime-factors (number)
  (if (is-prime number)
      number
      (prime-factors-loop number 2)))

;; 36
(defun prime-factors-multi (number)
  (encode (prime-factors number))) ;doi?

;; 37
;; This isn't how I was "supposed" to do it, however, it works
;; NOTE: This is ~3 times as efficient as the original
(defun phi-improved-loop (phi-list)
  (if (null phi-list)
      1
      (let ((cur-num (car phi-list)) (rest-of-list (cdr phi-list)))
	(let ((multi (car cur-num)) (prime (cadr cur-num)))
	  (* (* (- prime 1) (expt prime (- multi 1))) (phi-improved-loop rest-of-list))))))

(defun phi-improved (m)
  (phi-improved-loop (prime-factors-multi m)))

;; 39
(defun prime-range (lower upper)	;lower and upper are the limits
  (if (equal lower upper)
      nil
      (let ((new-lower (+ lower 1)))
	(if (is-prime lower)
	    (cons lower (prime-range new-lower upper))
	    (prime-range new-lower upper)))))

;; 40
(defun goldbach-logic (number fir)	;fir is the first "prime" number
  (if (is-prime fir)
      (let ((sec (- number fir)))
	(if (is-prime sec)
	    (list fir sec)
	    (goldbach-logic number (+ fir 1))))
      (goldbach-logic number (+ fir 1))))

(defun goldbach (number)
  (if (evenp number)
      (goldbach-logic number 2)
      0))

;; 41
;; Setup to display only the sums that contain numbers greater than 50
(defun goldbach-list (fir sec)		;[fir, sec]
  (if (> fir sec)
      'done
      (let ((primes-list (goldbach fir)))
	(if (equal 0 primes-list)
	    (goldbach-list (+ fir 1) sec)
	    (if (> (car primes-list) 49)
		(progn (format t "~d = ~d + ~d" fir (car primes-list) (cadr primes-list))
		       (fresh-line)
		       (goldbach-list (+ fir 1) sec))
		(goldbach-list (+ fir 1) sec))))))
  
;; That is all. The rest of the problems I'm not interesting in doing
