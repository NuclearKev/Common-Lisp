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

;; 26 UNFINISHED
(defun grouper (group-size orig-list)
  (if (equal 0 group-size)		;because we always have 2 elements from the start
      nil
      (cons (car orig-list) (grouper (- group-size 1) (cdr orig-list)))))

  
(defun combination (group-size orig-list)
  (if (null orig-list)
      nil
      (
