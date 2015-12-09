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

   
  
