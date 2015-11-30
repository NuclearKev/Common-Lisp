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
