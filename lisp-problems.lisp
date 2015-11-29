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
