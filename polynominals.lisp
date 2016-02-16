;; This program is Free Software under the GPLv3 and above.

;; I made this to rebel against my Control Theory Prof...

;; Current bug: -1 on first term.

;; Very very very ugly function
(defun make-pretty (final-list order)	;length - 1 = order of polynomial
  (unless (null final-list)
    (let ((coef (car final-list))
	  (next-coef (cadr final-list)))
      (cond ((equal 1 order)
	     (if (equal 1 coef)
		 (format t " x ")
		 (format t " ~dx " (car final-list))))
	    ((equal 0 order)
	     (format t " ~d" (car final-list)))
	    (t
	     (if (equal 1 coef)
		 (format t " x^~d " order)
		 (format t " ~dx^~d " (car final-list) order))))
      (cond ((null next-coef)
	     (format t " ")
	     (make-pretty  (cdr final-list) (- order 1)))
	    ((> 0 next-coef);is the next term negative
	     (format t "-")
	     (make-pretty  (cons (abs next-coef) (cddr final-list)) (- order 1)))
	    (t
	     (format t "+")
	     (make-pretty  (cdr final-list) (- order 1)))))))


(defun foil (x poly2)
  (unless (null poly2)
    (let ((y (car poly2)))
      (cons (* x y) (foil x (cdr poly2))))))

(defun polynomial-multiply (poly1 poly2)
  (unless (null poly1)
    (cons (foil (car poly1) poly2) (polynomial-multiply (cdr poly1) poly2))))

(defun polynomial-add (poly1 poly2)
  (if (null poly1)
      poly2
      (cons (+ (car poly1) (car poly2)) (polynomial-add (cdr poly1) (cdr poly2)))))

(defun adding-loop (uncombined-polynomial)
  (let ((excluded-term (caar uncombined-polynomial))
	(first-adding-poly (cdar uncombined-polynomial))
	(sec-adding-poly (cadr uncombined-polynomial)))
    (if (null sec-adding-poly)
	(car uncombined-polynomial)	;'((a b c ... z)), we just want in the inner list
        (cons excluded-term (adding-loop
			     (cons (polynomial-add first-adding-poly sec-adding-poly)
				   (cddr uncombined-polynomial)))))))

(defun multiply-polynomials (poly1 poly2)
  (let ((ugly-ass-list (adding-loop (polynomial-multiply poly1 poly2))))
    (make-pretty ugly-ass-list (- (length ugly-ass-list) 1)))) ;order = length - 1
