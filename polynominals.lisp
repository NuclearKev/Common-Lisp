;; This program is Free Software under the GPLv3 and above.

;; I made this to rebel against my Control Theory Prof...

;; Current bug: Negatives and zeros aren't working properly (beware!)

(defun sign-finder (number)
  (cond ((null number)
	 (format t " ")
	 nil)
	((equal 0 number)
	 0)
	((> 0 number)
	 (format t "-")
	 -1)
	(t
	 (format t "+")
	 1)))

;; Very very very ugly function DOES NOT WORK FULLY!
(defun make-pretty (final-list order)	;length - 1 = order of polynomial
  (unless (null final-list)
    (let ((coef (car final-list))
	  (next-coef (cadr final-list)))
      (cond ((equal 1 order)
	     (if (equal 1 coef)
		 (format t " x ")
		 (format t " ~dx " coef)))
	    ((> 1 order)
	     (format t " ~d " coef))
	    (t
	     (if (equal 1 coef)
		 (format t " x^~d " order)
		 (format t " ~dx^~d " coef order))))
      (case (sign-finder next-coef)
	((nil) (make-pretty  (cdr final-list) (- order 1)))
	((0)   (sign-finder (caddr final-list))
	 (make-pretty (cons (abs next-coef) (cddr final-list)) (- order 1)))
	((-1)  (make-pretty  (cons (abs next-coef) (cddr final-list)) (- order 1)))
	((1)   (make-pretty  (cdr final-list) (- order 1)))))))

;;; Multiplying ;;;

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

;;; Factoring ;;;

(defun possible-factor (new-coeffs)
  (if (null new-coeffs)
      t
      (let ((cur-coeff (car new-coeffs))
	    (rest-of (cdr new-coeffs)))
	(when (equal 0 cur-coeff)
	  (possible-factor rest-of)))))

(defun factor-constant (polynomial factee)
  (mapcar #'(lambda (x) (/ x factee)) polynomial))

(defun try-factors (polynomial smallest-coeff factee possible-factor)
  (let ((is-factor (possible-factor
		    (mapcar #'(lambda (x) (mod x factee)) polynomial)))) ;functional!
    (cond ((and (not is-factor) (equal smallest-coeff factee)) ;if the current factee is not a factor
	   (factor-constant polynomial possible-factor))
	  ((and is-factor (equal smallest-coeff factee)) ;if the current factee is a factor
	   (factor-constant polynomial factee))
	  (is-factor
	   (try-factors polynomial smallest-coeff (+ 1 factee) factee))
	  (t
	   (try-factors polynomial smallest-coeff (+ 1 factee) possible-factor)))))

(defun find-smallest-coefficient (polynomial smallest) ;you want to make smallest huge to start
  (if (null polynomial)
      smallest
      (let ((x (car polynomial))
	    (rest-of-poly (cdr polynomial)))
	(if (> x smallest)
	    (find-smallest-coefficient rest-of-poly smallest)
	    (find-smallest-coefficient rest-of-poly x)))))

(defun factor-constant-polynomial (polynomial)
  (try-factors polynomial (find-smallest-coefficient polynomial 100000) 1 nil))

;; Unfinished
(defun factor-variable-polynomial (polynomial)
  (let ((last-term (car (reverse polynomial))))
    (if (equal 0 last-term)
	(reverse (cdr (reverse polynomial)))
	(princ "Cannot factor"))))