;; This program is Free Software under the GPLv3 and above.

;; I made this to rebel against my Control Theory Prof...

;; Status: Don't use the make-pretty function. You can multiply polynomials of
;; any size together and you'll get the answer in list form. You can also factor
;; constants and a singular variable out. You can also solve any polynomial for
;; any real integer solutions. Yay! There is more to come!

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

(defparameter *factee* nil)		;used for coeff of factored variable

;; Just for readability
(defun factor-constant (polynomial factee)
  (if (equal 1 factee)
      (setf *factee* nil)
      (setf *factee* factee))
  (mapcar (lambda (x) (/ x factee)) polynomial))

(defun try-factors (polynomial smallest-coeff factee possible-factor)
  (let ((is-factor (possible-factor
		    (mapcar (lambda (x) (mod x factee)) polynomial))))
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

;; Only works for polynomials of the form:
;; ax^n + bx^n-1 + ... + cx, if there are any other zeros, it will recurse forever!
(defun factor-terms-polynomial (polynomial)
  (let ((last-term (car (reverse polynomial))))
    (if (equal 0 last-term)
	(let ((no-zero-poly (reverse (cdr (reverse polynomial))))) ;remove the zero
	  (let ((factored-poly (factor-constant-polynomial no-zero-poly)))
	    (append `(,*factee* 0) `(,factored-poly)))) ;quasiquoting!
	(let ((factored-poly (factor-constant-polynomial polynomial))) ;just factor a constant
	  (append `(,*factee*) `(,factored-poly)))))) ;more quasiquoting!

;; Remove the factored constant/variable from the front
(defun extract-poly (semi-factored-poly)
  (unless (null semi-factored-poly)
    (let ((cur-elem (car semi-factored-poly)))
      (if (listp cur-elem)
	  cur-elem
	  (extract-poly (cdr semi-factored-poly))))))

(defun plug-and-chug (polynomial pos-sol)
  (if (null polynomial)
      0
      (let ((cur-term (car polynomial))
	    (order (- (length polynomial) 1)))
	(+ (* cur-term (expt pos-sol order)) (plug-and-chug
					      (cdr polynomial) pos-sol)))))

(defun solution-checker (polynomial solution order)
  (unless (or (equal 0 order) (> solution 1000))
    (let ((is-solution (plug-and-chug polynomial solution)))
      (if (= 0 is-solution)		; I use "=" cuz it could be 0.0 or 0
	  (cons solution (solution-checker
			  polynomial (+ solution 1) (- order 1)))
	  (solution-checker polynomial (+ solution 1) order)))))

;; Can only factor real, integer solutions as of now
;; Works for any order polynominal too!
(defun factor-reals (polynomial)
  (solution-checker polynomial -1000.0 (- (length polynomial) 1)))
