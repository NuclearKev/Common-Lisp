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
	(car uncombined-polynomial)
        (cons excluded-term (adding-loop (cons (polynomial-add first-adding-poly sec-adding-poly) (cddr uncombined-polynomial)))))))

(defun multiply-polynomials (poly1 poly2)
  (adding-loop (polynomial-multiply poly1 poly2)))
