;; This program is released under the GPLv3 and above.
;; It will calculate huge fibonacci values using the big-number list from my
;; big number program.

;; Try (fibonacci 100000). This is the 100,000th fibonacci number... It's really
;; big. It has 20,899 digits! Enjoy.

;; Solely for bigger numbers
(load "/home/kevin/development/lisp/Common-Lisp/big-number.lisp")

;; Pads the front of the smaller number with zeros so we can do addition easier.
;; For example: 123 + 12, would be 123 + 012
(defun pad-front (list-to-pad num-zeros)
  (if (equal 0 num-zeros)
      list-to-pad
      (cons 0 (pad-front list-to-pad (- num-zeros 1)))))

(defun add-loop (fir-num sec-num)
  (if (null fir-num)			;both go null at the same time
      nil
      (let ((fir-digit (car fir-num)) (sec-digit (car sec-num)))
	(cons (+ fir-digit sec-digit) (add-loop (cdr fir-num) (cdr sec-num))))))

;; Adds digit by digit
(defun addition (fir-num sec-num)	;where fir- and sec-num are list numbers
  (let ((len-fir (length fir-num)) (len-sec (length sec-num)))
    (let ((pad-sec-num (pad-front sec-num (- len-fir len-sec))))
      (reverse
       (digit-fixer (add-loop (reverse fir-num) (reverse pad-sec-num)))))))

(defun fib-loop (fir-num sec-num steps)
  (let ((new-step (- steps 1)))
    (if (equal 0 new-step)
	fir-num
	(fib-loop (addition fir-num sec-num) fir-num new-step))))

(defun fibonacci (steps)		;number of steps
  (fib-loop '(1) '(0) steps))
