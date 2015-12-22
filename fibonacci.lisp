;; This program is released under the GPLv3 and above.
;; It will calculate huge fibonacci values using the big-number list from my
;; big number program.

;; Try (fibonacci 100000). This is the 100,000th fibonacci number... It's really
;; big. It has 20,899 digits! Enjoy.

;; Solely for bigger numbers
(load "/home/kevin/development/lisp/Common-Lisp/big-number.lisp")

(defun fib-loop (fir-num sec-num steps)
  (let ((new-step (- steps 1)))
    (if (equal 0 new-step)
	fir-num
	(fib-loop (addition fir-num sec-num) fir-num new-step))))

(defun fibonacci (steps)		;number of steps
  (fib-loop '(1) '(0) steps))
