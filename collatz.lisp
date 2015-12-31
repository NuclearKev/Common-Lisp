;;; This program computes the number of steps in the Collatz function given a
;;; natural number. You can also calculate the lowest number with a given number
;;; of steps using the collatz-reverse function
(defun even-or-odd (x)
  (if (evenp x)
      (/ x 2)
      (+ 1 (* 3 x))))

(defun collatz-loop (x steps)
  (if (equal 1 x)
      steps
      (collatz-loop (even-or-odd x) (+ 1 steps))))

(defun collatz-reverse-loop (x steps)
  (if (equal steps (collatz-loop x 0))
      x
      (collatz-reverse-loop (+ 1 x) steps)))

;; Run these functions
(defun collatz (x)
  (collatz-loop x 0))

(defun collatz-reverse (steps)
  (collatz-reverse-loop 2 steps))
