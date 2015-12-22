;;This program is Free Software under the GNU GPLv3.0 and above

;;This program will take the factorial of any number you give it.
;;It stores each digit of the number in a list so that you can deal with huge
;;numbers. 
;;In the comments, I use the terms MSD and LSD, which means, Most Significant 
;;Digit and Least Significant Digit.

;;There still may be a few changes that I'll add to make this more Lispy, but
;;for now, try doing 1000!; it has ~2500 digits in it and is awesome!

(load "/home/kevin/development/lisp/Common-Lisp/big-number.lisp")

;;Used to increment one of the numbers
(defun increment(number-list)
  (let ((reversed-list (reverse number-list)))
    (let ((elem-to-increment (car reversed-list)) (rest-of-list (cdr reversed-list)))
      (reverse (digit-fixer (cons (+ elem-to-increment 1) rest-of-list))))))

;;computes the factorial of the number fir-num (in list form)
(defun take-factorial(fir-num sec-num z)
  (if (compare fir-num sec-num)
      z
      (take-factorial fir-num (increment sec-num) (multiply z sec-num))))


(defun factorial(f)
  (take-factorial (convert-to-list f) '(2) '(1)))
