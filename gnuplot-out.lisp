;;This program is Free Software under the GNU GPLv3 and above. I think.

;;The purpose of this little program is to output coordinates to a *.out file
;;so you can plot points with GnuPlot.

;;As of now, this program can write the values to the file but *cannot* execute
;;the graphing part. I plan on adding that.
;;It should also be noted that this only does 1 ordered pair at a time, so you
;;would want to add this to a recursive function in the main program. Enjoy.

;;The sole purpose of this is to make the printing statement more readable
(defun print-x(x)
  (prin1-to-string x))

;;The sole purpose of this is to make the printing statement more readable
(defun print-y(y)
  (prin1-to-string y))

;;Used to create a blank, new file. (this must be done before writing to it
(defun new-file(fname)
  (with-open-file (my-stream
		   fname
		   :direction :output
		   :if-exists :supersede)))

;;Writes the coordinates to the file in the form "x y", then jumps to the next
;;line
(defun write-to-file(fname x y)
  (with-open-file (my-stream
		   fname
		   :direction :output
		   :if-exists :append)
    (princ
     (concatenate 'string (fresh-line) (print-x x) " " (print-y y)) my-stream)
    (fresh-line my-stream)))
