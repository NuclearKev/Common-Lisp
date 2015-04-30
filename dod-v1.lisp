;This game is NOT finished yet!

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst)) ;converts the playing board into an array.... clean code

(defun gen-board () ;creates a randomized board.... dirty code
  (board-array (loop for n below *board-hexnum* ;converts the newly made board from a list to an array by calling 'board-array'
		  collect (list (random *num-players*)
				(1+ (random *max-dice*)))))) ;randomly picks which player has which hexagons and how many are on each hexagon

(defun player-letter (n) ;converts the player's number to a letter.... clean code
  (code-char (+ 97 n))) ;converts ASCII code to the correct character (code-char does that)

(defun draw-board (board) ;draws the 'board' on screen, thus, dirty code
  (loop for y below *board-size* ;runs through all the rows
     do (progn (fresh-line)
	       (loop repeat (- *board-size* y) ;this loop basically gives the 'board' a tilted look, so it resembles hexagons
		  do (princ "  "))
	       (loop for x below *board-size*  ;runs through all the columns
		  for hex = (aref board (+ x (* *board-size* y))) ;calculates the appropiate hex number and gets that number from the array
		  do (format t "~a-~a " (player-letter (first hex)) 
			     (second hex)))))) ;prints out all that crap we got above

(defun game-tree (board player spare-dice first-move) ;builds a tree of all possible moves, only called in the beginning.... clean code
  (list player
	board
	(add-passing-move board
			  player
			  spare-dice
			  first-move
			  (attacking-moves board player spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves) ;if the player wants to pass, clean code
  (if first-move
      moves
      (cons (list nil
		  (game-tree (add-new-dice board player (1- spare-dice))
			     (mod (1+ player) *num-players*)
			     0
			     t))
	    moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (mapcan (lambda (src)
	      (when (eq (player src) cur-player)
		(mapcan (lambda (dst)
			  (when (and (not (eq (player dst) cur-player))
				     (> (dice src) (dice dst)))
			    (list
			     (list (list src dst)
				   (game-tree (board-attack board cur-player src dst (dice src))
					      cur-player
					      (+ spare-dice (dice dst))
					      nil)))))
			(neighbors src))))
			  (loop for n below *board-hexnum*
			       collect n))))
