(defvar playable
  '(0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 1 1 1 1 1 0
	0 0 0 0 1 1 1 1 1 1 0
	0 0 0 1 1 1 1 1 1 1 0
	0 0 1 1 1 1 1 1 1 1 0
	0 1 1 1 1 1 1 1 1 1 0
	0 1 1 1 1 1 1 1 1 0 0
	0 1 1 1 1 1 1 1 0 0 0
	0 1 1 1 1 1 1 0 0 0 0
	0 1 1 1 1 1 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0))

(defvar border
  '(0 0 0 0 0 1 3 3 3 3 2
	0 0 0 0 5 7 7 7 7 7 2
	0 0 0 5 7 7 7 7 7 7 2
	0 0 5 7 7 7 7 7 7 7 2
	0 5 7 7 7 7 7 7 7 7 2
	4 7 7 7 7 7 7 7 7 7 0
	4 7 7 7 7 7 7 7 7 0 0
	4 7 7 7 7 7 7 7 0 0 0
	4 7 7 7 7 7 7 0 0 0 0
	4 7 7 7 7 7 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0))

(defvar piece
  '(0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0
	0 0 0 0 0 0 0 0 0 0 0))

(defvar char-indent " ")

(defvar player 1)  ;; 1 or 2
(defvar swapped 0) ;; 0 or 1
(defvar moves 0)   ;; number of moves
(defvar last-move nil)
(defvar last-move-string nil)
(defvar move-tree nil)

;;; static eval patterns:
(defvar template '())
(setf template (list (cons '(0 1 1 0 1) +100)
					 (cons '(1 0 1 1 0) +120) ;; +100
					 (cons '(0 1 0 0 1) +50)
					 (cons '(1 0 0 1 0) +50)
					 (cons '(0 1 1 0) -20)
					 (cons '(0 1 0 1 0) -25)))

(defun print-board ()
  (let ((indent 0))
	(loop
	   for i from 0 to 10
	   do (progn
			(format t (make-string (* 2 indent) :initial-element (char char-indent 0)))
			(let ((row-border   (subseq border   (* i 11) (* (+ i 1) 11)))
				  (row-playable (subseq playable (* i 11) (* (+ i 1) 11)))
				  (row-piece    (subseq piece    (* i 11) (* (+ i 1) 11))))
			  (loop
				 for cell-border   in row-border
				 for cell-playable in row-playable
				 for cell-piece    in row-piece
				 do (progn
					  (format t char-indent)
					  (cond ((= cell-piece 0) (format t char-indent))
							((= cell-piece 1) (format t "X"))
							((= cell-piece 2) (format t "O")))
					  (format t char-indent)
					  (if (member cell-border '(4 5 7))
						  (format t "|")
						  (format t char-indent))))
			  (format t "~%")
			  (format t (make-string (* 2 indent) :initial-element (char char-indent 0)))
			  (loop
				 for cell-border   in row-border
				 for cell-playable in row-playable
				 for cell-piece    in row-piece
				 do (progn
					  (if (member cell-border '(2 3 7))
						  (format t "\\")
						  (format t char-indent))
					  (format t char-indent)
					  (if (member cell-border '(1 3 5 7))
						  (format t "/")
						  (format t char-indent))
					  (format t char-indent)))
			  (format t "~%"))
			(setf indent (+ indent 1)))))
  (format t "Player: ~A, moves: ~A, swapped ~A, Last move: ~A~%"
		  player moves swapped last-move-string))

(defun adjust-row (row)
  (cond ((< row 1) 10000) ;; this is ugly (TODO: change)
		((= row 1) 4)
		((= row 2) 3)
		((= row 3) 2)
		((= row 4) 1)
		((> row 4) 0)))

(defun move-to-index (move)
  "translates moves to index (ex B2 -> 27)"
  (if (not (parse-integer (subseq move 1 2) :junk-allowed t))
	  nil
	  (let* ((row (+ (- (char-int (char move 0)) (char-int #\A)) 1))
			 (col (+ (parse-integer (subseq move 1 2)) (adjust-row row))))
		(let ((index (+ (* row 11) col)))
		  (if (and (not (null (nth index playable)))
				   (= (nth index playable) 1))
			  index
			  nil)))))

(defun make-move (index value &optional (bypass-check nil))
  (if (and (not (null index))
		   (or bypass-check (zerop (nth index piece))))
	  (setf (nth index piece) value)
	  nil))

(defun swap-players ()
  (loop
	 for i from 0 to 120
	 do (if (not (zerop (nth i piece)))
			(setf (nth i piece) (invert-player (nth i piece))))
	 finally (return t))
  (setf swapped 1))

(defun board-input (input value)
  (cond ((and (string= input "X")
			  (= moves 1))
		 (swap-players))
		((= (length input) 2)
		 (progn
		   (setf last-move-string input)
		   (setf last-move (move-to-index input))
		   (make-move (move-to-index input) value)))))

(defun is-playable (index)
  (and (not (null (nth index playable)))
	   (= (nth index playable) 1)))

(defun match-pattern-direction (index offset pattern)
  (if (every #'identity
			 (loop
				for i from 0 to (- (length pattern) 1)
				collect (is-playable (+ index (* offset i)))))
	  (equal pattern
			 (loop
				for i from 0 to (- (length pattern) 1)
				collect (nth (+ (* offset i) index) piece)))))
  
(defun match-pattern (index pattern)
  (count 't
		 (list
		  (match-pattern-direction index 1  pattern)
		  (match-pattern-direction index 10 pattern)
		  (match-pattern-direction index 11 pattern))))

(defun static-evaluation-pattern (pattern points)
  (loop
	 for index from 0 to 117
	 for matches = (match-pattern index pattern)
	 when (not (zerop matches))
	 sum (* matches points) into result
	 finally (return result)))

(defun build-patterns (template)
  (loop
	 for cell in template
	 collect (cons
			  (car cell)
			  (if (= player 1) (cdr cell) (- (cdr cell))))
	 collect (cons
			  (mapcar #'invert-player (car cell))
			  (if (= player 1) (- (cdr cell)) (cdr cell)))))

(defun static-evaluation (patterns)
  (let ((result (test-winner)))
	(cond
	  ((equal result player) -1e9)
	  ((equal result (invert-player player)) 1e9)
	  (t (loop
			for cell in patterns
			sum (static-evaluation-pattern (car cell) (cdr cell)) into sum
			finally (return sum))))))

(defun test-in-a-row (n)
  (loop
	 for i from 0 to 120
	 for current = (nth i piece)	   
	 when (and (not (zerop current))
			   (not (zerop (match-pattern
							i
							(make-list n :initial-element current)))))
	 return current))

(defun test-draw ()
  (loop
	 for i from 0 to 120
	 when (and (= (nth i playable) 1)
			   (= (nth i piece) 0))
	 return nil
	 finally (return t)))

(defun invert-player (p)
  (cond ((= p 1) 2)
		((= p 2) 1)
		((= p 0) 0)))

(defun test-winner ()
  (let ((t4 (test-in-a-row 4))
		(t3 (test-in-a-row 3))
		(td (test-draw)))
	(cond
	  ((not (null t3)) (invert-player t3))
	  ((not (null t4)) t4)
	  (td 0))))

(defun play-one-ply ()
  (print-board)
  (format t "Current eval: ~A~%" (static-evaluation (build-patterns template)))
  (format t "> ")
  (finish-output)
  (let ((input (read-line)))
	(if (not (board-input input player))
		(progn
		  (format t "Invalid move!~%")
		  (play-one-ply))
		(progn
		  (setf moves (+ moves 1))
		  (setf player (invert-player player))))))

(defun play ()
  (loop
	 while (not (test-winner))
	 do (play-one-ply))
  (print-board)
  (format t "The winner is: ~A~%" (test-winner)))

(defun generate-moves ()
  (if (not (test-winner))
	  (loop
		 for i from 0 to 120
		 when (and (not (zerop (nth i playable)))
				   (zerop (nth i piece)))
		 collect i)))

(defun ab-make-move (move)
  (make-move move player)
  (setf moves (+ moves 1))
  (setf player (invert-player player)))

(defun ab-unmake-move (move)
  (make-move move 0 't)
  (setf moves (- moves 1))
  (setf player (invert-player player)))

(defun alpha-beta (search-height depth achievable hope &optional previous-move)
  (let ((possible-moves (generate-moves)))
	(if (or (= depth search-height)
			(equal possible-moves '()))
		(cons previous-move (static-evaluation (build-patterns template)))
		(loop
		   for move in possible-moves
		   with temp
		   with best-move = nil
		   do (progn
				(ab-make-move move)
				(setf temp (- (cdr (alpha-beta search-height
											   (+ depth 1)
											   (- hope)
											   (- achievable)
											   move))))
				(ab-unmake-move move)
				(if (>= temp hope) (return (cons move temp)))
				(if (> temp achievable)
					(progn
					  (setf achievable temp)
					  (setf best-move (cons move temp)))))
		   finally (if (null best-move)
					   (return (cons -1 achievable))
					   (return best-move))))))

(defun alpha-beta-search (search-height)
  (car (alpha-beta search-height 1 -1e9 1e9)))

(defun auto-play (depth)
  (loop
	 while (not (test-winner))
	 with move
	 do (progn
		  (print-board)
		  (setf move (alpha-beta-search depth))
		  (if (null move) (return nil))
		  (make-move move player)
		  (setf last-move move)
		  (setf moves (+ 1 moves))
		  (setf player (invert-player player))))
  (print-board)
  (format t "The winner is: ~A~%" (test-winner)))

(defun reset-game ()
  (setf moves 0)
  (setf player 1)
  (setf swapped 0)
  (setf last-move nil)
  (setf last-move-string "")
  (setf move-tree nil)
  (loop
	 for index from 0 to 120
	 do (setf (nth index piece) 0)))
