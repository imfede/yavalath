;; optmization
(declaim (optimize (speed 3) (safety 0)))

(defvar playable
  (vector 0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 1 1 1 1 1 0
          0 0 0 0 1 1 1 1 1 1 0
          0 0 0 1 1 1 1 1 1 1 0
          0 0 1 1 1 1 1 1 1 1 0
          0 1 1 1 1 1 1 1 1 1 0
          0 1 1 1 1 1 1 1 1 0 0
          0 1 1 1 1 1 1 1 0 0 0
          0 1 1 1 1 1 1 0 0 0 0
          0 1 1 1 1 1 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0)
  "Which cells are playable")

;; which 
(defvar border
  (vector 0 0 0 0 0 1 3 3 3 3 2
          0 0 0 0 5 7 7 7 7 7 2
          0 0 0 5 7 7 7 7 7 7 2
          0 0 5 7 7 7 7 7 7 7 2
          0 5 7 7 7 7 7 7 7 7 2
          4 7 7 7 7 7 7 7 7 7 0
          4 7 7 7 7 7 7 7 7 0 0
          4 7 7 7 7 7 7 7 0 0 0
          4 7 7 7 7 7 7 0 0 0 0
          4 7 7 7 7 7 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0)
  "Used to draw borders")

(defvar piece
  (vector 0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0)
  "Stores the state of the board")

(defvar cells-linear-order
  (vector 16 17 18 19 20 26 27 28 29 30 31 36
          37 38 39 40 41 42 46 47 48 49 50 51
          52 53 56 57 58 59 60 61 62 63 64 67
          68 69 70 71 72 73 74 78 79 80 81 82
          83 84 89 90 91 92 93 94 100 101 102
          103 104)
  "Playable cells in another format (hardcoded 
   for performance)")

(defvar cells-spiral-order
  '(60 61 50 49 59 70 71 72 62 51 40 39
    38 48 58 69 80 81 82 83 73 63 52 41
    30 29 28 27 37 47 57 68 79 90 91 92
    93 94 84 74 64 53 42 31 20 19 18 17
    16 26 36 46 56 67 78 89 100 101 102
    103 104 -1)
  "Playable cells in another order (hardcoded 
   for performance)")

(defvar char-indent " "
  "String used for indentation")

(defvar player 1
  "Current player. Will change from 1 to 2 and vice versa during the game")
(defvar swapped nil
  "Record if the move X was played")
(defvar moves 0
  "How many moves have been played")
(defvar last-move nil
  "What was the index of the last move")
(defvar history-eur nil
  "Will contain the hashtable for the history euristic. Also the trigger: if set to true will enable this euristic")
(defvar killer-eur nil
  "Will contain the hashtable for the killer euristic. Also the trigger: if set to true will enable this euristic")
(defvar eval-counter 0
  "Number of static evaluations")
(defvar runtime-goodies t
  "Do I want an indication of the progress?")

(defvar template (list
                  (cons '(0 1 1 0 1) +1000)
                  (cons '(1 0 1 1 0) +1000)
                  (cons '(0 1 0 0 1) +100)
                  (cons '(1 0 0 1 0) +100)
                  (cons '(0 1 1 0) -50)
                  (cons '(0 1 0 1 0) -30))
  "Patterns and scores for the evaluation function, they will be built for each player")

(defvar patterns-built nil
  "Will keep a version of the built patterns to avoid aving to build them every time")
(defvar patterns-built-player 0
  "Specify for which player the built patterns are (if (not (= player patterns-built-player)) (rebuild-patterns))")


(defvar eval-counter-total   0
  "Total count of evaluations in a game with no euristics")
(defvar eval-counter-total-k 0
  "Total count of evaluations in a game with killer euristics")
(defvar eval-counter-total-h 0
  "Total count of evaluations in a game with history euristics")
(defvar eval-counter-disagreement 0
  "Count of how many thime different/no euristics yielded different results")

(defun adjust-row (row)
  "Helper function for translating between string and indexes"
  (cond ((< row 1) 10000) ;; this is ugly (TODO: change)
        ((= row 1) 4)
        ((= row 2) 3)
        ((= row 3) 2)
        ((= row 4) 1)
        ((> row 4) 0)))

(defun string-to-index (move)
  "Translates moves to index (ex B2 -> 27)"
  (if (string= move "X")
      -1
      (if (parse-integer (subseq move 1 2) :junk-allowed t)
          (let* ((row (1+ (- (char-int (char move 0)) (char-int #\A))))
                 (col (+ (parse-integer (subseq move 1 2)) (adjust-row row)))
                 (index (+ (* row 11) col)))
            (if (and (>= index 0)
                     (< index 121)
                     (= (elt playable index) 1))
                index)))))

(defun index-to-string (index)
  "Translates an index to a move (ex 27 -> B2)"
  (cond
    ((null index) "N/A")
    ((= index -1) "X")
    ((< index  21) (concatenate 'string "A" (write-to-string (- index 15))))
    ((< index  32) (concatenate 'string "B" (write-to-string (- index 25))))
    ((< index  43) (concatenate 'string "C" (write-to-string (- index 35))))
    ((< index  54) (concatenate 'string "D" (write-to-string (- index 45))))
    ((< index  65) (concatenate 'string "E" (write-to-string (- index 55))))
    ((< index  75) (concatenate 'string "F" (write-to-string (- index 66))))
    ((< index  85) (concatenate 'string "G" (write-to-string (- index 77))))
    ((< index  95) (concatenate 'string "H" (write-to-string (- index 88))))
    ((< index 105) (concatenate 'string "I" (write-to-string (- index 99))))))

(defun print-board ()
  "Print a representation of the board"
  (let ((indent 0))
    (loop
       for i from 0 to 10
       do (progn
            (format t (make-string (* 2 indent) :initial-element (char char-indent 0)))
            (let ((row-border   (subseq border   (* i 11) (* (+ i 1) 11)))
                  (row-playable (subseq playable (* i 11) (* (+ i 1) 11)))
                  (row-piece    (subseq piece    (* i 11) (* (+ i 1) 11))))
              (loop
                 for cell-border   across row-border
                 for cell-playable across row-playable
                 for cell-piece    across row-piece
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
                 for cell-border   across row-border
                 for cell-playable across row-playable
                 for cell-piece    across row-piece
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
          player moves swapped (index-to-string last-move)))

(defun invert-player (p)
  "Helper function which evaluates to the other player"
  (declare (fixnum p))
  (the fixnum
       (cond ((= p 1) 2)
             ((= p 2) 1)
             (t 0))))

(defun swap-players ()
  "This function will change ownership of every piece on the board"
  (loop
     for i from 0 to 120
     do (if (not (zerop (elt piece i)))
            (setf (elt piece i) (invert-player (elt piece i)))))
  (setf swapped (not swapped))
  (setf last-move -1))

(defun make-move (index value &optional (bypass-check nil))
  "This function will set index cell to value, if it a valid move. If index = -1 it will perform the X move, if it is 
   valid. If bypass-check evals to t it will perform the move regardless of validity"
  (declare (fixnum index value))
  (if (or bypass-check (= index -1) (zerop (elt piece index)))
      (if (and (= index -1)
               (or bypass-check (= moves 1)))
          (swap-players)
          (progn
            (setf last-move index)
            (setf (elt piece index) value)))))

(defun is-playable (index)
  "Evals to t iff index is a valid move"
  (declare (fixnum index))
  (the boolean
       (and (> index 0)
            (< index 121)
            (= (elt playable index) 1))))

(defun match-pattern-direction (index offset pattern)
  "Evals to t iff the list pattern is equal to the sublist created from the array piece, from the element index 
   advancing using offset as a step."
  (declare (fixnum index offset)
           (list pattern))
  (the boolean
       (if (every #'identity
                  (loop
                     for i from 0 to (- (length pattern) 1)
                     collect (is-playable (+ index (* offset i)))))
           (equal pattern
                  (loop
                     for i from 0 to (- (length pattern) 1)
                     collect (elt piece (+ (* offset i) index)))))))
  
(defun match-pattern (index pattern)
  "Evals to t if cell index is the starting point of pattern on the board"
  (declare (fixnum index)
           (list pattern))
  (the fixnum
       (count 't
              (list
               (match-pattern-direction index 1  pattern)
               (match-pattern-direction index 10 pattern)
               (match-pattern-direction index 11 pattern)))))

(defun test-in-a-row (n)
  "Evals to t iff n element != 0 exists in a row on the board"
  (declare (fixnum n))
  (the boolean
       (loop
          for i from 0 to 120
          for current = (elt piece i)       
          when (and (not (zerop current))
                    (not (zerop (match-pattern
                                 i
                                 (make-list n :initial-element current)))))
          return t
          finally (return nil))))

(defun test-draw ()
  "Evals to t iff every cell is not empty (if used after (test-in-a-row 3) and (test-in-a-row 4) will reveal if there
   is a draw"
  (the boolean
       (loop
          for i from 0 to 120
          when (and (= (elt playable i) 1)
                    (= (elt piece i) 0))
          return nil
          finally (return t))))

(defun test-winner ()
  "Evals to the winning player, to 0 if there is a draw or to nil if the game is still on"
  (let ((t4 (test-in-a-row 4))
        (t3 (test-in-a-row 3))
        (td (test-draw)))
    (cond
      ((not (null t4)) (invert-player player))
      ((not (null t3)) player)
      (td 0))))

(defun static-evaluation-pattern (pattern points)
  "Evals to the number of times pattern is present in the board times points"
  (declare (fixnum points)
           (list pattern))
  (the fixnum
       (* points
          (loop
             for index across cells-linear-order
             for matches = (match-pattern index pattern)
             when (not (zerop matches))
             sum matches))))

(defun build-patterns (template)
  "Build the template according to the current player if it is not valid. Evals to the correct set of patterns"
  (if (not (= player patterns-built-player))      
      (progn
        (setf patterns-built
              (loop
                 for cell in template
                 collect (cons
                          (car cell)
                          (if (= player 1) (cdr cell) (- (cdr cell))))
                 collect (cons
                          (mapcar #'invert-player (car cell))
                          (if (= player 1) (- (cdr cell)) (cdr cell)))))
        (setf patterns-built-player player)))
  patterns-built)

(defun static-evaluation (patterns)
  "Evals to a score evaluating the situation on the board according to the current player"
  (incf eval-counter)
  (the fixnum
       (let ((result (test-winner)))
         (cond
           ((equal result player) 1000000000)
           ((equal result (invert-player player)) -1000000000)
           (t (loop
                 for cell in patterns
                 sum (static-evaluation-pattern (car cell) (cdr cell))))))))

(defun fill-hashtable (hashtable)
  "Helper function which initialize hashtable"
  (loop
     for move in cells-spiral-order
     do (setf (gethash move hashtable) 0)
     finally (return hashtable)))

(defun setup-history ()
  "Helper function to prepare a hashtable for the history euristic"
  (setf killer-eur nil)
  (setf history-eur (fill-hashtable (make-hash-table))))

(defun setup-killer (depth)
  "Helper function to prepare a hashtable for the killer euristic"
  (setf history-eur nil)
  (setf killer-eur (make-hash-table))
  (loop
     for i from 0 to depth
     do (setf (gethash i killer-eur) (fill-hashtable (make-hash-table)))))

(defun generate-moves (depth)
  "Evals to a list of possible moves, using one of the two euristics if specified"
  (declare (fixnum depth))
  (the list
       (if (not (test-winner))
           (let ((cells (if (= moves 1)
                            cells-spiral-order
                            (remove -1 cells-spiral-order))))
             (if (or history-eur killer-eur)
                 (loop
                    for cell in cells
                    with best-move = 0
                    with best-value = 0
                    with hash-table = (cond (history-eur history-eur)
                                            (killer-eur (gethash depth killer-eur)))
                    when (and (gethash cell hash-table)
                              (> (gethash cell hash-table) best-value))
                    do (progn
                         (setf best-move (length moves))
                         (setf best-value (gethash cell hash-table)))
                    when (or (= cell -1) (zerop (elt piece cell)))
                    collect cell into moves
                    finally (progn
                              (rotatef (nth 0 moves) (nth best-move moves))
                              (return moves)))
                 (loop
                    for cell in cells
                    when (or (= cell -1) (zerop (elt piece cell)))
                    collect cell))))))

(defun ab-make-move (move)
  "Function which should be called from the alpha-beta algorithm to make a move"
  (declare (fixnum move))
  (make-move move player)
  (incf moves)
  (setf player (invert-player player)))

(defun ab-unmake-move (move)
  "Function which should be called from the alpha-beta algorithm to un-make a move"
  (declare (fixnum move))
  (make-move move 0 't)
  (decf moves)
  (setf player (invert-player player)))

(defun alpha-beta (search-height depth achievable hope &optional previous-move)
  "The actual search algorithm, evals to a cons cell (move . value)"
  (declare (fixnum search-height depth achievable hope))
  (let ((possible-moves (generate-moves depth)))
    (if (or (= depth search-height)
            (equal possible-moves '()))
        (cons previous-move (static-evaluation (build-patterns template)))
        (loop
           for move in possible-moves
           with temp and best-move and move-analysis and count = 0
           do (progn
                (ab-make-move move)
                (setf move-analysis (alpha-beta search-height
                                                (+ depth 1)
                                                (- hope)
                                                (- achievable)
                                                move))
                (setf temp (- (cdr move-analysis)))
                (ab-unmake-move move)
                ;; --- runtime goodies 
                (if (and runtime-goodies
                         (= depth 0))
                    (progn
                      (format t "~ASearching: ~A / ~A " #\return (incf count) (length possible-moves))
                      (finish-output)))
                (if (null best-move) (setf best-move (cons move temp)))
                (if (>= temp hope) (progn
                                     (if (or killer-eur history-eur)
                                         (let ((hash-table (if history-eur
                                                               history-eur
                                                               (gethash depth killer-eur))))
                                           (setf (gethash move hash-table)
                                                 (+ (gethash move hash-table) 1))))
                                     (return (cons move temp))))
                (if (> temp achievable)
                    (progn
                      (setf achievable temp)
                      (setf best-move (cons move temp)))))
           finally (return best-move)))))

(defun ai-get-next-move (search-height)
  "Evals to the best possible moves looking ahead search-height moves, if specified will use euristic"
  (setf eval-counter 0)
  (cond
    (history-eur (setup-history))
    (killer-eur (setup-killer search-height)))
  (car (alpha-beta search-height 0 -1000000000 1000000000)))

(defun ai-get-next-move-all-eur (search-height &aux move-k count-k move-h count-h move count)
  "Evals to a list gathering statistics about moves chosen by euristics and static evaluations count"
  (setf history-eur t)
  (setf move-h (ai-get-next-move search-height))
  (format t "~&")
  (setf count-h eval-counter)
  (setf history-eur nil)
  (setf killer-eur t)
  (setf move-k (ai-get-next-move search-height))
  (format t "~&")
  (setf count-k eval-counter)
  (setf killer-eur nil)
  (setf move (ai-get-next-move search-height))
  (format t "~&")
  (setf count eval-counter)
  (list :move-h move-h
        :move-k move-k
        :move move
        :count-h count-h
        :count-k count-k
        :count count))

(defun reset-game ()
  "Will reset the internal state to play another game"
  (setf eval-counter-total 0)
  (setf eval-counter-total-h 0)
  (setf eval-counter-total-k 0)
  (setf eval-counter-disagreement 0)
  (setf moves 0)
  (setf player 1)
  (setf swapped nil)
  (setf last-move nil)
  (setf history-eur nil)
  (setf killer-eur nil)
  (loop
     for index from 0 to 120
     do (setf (elt piece index) 0)))

(defun play-one-ply-human ()
  "Contains the logic to make a human player play a ply"
  (print-board)
  (format t "> ")
  (finish-output)
  (let ((input (read-line)))
    (if (null (string-to-index input))
        (progn
          (format t "Invalid move!~%")
          (play-one-ply-human))
        (progn
          (setf last-move (string-to-index input))
          (make-move (string-to-index input) player)
          (incf moves)
          (setf player (invert-player player))))))

(defun play-one-ply-ai (search-height &aux move)
  "Contains the logic to use the alpha-beta algorithm (with euristics if specified) to play one ply"
  (print-board)
  (setf move (ai-get-next-move search-height))
  (make-move move player)
  (setf last-move move)
  (incf moves)
  (setf player (invert-player player)))

(defun play-one-ply-ai-all-eur (search-height &aux result move)
  "Will use the alpha-beta algorithm to play a ply with all the euristics and gather data about their performance"
  (print-board)
  (setf result (ai-get-next-move-all-eur search-height))
  (incf eval-counter-total   (getf result :count))
  (incf eval-counter-total-h (getf result :count-h))
  (incf eval-counter-total-k (getf result :count-k))
  (setf move (getf result :move))
  (if (not (= (getf result :move)
              (getf result :move-h)
              (getf result :move-k)))
      (incf eval-counter-disagreement))
  (make-move move player)
  (setf last-move move)
  (incf moves)
  (setf player (invert-player player)))

(defun play (search-height &optional is-player1-h is-player2-h)
  "Contains the logic to play a game, if the optional paramters are non-nil the corrisponding player is assumed to
   be human. If the optional paramters are omitted the AI will play against itself"
  (loop
     while (not (test-winner))
     do (if (or (and (= player 1)
                     is-player1-h)
                (and (= player 2)
                     is-player2-h))
            (play-one-ply-human)
            (play-one-ply-ai search-height)))
  (print-board)
  (format t "The winner is: ~A~%" (test-winner)))

(defun play-all-eur (search-height)
  "Will perform an entire play human vs AI gathering data about euristics performance"
  (loop
     while (not (test-winner))
     do (if (= (mod moves 2) 0)
            (play-one-ply-human)
            (play-one-ply-ai-all-eur search-height)))
  (print-board)
  (format t "The winner is: ~A~%" (test-winner))
  (list :count   eval-counter-total
        :count-h eval-counter-total-h
        :count-k eval-counter-total-k
        :disag   eval-counter-disagreement
        :moves   moves))
  
