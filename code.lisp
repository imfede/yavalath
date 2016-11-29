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

(defvar cells-spiral-order
  '(60 61 50 49 59 70 71 72 62 51 40 39
    38 48 58 69 80 81 82 83 73 63 52 41
    30 29 28 27 37 47 57 68 79 90 91 92
    93 94 84 74 64 53 42 31 20 19 18 17
    16 26 36 46 56 67 78 89 100 101 102
    103 104))

(defvar char-indent " ")

(defvar player 1)  ;; 1 or 2
(defvar swapped 0) ;; 0 or 1
(defvar moves 0)   ;; number of moves
(defvar last-move nil)
(defvar history-eur nil)
(defvar killer-eur nil)

;;; static eval patterns:
(defvar template '())
(setf template (list
                (cons '(1 1 0 1) +1000)
                (cons '(1 0 1 1) +1000)
                (cons '(1 0 1 1 0 1) +1000)
                (cons '(1 1 0 1 0 1 1) +1000)
                (cons '(1 0 0 1) +50)
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
          player moves swapped (index-to-string last-move)))

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

(defun index-to-string (index)
  (cond
    ((null index) "N/A")
    ((< index  21) (concatenate 'string "A" (write-to-string (- index 15))))
    ((< index  32) (concatenate 'string "B" (write-to-string (- index 25))))
    ((< index  43) (concatenate 'string "C" (write-to-string (- index 35))))
    ((< index  54) (concatenate 'string "D" (write-to-string (- index 45))))
    ((< index  65) (concatenate 'string "E" (write-to-string (- index 55))))
    ((< index  75) (concatenate 'string "F" (write-to-string (- index 66))))
    ((< index  85) (concatenate 'string "G" (write-to-string (- index 77))))
    ((< index  95) (concatenate 'string "H" (write-to-string (- index 88))))
    ((< index 105) (concatenate 'string "I" (write-to-string (- index 99))))))


(defun make-move (index value &optional (bypass-check nil))
  (if (and (not (null index))
           (or bypass-check (zerop (nth index piece))))
      (progn
        (setf last-move index)
        (setf (nth index piece) value))))

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
     for index from 0 to 120
     for matches = (match-pattern index pattern)
     when (not (zerop matches))
     sum (* matches points)))

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
      ((equal result player) 1e6)
      ((equal result (invert-player player)) -1e6)
      (t (loop
            for cell in patterns
            sum (static-evaluation-pattern (car cell) (cdr cell)))))))

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
      ((not (null t4)) t4)
      ((not (null t3)) (invert-player t3))
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

(defun fill-hashtable (hashtable)
  (loop
     for move in cells-spiral-order
     do (setf (gethash move hashtable) 0)
     finally (return hashtable)))

(defun setup-history ()
  (setf killer-eur nil)
  (setf history-eur (fill-hashtable (make-hash-table))))

(defun setup-killer (depth)
  (setf history-eur nil)
  (setf killer-eur (make-hash-table))
  (loop
     for i from 0 to depth
     do (setf (gethash i killer-eur) (fill-hashtable (make-hash-table)))))

(defun generate-moves (depth)
  (if (not (test-winner))
      (if (or history-eur killer-eur)
          (loop
             for cell in cells-spiral-order
             with best-move = 0
             with best-value = 0
             with hash-table = (cond (history-eur history-eur)
                                     (killer-eur (gethash depth killer-eur)))
             when (and (gethash cell hash-table)
                       (> (gethash cell hash-table) best-value))
             do (progn
                  (setf best-move (length moves))
                  (setf best-value (gethash cell hash-table)))
             when (zerop (nth cell piece))
             collect cell into moves
             finally (progn
                       (rotatef (nth 0 moves) (nth best-move moves))
                       (return moves)))
          (loop
             for cell in cells-spiral-order
             when (zerop (nth cell piece))
             collect cell))))

(defun ab-make-move (move)
  (make-move move player)
  (setf moves (+ moves 1))
  (setf player (invert-player player)))

(defun ab-unmake-move (move)
  (make-move move 0 't)
  (setf moves (- moves 1))
  (setf player (invert-player player)))

(defun invert-value (move-analysis)
  (list :move  (getf move-analysis :move)
        :value (- (getf move-analysis :value))
        :depth (getf move-analysis :depth)
        :moves (getf move-analysis :moves)))

(defun alpha-beta (search-height depth achievable hope &optional previous-move)
  (let ((possible-moves (generate-moves depth)))
    (if (or (= depth search-height)
            (equal possible-moves '()))
        (list :move previous-move
              :moves (list previous-move)
              :value (static-evaluation (build-patterns template))
              :depth depth)
        (loop
           for move in possible-moves
           with temp and best-move and move-analysis
           do (progn
                (ab-make-move move)
                (setf move-analysis (invert-value (alpha-beta search-height
                                                              (+ depth 1)
                                                              (- hope)
                                                              (- achievable)
                                                              move)))
                (if (not (null previous-move))
                    (setf (getf move-analysis :moves)
                          (append (getf move-analysis :moves) (list previous-move))))
                (setf temp (getf move-analysis :value))
                (ab-unmake-move move)
                (if (null best-move) (setf best-move move-analysis))
                (if (>= temp hope) (progn
                                     (if (or killer-eur history-eur)
                                         (let ((hash-table (if history-eur
                                                               history-eur
                                                               (gethash depth killer-eur))))
                                           (setf (gethash move hash-table)
                                                 (+ (gethash move hash-table) 1))))
                                     (return move-analysis)))
                (if (> temp achievable)
                    (progn
                      (setf achievable temp)
                      (setf best-move move-analysis))))
           finally (return best-move)))))

(defun translate-moves (move-analysis)
  (setf (getf move-analysis :moves)
        (loop
           for move in (getf move-analysis :moves)
           collect (index-to-string move)))
  (setf (getf move-analysis :translated) (index-to-string (getf move-analysis :move)))
  move-analysis)

(defun ai-get-next-move (search-height)
  (cond
    (history-eur (setup-history))
    (killer-eur (setup-killer search-height)))
  (translate-moves (alpha-beta search-height 0 -1e9 1e9)))

(defun auto-play (depth)
  (loop
     while (not (test-winner))
     with move
     do (progn
          (print-board)
          (format t "Eval: ~A~%" (static-evaluation (build-patterns template)))
          (setf move (ai-get-next-move depth))
          (format t "Ai says: ~A~%" move)
          (if (null move) (return nil)) ;; should never happen
          (setf move (getf move :move))
          (make-move move player)
          (setf last-move move)
          (setf moves (+ 1 moves))
          (setf player (invert-player player))))
  (print-board)
  (format t "The winner is: ~A~%" (test-winner)))

(defun play-against (depth)
  (loop
     while (not (test-winner))
     with move
     do (progn
          (play-one-ply)
          (print-board)
          (setf move (ai-get-next-move depth))
          (format t "Ai says: ~A~%" move)          
          (make-move (getf move :move) player)
          (setf last-move (getf move :move))
          (setf moves (+ 1 moves))
          (setf player (invert-player player))))
  (print-board))

(defun reset-game ()
  (setf moves 0)
  (setf player 1)
  (setf swapped 0)
  (setf last-move nil)
  (setf history-eur nil)
  (setf killer-eur nil)
  (loop
     for index from 0 to 120
     do (setf (nth index piece) 0)))

(defun print-hash (&optional (search-height 3))
  (cond
    (history-eur
     (loop
        for index being the hash-keys in history-eur
        when (> (gethash index history-eur) 0)
        do (format t "~A: ~A~%" (index-to-string index) (gethash index history-eur))))
    (killer-eur
     (loop 
        for depth from 0 to search-height
        do (progn
             (loop
                for cell in cells-spiral-order
                with hash-table = (gethash depth killer-eur)
                when (> (gethash cell hash-table) 0)
                do (format t "~A: ~A~%"
                           (index-to-string cell)
                           (gethash cell hash-table)))
             (format t "---------~%"))))))
