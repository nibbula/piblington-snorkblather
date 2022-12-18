;;;
;;; 2048-game.lisp - Mindless doubling.
;;;

(defpackage :2048-game
  (:documentation "Mindless doubling.")
  (:use :cl :dlib :inator :terminal :terminal-inator :keymap :scores :fatchar
	:terminal-utils)
  (:export
   #:play
   ))
(in-package :2048-game)

(defvar *game* nil
  "Then current game state.")

(defkeymap *2048-keymap* ()
  `((:up      . move-up)
    (:down    . move-down)
    (:left    . move-left)
    (:right   . move-right)

    (#\h      . move-left)
    (#\j      . move-down)
    (#\k      . move-up)
    (#\l      . move-right)

    (#\a      . move-left)
    (#\s      . move-down)
    (#\w      . move-up)
    (#\d      . move-right)

    (#\?      . help)
    (#\space  . show-scores)
    (#\n      . new-game)
    (#\q      . quit)
    (#\escape . quit)))

(defclass 2048-score-v1 (score) ())
(defclass 2048-scores (scores)
  ()
  (:default-initargs
   :name "2048"
   :version 1
   :magic "2048-Scores"))

(defstruct piece
  value
  merged)

(defparameter *piece-width* 6)
(defparameter *piece-height* 3)
(defparameter *winning-value* 2048)

(defclass 2048-game (terminal-inator)
  ((board
    :initarg :board :accessor board
    :documentation "The 2d board array of pieces.")
   (board-width
    :initarg :board-width :accessor board-width :type integer :initform 4
    :documentation "The number of cells across.")
   (board-height
    :initarg :board-height :accessor board-height :type integer :initform 4
    :documentation "The number of cells down.")
   (board-x
    :initarg :board-x :accessor board-x :type integer :initform 0
    :documentation "Horizontal coordinate of the board.")
   (board-y
    :initarg :board-y :accessor board-y :type integer :initform 0
    :documentation "Vertical coordinate of the board.")
   (restart-game-p
    :initarg :restart-game-p :accessor restart-game-p
    :type boolean :initform nil
    :documentation "True to restart the game.")
   (score
    :initarg :score :accessor score :type integer :initform 0
    :documentation "Points accumulated for the current game.")
   ;; (score-time
   ;;  :initarg :score-time :accessor score-time :type integer :initform 0
   ;;  :documentation "Time the score happened.")
   (scores
    :initarg :scores :accessor scores :type 2048-scores
    :initform (make-scores '2048-scores '2048-score-v1)
    :documentation "High score list."))
  (:default-initargs
   :keymap `(,*2048-keymap* ,*default-inator-keymap*))
  (:documentation
   "The famously boring 2048 game.

Keep fusing number blocks by mashing them all in one direction
until you get the big one."))

(defun initialize-board (o)
  "Make the board empty."
  (with-slots (board board-height board-width) o
    (loop :for y :from 0 :below board-height :do
      (loop :for x :from 0 :below board-width :do
        (setf (aref board y x) (make-piece))))))

(defun center-board (o)
  (let ((grid-width  (1+ (* (slot-value o 'board-width) (1+ *piece-width*))))
	(grid-height (1+ (* (slot-value o 'board-width) (1+ *piece-height*)))))
    (setf (slot-value o 'board-x)
	  (- (truncate (tt-width) 2) (* 2 (round grid-width 3)))
	  (slot-value o 'board-y)
	  (min 3 (max 0 (- (tt-height) grid-height))))))

(defmethod initialize-instance
    :after ((o 2048-game) &rest initargs &key &allow-other-keys)
  "Initialize a 2048-game."
  (declare (ignore initargs))
  (setf (slot-value o 'board)
	(make-array (list (slot-value o 'board-width)
			  (slot-value o 'board-height))))
  ;; Center the grid
  (center-board o)
  (initialize-board o))

(defun next-piece (o)
  "Add a new piece in a random free spot."
  (with-slots (board board-width board-height restart-game-p) o
    (let* ((len 0)
	   (free-spaces (loop :for i :from 0 :below (array-total-size board)
			      :when (null (piece-value (row-major-aref board i)))
			        :collect i
			        :and
			        :do (incf len))))
      (if (not (zerop len))
	  (setf (piece-value
		 (row-major-aref board (elt free-spaces (random len))))
		(if (zerop (random 2)) 2 4))
	  ;; No more free spaces. @@@@ shouldn't lose until no moves
	  (progn
	    (setf (inator-quit-flag o) t)
	    (when (confirm-box "Game Over" '("Play again? (y / n)"))
	      (setf restart-game-p t)))))))

(defmethod quit ((o 2048-game))
  "Quit the game."
  (when (confirm-box "Really?" '("Really quit? (y / n)"))
    (setf (inator-quit-flag o) t
	  ;; Don't save the score when quitting.
	  (score o) 0)))

(defmethod resize ((o 2048-game))
  (center-board o))

(defmethod new-game ((o 2048-game))
  "Restart the game."
  (when (confirm-box "Really?" '("Restart the game? (y / n)"))
    (restart-game)))

(defvar *exclaim*
  #("Holy shit!! You WON!"
    "I can't believe it, but, you WON!?!?!"
    "WTF!! You won!!!!!!"
    "Holy Heckin' Flippers!! You WON!"
    "Incredible!! You fucking WON!"
    "Gosh double dang it!! You actually Won!?!"
    "NO WAY! You Won??!?!"))

(defun win-exclaim ()
  (ß `(:cyan ,(aref *exclaim* (random (length *exclaim*))))))

(defun restart-game ()
  (with-slots (score restart-game-p) *game*
    (setf score 0
	  restart-game-p nil
	  (inator-quit-flag *game*) nil)
    (initialize-board *game*)
    (next-piece *game*)))

(defun show-scores (o)
  "Show the high scores."
  (with-slots (scores) o
    (read-scores scores)
    (fui:show-text
     (with-terminal-output-to-fat-string (:trim t)
       (table-print:print-table
	(table:make-table-from
	 (loop :for s :in
		  (sort-muffled (scores-scores scores) #'>
					:key #'score-n)
	       :for i :from 0 :below (- (tt-height) 4)
	       :collect s)
	 :columns `((:name "Score")
		    (:name "Name")
		    (:name "Date"
		     :format ,(lambda (c w)
				(declare (ignore w))
				(dtime:date-string :time c)))))
	:long-titles t
	:print-titles t
	:stream *terminal*
	:renderer (make-instance
                   ;; 'terminal-table:terminal-box-table-renderer
                   ;; :box-style table-print::*fancy-box*
                   ;; :box-color :magenta :title-color :cyan :x 0
                   'terminal-table:terminal-table-renderer
		   )))
     :title "High Scores")))

#|
(defun dork (o)
 (declare (ignore o))
  (update-display o)
  (when (eql #\q (tt-get-key))
    (break)))
|#

(defun coords (o x-off y-off)
  "Return a list of coordinates we should traverse for moving in the direction
indicated by ‘x-off’ and ‘y-off’ for the game board ‘o’."
  (with-slots (board board-height board-width) o
    (let* ((h (zerop y-off))
	   (v (zerop x-off))
	   (end-x (1- board-width))
	   (end-y (1- board-height))
	   (start-x (if h (if (minusp x-off) 1 (1- end-x)) 0))
	   (start-y (if v (if (minusp y-off) 1 (1- end-y)) 0))
	   (x-inc (if (zerop x-off) 1 (- x-off)))
	   (y-inc (if (zerop y-off) 1 (- y-off)))
	   )
      (when (and h (plusp x-off)) (setf end-x 0))
      (when (and v (plusp y-off)) (setf end-y 0))
      ;; (format t "x-off ~s y-off ~s h ~s v ~s~%" x-off y-off h v)
      ;; (format t "start [~s ~s] end [~s ~s]~%" start-x start-y end-x end-y)
      ;; (format t "x-inc ~s y-inc ~s~%" x-inc y-inc)
      ;; (finish-output)
      ;; (read-line)
      (cond
	(h ;; horizontal
	 (loop :for y := start-y :then (+ y y-inc) :nconc
	   (loop :for x := start-x :then (+ x x-inc)
		 :collect (cons x y)
		 ;; :do (print (list x y))
		 :until (= x end-x))
	       :until (= y end-y)))
	(t ;; vertical
	 (loop :for x := start-x :then (+ x x-inc) :nconc
	   (loop :for y := start-y :then (+ y y-inc)
		 :collect (cons x y)
		 ;; :do (print (list x y))
		 :until (= y end-y))
	       :until (= x end-x)))))))

(defun clear-merged (o)
  (loop :for i :from 0 :below (array-total-size (board o))
	:do (setf (piece-merged (row-major-aref (board o) i)) nil)))

(defun move-dir (o x-off y-off)
  "Move the board ‘o’ in the direction indicated by ‘x-off’ and ‘y-off’."
  (with-slots (board board-height board-width score) o
    (let* ((h (zerop y-off))
	   (v (zerop x-off))
	   (end-x (1- board-width))
	   (end-y (1- board-height))
	   (start-x (if h (if (minusp x-off) 1 (1- end-x)) 0))
	   (start-y (if v (if (minusp y-off) 1 (1- end-y)) 0))
	   ;; (x start-x)
	   ;; (y start-y)
	   (val 0)
	   )
      (when (and h (plusp x-off)) (setf end-x 0))
      (when (and v (plusp y-off)) (setf end-y 0))
      (dbugf :gg "x-off ~s y-off ~s h ~s v ~s~%" x-off y-off h v)
      (dbugf :gg "start [~s ~s] end [~s ~s]~%" start-x start-y end-x end-y)
      (dbugf :gg "coords ~s~%" (coords o x-off y-off))
      (labels
          ((piece (x y) (aref board y x))
	   (value (x y) (piece-value (piece x y)))
	   ;; (set-value (x y v)
	   ;;   (setf (piece-value (piece x y)) v))
	   (empty-p (x y) (null (value x y)))
	   (make-empty (x y)
	     (let ((p (piece x y)))
	       (setf (piece-value p) nil
		     (piece-merged p) nil)))
	   (merged (x y) (piece-merged (aref board y x)))
	   ;; (set-merged (x y)
	   ;;   (setf (piece-merged (aref board y x)) t))
	   (next-cell (x y)
	     "The next cell in the sliding direction."
	     (piece (+ x x-off) (+ y y-off)))
	   (slide-end-p (x y)
	     (if h
		 (if (minusp x-off)
		     (= x 0)
		     ;; (>= x end-x))
		     (>= x (1- board-width)))
		 (if (minusp y-off)
		     (= y 0)
		     ;; (>= y end-y))))
		     (>= y (1- board-height)))))
	   (slide (x y)
	     (loop
	       :with ix = x :and iy = y
	       :until (slide-end-p ix iy)
	       :do
	       (dbugf :gg "~s ~s " ix iy)
	       (cond
		 ;; Skip empty cells
		 ((empty-p ix iy)
		  (dbugf :gg "empty~%")
		  (return nil))
		 ;; next Empty: move the piece to the cell
		 ((null (piece-value (next-cell ix iy)))
		  (dbugf :gg "scoot~%")
		  (setf (piece-value (aref board (+ iy y-off) (+ ix x-off)))
			(value ix iy))
		  (make-empty ix iy))
		 ;; Same value and not merged: join
		 ((and (eql (piece-value (next-cell ix iy)) (value ix iy))
		       (not (or (merged ix iy)
				(piece-merged (next-cell ix iy)))))
		  ;; (dbugf :gg "join ~s ~s~%"
		  ;; 	 (piece-value (aref board (+ iy y-off) (+ ix x-off)))
		  ;; 	 (value ix iy))
		  (setf val (* 2 (value ix iy)))
		  (incf (piece-value (aref board (+ iy y-off) (+ ix x-off)))
			(value ix iy))
		  (incf score (value ix iy))
		  (setf (piece-merged (aref board (+ iy y-off) (+ ix x-off))) t)
		  (when (= *winning-value* val)
		    (make-empty ix iy)
		    (update-display o)
		    (when (not (confirm-box "You Won!"
					    `(,(win-exclaim)
					      ""
					      "Keep playing? (y / n)")))
		      (setf (inator-quit-flag o) t)))
		  (make-empty ix iy))
		 (t ;; Otherwise we've been stopped
		  (dbugf :gg "stop~%")
		  (return nil)))
	       (incf ix x-off)
	       (incf iy y-off)
	       ;; (dork o)
	       )))
	(loop :for (x . y) in (coords o x-off y-off)
	  :do
	  (dbugf :gg "at [~s ~s]~%" x y)
	  ;; (dork o)
	  (slide x y)
	  )
	(next-piece o)
	(clear-merged o)
	))))

(defun move-left  (o)
  "Move the blocks left."
  (move-dir o -1  0))

(defun move-down (o)
  "Move the blocks down."
  (move-dir o  0  1))

(defun move-up (o)
  "Move the blocks up."
  (move-dir o  0 -1))

(defun move-right (o)
  "Move the blocks right."
  (move-dir o  1  0))

(defun confirm-box (title message)
  (char-equal
   #\y
   (loop :with c
	 :do
	    (setf c (fui:display-text title message :y 9 #|:x 10 |#))
	 :until (and (characterp c)
		     (member c '(#\y #\n) :test #'char-equal))
	 :finally (return c))))

(defun piece-y (y)
  (+ (board-y *game*) 1 (* y (+ *piece-height* 1))))

(defun piece-x (x)
  (+ (board-x *game*) 2 (* x (+ *piece-width* 3))))

(defun draw-piece (y x)
  (with-slots (board board-x board-y board-width board-height) *game*
    (let* ((value (piece-value (aref board y x)))
	   #|(value (ash 1 (random 12)))|#
	   (color (if value
		      (vector :rgb 1 (/ 1 (log value 2)) 0)
		      :default)))
      (tt-color :black
		color
		;; :yellow
		;; (vector :hsv (* (log fake-value 2) (/ 360 16)) 1 1)
		;; (vector :rgb8 (random 255) (random 255) (random 255))
		)
      (tt-write-string-at      (piece-y y)  (piece-x x) "      ")
      (tt-format-at       (+ 1 (piece-y y)) (piece-x x) "~v:@<~d~>" 6 value)
      (tt-write-string-at (+ 2 (piece-y y)) (piece-x x) "      ")
      (tt-color :default :default))))

(defun draw-grid (o)
  "Display the grid."
  (with-slots (board board-width board-height board-x board-y score) o
    (tt-color :white :black)
    (tt-write-string-at board-y board-x "┌────────┬────────┬────────┬────────┐")
    (tt-format "  Score ~d" score)
    (tt-newline)
    (flet ((cell-y (y) (+ board-y 1 (* y (+ *piece-height* 1))))
	   (cell-x (x) (+ board-x 1 (* x (+ *piece-width* 3)))))
      (loop :for y :from 0 :below board-height :do
        (loop :for i :from 0 :below 3
          :do
          (tt-write-string-at (+ (cell-y y) i) board-x "│ ")
          (loop :for x :from 0 :below board-width :do
            (tt-write-string-at (+ (cell-y y) i)
				(+ (cell-x x)) "        │")
	    ;; (tt-get-key)
	    )
          (tt-newline))
	(tt-move-to-col board-x)
        (tt-write-string
         (if (= y (1- board-height))
             "└────────┴────────┴────────┴────────┘"
             "├────────┼────────┼────────┼────────┤"))))))

(defmethod update-display ((o 2048-game))
  (with-slots (board board-width board-height board-x board-y score) o
    (tt-home)
    (tt-erase-below)
    (draw-grid o)

    (loop :for y :from 0 :below board-height :do
      (loop :for x :from 0 :below board-width :do
    	(draw-piece y x)))
    ))

(defun play ()
  (with-terminal ()
    (when (< (tt-height) 20)
      (fui:display-text "Called off!"
        '("I'm sorry, but your terminal doesn't have enough lines to play
           2048 properly. Try making your window at least 20 lines high."))
      (return-from play nil))

    (let ((*game* (make-instance '2048-game)))
      (tt-clear)
      (unwind-protect
	   (with-slots (scores score restart-game-p) *game*
	     (tt-cursor-off)
	     (loop :do
	       (when restart-game-p
		 (restart-game))
	       ;; (setf (piece-value (aref (board *game*) 0 3)) 2)
	       (next-piece *game*)
	       (event-loop *game*)

	       ;; Save the score
	       (when (not (zerop score))
		 (let ((s (make-instance '2048-score-v1
					 :n score)))
		   (save-score scores s)))

	       :while restart-game-p)
	     ;; Final update
	     (update-display *game*))
	;; Go to the bottom and turn the cursor back on.
	(tt-move-to (1- (tt-height)) 0)
	(tt-cursor-on)))))

#+lish
(lish:defcommand 2048-game ()
  "Play the 2048 game."
  (play))

;; End
