;;
;; tetris.lisp - A game you may know.
;;

(defpackage :tetris
  (:documentation "Waste your time.")
  (:use :cl :dlib :terminal :inator :terminal-inator :fui :fatchar :keymap)
  (:export
   #:tetris
   ))
(in-package :tetris)

;; I invented Tetris in 1913, and again in 1929. I initially conceived it with
;; actual bricks, in which I had noticed were arranged in a set of interesting
;; interlocking patterns, after have being hurled towards my person by my
;; Russian neighbor. I had originally called the game, "Puzzbuttle", and in my
;; mind, this shall always be it's true name, but I had taken to calling it
;; "Tetris", after perhaps misunderstanding my aformentioned neighbor's rounds
;; of shouting, as well as my wife's less than enthusiastic response to the
;; name "Puzzbuttle". My neighbor was completely within reason, since I had
;; been practicing my amateur rocketry in, or rather, partially through, his
;; garden, and had on a number of occasions, caused minor conflagurations to
;; occur in the vicinity. In those days rocketry was not as technologically
;; sound as it is today, and mishaps with fuel were quite usual. All that
;; notwithstanding, I give notice, yet again, as I first did in 1938, after
;; many failings at getting backers for the game with bricks, no matter how
;; small, or decorative I was prepared to make them. I hereby give notice, yet
;; again, that I disavow all claim to rights to the design or titles, and wish
;; for all humanity, and even all beings throughout the cosmos, to be able to
;; play this game, in whatever form they deem fit, in perpetuity, for all
;; eternity, or as long as they see fit to do so, and without any charge or
;; obligation whatsoever.

(defparameter +blank+ 0)
(defparameter +T+ 1)
(defparameter +L+ 2)
(defparameter +J+ 3)
(defparameter +S+ 4)
(defparameter +Z+ 5)
(defparameter +O+ 6)
(defparameter +I+ 7)

;;(deftype style () `(integer -1 6))
(deftype style () 'integer)

#|

 @  @        @
@@@ @@  @@@ @@
    @    @   @

 @  @@@  @@ @
 @  @     @ @@@
 @@       @

  @ @   @@ @@@
  @ @@@ @    @
 @@     @

 @    @@  @    @@
 @@  @@   @@  @@ 
  @        @    

  @  @@    @  @@  
 @@   @@  @@   @@ 
 @        @       

  @       @     
  @       @  @@@@
  @ @@@@  @  
  @       @

|#

(defparameter *base-pieces*
  #(#(#*0100 ; T
      #*1110
      #*0000
      #*0000)
    #(#*1000 ; L
      #*1000
      #*1100
      #*0000)
    #(#*0001 ; J
      #*0001
      #*0011
      #*0000)
    #(#*0010 ; S
      #*0110
      #*0100
      #*0000)
    #(#*0100 ; Z
      #*0110
      #*0010
      #*0000)
    #(#*0000 ; O
      #*0110
      #*0110
      #*0000)
    #(#*0010 ; I
      #*0010
      #*0010
      #*0010)))

(defun rotate-bits (bits)
  (let ((result (make-array 4 :element-type 'bit-vector
			    :initial-contents
			    (loop :repeat 4 :collect
				 (make-array 4 :element-type 'bit)))))
    (loop :for i :from 0 :below 4 :do
      (loop :for j :from 0 :below 4 :do
         (setf (aref (aref result (- 3 j)) i)
	       (aref (aref bits i) j))))
    result))

(defun make-pieces ()
  "Make the rotations of the pieces."
  (let ((result (make-array 7)))
    (loop :for p :from 0 :below 7 :do
      (loop :with pp = (setf (aref result p) (make-array 4))
	 :for rot :from 0 :below 4 :do
         (setf (aref pp rot) (if (zerop rot)
				 (copy-seq (aref *base-pieces* p))
				 (rotate-bits (aref pp (1- rot)))))))
    result))

(defparameter *piece* (make-pieces)
  "Piece rotation bitmaps.")

(defstruct piece
  "A block."
  (x           3 :type fixnum)	; Horizontal coordinaten in the board.
  (y           0 :type fixnum)	; Vertical coordinate in the board.
  (orientation 0 :type fixnum)	; 0 - 3 index into *piece*.
  (style       0 :type fixnum)) ; The piece number, an index 1 - 7 in *piece*.

(defparameter *piece-char*
  (make-array 8 :element-type 'fatchar
	      :initial-contents
	      `(,(make-fatchar :c #\space)
		 ;; ,(make-fatchar :bg :black   :fg :cyan    :c #\#
		 ;; 		:attrs '(:bold :inverse))	    ; T
		 ;; ,(make-fatchar :bg :yellow  :fg :red     :c #\*)   ; L
		 ;; ,(make-fatchar :bg :magenta :fg :blue    :c #\@)   ; J
		 ;; ,(make-fatchar :bg :white   :fg :magenta :c #\%)   ; S
		 ;; ,(make-fatchar :bg :green   :fg :black   :c #\*)   ; Z
		 ;; ,(make-fatchar :bg :blue    :fg :cyan    :c #\O)   ; O
		 ;; ,(make-fatchar :bg :red     :fg :yellow  :c #\+)))) ; I
		 ;;;
		 ;; ,(make-fatchar :bg :yellow  :fg :red     :c #\#)   ; T
		 ;; ,(make-fatchar :bg :white   :fg :blue    :c #\*)   ; L
		 ;; ,(make-fatchar :bg :magenta :fg :blue    :c #\@)   ; J
		 ;; ,(make-fatchar :bg :green   :fg :black   :c #\%)   ; S
		 ;; ,(make-fatchar :bg :cyan    :fg :black   :c #\*)   ; Z
		 ;; ,(make-fatchar :bg :blue    :fg :cyan    :c #\O)   ; O
		 ;; ,(make-fatchar :bg :red     :fg :yellow  :c #\+)))) ; I
		 ;; ░▒▓
		 ;; ,(make-fatchar :bg :yellow  :fg :red     :c #\space)   ; T
		 ;; ,(make-fatchar :bg :white   :fg :blue    :c #\░)   ; L
		 ;; ,(make-fatchar :bg :magenta :fg :blue    :c #\▒)   ; J
		 ;; ,(make-fatchar :bg :green   :fg :black   :c #\▓)   ; S
		 ;; ,(make-fatchar :bg :cyan    :fg :black   :c #\░)   ; Z
		 ;; ,(make-fatchar :bg :blue    :fg :cyan    :c #\space)   ; O
		 ;; ,(make-fatchar :bg :red     :fg :yellow  :c #\space)))) ; I
		 ;;
		 ,(make-fatchar :bg :yellow  :fg :red     :c #\space)   ; T
		 ,(make-fatchar :bg :white   :fg :blue    :c #\space)   ; L
		 ,(make-fatchar :bg :magenta :fg :blue    :c #\space)   ; J
		 ,(make-fatchar :bg :green   :fg :black   :c #\space)   ; S
		 ,(make-fatchar :bg :cyan    :fg :black   :c #\space)   ; Z
		 ,(make-fatchar :bg :blue    :fg :cyan    :c #\space)   ; O
		 ,(make-fatchar :bg :red     :fg :yellow  :c #\space)))) ; I

(defkeymap *tetris-keymap*
  `((:up      . rotate-right)
    (#\f      . rotate-right)
    (#\d      . rotate-left)
    (:down    . move-down)
    (:left    . move-left)
    (:right   . move-right)
    (#\h      . move-left)
    (#\j      . move-down) ;; or maybe rotate-left
    (#\k      . rotate-right)
    (#\l      . move-right)
    (#\<      . move-left)
    (#\>      . move-right)
    (#\space  . drop)
    (#\p      . pause)
    ;;(#\z      . toggle-debug)
    (#\q      . quit)
    (#\escape . quit)))

(defparameter *initial-rate* 0.3 "Drop rate to start with.")
(defparameter *drop-sleep* 0.004 "Time to sleep when dropping a block.")
(defparameter *zap-sleep* 0.01  "Time to sleep when zapping lines.")

(defparameter *board-left* 13
  "Start of the border at the left edge of the board.")

(defparameter *block-width* 2
  "Width of a sub-block in characters.")

(defparameter *paused-char* (make-fatchar :c #\X)
  "Fatchar to fill the screen with when paused.")

(defparameter *zap-char* (make-fatchar :c #\* :fg :yellow)
  "Fatchar to erase filled lines with.")

(defclass tetris (terminal-inator)
  ((board
    :initarg :board :accessor board ;; :type (array style (* *))
    :documentation "Grid")
   (width
    :initarg :width :accessor tetris-width :initform 10 :type fixnum
    :documentation "Width in blocks.")
   (height
    :initarg :height :accessor tetris-height :initform 20 :type fixnum
    :documentation "Height in blocks.")
   (rate
    :initarg :rate :accessor rate :initform *initial-rate* :type float
    :documentation "Rate a which blocks fall.")
   (current
    :initarg :current :accessor current :initform nil :type (or null piece)
    :documentation "The active block.")
   (next
    :initarg :next :accessor tetris-next :initform nil :type (or null piece)
    :documentation "The next block.")
   (score
    :initarg :score :accessor score :initform 0 :type integer
    :documentation "The game score.")
   (paused
    :initarg :paused :accessor paused :initform nil :type boolean
    :documentation "True if the game is paused.")
   (game-over
    :initarg :lost :accessor game-over :initform nil :type boolean
    :documentation "True when the game is over.")
   (debug-flag
    :initarg :debug-flag :accessor debug-flag :initform nil :type boolean
    :documentation "True to do debugging things."))
  (:default-initargs
   :keymap `(,*tetris-keymap* ,*default-inator-keymap*))
  (:documentation "State of the game."))

(defvar *tetris* nil
  "The current tetris game.")

(defun random-style ()
  (1+ (random 7)))

(defun load-piece (o)
  "Make the next piece be the current, and make a new "
  (with-slots (current next) o
    (setf current (or next (make-piece :style (random-style)))
	  next (make-piece :style (random-style)))))

(defun initialize-board (o)
  (with-slots (width height) o
    (setf (slot-value o 'board) (make-array `(,height ,width)
					    :initial-element 0
					    :element-type 'style))))

(defmethod initialize-instance
    :after ((o tetris) &rest initargs &key &allow-other-keys)
  "Initialize a tetris."
  (declare (ignore initargs))
  (with-slots (width height next current) o
    (initialize-board o)
    (load-piece o)))

(defun piece-bits (piece)
  (aref (aref *piece* (1- (piece-style piece)))
	(piece-orientation piece)))

(defmacro zz (form)
  `(tt-format "~a = ~s~%" ',form ,form))

(defun collision-p (piece &key (game-over-check t))
  (with-slots (board width height debug-flag game-over) *tetris*
    (let ((p (piece-bits piece))
	  bx by)
      (loop :for y :from 0 :below 4 :do
        (loop :for x :from 0 :below 4 :do
	  (setf bx (+ (piece-x piece) x)
		by (+ (piece-y piece) y))
          (when (plusp (aref (aref p y) x))
	    (cond
	      ((< (+ (piece-x piece) x) 0)
	       ;; Hit the left wall
	       (return-from collision-p t))
	      ((>= (+ (piece-x piece) x) width)
	       ;; Hit the right wall
	       (return-from collision-p t))
	      ((>= (+ (piece-y piece) y) height)
	       ;; Hit the bottom
	       (return-from collision-p t))
	      ((and (>= bx 0) (>= by 0)
		    (plusp (aref board by bx)))
	       ;; Hit something
	       (when (and game-over-check (= 1 (piece-y piece)))
		 (setf game-over t
		       (inator-quit-flag *tetris*) t))
	       (return-from collision-p t)))))))))

(defun rotate-right (o)
  (with-slots (current) o
    (let ((saved-orientation (piece-orientation current)))
      (setf (piece-orientation current)
	    (mod (1+ (piece-orientation current)) 4))
      (when (collision-p current :game-over-check nil)
	(setf (piece-orientation current) saved-orientation)))))

(defun rotate-left (o)
  (with-slots (current) o
    (let ((saved-orientation (piece-orientation current)))
      (decf (piece-orientation current))
      (when (minusp (piece-orientation current))
	(setf (piece-orientation current) 3))
      (when (collision-p current :game-over-check nil)
	(setf (piece-orientation current) saved-orientation)))))

(defun zap-line (o line char)
  "Animate filling a line with CHAR."
  (with-slots (width) o
    (loop :for x :from 0 :below (* width *block-width*) :by *block-width*
       :do (tt-write-char-at line (+ x *board-left* *block-width*) char)
	 (tt-write-char char)
	 (sleep *zap-sleep*)
	 (tt-finish-output)))
  ;; (tt-get-key)
  )

(defun label-tetris (n)
  (tt-color :red :black)
  (tt-bold t)
  (tt-write-char-at (+ n 1) (+ *board-left* 2 1)    #\T)
  (tt-write-char-at (+ n 2) (+ *board-left* 2 1 3)  #\E)
  (tt-write-char-at (+ n 1) (+ *board-left* 2 1 6)  #\T)
  (tt-write-char-at (+ n 2) (+ *board-left* 2 1 9)  #\R)
  (tt-write-char-at (+ n 1) (+ *board-left* 2 1 12) #\I)
  (tt-write-char-at (+ n 2) (+ *board-left* 2 1 15) #\S)
  (tt-write-char-at (+ n 1) (+ *board-left* 2 1 18) #\!)
  (tt-normal)
  (tt-finish-output)
  (sleep .04))

(defun remove-lines (o)
  (with-slots (board height width score) o
    (let ((rows-to-zap
	   (loop :with row
	      :for i :from (1- height) :downto 0 :do
		(setf row (board-row o i))
	      :when (every (_ (not (zerop _))) row)
	      :collect i
	      :and
	      :do (zap-line o i *zap-char*))))
      (incf score
	    (case (length rows-to-zap)
	      (0 0)
	      (1 100)
	      (2 400)
	      (3 777)
	      (4 (prog1 1111 (label-tetris (car (last rows-to-zap)))))
	      (otherwise 0)))
      (loop :for i :in (nreverse rows-to-zap)
	 :do
	   (zap-line o i (aref *piece-char* 0))
	   ;; Copy all the above rows down
	   (loop :for l :from i :downto 1 :do
		(loop :for x :from 0 :below width
		   :do (setf (aref board l x) (aref board (1- l) x))))
	   ;; Make the top row blank.
	   (loop :for x :from 0 :below width
	      :do (setf (aref board 0 x) +blank+))))))

(defun land-piece (piece)
  "Make the block land and become part of the board. Also load the next piece."
  (with-slots (board current next width) *tetris*
    (let ((p (piece-bits piece)))
      (loop :for y :from 0 :below 4 :do
        (loop :for x :from 0 :below 4 :do
          (when (and (plusp (aref (aref p y) x))
		     (>= (+ (piece-x piece) x) 0)
		     (< (+ (piece-x piece) x) width))
	    (setf (aref board
			(+ (piece-y piece) y)
			(+ (piece-x piece) x))
		  (piece-style piece))))))
    (remove-lines *tetris*)
    (load-piece *tetris*)))

(defun move-down (o)
  (with-slots (current) o
    (incf (piece-y current))
    (when (collision-p current)
      (decf (piece-y current))
      (land-piece current))))

(defun move-left (o)
  (with-slots (current) o
    (decf (piece-x current))
    (when (collision-p current)
      (incf (piece-x current)))))

(defun move-right (o)
  (with-slots (current) o
    (incf (piece-x current))
    (when (collision-p current)
      (decf (piece-x current)))))

(defun drop (o)
  (with-slots (current) o
    (loop
       :do (incf (piece-y current))
       :while (not (collision-p current))
       :do
	 (update-display o)
	 (tt-finish-output)
	 (sleep *drop-sleep*))
    (decf (piece-y current))
    (land-piece current)))

(defun new-piece (o)
  (setf (current o) (make-piece :style (random-style))))

(defun board-row (o n)
  (with-slots (board height width) o
    (make-array width
		:displaced-to board
		:displaced-index-offset (array-row-major-index board n 0)
		:element-type (array-element-type board))))

(defun pause (o)
  "Pause the game."
  (setf (paused o) (not (paused o))))

(defun toggle-debug (o)
  "Pause the game."
  (setf (debug-flag o) (not (debug-flag o))))

(defmethod quit ((o tetris))
  "Quit the game."
  (setf (inator-quit-flag o) t))

(defun draw-piece (piece &key (x 0) (y 0))
  (let ((p (piece-bits piece)))
    (loop :for py :from 0 :below 4 :do
      (loop :with c
        :for px :from 0 :below 4 :do
        (when (plusp (aref (aref p py) px))
	  (setf c (aref *piece-char* (piece-style piece)))
	  (tt-write-char-at (+ (piece-y piece) y py)
			    (+ (* (piece-x piece) *block-width*)
			       x (* px *block-width*)) c)
	  (tt-write-char c))))))
	  

(defmethod await-event ((o tetris))
  "Image viewer event."
  (with-slots (paused rate debug-flag next) o
    (cond
      (paused
       (if (and (char-equal #\z (tt-get-key)) debug-flag)
	   (setf next (make-piece :style (random-style)))
	   (setf paused nil)))
      ((tt-listen-for rate)
       (tt-get-key))
      (t
       (move-down o)))))

(defmethod update-display ((o tetris))
  (with-slots (board score next current paused width height debug-flag) o
    (tt-home)
    (tt-erase-below)
    (tt-color :red :default)
    (tt-write-string-at 0 2 "TETЯIS")

    ;; Write borders and board contents.
    (loop :with c
      :for y :from 0 :below height :do
      (tt-color :black :white)
      (tt-write-string-at y *board-left* "[]")
      (loop :for x :from 0 :below width :do
	(setf c (if (and paused (not debug-flag))
		    *paused-char*
		    (aref *piece-char* (aref (board o) y x))))
        (tt-write-char-at y (+ *board-left* *block-width* (* x *block-width*)) c)
	(tt-write-char c)
	(tt-color :black :white)
	(tt-write-string "[]")))
    (tt-move-to height *board-left*)
    (dotimes (i (+ width *block-width*))
      (tt-write-string "[]"))

    ;; Score
    (tt-color :white :default)
    (tt-write-string-at 2 2 "Score:")
    (tt-move-to 3 2)
    (tt-format "~d" score)

    ;; Next piece
    (tt-write-string-at 0 40 "Next:")
    (draw-box 40 1 12 8)
    (draw-piece next :x 36 :y 3)

    ;; Key help
    (tt-write-string-at 11 41 "Keys:")
    (loop :for k :in '(("q"           . "Quit")
		       ("d"           . "Rotate Left")
		       ("f"           . "Roate Right")
		       ("<left>  / h" . "Move left")
		       ("<right> / l" . "Move right")
		       ("space"       . "Drop piece")
		       ("p"           . "Pause")
		       ("s"           . "Show high scores")
		       ("o"           . "Options"))
	 :for i :from 0
       :do
	 (tt-write-string-at (+ 13 i) 41 (car k))
	 (tt-move-to (+ 13 i) 55)
	 (tt-format "= ~a" (cdr k)))

    ;; The current piece
    (when (or (not paused) debug-flag)
      (draw-piece current :x 15))

    (when paused
      (tt-write-string-at 9 (+ *board-left* 5) " P A U S E D "))))

(defun restart-game ()
  (with-slots (rate score paused game-over) *tetris*
    (setf rate *initial-rate*
	  score 0
	  paused nil
	  game-over nil
	  (inator-quit-flag *tetris*) nil)
    (initialize-board *tetris*)
    (load-piece *tetris*)))


(defun confirm-quit ()
  (char-equal
   #\y
   (loop :with c
      :do
	(setf c (display-text
		 "Game Over"
		 '("Do you want to play again? (y / n)")))
      :until (member c '(#\y #\n) :test #'char-equal)
      :finally (return c))))

(defun tetris ()
  "Play Tetris."
  (if (< (tt-height) 20)
      (display-text "Called off!"
       '("I'm sorry, but your terminal doesn't have enough lines to play
          Tetris properly. Try making your window at least 20 lines high."))
      (with-terminal ()
	(let ((*tetris* (make-instance 'tetris)))
	  (setf (tt-input-mode) :char)
	  (tt-clear)
	  (unwind-protect
               (progn
		 (tt-cursor-off)
		 (with-slots (game-over) *tetris*
		   (loop
		      :do
			(when game-over
			  (restart-game))
			(event-loop *tetris*)
		      :while
			(and game-over (confirm-quit)))
		   (update-display *tetris*)))
	    (tt-move-to (1- (tt-height)) 0)
	    (tt-cursor-on))))))

#+lish
(lish:defcommand tetris ()
  "Zone out with a certain gang of blocks."
  (catch nil
    (tetris)))

;; EOF
