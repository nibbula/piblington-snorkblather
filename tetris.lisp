;;
;; tetris.lisp - A game you may know.
;;

(defpackage :tetris
  (:documentation "Waste your time.")
  (:use :cl :dlib :dlib-misc :terminal :inator :terminal-inator :fatchar :keymap
	:scores :collections)
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

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defparameter +blank+ 0)
(defparameter +T+ 1)
(defparameter +L+ 2)
(defparameter +J+ 3)
(defparameter +S+ 4)
(defparameter +Z+ 5)
(defparameter +O+ 6)
(defparameter +I+ 7)

(deftype style () `(integer 0 7))
;(deftype style () 'integer)

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

(defun trim-piece (piece)
  ;; Trim vertically
  (let* ((new-piece (remove-if (_ (every #'zerop _)) piece))
	 (keep-cols
	  (loop :for x :from 0 :below (length (aref new-piece 0))
	     :if (some (_ (plusp (aref _ x))) piece)
	     :collect x)))
    (setf new-piece
	  (make-array
	   (length new-piece)
	   :element-type 'bit-vector
	   :initial-contents
	   (loop :for y :from 0 :below (length new-piece)
	      :collect
		(make-array
		 (length keep-cols) :element-type 'bit
		 :initial-contents
		 (loop :for c :in keep-cols
		    :collect (aref (aref new-piece y) c))))))
    new-piece))

(defun make-pieces ()
  "Make the rotations of the pieces."
  (let ((result (make-array 7)))
    ;; Make the rotations.
    (loop :for p :from 0 :below 7 :do
      (loop :with pp = (setf (aref result p) (make-array 4))
	 :for rot :from 0 :below 4 :do
	   ;;(format t "rot ~s ~s~%" p rot)
           (setf (aref pp rot)
		  (if (zerop rot)
		      (copy-seq (aref *base-pieces* p))
		      (rotate-bits (aref pp (1- rot)))))))
    ;; Trim pieces
    (loop :for p :from 0 :below 7 :do
      (loop :with pp = (aref result p)
	 :for rot :from 0 :below 4 :do
	   ;;(format t "trim ~s ~s~%" p rot)
	   (setf (aref pp rot) (trim-piece (aref pp rot)))))
    result))

(defparameter *piece* (make-pieces)
  "Piece rotation bitmaps.")

(defstruct piece
  "A block."
  (x           3 :type fixnum)	; Horizontal coordinaten in the board.
  (y           0 :type fixnum)	; Vertical coordinate in the board.
  (orientation 0 :type fixnum)	; 0 - 3 index into *piece*.
  (style       0 :type fixnum)) ; The piece number, an index 1 - 7 in *piece*.

(defparameter *low-color-piece-char*
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

(defparameter *high-color-piece-char*
  (make-array
   8 :element-type 'fatchar
   :initial-contents
   `(,(make-fatchar :c #\space)
      ;; ,(make-fatchar :bg #(:rgb  1  1  0) :fg #(:rgb .8 .8  0) :c #\◙)  ; T
      ;; ,(make-fatchar :bg :white           :fg #(:rgb .7 .7 .7) :c #\☐)  ; L
      ;; ,(make-fatchar :bg :magenta         :fg #(:rgb .8  0 .8) :c #\☐)  ; J
      ;; ,(make-fatchar :bg #(:rgb .0 .8  0) :fg #(:rgb .0  1  0) :c #\■)  ; S
      ;; ,(make-fatchar :bg :cyan            :fg #(:rgb .0 .8 .8) :c #\❖)  ; Z
      ;; ,(make-fatchar :bg :blue            :fg #(:rgb .0 .0 .8) :c #\╳)  ; O
      ;; ,(make-fatchar :bg :red             :fg #(:rgb  1 .3 .3) :c #\♥) ; I
      ,(make-fatchar :bg #(:rgb  1  1  0) :fg #(:rgb .8 .8  0) :c #\┼)  ; T
      ,(make-fatchar :bg :white           :fg #(:rgb .7 .7 .7) :c #\╱)  ; L
      ,(make-fatchar :bg :magenta         :fg #(:rgb .7  0 .7) :c #\╲)  ; J
      ,(make-fatchar :bg #(:rgb .0 .8  0) :fg #(:rgb .0  1  0) :c #\o)  ; S
      ,(make-fatchar :bg :cyan            :fg #(:rgb .0 .7 .7) :c #\*)  ; Z
      ,(make-fatchar :bg :blue            :fg #(:rgb .0 .0 .8) :c #\╳)  ; O
      ,(make-fatchar :bg :red             :fg #(:rgb  1 .3 .3) :c #\│) ; I
      )))

(defparameter *piece-char* *low-color-piece-char*)

(defkeymap *tetris-keymap* ()
  `((:up      . rotate-right)
    (#\f      . rotate-right)
    (#\d      . rotate-left)
    (:down    . move-down-now)
    (:left    . move-left)
    (:right   . move-right)
    (#\h      . move-left)
    (#\j      . move-down-now) ;; or maybe rotate-left
    (#\k      . rotate-right)
    (#\l      . move-right)
    (#\<      . move-left)
    (#\>      . move-right)
    (#\space  . drop)
    (#\p      . pause-game)
    (#\z      . toggle-debug)
    (#\s      . show-tetris-scores)
    (#\n      . new-game)
    (#\q      . quit)
    (#\escape . quit)))

(defun level-rate (level)
  "Return the fall rate based on the level."
  (make-dtime-as
   (cond
     ((< level 10) (aref #(4/5 43/60 19/30 11/20 7/15 23/60 3/10 13/60 2/15 1/10)
			 level))
     ((< level 13) 1/12)
     ((< level 15) 1/15)
     ((< level 18) 1/20)
     ((< level 28) 1/30)
     (t 1/60))
   :seconds))

(defun lines-level (lines)
  "Return the level for number of lines cleared."
  (truncate lines 10))

(defparameter *drop-sleep* 0.004 "Time to sleep when dropping a block.")
(defparameter *zap-sleep* 0.01  "Time to sleep when zapping lines.")

(defparameter *board-left* 13
  "Start of the border at the left edge of the board.")

(defparameter *block-width* 2
  "Width of a sub-block in characters.")

(defparameter *paused-char* (make-fatchar :c #\X :fg :green :bg :black)
  "Fatchar to fill the screen with when paused.")

(defparameter *zap-char* (make-fatchar :c #\* :fg :yellow)
  "Fatchar to erase filled lines with.")

(defclass tetris-score-v1 (score) ())
(defclass tetris-scores (scores)
  ()
  (:default-initargs
   :name "tetris"
   :version 1
   :magic "Tetris-Scores"))

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
    :initarg :rate :accessor rate :initform (level-rate 0) :type dtime
    :documentation "Rate a which blocks fall.")
   (drop-start
    :initarg :drop-start :accessor drop-start
    :documentation "Starting time of the block dropping.")
   (current
    :initarg :current :accessor current :initform nil :type (or null piece)
    :documentation "The active block.")
   (next
    :initarg :next :accessor tetris-next :initform nil :type (or null piece)
    :documentation "The next block.")
   (score
    :initarg :score :accessor score :initform 0 :type integer
    :documentation "The game score.")
   (scores
    :initarg :scores :accessor scores
    :initform (make-scores 'tetris-scores 'tetris-score-v1) :type tetris-scores
    :documentation "High score list.")
   (score-time
    :initarg :score-time :accessor tetris-score-time :initform 0 :type integer
    :documentation "Time the score happened.")
   (level
    :initarg :level :accessor tetris-level :initform 0 :type integer
    :documentation "The so-called “level”.")
   (lines-cleared
    :initarg :lines-cleared :accessor tetris-lines-cleared
    :initform 0 :type integer
    :documentation "Number of lines cleared.")
   (down-count
    :initarg :down-count :accessor down-count :initform 0 :type fixnum
    :documentation "Count of how many times down was pressed for this piece.")
   (paused
    :initarg :paused :accessor paused :initform nil :type boolean
    :documentation "True if the game is paused.")
   (game-over
    :initarg :lost :accessor game-over :initform nil :type boolean
    :documentation "True when the game is over.")
   (debug-flag
    :initarg :debug-flag :accessor debug-flag :initform nil :type boolean
    :documentation "True to do debugging things.")
   (high-color
    :initarg :high-color :accessor high-color :initform nil :type boolean
    :documentation "True to use lots of colors.")
   (plain
    :initarg :plain :accessor tetris-plain :initform nil :type boolean
    :documentation "True to use plain characters."))
  (:default-initargs
   :keymap `(,*tetris-keymap* ,*default-inator-keymap*))
  (:documentation "State of the game."))

(defvar *tetris* nil
  "The current tetris game.")

(defun random-style ()
  (1+ (random 7)))

(defun load-piece (o)
  "Make the next piece be the current, and make a new "
  (with-slots (current next drop-start) o
    (setf current (or next (make-piece :style (random-style)))
	  next (make-piece :style (random-style))
	  drop-start (get-dtime))))

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

(defun collision-p (piece &key (game-over-check t))
  (with-slots (board width height debug-flag game-over) *tetris*
    (let ((p (piece-bits piece))
	  bx by)
      (loop :for y :from 0 :below (length p) :do
        (loop :for x :from 0 :below (length (aref p y)) :do
	  (setf bx (+ (piece-x piece) x)
		by (+ (piece-y piece) y))
          (when (plusp (aref (aref p y) x))
	    (cond
	      ((< (+ (piece-x piece) x) 0)
	       ;; Hit the left wall
	       (return-from collision-p :left))
	      ((>= (+ (piece-x piece) x) width)
	       ;; Hit the right wall
	       (return-from collision-p :right))
	      ((>= (+ (piece-y piece) y) height)
	       ;; Hit the bottom
	       (return-from collision-p :bottom))
	      ((and (>= bx 0) (>= by 0)
		    (plusp (aref board by bx)))
	       ;; Hit something
	       (when (and game-over-check (= 1 (piece-y piece)))
		 (setf game-over t
		       (inator-quit-flag *tetris*) t))
	       (return-from collision-p :board)))))))))

(defun check-rotate-collision (o saved-orientation)
  (with-slots (current) o
    (let ((type (collision-p current :game-over-check nil)))
      (dbugf :tet "WALL ??? ~s~%" type)
      (case type
	((nil) #| good |#)
	(:right
	 (dbugf :tet "WALL KICK~%")
	 ;; Wall kick
	 ;; @@@ This still isn't right. It can mess you up in tight spots.
	 (let ((saved-x (piece-x current)))
	   (setf (piece-x current)
		 (- *board-left* (length (aref (piece-bits current) 0)) 3))
	   (when (collision-p current :game-over-check nil)
	     (setf (piece-x current) saved-x
		   (piece-orientation current) saved-orientation))))
	(otherwise
	 (setf (piece-orientation current) saved-orientation))))))

(defun rotate-right (o)
  (with-slots (current) o
    (let ((saved-orientation (piece-orientation current)))
      (setf (piece-orientation current)
	    (mod (1+ (piece-orientation current)) 4))
      (check-rotate-collision o saved-orientation))))

(defun rotate-left (o)
  (with-slots (current) o
    (let ((saved-orientation (piece-orientation current)))
      (decf (piece-orientation current))
      (when (minusp (piece-orientation current))
	(setf (piece-orientation current) 3))
      (check-rotate-collision o saved-orientation))))

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
  (with-slots (board height width score level lines-cleared rate) o
    (let* ((rows-to-zap
	    (loop :with row
	       :for i :from (1- height) :downto 0 :do
		 (setf row (board-row o i))
	       :when (every (_ (not (zerop _))) row)
	       :collect i
	       :and
	       :do (zap-line o i *zap-char*)))
	   (cleared (length rows-to-zap)))
      (incf score
	    (case cleared
	      (1 (* 40  (1+ level)))
	      (2 (* 100 (1+ level)))
	      (3 (* 300 (1+ level)))
	      (4 (prog1 (* 300 (1+ level))
		   (label-tetris (car (last rows-to-zap)))))
	      (otherwise 0)))
      (incf lines-cleared cleared)
      (setf level (lines-level lines-cleared)
	    rate (level-rate level))
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
  (with-slots (board current next width score down-count) *tetris*
    (let ((p (piece-bits piece)))
      (loop :for y :from 0 :below (length p) :do
        (loop :for x :from 0 :below (length (aref p y)) :do
          (when (and (plusp (aref (aref p y) x))
		     (>= (+ (piece-x piece) x) 0)
		     (< (+ (piece-x piece) x) width))
	    (setf (aref board
			(+ (piece-y piece) y)
			(+ (piece-x piece) x))
		  (piece-style piece))))))
    (incf score down-count)
    (setf down-count 0)
    (remove-lines *tetris*)
    (load-piece *tetris*)))

(defun move-down-now (o)
  (incf (down-count o))
  (move-down o))

(defun move-down (o)
  (with-slots (current score) o
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
  (with-slots (current score down-count) o
    (loop
       :do (incf (piece-y current))
       :while (not (collision-p current))
       :do
	 (incf down-count)
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

(defun pause-game (o)
  "Pause the game."
  (setf (paused o) t)
  (update-display o)
  (tt-get-key)
  (setf (paused o) nil))

(defun toggle-debug (o)
  (setf (debug-flag o) (not (debug-flag o))))

(defmethod quit ((o tetris))
  "Quit the game."
  (setf (paused o) t)
  (update-display o)
  (when (confirm-box "Really?" '("Really quit? (y / n)"))
    (setf (inator-quit-flag o) t))
  (setf (paused o) nil))

(defmethod new-game ((o tetris))
  "Restart the game."
  (setf (paused o) t)
  (update-display o)
  (when (confirm-box "Really?" '("Restart the game? (y / n)"))
    (restart-game))
  (setf (paused o) nil))

(defun draw-piece (piece &key (x 0) (y 0) bg)
  (let ((p (piece-bits piece)))
    ;; (dbugf :tet "p = ~s~%" p)
    (loop :for py :from 0 :below (length p) :do
      (loop :with c
        :for px :from 0 :below (length (aref p py)) :do
           (if (plusp (aref (aref p py) px))
	       (progn
		 (setf c (aref *piece-char* (piece-style piece)))
		 (tt-write-char-at (+ (piece-y piece) y py)
				   (+ (* (piece-x piece) *block-width*)
				      x (* px *block-width*)) c)
		 (tt-write-char c))
	       (when bg
		 (setf c bg)
		 (tt-write-char-at (+ (piece-y piece) y py)
				   (+ (* (piece-x piece) *block-width*)
				      x (* px *block-width*)) c)))))))

(defun draw-all-pieces ()
  (with-terminal ()
    (tt-home)
    (tt-erase-below)
    (loop :with y = 0 :and bits
       :for style :from 1 :to 7 :do
	 (loop :with x = 0 :and p
	    :for rot :from 0 :below 4 :do
	      (setf p (make-piece :x x :y y :orientation rot :style style)
		    bits (piece-bits p))
	      (draw-piece p :bg #\.)
	      (incf x (1+ (length (aref bits 0)))))
	 (incf y (1+ (length bits))))
    (tt-get-key)))

(defun show-tetris-scores (o &optional (pause-after t))
  (with-slots (scores) o
    (setf (paused o) t)
    (update-display o)
    (read-scores scores)
    (show-scores o pause-after)
    (setf (paused o) nil)))

(defun our-score-p (o score)
  "Return true if SCORE is our score."
  (and (not (zerop (tetris-score-time o)))
       (= (score-time score) (tetris-score-time o))))

(defun show-scores (o &optional (pause-after t))
  "Show a box with the top scores and pause for input if PAUSE-AFTER is true."
  (with-slots (scores plain) o
    (let ((lines (- (tt-height) 2))
	  (left (+ *board-left* (* 12 *block-width*) 1))
	  (top 0)
	  line)
      (locally				; just for muffling
	  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	  (setf (scores-scores scores)
		(sort (scores-scores scores) #'> :key #'score-n)))
      (fui:draw-box left top (- (tt-width) left 1) lines #|:plain plain|#)
      (tt-move-to (+ top 1) (1+ left))
      (setf line
	    (span-to-fat-string
	     `(#\space
	       (:underline ,(format nil "~10a" "Score")) #\space
	       (:underline ,(format nil "~10a" "Name")) #\space
	       (:underline ,(format nil "~16a" "Time")) #\space)))
      (tt-write-string (osubseq line 0 (min (- (tt-width) left 3)
					    (olength line))))
      (loop :with s
	 :for i :from 0 :below (- lines 3)
	 :for sl = (scores-scores scores) :then (cdr sl)
	 :while sl
	 :do
	   (setf s (car sl))
	   (tt-move-to (+ top 2 i) (1+ left))
	   (tt-write-char #\space)
	   (when (our-score-p o s)
	     (tt-inverse t))
	   (setf line
		 (with-output-to-string (str)
		   (if s
		       (format str "~10d ~10a ~16a"
			       (score-n s) (score-name s)
			       (dlib-misc:format-date
				"~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
				(:year :month :date :hours :minutes)
				:time (score-time s)))
		       (dotimes (i (+ 10 1 10 1 16)) (write-char #\space str)))))
	   (tt-write-string (subseq line 0 (min (- (tt-width) left 4)
						(length line))))
	   (when (our-score-p o s)
	     (tt-inverse nil))
	   ;;(tt-write-char #\space)
	   )
      (tt-finish-output)
      (when pause-after
	(tt-get-key)))))

(defmethod await-event ((o tetris))
  "Image viewer event."
  (with-slots (paused rate debug-flag next drop-start) o
    (let ((time-left (dtime- rate (dtime- (get-dtime) drop-start))))
      ;; (tt-move-to 23 0)
      ;; (tt-format "~s ~s" time-left
      ;; 		 (coerce (dtime-to time-left :seconds) 'float))
      (cond
	(paused
	 (if (and (char-equal #\z (tt-get-key)) debug-flag)
	     (setf next (make-piece :style (random-style)))
	     (setf paused nil)))
	((and (dtime-plusp time-left)
	      (tt-listen-for (dtime-to time-left :seconds)))
	 (tt-get-key))
	(t
	 (move-down o)
	 (setf drop-start (get-dtime)))))))

(defun show-key-help ()
  ;; Turn / turning          - 8 поворот
  ;; Left                    - 7 налево
  ;; Right                   - 9 направо
  ;; Speed up                - 4 ускорить
  ;; Reset                   - 5 сбросить
  ;; Show next               - 1 показать следующую
  ;; Clear/erase this text   - 0 следующую зтот текст
  ;; space - reset           - проБел - сбросить
  (tt-write-string-at 11 41 "Keys:")
  (loop :with line
     :for k :in '(("q"           . "Quit")
		  ("d / 7"       . "Rotate Left")
		  ("f / 9"       . "Roate Right")
		  ("<left>  / h" . "Move left")
		  ("<right> / l" . "Move right")
		  ("space"       . "Drop piece")
		  ("p / s"       . "Pause / Show scores")
		  ("o"           . "Options")
		  ("n"           . "New Game"))
     :for i :from 0
     :do
       (setf line (format nil "~14a = ~a" (car k) (cdr k)))
       (tt-move-to (+ 13 i) 41)
       (tt-write-string (subseq line 0 (min (- (tt-width) 42)
					    (length line))))))

(defun set-bg-fill (x width)
  (let ((color
	 (- 1 (+ .434 (/ (* x .376) width)))))
    (tt-color :black
	      (vector :rgb
		      (max 0 (- color .3))
		      0
		      (max 0 (- 1 color .4))))))

(defun grey-fill-color (pos max)
  (let ((grey (- #xff (+ #x80 (truncate (/ (* pos #x60) max))))))
    (vector :rgb8 grey grey grey)))

(defun paused-char (o y x)
  (with-slots (high-color width height) o
    (if (high-color o)
	(make-fatchar
	 :c (fatchar-c *paused-char*)
	 :fg :green
	 :bg (vector :rgb 0 (max 0 (+ (/ (* y .4) height)
				      (/ (* x .3) width))) 0))
	*paused-char*)))

(defmethod update-display ((o tetris))
  (with-slots (board score next current paused width height level lines-cleared
	       debug-flag high-color plain) o
    (tt-home)
    (tt-erase-below)
    (tt-color :red :default)
    (tt-write-string-at 0 2 (if plain "TETRIS" "TETЯIS"))

    ;; Write borders and board contents.
    (loop :with c :and grey :and style
       :for y :from 0 :below height :do
	 (tt-color :white :black)
	 (tt-write-char-at y (1- *board-left*) #\<)
	 (if high-color
	     (tt-color :black (setf grey (grey-fill-color y height)))
	     (tt-color :black :white))
	 (tt-write-string-at y *board-left* "[]")
	 (loop :for x :from 0 :below width
	      :for x1 :from 0 :below (* width 2) :by 2
	    :do
              (setf style (aref (board o) y x))
	      (setf c
		    (cond
		      ((and paused (not debug-flag)) (paused-char o x y))
		      ((and high-color (zerop style))
		       (set-bg-fill x1 (* width 2))
		       #\space)
		      (t
		       (aref *piece-char* style))))
              (tt-write-char-at y (+ *board-left* *block-width*
				     (* x *block-width*)) c)
	      (set-bg-fill (1+ x1) (* width 2))
	      (tt-write-char c))
	 (if high-color
	     (tt-color :black grey)
	     (tt-color :black :white))
	 (tt-write-string "[]")
	 (tt-color :white :black)
	 (tt-write-char #\>))
    (tt-move-to height (1- *board-left*))
    (tt-write-char #\<)
    (tt-color :black :white)

    (loop :with w = (* (+ width *block-width*) 2)
       :for i :from 0 :below w :by 2
       :do
	 (when high-color
	   (tt-color :black (grey-fill-color i w)))
	 (tt-write-string "[")
	 (when high-color
	   (tt-color :black (grey-fill-color (1+ i) w)))
	 (tt-write-string "]"))

    (tt-color :white :black)
    (tt-write-char #\>)
    (tt-move-to (1+ height) *board-left*)
    (dotimes (i (+ width *block-width*))
      (tt-write-string "\\/"))

    ;; Score
    (tt-color :white :default)
    (tt-write-string-at 2 2 "Score:") ;; счет
    (tt-move-to 3 2)
    (tt-format "~d" score)
    (tt-write-string-at 5 2 "Level:") ;; уровень
    (tt-move-to 6 2)
    (tt-format "~d" level)

    ;; полных "Full / total" строк "lines"
    (tt-write-string-at 8 2 "Lines:")
    (tt-move-to 9 2)
    (tt-format "~d" lines-cleared)

    ;; Next piece
    (tt-write-string-at 0 40 "Next:")
    (fui:draw-box 40 1 12 8 #|:plain plain|#)
    (draw-piece next :x 36 :y 3)

    ;; Key help
    (show-key-help)

    ;; The current piece
    (when (or (not paused) debug-flag)
      (draw-piece current :x 15))

    (when paused
      (tt-write-string-at 9 (+ *board-left* 5) " P A U S E D "))))

(defun restart-game ()
  (with-slots (rate score score-time level lines-cleared down-count
	       paused game-over) *tetris*
    (setf rate (level-rate 0)
	  score 0
	  score-time 0
	  level 0
	  lines-cleared 0
	  down-count 0
	  paused nil
	  game-over nil
	  (inator-quit-flag *tetris*) nil)
    (initialize-board *tetris*)
    (load-piece *tetris*)))

(defun confirm-box (title message)
  (char-equal
   #\y
   (loop :with c
      :do
	(setf c (fui:display-text title message :y 9 :x 10))
      :until (member c '(#\y #\n) :test #'char-equal)
      :finally (return c))))

(defparameter *plain-acs-table* nil)

(defun tetris (&key high-color plain)
  "Play Tetris."
  (when (< (tt-height) 20)
    (fui:display-text "Called off!"
       '("I'm sorry, but your terminal doesn't have enough lines to play
          Tetris properly. Try making your window at least 20 lines high."))
    (return-from tetris nil))
  (when (and plain high-color)
    (error "Plain and High-Color are contradictory settings. Please only use ~
            one of them."))

  (with-terminal ()
    (when (and plain (not *plain-acs-table*))
      (let (terminal-ansi::*acs-table*)
	(terminal-ansi::make-acs-table terminal-ansi::*acs-table-data-plain*)
	(setf *plain-acs-table* terminal-ansi::*acs-table*)))
    (when plain
      (setf (terminal-ansi::translate-alternate-characters
	     (or (terminal-wrapped-terminal *terminal*)
		 *terminal*)) t))
    (let ((*tetris* (make-instance 'tetris
				   :high-color high-color
				   :plain plain))
	  (terminal-ansi::*acs-table* *plain-acs-table*))
      (setf *piece-char* (if high-color
			     *high-color-piece-char*
			     *low-color-piece-char*)
	    (tt-input-mode) :char)
      ;; (when plain
      ;; 	((tt-alternate-characters nil))
      (tt-clear)
      (unwind-protect
           (progn
	     (tt-cursor-off)
	     (with-slots (game-over scores score score-time) *tetris*
	       (loop
		  :do
		    (when game-over
		      (restart-game))
		    (event-loop *tetris*)
		    (when (and game-over (not (zerop score)))
		      (let ((s (make-instance 'tetris-score-v1
					      :n score)))
			(save-score scores s)
			(setf score-time (score-time s))))
		  :while
		    (and game-over
			 (progn
			   (show-tetris-scores *tetris* nil)
			   (confirm-box
			    "Game Over"
			    '("Play again? (y / n)")))))
	       (update-display *tetris*)))
	(tt-move-to (1- (tt-height)) 0)
	(tt-cursor-on)))))

#+lish
(lish:defcommand tetris
  ((plain      boolean :short-arg #\p :help "True to use very plain characters.")
   (high-color boolean :short-arg #\c :help "True to use lots of colors."))
  "Zone out with a certain gang of blocks."
  (catch nil
    (tetris :high-color high-color :plain plain)))

;; EOF
