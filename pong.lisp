;;;
;;; pong.lisp - Poorly Optimized Nerd Game
;;;

(defpackage :pong
  (:documentation "Poorly Optimized Nerd Game")
  (:use :cl :dlib :terminal :fui)
  (:export
   #:pong
   ))
(in-package :pong)

(defparameter *logo*
  #("███████▄         ▗▄██████▄▖         ▄█████▄          ▗▄████ "
    "██    ▝██▖     ▄██▀      ▀██▄     ▗██▘   ▝██▖      ▄██▛▘    "
    "██     ▝██   ▗██▘          ▝██▖   ██▘     ▝██    ▗██▀       "
    "██      ██  ▗█▛              ▜█▖  ██       ██   ▗█▛         "
    "██    ▗▟█▘  ██                ██  ██       ██   ██          "
    "███████▀    ██                ██  ██       ██   ██      ████"
    "██          ▝█▙              ▟█▘  ██       ██   ▝█▙       ██"
    "██           ▝██▖          ▗██▘   ██       ██    ▝██▄     ██"
    "██             ▀██▄      ▄██▀     ██       ██      ▀█▙▄   ██"
    "██               ▝▀██████▀▘       ██       ██        ▝▜█████"))

(defparameter *pong-font*
  #(#("█████"
      "█   █"
      "█   █"
      "█   █"
      "█   █"
      "█████")
    #("  ██ "
      "  ██ "
      "  ██ "
      "  ██ "
      "  ██ "
      "  ██ ")
    #("█████"
      "    █"
      "█████"
      "█    "
      "█    "
      "█████")
    #("█████"
      "    █"
      "█████"
      "    █"
      "    █"
      "█████")
    #("█   █"
      "█   █"
      "█████"
      "    █"
      "    █"
      "    █")
    #("█████"
      "█    "
      "█████"
      "    █"
      "    █"
      "█████")
    #("█████"
      "█    "
      "█████"
      "█   █"
      "█   █"
      "█████")
    #("█████"
      "    █"
      "    █"
      "    █"
      "    █"
      "    █")
    #("█████"
      "█   █"
      "█████"
      "█   █"
      "█   █"
      "█████")
    #("█████"
      "█   █"
      "█████"
      "    █"
      "    █"
      "█████")))

(defconstant +block+ (code-char #x2588))

(defclass coord ()
  ((x :initarg :x :accessor x :type number :initform 0)
   (y :initarg :y :accessor y :type number :initform 0)
   (z :initarg :z :accessor z :type number :initform 0)))

(defclass point (coord) ())
(defclass entity (point) ())

(defgeneric draw (entity)
  (:documentation "Draw an entity."))

(defclass pictured (entity)
  ((picture
    :initarg :picture :accessor picture
    :documentation "What the entity looks like."))
  (:documentation "A thing that has a picture."))

(defclass ball (pictured)
  ((velocity
    :initarg :velocity :accessor velocity
    :documentation "How fast it's moving ."))
  (:default-initargs
   :x (truncate (tt-width) 2)
   :y (truncate (tt-height) 2)
   :picture #\@
   :velocity (make-instance 'point))
  (:documentation "Really just a point."))

(defmethod draw ((ball ball))
  (with-slots (state) *pong*
    (with-slots (x y picture) ball
      (case state
	(:ready
	 ;; Make it show up more
	 (tt-write-string-at (- y 2) (- x 2) "█████")
	 (tt-write-string-at (- y 1) (- x 2) "█   █")
	 (tt-write-string-at    y    (- x 2) "█ @ █") ;; should be picture
	 (tt-write-string-at (+ y 1) (- x 2) "█   █")
	 (tt-write-string-at (+ y 2) (- x 2) "█████")
	 (tt-move-to y x))
	(t
	 (tt-inverse t)
	 (tt-write-char-at y x picture)
	 (tt-inverse nil)
	 (tt-backward))))))

(defclass paddle (pictured)
  ((size
    :initarg :size :accessor size :type number :initform 9
    :documentation "How long the paddle is.")
   (paddle-speed
    :initarg :paddle-speed :accessor paddle-speed :type number :initform 1
    :documentation "How fast the paddle can move each frame.")
   (paddle-bounce
    :initarg :paddle-bounce :accessor paddle-bounce
    :documentation "Offsets for paddle bounces.")
   (paddle-last-count
    :initarg :paddle-last-count :accessor paddle-last-count
    :type number :initform 0
    :documentation
    "Number of frames last paddle movement was. Sign indicates movement
     direction. Used for bounce direction."))
  (:default-initargs
   :picture +block+
   :paddle-speed (floor (tt-height) 16)
   )
  (:documentation "Really just a line."))

(defparameter *bounce-countdown* 10
  "Number of frames before paddle isn't considered moving. Initial value for
   paddle-last-count.")

(defun make-paddle (side)
  (let ((paddle
	  (ecase side
	    (:left (make-instance 'paddle :x 1))
	    (:right (make-instance 'paddle :x (- (tt-width) 2))))))
    ;; Center it on the edge
    (setf (y paddle) (- (floor (tt-height) 2) (floor (size paddle) 2)))
    ;; Set the bounce values to be the distance from the center of the paddle.
    (setf (paddle-bounce paddle) (make-array (1+ (size paddle))))
    ;; (loop :for i :from 0 :below (size paddle)
    ;; 	  :do (setf (aref (paddle-bounce paddle) i)
    ;; 		    ;; (- i (round (size paddle) 2))))
    ;; 		    (round (- i (round (size paddle) 2)) 2)))
    (setf (paddle-bounce paddle) #(2 1 1 1 0 1 1 1 2 2))
    paddle))

(defmethod draw ((paddle paddle))
  (with-slots (x y picture size) paddle
    (loop :for i :from y :below (+ y size)
	  :do (tt-write-char-at i x picture))))

(defclass player ()
  ((name
    :initarg :name :accessor player-name 
    :documentation "Name of the player.")
   (score
    :initarg :score :accessor score :type fixnum :initform 0
    :documentation "Amount of time the other player has screwed up.")
   (paddle
    :initarg :paddle :accessor player-paddle
    :documentation "The players paddle."))
  (:documentation "State for a player."))

(defclass pong-game (#|inator|#)
  ((ball
    :initarg :ball :accessor ball :initform (make-instance 'ball)
    :documentation "The thing that bounces around.")
   (trail
    :initarg :trail :accessor trail :initform nil
    :documentation "Trail of previous ball positions.")
   (show-trail
    :initarg :show-trail :accessor show-trail :type boolean :initform nil
    :documentation "True to show the trail.")
   (players
    :initarg :players :accessor players
    :documentation "Array of players.")
   (left-paddle
    :initarg :left-paddle :accessor left-paddle)
   (right-paddle
    :initarg :right-paddle :accessor right-paddle)
   (max-score
    :initarg :max-score :accessor max-score :initform 11
    :documentation "Score at which a player wins.")
   (state
    :initarg :state :accessor state :initform :start
    :documentation "State of the game. :start :ready :running")
   (frame-rate
    :initarg :frame-rate :accessor frame-rate :type number :initform 30
    :documentation "How many frames per second we run at.")
   (pre-input
    :initarg :pre-input :accessor pre-input :initform nil
    :documentation "List of input already read.")
   (serve-message
    :initarg :serve-message :accessor serve-message :initform nil
    :documentation "Message displayed when serving.")
   (messages
    :initarg :messages :accessor messages :type boolean :initform nil
    :documentation "True to show un-retro-like but helpful messages."))
  (:default-initargs
   #| :default-keymap *pong-keymap* |#
   :frame-rate (round (tt-height) 2.1))
  (:documentation "A Pong game."))

(defmethod initialize-instance
    :after ((o pong-game) &rest initargs &key &allow-other-keys)
  "Initialize a pong-game."
  (declare (ignore initargs))
  (setf (slot-value o 'left-paddle) (make-paddle :left)
	(slot-value o 'right-paddle) (make-paddle :right)
	(slot-value o 'players)
	(vector
	 (make-instance 'player :name "Player 1"
				:paddle (slot-value o 'left-paddle))
	 (make-instance 'player :name "Player 2"
				:paddle (slot-value o 'left-paddle)))))

(defun reset-players ()
  (with-slots (left-paddle right-paddle players) *pong*
    (setf left-paddle (make-paddle :left)
	  right-paddle (make-paddle :right)
	  (player-paddle (aref players 0)) left-paddle
	  (player-paddle (aref players 1)) right-paddle)))

(defun reset-scores ()
  (with-slots (players) *pong*
    (setf (score (aref players 0)) 0
	  (score (aref players 1)) 0)))

(defvar *pong* nil
  "The current Pong game.")

(defun center (s)
  (tt-beginning-of-line)
  (tt-forward (- (truncate (tt-width) 2) (truncate (length s) 2)))
  (tt-write-string s))

(defun message (s)
  (with-slots (pre-input) *pong*
    (push (show-text (s+ #\newline s #\newline)
		     :y (truncate (tt-height) 3)
		     :centered t)
	  pre-input)))

(defun draw-logo ()
  "Draw the logo centered in the screen."
  (let* ((w (length (aref *logo* 0)))
	 (h (length *logo*))
	 (start-x (- (truncate (tt-width) 2) (truncate w 2)))
	 (start-y (- (truncate (tt-height) 2) (truncate h 2))))
    (loop :for y :from start-y :below (+ start-y (length *logo*))
	  :for i :from 0
	  :do (tt-write-string-at y start-x (aref *logo* i)))
    (tt-down 3)
    (center "Press [SPACE] to start, [?] for help, or [Q] to quit.")))

(defun font-glyph (c)
  "Return the font glyph for character ‘c’."
  (let ((d (digit-char-p c)))
    (if d
	(aref *pong-font* d)
	(error "BUG: ~s isn't a digit character." c))))

(defun char-font-width (c)
  "Return the font width of character ‘c’."
  ;; Assume every scan line is the same width.
  (length (aref (font-glyph c) 0)))

(defun string-font-width (s)
  "Return the string width of ‘s’."
  (loop :for c :across s
	:sum (char-font-width c)))

(defun draw-number-at (y x n)
  (loop :with glyph :and j = x
    :for c :across (princ-to-string n) :do
    (setf glyph (font-glyph c))
    (loop :for i :from y
	  :for line :across glyph :do
	     (tt-move-to i j)
	     (tt-write-string line))
    (incf j (char-font-width c))))

(defun draw-scores ()
  (with-slots (players) *pong*
    (let ((s1 (score (aref players 0)))
	  (s2 (score (aref players 1))))
      (draw-number-at 1 4 s1)
      (draw-number-at 1 (- (tt-width) 4
			   (string-font-width (princ-to-string s2))) s2))))

(defun draw-trail ()
  (with-slots (trail) *pong*
    (loop :for i :from 0 :below (array-dimension trail 0)
	  :do (let ((e (aref trail i)))
		(when (consp e)
		  (tt-write-char-at (cdr e) (car e) #\.))))))

(defun draw-game ()
  (with-slots (state ball left-paddle right-paddle serve-message messages
	       show-trail) *pong*
    (tt-home)
    (tt-erase-below)
    (case state
      (:start
       (draw-logo))
      (t
       (when show-trail
	 (draw-trail))
       ;; Net
       (loop :with x = (truncate (tt-width) 2)
	     :for i :from 0 :below (tt-height) :by 2
	     :do (tt-write-char-at i x +block+))
       (when messages
	 (tt-move-to (1- (tt-height)) 0)
	 (center "Press [?] for help."))
       (draw-scores)
       (draw left-paddle)
       (draw right-paddle)
       (draw ball)
       (when (eq state :ready)
	 (when messages
	   (message (s+ (or serve-message "Ready!") #\newline #\newline
			"Press [SPACE] to serve the ball."))))))
    (tt-finish-output)))

(defun random-direction ()
  "Return a random direction for the ball."
  (if (zerop (random 2)) -1 1))

(defun start ()
  "Start the simulation from the :ready state to :running."
  (with-slots (ball state) *pong*
    (setf (x (velocity ball)) (random-direction)
	  (y (velocity ball)) (random-direction)
	  state :running)))

(defun ready ()
  "Go to the :ready state."
  (with-slots (state ball) *pong*
    (setf state :ready
	  ball (make-instance 'ball))
    (reset-players)))

(defun reset-game ()
  (with-slots (state left-paddle right-paddle ball) *pong*
    (setf ball (make-instance 'ball))
    (reset-players)
    (reset-scores)))

(defun win (player)
  (with-slots (state players messages) *pong*
    (if messages
	(progn
	  (message
	   (s+ (player-name (aref players player)) " Wins!!" #\newline #\newline
	       "Press [SPACE] to play again, or [Q] to quit."))
	  (setf state :win)
	  (reset-game))
	(progn
	  (ready)
	  (start)
	  (setf state :float)))))

(defun add-score (player &key (amount 1))
  (with-slots (players max-score serve-message) *pong*
    (let ((score (incf (score (aref players player)) amount)))
      (setf serve-message
	    (format nil "Point to ~a" (player-name (aref players player))))
      (when (>= score max-score)
	(draw-game)
	(win player)))))

(defun add-trail (x y)
  "Add postion ‘x’ and ‘y’ to the trail."
  (with-slots (trail) *pong*
    (when (not trail)
      (setf trail (make-array 1000 :fill-pointer 0)))
    (when (= (fill-pointer trail) (array-dimension trail 0))
      (setf (fill-pointer trail) 0))
    (vector-push (cons x y) trail)))

(defun count-down (paddle)
  "Count down the recent movement counter."
  (cond
    ((plusp (paddle-last-count paddle))
     (decf (paddle-last-count paddle)))
    ((minusp (paddle-last-count paddle))
     (incf (paddle-last-count paddle)))))

(defun bounce (paddle ball)
  "Bounce ‘ball’ against ‘paddle’."
  (setf (x (velocity ball)) (- (x (velocity ball))))
  ;; Bounce at an angle based on where it hits the paddle.
  ;; Bounce in the direction of recent movement, otherwise reflect.
  (if (zerop (paddle-last-count paddle))
      (setf (y (velocity ball))
	    (aref (paddle-bounce paddle) (- (y ball) (y paddle))))
      (setf (y (velocity ball))
	    (* (signum (paddle-last-count paddle))
	       (aref (paddle-bounce paddle) (- (y ball) (y paddle))))
	    (paddle-last-count paddle) 0)))

(defun simulation-step ()
  (with-slots (ball left-paddle right-paddle state show-trail)
      *pong*
    (with-slots (x y velocity) ball
      (with-slots ((dx x) (dy y)) velocity
	(let ((x1 (x left-paddle))
	      (y1 (y left-paddle))
	      (x2 (x right-paddle))
	      (y2 (y right-paddle))
	      (w (tt-width))
	      (h (tt-height)))
	  (flet ((out-of-bounds-p ()
		   "Return true if the ball is out of bounds."
		   (or (= x 0) (= x (1- w)))))
	    (setf y (+ y dy)
		  x (+ x dx))
	    (case state
	      (:float
	       (when (< x 0)  (setf x (1- w)))
	       (when (= x w)  (setf x 0))
	       (when (< y 0)  (setf y (1- h)))
	       (when (= y h)  (setf y 0))
	       (when (zerop (random 100))
	         (setf (x (velocity ball)) (random-direction)
		       (y (velocity ball)) (random-direction))))
	      (t
	       ;; Top or bottom wall bounce
	       (when (or (<= y 0) (>= y (1- h)))
		 (setf dy (- dy)))
	       ;; Paddle bounce
	       (cond
		 ((and (= x x1) (<= y1 y (+ y1 (size left-paddle))))
		  ;; (setf dx (- dx))
		  ;; dy (aref (paddle-bounce left-paddle) (- y y1))
		  (bounce left-paddle ball))
		 ((and (= x x2) (<= y2 y (+ y2 (size right-paddle))))
		  ;; (setf dx (- dx))
		  ;; dy (aref (paddle-bounce right-paddle) (- y y2))
		  (bounce right-paddle ball)))
	       ;; Miss
	       (cond
		 ((out-of-bounds-p)
		  (cond
		    ((= x 0)      (add-score 1))
		    ((= x (1- w)) (add-score 0)))
		  (when (not (member state '(:win :float)))
		    (ready)))
		 (show-trail
		  (add-trail x y)))
	       (count-down left-paddle)
	       (count-down right-paddle)))))))))

(defun movement-key-p (key)
  "Return true if ‘key’ is a movement key."
  (find key #(#\w #\W #\s #\S #\e #\E #\d #\D
	      #\p #\P #\l #\L #\o #\O #\k #\K :up :down :s-up :s-down)))

(defun move (paddle amount)
  "Move ‘paddle’ by ‘amount’. Set the recent movement counter."
  (incf (y paddle) amount)
  (setf (paddle-last-count paddle) (* (signum amount) *bounce-countdown*)
	;; Make sure we don't go off the edge.
	(y paddle) (min (max 0 (y paddle)) (- (tt-height) (size paddle)))))

(defun move-paddle (key)
  (with-slots ((left left-paddle) (right right-paddle)) *pong*
    (with-slots ((x1 x) (y1 y) (left-speed paddle-speed)) left
      (with-slots ((x2 x) (y2 y) (right-speed paddle-speed)) right
	(let ((h (tt-height))
	      (n1 (size left))
	      (n2 (size right)))
	  (case key
	    ;; medium
	    (#\e         (when (> y1 0)        (move left (- left-speed))))
	    (#\d         (when (< (+ y1 n1) h) (move left left-speed)))
	    ((#\o :up)   (when (> y2 0)        (move right (- right-speed))))
	    ((#\k :down) (when (< (+ y2 n2) h) (move right right-speed)))
	    ;; fast
	    (#\w    (when (> y1 0)        (move left (- (* left-speed 2)))))
	    (#\s    (when (< (+ y1 n1) h) (move left (* left-speed 2))))
	    (#\p    (when (> y2 0)        (move right (- (* right-speed 2)))))
	    (#\l    (when (< (+ y2 n2) h) (move right (* right-speed 2))))
	    ;; slowest, either with shift
	    ((#\W #\E)         (when (> y1 0)        (move left -1)))
	    ((#\S #\D)         (when (< (+ y1 n1) h) (move left 1)))
	    ((#\P #\O :s-up)   (when (> y2 0)        (move right -1)))
	    ((#\L #\K :s-down) (when (< (+ y2 n2) h) (move right 1)))
	    ))))))

(defun help ()
  (with-slots (max-score) *pong*
    (show-text (s+
"Keys:

  Quit the game.             [q]
  Show this help.            [?] or [h] or [F1]
  Toggle showing messages    [m]
    which seems less retro.

  Player 1 (left side)       Fast                 Medium  Slow
    Paddle up                [w]                  [e]     [shift + key]
    Paddle down              [s]                  [d]     [shift + key]

  Player 2 (right side)      Fast                 Medium  Slow
    Paddle up                [p] or [up arrow]    [o]     [shift + key]
    Paddle down              [l] or [down arrow]  [k]     [shift + key]

   Player 2 can also use [Up Arrow] for fast and [Shift Up Arrow] for slow.

  The game is won at " max-score " points.

  Press any key to return to the game.
"))))

(defun handle-other-key (key)
  "Handle keys that are active in multiple modes."
  (with-slots (messages show-trail state) *pong*
    (case key
      ((#\q #\Q) (setf state :done))
      ((#\? #\h #\H :f1) (help))
      ((#\m #\M) (setf messages (not messages)))
      ((#\t #\T) (setf show-trail (not show-trail)))
      ((:resize #.(char-util:ctrl #\L)) (tt-clear)))))

(defun pong ()
  "Pretend your terminal is one of the first arcade games."
  (with-terminal ()
    (let ((*pong* (make-instance 'pong-game)))
      (with-slots (frame-rate state pre-input messages show-trail) *pong*
	(loop :until (eq state :done) :do
	  (draw-game)
	  (case state
	    ((:running :float)
	     (when (tt-listen-for (/ 1 frame-rate))
	       (let ((key (tt-get-key)))
		 (case key
		   (#\space
		    (when (eq state :float)
		      (reset-scores)
		      (ready)))
		   (t
		    (handle-other-key key)
		    (when (and (not (eq state :float)) (movement-key-p key))
		      (move-paddle key))))))
	     (simulation-step))
	    (t ;; start or ready or win
	     (let ((key (or (and pre-input (pop pre-input)) (tt-get-key))))
	       (case key
		 (#\space (start))
		 (t (handle-other-key key)))))))
	(tt-move-to (1- (tt-height)) 0)))))

;; End
