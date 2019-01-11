;;
;; robots.lisp - Dumb old robots game
;;

(defpackage :robots
  (:documentation "The dumb old robots game.")
  (:use :cl :dlib :opsys #| :curses |# :char-util :fui :terminal)
  (:export
   ;; Main entry point
   #:robots
   ))
(in-package :robots)

(defstruct thing
  "Something on the board."
  id x y type char)

(defstruct robots
  "Robots game."
  width
  height
  board				; array of things or nil
  robots			; list of working robots
  score
  score-time			; time when the score was saved
  score-saved			; true if the score was saved
  level				; incremented for each board cleared
  player			; thing
  zap-charges			; how many times we can zap
  turn				; aka time
  until-danger			; nil or movement key
  message			; message string to display
  god-mode			; cheat for testing
  use-color
  hide-cursor
  retry-flag
  quit-flag)

(defstruct score n name time)

(defvar *high-scores* nil)
(defparameter *zap-range* 2
  "Radius of the sonic screwdriver effect.")
(defparameter *zap-max* 3
  "Maximum charges of the sonic screwdriver.")

(defparameter *person-char*  #\@ "The character for the person.")
(defparameter *robot-char*   #\+ "The character for robots.")
(defparameter *shooter-char* #\% "The character for shooter robots.")
(defparameter *junk-char*    #\# "The character for junk heaps.")
(defparameter *zap-char*     #\* "The character for explosions.")
(defparameter *use-color*    t   "True to highlight piece characters.")
(defparameter *hide-cursor*  nil "True to hide the cursor and other effects.")
(defparameter *death-anim* "@*@*@*|/-\\|/-\\|______")

(defun type-char (type)
  "I'll overhaul the architecture when I damn well give a damn. For now I
should stop wasting time on this damn thing."
  (case type
    (:robot *robot-char*)
    (:person *person-char*)
    (:shooter *shooter-char*)
    (:junk *junk-char*)))

(defun make-bots (r count type)
  (with-slots (width height board robots) r
    (loop :with i = 0
       :while (< i count)
       :do
       (let ((x (random width))
	     (y (random height)))
	 (when (not (aref board x y))
	   (let ((bot (make-thing :x x :y y :type type
				  :char (type-char type) :id i)))
	     (push bot robots)
	     (setf (aref board (thing-x bot) (thing-y bot)) bot))
	   (incf i))))))

(defun make-derp-bots (r)
  (with-slots (width height level) r
    (make-bots r (min (* width height .9) (* 5 level level)) :robot)))

(defun make-shooter-bots (r)
  (with-slots (width height level) r
    (let ((lvl (- level 19)))
      (make-bots r (min (* width height .9) (* 5 lvl lvl)) :shooter))))

(defun init-level (r)
  "Initialize a level."
  (with-slots (width height board level robots player zap-charges) r
    ;; Clear the board
    (loop :for y :from 0 :below height :do
       (loop :for x :from 0 :below width :do
	  (setf (aref board x y) nil)))
    ;; Make the player
    (setf player (make-thing :x (truncate width 2) :y (truncate height 2)
			     :type :player :char *person-char*)
	  (aref board (thing-x player) (thing-y player)) player)
    ;; Put random bots on the board
    (cond
      ((< level 20) (make-derp-bots r))
      ((>= level 20) (make-shooter-bots r)))
    ;; Recharge the screwdriver
    (incf zap-charges)
    (setf zap-charges (min *zap-max* zap-charges))))

(defun init (r)
  "Initialize the game."
  (with-slots (width height board robots score score-time score-saved level
	       player zap-charges turn until-danger message god-mode use-color
	       hide-cursor quit-flag retry-flag) r
    (setf width (- (tt-width) 2)
	  height (- (tt-height) 4)
	  board (make-array `(,width ,height) :initial-element nil)
	  robots nil
	  score 0
	  score-time nil
	  score-saved nil
	  level 1
	  player nil
	  zap-charges 0
	  turn 1
	  until-danger nil
	  message nil
	  god-mode nil
	  use-color *use-color*
	  hide-cursor *hide-cursor*
	  retry-flag nil
	  quit-flag nil)
    (init-level r)))

(defun move-player (r x y)
  "Move the player to coordinates X Y."
  (with-slots (player board) r
    (setf (aref board (thing-x player) (thing-y player)) nil
	  (thing-x player) x
	  (thing-y player) y
	  (aref board x y) player)))

(defun teleport (r)
  "Teleport the player to a random location."
  (move-player r (random (robots-width r))
	         (random (robots-height r))))

(defun wait-for-end (r)
  "Wait until either you die or all the robots die."
  (with-slots (robots turn score level) r
    (incf score (* 100 level))
    (loop :until (zerop (length robots)) :do
       (robots-move r)
       (incf turn)
       (update-screen r))))

(defun crash (r x y)
  "Turn the thing at X Y on the board, into a junk heap."
  (with-slots (board robots score) r
    (let ((bot (aref board x y)))
      (when (and bot (eql (thing-type bot) :robot))
	(setf (thing-type bot) :junk
	      (thing-char bot) *junk-char*)
	(setf robots (delete (thing-id bot) robots :key #'thing-id))
	(incf score 20)))))

(defun zap (r)
  "Turn all the robots in the vicinity of the player into junk heaps."
  (with-slots (player board width height zap-charges) r
    (when (not (zerop zap-charges))
      (let ((y-start (max 0           (- (thing-y player) *zap-range*)))
	    (y-end   (min (1- height) (+ (thing-y player) *zap-range*)))
	    (x-start (max 0           (- (thing-x player) *zap-range*)))
	    (x-end   (min (1- width)  (+ (thing-x player) *zap-range*))))
	(loop :for y :from y-start :to y-end :do
	   (loop :for x :from x-start :to x-end :do
	      (crash r x y)
	      (tt-move-to y x)
	      (tt-write-char *zap-char*)
	      ;;(mvaddch y x (char-int #\*))
	      ))
	(tt-finish-output)
	(sleep .2)
	(decf zap-charges)))))

(defun help (r)
  "Show what keys do what."
  (declare (ignore r))
  (fui:display-text "Robots Help"
   `("q - Quit"
     "t - Teleport"
     "w - Wait for the end"
     "z - Zap sonic screwdriver"
     "r - Restart game"
     "s - Show scores"
     "? - Help"
     "space or . - Wait one turn"
     "Movement: "
     "  y k u "
     "   \\|/ "
     "  h- -l  Or Arrow keys"
     "   /|\\"
     "  b k n"
     "Shifted versions go until danger."
     "Characters:"
     ,(format nil "  ~c : robot" *robot-char*)
     ,(format nil "  ~c : junk heap" *junk-char*)
     ,(format nil "  ~c : you"   *person-char*))
   :justify nil))

(defun tmp-message (r format &rest args)
  "Set the temporary message."
  (setf (robots-message r) (apply #'format nil format args)))

(defun message (string)
  "Display a message now."
  (tt-move-to (- (tt-height) 1) 0)
  (tt-erase-to-eol)
  (tt-write-string string))

(defun player-move (r c)
  "The player's move, with the input character C."
  (with-slots (quit-flag retry-flag player width height board zap-charges
               until-danger god-mode) r
    (let* ((x (thing-x player)) (old-x x)
	   (y (thing-y player)) (old-y y)
	   (used-turn t)
      	   (moved t))
      (case c
	((#\h :left)  (decf x))
	((#\l :right) (incf x))
	((#\j :down)  (incf y))
	((#\k :up)    (decf y))
	(#\y (decf x) (decf y))
	(#\u (incf x) (decf y))
	(#\b (incf y) (decf x))
	(#\n (incf y) (incf x))
	(t ; Non movement commands
	 (setf moved nil)
	 (case c
	   (#\w (wait-for-end r))	; wait for end
	   ((#\space #\.))		; wait for one
	   (#\t (teleport r))
	   (#\z (cond ((zerop zap-charges)
		       (setf used-turn nil)
		       (tmp-message r "No more charges."))
		      (t (zap r))))
	   ;; !!!! CHEATS !!!! @@@ REMOVE
	   (#\Z (incf zap-charges) (zap r))
	   (t ; Non turn taking commands
	    (setf used-turn nil)
	    (case c
	      (#\H (setf until-danger #\h))
	      (#\L (setf until-danger #\l))
	      (#\J (setf until-danger #\j))
	      (#\K (setf until-danger #\k))
	      (#\Y (setf until-danger #\y))
	      (#\U (setf until-danger #\u))
	      (#\B (setf until-danger #\b))
	      (#\N (setf until-danger #\n))
	      (#\W (setf until-danger #\w)) ; wait for nearly the end
	      (#\q (when (ask "Quit? Are you sure?") (setf quit-flag t)))
	      (#\r (when (ask "Restart?") (setf retry-flag t)))
	      (#\G (setf god-mode (not god-mode))
		   (tmp-message r "God mode: ~:[OFF~;ON~]" god-mode))
	      (#\s (show-scores r))
	      (#\? (help r))
	      (#.(char-util:ctrl #\L) (tt-clear) (update-screen r))))))) ; redraw
      ;; Valid move check
      (when moved
	(if (and (>= x 0) (< x width)			; in bounds
		 (>= y 0) (< y height))
	    (if (aref board x y)			; occupied
	      (case (thing-type (aref board x y))
		(:junk
		 (setf used-turn nil
		       until-danger nil))
		(:robot
		 (setf used-turn nil)
		 (if until-danger
		     (setf x old-x
			   y old-y
			   until-danger nil)
		     (lose r)))
		(t
		 (move-player r x y)))
	      (move-player r x y))
	    (progn
	      (setf used-turn nil
		    until-danger nil))))
      used-turn)))

(defun move-thing (r thing x y)
  "Move a THING on the board to the coordinates X Y."
  (with-slots (board) r
    (setf (aref board (thing-x thing) (thing-y thing)) nil
	  (thing-x thing) x
	  (thing-y thing) y
	  (aref board x y) thing)))

(defun msg (string)
  "Display a message and wait for any character."
  (message string) (tt-get-key))

(defun ask (string)
  "Ask a yes or no question, with prompt STRING, returning a boolean result."
  (message string) (tt-write-char #\space)
  (tt-cursor-on)
  (loop :with c
     :do (setf c (tt-get-char))
     (case c
       ((#\N #\n) (tt-write-string "No.")  (return nil))
       ((#\Y #\y) (tt-write-string "Yes.") (return t)))))

(defun get-user ()
  "Return the name of the user."
  (opsys:user-name))

(defun show-scores (r &optional (pause t))
  "Show a box with the top scores and pause for input if PAUSE is true."
  (let ((lines (- (tt-height) 8)))
    (locally ; just for muffling
	#+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	(setf *high-scores* (sort *high-scores* #'> :key #'score-n)))
    ;;(draw-box 3 3 (- curses:*cols* 8) lines)
    (draw-box 3 3 55 lines)
    (tt-move-to 4 4)
    (tt-write-span `(#\space
		     (:underline ,(format nil "~10a" "Score")) #\space
		     (:underline ,(format nil "~20a" "Name")) #\space
		     (:underline ,(format nil "~19a" "Time")) #\space))
    (loop :for i :from 0 :below (- lines 3)
       :for s :in *high-scores*
       :do
	 (tt-move-to (+ 4 1 i) 4)
	 (tt-write-char #\space)
	 (when (and (robots-score-time r)
		    (= (score-time s) (robots-score-time r)))
	   (tt-inverse t))
	 (tt-format "~10d ~20a ~19a"
		    (score-n s) (score-name s)
		    (dlib-misc:date-string :time (score-time s)))
	 (when (and (robots-score-time r)
		    (= (score-time s) (robots-score-time r)))
	   (tt-inverse nil))
	 (tt-write-char #\space))
    (tt-finish-output)
    (when pause
      (tt-get-key))))

(defvar *score-file* (merge-pathnames ".robots-scores"
				      (user-homedir-pathname)))
(defvar *lock-file* (merge-pathnames ".robots-scores-lock"
				      (user-homedir-pathname)))

(define-constant +score-version+ 1
  "Format version of the score file.")

(define-constant +score-magic+ "RbtS"
  "Magic number of the score file, soes youse knows what it is.")

(defgeneric read-score-version (r vers stream))
(defmethod read-score-version ((r robots) (vers (eql 1)) stream)
  (setf *high-scores* nil)
  (loop :with s = nil
     :while (setf s (read stream nil nil))
     :do (push s *high-scores*)))

(defgeneric write-score-version (r vers stream))
(defmethod write-score-version ((r robots) (vers (eql 1)) stream)
  (format stream "~a ~w~%" +score-magic+ +score-version+)
  (loop :for s :in *high-scores*
     :do (format stream "~w~%" s)))

(defun read-scores (r)
  (with-open-file (stream *score-file* :direction :input
			  :if-does-not-exist nil)
    (when stream
      (let ((*read-eval* nil)
	    (magic (make-string (length +score-magic+)))
	    version)
	(when (or (/= (read-sequence magic stream) (length +score-magic+))
		  (string/= magic +score-magic+))
	  (error "Bad magic number in score file."))
	(setf version (read stream))
	(when (/= version +score-version+)
	  (error "The score version is too new?"))
	(read-score-version r version stream)))))

(defun save-score (r &key (pause t))
  (with-slots (score score-saved) r
    (when (> score 0)
      (with-locked-file (*score-file*)
	(read-scores r)
	(setf (robots-score-time r) (get-universal-time))
	(push (make-score :n score :name (get-user) :time (robots-score-time r))
	      *high-scores*)
	(with-open-file (stm *score-file* :direction :output
			     :if-exists :overwrite
			     :if-does-not-exist :create)
	  (write-score-version r +score-version+ stm)))
      (setf score-saved t)
      (show-scores r pause))))

(defun death (r)
  "Show the death animation."
  (let ((x (1+ (thing-x (robots-player r))))
	(y (1+ (thing-y (robots-player r)))))
    (loop :for c :across *death-anim* :do
       (tt-move-to y x)
       (tt-write-char c)
       ;;(mvaddch y x (char-code c))
       (tt-move-to y x)
       (tt-finish-output)
       (sleep .1))))

(defun lose (r)
  "You lose."
  (when (robots-god-mode r)
    (return-from lose nil))
  (death r)
  (save-score r :pause nil)
  (if (ask "You died. Try again?")
      (progn
	(setf (robots-retry-flag r) t)
	(throw 'retry nil))
      (progn
	(setf (robots-quit-flag r) t)
	(throw 'bye-bye-now nil))))

(define-constant +around+
  '((-1 -1) (0 -1) (1 -1)
    (-1  0)        (1  0)
    (-1  1) (0  1) (1  1))
  "Coordinates of positions around this one.")

(defun danger-check (r)
  "Return true if there are any robots one space away."
  (with-slots (board player width height) r
    (let ((x (thing-x player))
	  (y (thing-y player)))
      (loop :for (dx dy) :in +around+ :do
	 (let ((obj (aref board
			  (max 0 (min (1- width)  (+ x dx)))
			  (max 0 (min (1- height) (+ y dy))))))
	   (when (and obj (eql (thing-type obj) :robot))
	     (return-from danger-check t))))))
  nil)

(defun robots-move (r)
  "It's the robots turn. They are not handing out kittens."
  (with-slots (width height player robots board score quit-flag until-danger) r
    (when (and until-danger (danger-check r))
      (setf until-danger nil)
      (return-from robots-move nil))
    (let (dx dy new-x new-y to del-list (i 0))
      (loop :for bot :in robots :do
	 (setf dx 0 dy 0)
	 ;; Move toward player
	 (if (> (thing-x player) (thing-x bot))
	     (incf dx)
	     (when (< (thing-x player) (thing-x bot))
	       (decf dx)))
	 (if (> (thing-y player) (thing-y bot))
	     (incf dy)
	     (when (< (thing-y player) (thing-y bot))
	       (decf dy)))
	 (setf new-x (+ (thing-x bot) dx)
	       new-y (+ (thing-y bot) dy))
	 ;; Make sure they don't go off the board
	 (cond
	   ((>= new-x width) (setf new-x width))
	   ((< new-x 0) (setf new-x 0)))
	 (cond
	   ((>= new-y height) (setf new-y height))
	   ((< new-y 0) (setf new-y 0)))
	 (setf to (aref board new-x new-y))
	 ;; Check for collisions
	 (if to
	     (case (thing-type to)
	       (:player (update-screen r) (lose r) (return))
	       (:robot (setf (aref board (thing-x bot) (thing-y bot)) nil)
	       	       (crash r (thing-x to) (thing-y to))
		       (push (thing-id bot) del-list))
	       (:junk
		(setf (aref board (thing-x bot) (thing-y bot)) nil)
		(push (thing-id bot) del-list)
		(incf score 20)))
	     (move-thing r bot new-x new-y))
	 (incf i))
      ;; Delete crashed robots
      (loop :for n :in del-list :do
	 (setf robots (delete n robots :key #'thing-id))))))

#|
(defun draw-box (x y width height)
  "Draw a box at X, Y of WIDTH, HEIGHT."
;  (attrset +a-altcharset+)
  (attron +a-altcharset+)
  (let ((iwidth (max 0 (- width 2))))
    (mvaddch y x (acs-ulcorner)) 
    (loop :for i :from 0 :below iwidth
       :do (addch (acs-hline)))
    (addch (acs-urcorner))
    (loop :for i :from 1 :below (1- height)
       :do
       (mvaddch (+ y i) x (acs-vline))
       (mvaddch (+ y i) (+ x (1- width)) (acs-vline)))
    (mvaddch (+ y (- height 1)) x (acs-llcorner))
    (loop :for i :from 0 :below iwidth
       :do (addch (acs-hline)))
    (addch (acs-lrcorner))
;    (attrset +a-normal+)
    (attroff +a-altcharset+)
    ))
|#

(defun draw-status (r)
  "Show the status of the game, like the score and level."
  (with-slots (score level zap-charges) r
    (tt-move-to (- (tt-height) 2) 0)
    (tt-erase-to-eol)
    (tt-format "Level: ~a Charges: ~a Score: ~a " level zap-charges score)))

(defun draw-board (r)
  "Draw the just board, without anything in it."
  (with-slots (width height) r
    (draw-box 0 0 (+ width 2) (+ height 2))
    (draw-status r)))

(defun update-screen (r)
  "Draw the whole screen."
  (with-slots (width height board player use-color hide-cursor message) r
    (tt-home)
    (tt-erase-below)
    (draw-board r)
    (loop :for y :from 0 :below height :do
       (loop :for x :from 0 :below width :do
	  (let ((thing (aref board x y)))
	    (when (and thing (thing-char thing))
	      (when use-color
		(case (thing-type thing)
		  (:robot (tt-color :red :black))
		  (:junk  (tt-color :cyan (if hide-cursor
					      :black
					      :blue)))
		  (t (tt-normal))))
	      (tt-move-to (1+ y) (1+ x))
	      (tt-write-char (thing-char thing))
	      (when use-color
		(tt-normal))))))
    (when message
      (message message)
      (setf message nil))
    (when hide-cursor
      (tt-cursor-off))
    (tt-move-to (1+ (thing-y player)) (1+ (thing-x player)))
    (tt-finish-output)))

(defun robots ()
  "Wherein many stupid, but persistent, robots try to kill you."
  (let ((r (make-robots)))
    (with-terminal ()
      (catch 'bye-bye-now
	(with-slots (quit-flag retry-flag score-saved turn level until-danger
		     robots hide-cursor) r
	  (tt-clear)
	  (when hide-cursor
	    (tt-cursor-off))
	  (loop :while (not quit-flag) :do
	     (catch 'retry
	       (init r)
	       (update-screen r)
	       (loop :with c
		  :while (not (or quit-flag retry-flag)) :do
		  (setf c (or until-danger (tt-get-key)))
		  (when (player-move r c)
		    (robots-move r))
		  (when (zerop (length robots))
		    (incf level)
		    (init-level r))
		  (incf turn)
		  (update-screen r))
	       (when (not score-saved)
		 (save-score r))))))
      (tt-move-to (tt-height) 0)
      (tt-cursor-on))))

#+lish
(lish:defcommand robots
  ((emoji   boolean :short-arg #\e :help "True to use emoji characters.")
   (unicode boolean :short-arg #\u :help "True to use unicode characters."))
  "Play a game of robots."
  (cond
    (emoji
     (let ((*junk-char*   (code-char #x1f4a5))  ; #\COLLISION_SYMBOL
	   (*person-char* (code-char #x1f603))  ; #\SMILING_FACE_WITH_OPEN_MOUTH
	   (*robot-char*  (code-char #x1f608))  ; #\SMILING_FACE_WITH_HORNS
	   (*zap-char*    (code-char #x1f4a2))	; #\ANGER_SYMBOL
	   (*death-anim*
	   "🌕🌖🌗🌑🌒🌓🌔🌕🌖🌗🌑🌑🌑💀")
	   (*hide-cursor* t))
       (robots)))
    (unicode
     (let ((*junk-char*   (code-char #x2591))  ; #\light_shade
	   (*person-char* (code-char #x263b))  ; #\black_smiling_face
	   (*robot-char*  (code-char #x2603))  ; #\snowman
	   (*zap-char*    (code-char #x2605))  ; #\black_star
	   (*death-anim*
	    "●○●○●○◒◐◓◑◒◐◓◑❋❊❈❇❇······⎽⎽⎽⎽⎽⎽⎽⎽⎽⎽")
	   (*hide-cursor* t))
       (robots)))
    (t
     (robots))))

;; EOF
