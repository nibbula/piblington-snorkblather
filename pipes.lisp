;;
;; pipes.lisp - A text imitation of the classic pipe screensaver.
;;

(defpackage :pipes
  (:documentation "A text imitation of the classic pipe screensaver.")
  (:use :cl :dlib :char-util :terminal)
  (:export
   #:pipes
   ))
(in-package :pipes)

;; (declaim (optimize (speed 3) (safety 1) (debug 3) (space 0)
;; 		   (compilation-speed 0)))

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
 		   (compilation-speed 0)))

(define-constant +dirs+ #(:north :east :south :west)
  "Directions the pipelayer can be facing.")
(declaim (type (simple-vector 4) +dirs+))

(deftype direction () '(member :north :east :south :west))

(defparameter *turn-prob* 10
  "How frequently to turn.")
(declaim (type fixnum *turn-prob*))

;; (defparameter *color-prob* 100
;;   "How frequently to change colors.")
;; (declaim (type fixnum *color-prob*))

(defun random-dir ()
  (aref +dirs+ (random (length +dirs+))))

(defun random-turn (dir)
  (let ((r (random 2)))
    (case dir
      (:north (aref #(:west :east) r))
      (:east  (aref #(:north :south) r))
      (:south (aref #(:east :west) r))
      (:west  (aref #(:south :north) r)))))

(defparameter *unicode-line-table*
  `#(#\space					                         ;;  
     ,(code-char #x2577) ; #\box_drawings_light_down)		         ;; ╷
     ,(code-char #x2576) ; #\box_drawings_light_right)		         ;; ╶
     ,(code-char #x250c) ; #\box_drawings_light_down_and_right)	         ;; ┌
     ,(code-char #x2575) ; #\box_drawings_light_up)		         ;; ╵
     ,(code-char #x2502) ; #\box_drawings_light_vertical)		 ;; │
     ,(code-char #x2514) ; #\box_drawings_light_up_and_right)	         ;; └
     ,(code-char #x251c) ; #\box_drawings_light_vertical_and_right)      ;; ├
     ,(code-char #x2578) ; #\box_drawings_light_left)		         ;; ╸
     ,(code-char #x2510) ; #\box_drawings_light_down_and_left)	         ;; ┐
     ,(code-char #x2500) ; #\box_drawings_light_horizontal)	         ;; ─
     ,(code-char #x252c) ; #\box_drawings_light_down_and_horizontal)     ;; ┬
     ,(code-char #x2518) ; #\box_drawings_light_up_and_left)	         ;; ┘
     ,(code-char #x2524) ; #\box_drawings_light_vertical_and_left)       ;; ┤
     ,(code-char #x2534) ; #\box_drawings_light_up_and_horizontal)       ;; ┴
     ,(code-char #x253c) ; #\box_drawings_light_vertical_and_horizontal) ;; ┼
     ))
(declaim (type simple-vector *unicode-line-table*))

(defparameter *unicode-heavy-line-table*
  `#(#\space				                                 ;;  
     ,(code-char #x257b) ; #\box_drawings_heavy_down)		         ;; ╻
     ,(code-char #x257a) ; #\box_drawings_heavy_right)		         ;; ╺
     ,(code-char #x250f) ; #\box_drawings_heavy_down_and_right)	         ;; ┏
     ,(code-char #x2579) ; #\box_drawings_heavy_up)		         ;; ╹
     ,(code-char #x2503) ; #\box_drawings_heavy_vertical)	         ;; ┃
     ,(code-char #x2517) ; #\box_drawings_heavy_up_and_right)	         ;; ┗
     ,(code-char #x2523) ; #\box_drawings_heavy_vertical_and_right)      ;; ┣
     ,(code-char #x2578) ; #\box_drawings_heavy_left)		         ;; ╸
     ,(code-char #x2513) ; #\box_drawings_heavy_down_and_left)	         ;; ┓
     ,(code-char #x2501) ; #\box_drawings_heavy_horizontal)	         ;; ━
     ,(code-char #x2533) ; #\box_drawings_heavy_down_and_horizontal)     ;; ┳
     ,(code-char #x251b) ; #\box_drawings_heavy_up_and_left)	         ;; ┛
     ,(code-char #x252b) ; #\box_drawings_heavy_vertical_and_left)       ;; ┫
     ,(code-char #x253b) ; #\box_drawings_heavy_up_and_horizontal)       ;; ┻
     ,(code-char #x254b) ; #\box_drawings_heavy_vertical_and_horizontal) ;; ╋
     ))
(declaim (type simple-vector *unicode-heavy-line-table*))

(defparameter *ascii-line-table*
  `#(#\space             ;;  
     ,(code-char #x257b) ;; ╻
     ,(code-char #x257a) ;; ╺
     #\+		 ;; ┏
     ,(code-char #x2579) ;; ╹
     #\|		 ;; ┃
     #\+		 ;; ┗
     ,(code-char #x2523) ;; ┣
     ,(code-char #x2578) ;; ╸
     #\+		 ;; ┓
     #\-		 ;; ━
     ,(code-char #x2533) ;; ┳
     #\+		 ;; ┛
     ,(code-char #x252b) ;; ┫
     ,(code-char #x253b) ;; ┻
     ,(code-char #x254b) ;; ╋
     ))
(declaim (type simple-vector *unicode-line-table*))

(defparameter *unicode-double-line-table*
  `#(#\space		 ;;  
     ,(code-char #x257b) ;; ╻
     ,(code-char #x257a) ;; ╺
     ,(code-char #x2554) ;; ╔
     ,(code-char #x2579) ;; ╹
     ,(code-char #x2551) ;; ║
     ,(code-char #x255a) ;; ╚
     ,(code-char #x2523) ;; ┣
     ,(code-char #x2578) ;; ╸
     ,(code-char #x2557) ;; ╗
     ,(code-char #x2550) ;; ═
     ,(code-char #x2533) ;; ┳
     ,(code-char #x255d) ;; ╝
     ,(code-char #x252b) ;; ┫
     ,(code-char #x253b) ;; ┻
     ,(code-char #x254b) ;; ╋
     ))
(declaim (type simple-vector *unicode-double-line-table*))

(defparameter *unicode-round-line-table*
  `#(#\space					                         ;;  
     ,(code-char #x2577) ; #\box_drawings_light_down)                    ;; ╷
     ,(code-char #x2576) ; #\box_drawings_light_right)                   ;; ╶
     ,(code-char #x256d) ; #\box_drawings_light_down_and_right)          ;; ╭
     ,(code-char #x2575) ; #\box_drawings_light_up)                      ;; ╵
     ,(code-char #x2502) ; #\box_drawings_light_vertical)                ;; │
     ,(code-char #x2570) ; #\box_drawings_light_up_and_right)            ;; ╰
     ,(code-char #x251c) ; #\box_drawings_light_vertical_and_right)      ;; ├
     ,(code-char #x2578) ; #\box_drawings_light_left)                    ;; ╸
     ,(code-char #x256e) ; #\box_drawings_light_down_and_left)           ;; ╮
     ,(code-char #x2500) ; #\box_drawings_light_horizontal)              ;; ─
     ,(code-char #x252c) ; #\box_drawings_light_down_and_horizontal)     ;; ┬
     ,(code-char #x256f) ; #\box_drawings_light_up_and_left)             ;; ╯
     ,(code-char #x2524) ; #\box_drawings_light_vertical_and_left)       ;; ┤
     ,(code-char #x2534) ; #\box_drawings_light_up_and_horizontal)       ;; ┴
     ,(code-char #x253c) ; #\box_drawings_light_vertical_and_horizontal) ;; ┼
     ))
(declaim (type simple-vector *unicode-round-line-table*))

(defparameter *pipe-type*
  `((:normal  ,*unicode-line-table*)
    (:heavy   ,*unicode-heavy-line-table*)
    (:double  ,*unicode-double-line-table*)
    (:ascii   ,*ascii-line-table*)
    (:round   ,*unicode-round-line-table*)
    )
  "Pipe type names to char tables.")

(defun from-dir-bit (dir)
  ;; This can be confusing because it's the opposite because we're coming from
  ;; that direction.
  (case dir
    (:north 1)
    (:west  (ash 1 1))
    (:south (ash 1 2))
    (:east  (ash 1 3))
    ))

(defun to-dir-bit (dir)
  (case dir
    (:south 1)
    (:east  (ash 1 1))
    (:north (ash 1 2))
    (:west  (ash 1 3))
    ))

(defstruct pipe
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (dir :north :type direction)
  fg)

(defun pipes (&key modeline paused (wrap t) (type :normal) (frame-rate 60) bold
		cursor 7-color (number 3) (reset-interval 2000)
		(color-change-probability .01) #| color-walk |#)
  (with-terminal (#+unix :ansi)
    (let ((char-array (second (assoc (keywordify type) *pipe-type*)))
	  pipes
	  (count 0)
	  quit-flag input-ready)
      (declare (type boolean quit-flag)
	       ;;(type simple-vector pipes)
	       )
      (labels ((random-color ()
		 (if 7-color
		     (aref #(:red :blue :green :cyan :magenta :yellow
			     :white) (random 7))
		     (vector :rgb8 (random 255) (random 255) (random 255))))
	       (turn-char (old-dir new-dir)
		 "Return a character turning from old-dir to new-dir."
		 (aref char-array
		       (logior (from-dir-bit old-dir) (to-dir-bit new-dir))))
	       (output-turn (old-dir new-dir)
		 (tt-write-char (turn-char old-dir new-dir))
		 (tt-backward 1))
	       (turn (old-dir)
		 "Turn from old-dir to a random new direction."
		 (let ((new-dir (random-turn old-dir)))
		   (output-turn old-dir new-dir)
		   new-dir)))
	(tt-clear)
	;; (tt-format "derp~%")
	(when (not cursor)
	  (tt-cursor-off))
	(when bold
	  (tt-bold t))

	;; (tt-format "init~%")
	;; Initialize all the pipes.
	(setf pipes (make-array number
			     :element-type 'pipe
			     :initial-contents
			     (loop :for i :from 0 :below number
				;; :do (tt-format "init ~s~%" i)
				:collect
				(make-pipe
				 :dir (random-dir)
				 :x (random (1- (tt-width)))
				 :y (random (1- (tt-height)))
				 :fg (random-color)))))

	;; (tt-format "start ~w~%" pipes)
	;; (tt-get-char)
	(loop
	   :while (not quit-flag)
	   :do
	   (when (and reset-interval (not (zerop reset-interval))
		      (> count reset-interval))
	     (tt-clear)
	     (setf count 0))

	   (setf input-ready nil)
	   (loop :for i :from 0 :below number
	      :do
	      (when (setf input-ready (tt-listen-for 0))
		(loop-finish))
	      ;; (dbug "pipe ~a~%" i)
	      (with-slots (x y fg dir) (aref pipes i)

		;; Randomly change color
		(when (and (not (zerop color-change-probability))
			   (or (>= color-change-probability 1)
			       (let ((inv
				      (truncate (/ 1 color-change-probability))))
				 (= (random inv)
				    (/ inv 2)))))
		  (setf fg (random-color)))

		(tt-color fg :black)

		;; Wrap pipes around the edges
		(when wrap
		  (cond
		    ((< y 0)                 (setf y (1- (tt-height))))
		    ((>= y (1- (tt-height))) (setf y 0))
		    ((< x 0)                 (setf x (1- (tt-width))))
		    ((>= x (1- (tt-width)))  (setf x 0))))

		(tt-move-to y x)

		;; Randomly turn
		(if (= (random *turn-prob*) (/ *turn-prob* 2))
		    (setf dir (turn dir))
		    (progn
		      (case dir
			((:north :south)
			 (tt-write-char (turn-char :south :south)))
			((:west :east)
			 (tt-write-char (turn-char :east :east))))))

		;; Keep moving
		(incf y (case dir (:north -1) (:south 1) (otherwise 0)))
		(incf x (case dir (:west -1) (:east 1) (otherwise 0)))

		(when modeline
		  (tt-move-to (1- (tt-height)) 0)
		  ;; (tt-format "Rate: ~s" frame-rate)
		  (tt-format "~w X:~d Y:~d ~s Rate: ~s ~s"
			     i x y dir frame-rate fg)
		  (tt-move-to y x))
		(tt-finish-output)))

	   (when (or input-ready
		     (tt-listen-for
		      (or (and (plusp frame-rate)
			       (/ 1 frame-rate)) 0.000001))
		     paused)
	     (case (tt-get-char)
	       ((#\q #\Q) (setf quit-flag t))
	       ;; (#\c (setf x (/ (tt-width) 2)
	       ;; 		  y (/ (tt-height) 2)))
	       (#\d (incf frame-rate))
	       (#\D (if (> frame-rate 0) (decf frame-rate)))
	       (#\space
		(setf paused (not paused)))
	       (#\b
		(setf bold (not bold))
		(tt-bold bold))
	       (#\m
		(setf modeline (not modeline)))
	       (#.(ctrl #\l)
		  (tt-clear))
	       ;; (#\R
	       ;; 	(tt-color (random-color) :black))
	       ))
	   (incf count))
	(tt-move-to (1- (tt-height)) 0)
	(tt-cursor-on)
	(tt-color :default :default)
	(tt-normal)))))

#+lish
(lish:defcommand pipes
  ((wrap boolean :short-arg #\w :default t
    :help "True to wrap around the edges of the screen.")
   (frame-rate number :short-arg #\f :default 60
    :help "Loop rate per second.")
   (bold boolean :short-arg #\b :help "True to draw in bold.")
   (7-color boolean :short-arg #\7
    :help "True to use only seven colors.")
   (color-change-probability number :short-arg #\c :default .01
    :help "Color change probability, from zero to 1.")
   (number integer :short-arg #\n :default 3
    :help "Number of pipes to draw.")
   (reset-interval integer :short-arg #\r :default 2000
    :help "How many loops until clearing the screen.")
   ;; (color-walk type :short-arg #\w
   ;;  :help "True to change pipe colors as they are drawn.")
   (type choice :short-arg #\t :default :normal
    :choices '("normal" "heavy" "double" "ascii" "round")
    :help "Type of pipes."))
  "Lay some pipes in your terminal."
  (pipes :wrap wrap :type type :frame-rate frame-rate :bold bold
	 :7-color 7-color :number number :reset-interval reset-interval
	 :color-change-probability color-change-probability
	 #| :color-walk color-walk |#))

;; EOF
