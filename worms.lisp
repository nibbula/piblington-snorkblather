;;;
;;; worms.lisp - New worms
;;;

;;; So, this is really a thing to test Lisp performance. I would like it to
;;; be close to as fast as the worms.c from ncurses. I did a straightforward
;;; translation from the C code in worms.lisp. Even after lots of optimization
;;; effort, it still is much slower than C.
;;;
;;; So this is a re-write from scratch, which should do the same thing, just
;;; in a lispy-er way. Hopefully it's easier to read too. If I can get the
;;; performance close to the C code, at least on SBCL, I'll be happy with it.
;;; Presumably after that I can use it to test performance on different
;;; implementations. Of course this is highly dependent on the performance
;;; of CFFI and the platform FFI, as well as my curses library working right.
;;;
;;; So the point is to do the same thing as the C code, in the fastest way
;;; in Lisp. This means we're gonna have lots of type decls, and perhaps do
;;; weird stuff to make SBCL compiler notes happy.
;;;
;;; Hooray! It's pretty damn fast on SBCL and CCL!
;;; It's even not too shabby on CLisp.
;;;
;;; Performance could be evaluated better if we change worms.c to output
;;; the generaction count at the end, or better yet to run for certain number
;;; of generations. Then we can compare how long it takes for each to do
;;; a run of N generations with a certain set of parameters.

(defpackage :worms
  (:use :cl :curses)
  (:documentation "Now you too can have some new worms.")
  (:export #:worms #:!worms))

(in-package :worms)

(declaim (optimize (speed 3)) (optimize (safety 0))
	 (optimize (debug 0)) (optimize (space 0))
	 (optimize (compilation-speed 0)))

;; (declaim (optimize (debug 3)))

(deftype fast-uint ()
  "A fast unsigned integer."
;  '(unsigned-byte 29))
  'fixnum)

(defvar rand-state 93939382)
(declaim (type fixnum rand-state))

(defun cheap-random (n)
  "Cheap crappy random function."
  (mod
   (setf rand-state (logand (+ (* rand-state 1103515245) 12345) 536870911))
   n))

(defmacro my-random (n)
;  `(cheap-random ,n)
  `(random ,n)
  )

;; The weird initialization is so we can get an actual array of fixnum and
;; not freak out the compiler.
(defparameter *flavor*
  #.(let ((flavy
	   `#(,(char-code #\@)
	      ,(char-code #\#)
	      ,(char-code #\$)
	      ,(char-code #\%)
	      ,(char-code #\&)
	      ,(char-code #\*)
	      ,(char-code #\+))))
      (make-array (length flavy) :element-type 'fixnum
		  :initial-contents flavy))
  "Array of characters to draw worms with.")
(declaim (type (simple-array fixnum *) *flavor*))

(defparameter *trail* #.(char-code #\space)
  "Character to erase worm tails with.")
(declaim (type fixnum *trail*))

(defstruct worm
  "Representation of a worm."
  (orientation	0	:type fast-uint)  ; which way it's looking? @@@ unused!
  (head		0	:type fast-uint)  ; index into x and y arrays
  (length	0	:type fast-uint)  ; length of x and y @@@ not needed?
  (flavor	0	:type fast-uint)  ; char in *flavor* we used to draw
  (x		nil	:type (simple-array fast-uint))
  (y		nil	:type (simple-array fast-uint)))

(defvar *worms* #() "Array of all the worms.")
(declaim (type (simple-array worm) *worms*))

(defstruct land
  "Land where the worms live."
  (width	0	:type fast-uint)
  (height	0	:type fast-uint)
  (spot		nil	:type (simple-array fast-uint (* *))))

(defvar *land* nil "The land of the worms.")

(defun resize-land ()
  "Initialize the land array to be the size of the screen and be empty."
  (let ((lines *lines*)
	(cols  *cols*))
    (declare (type fast-uint lines cols))
    (if (not *land*)
	(setf *land* (make-land :width cols :height lines
				:spot (make-array (list lines cols)
						  :element-type 'fast-uint
						  :initial-element 0)))
	(if (or (not (= cols  (land-width *land*)))
		(not (= lines (land-height *land*))))
	    (setf (land-spot *land*) (make-array (list lines cols)
						 :element-type 'fast-uint
						 :initial-element 0)
		  (land-width *land*) cols
		  (land-height *land*) lines)
	    ;; It's probably just faster to make a new array than to clear
	    ;; the current one.
	    (loop for i from 0 below (land-height *land*)
		  do
		  (loop for j from 0 below (land-width *land*)
			do (setf (aref (land-spot *land*) i j) 0)))))))

(defparameter *quit-flag* nil "True to quit the program.")
(declaim (type boolean *quit-flag*))

(defparameter *worm-length* 16 "Length of all worms.")
(declaim (type fast-uint *worm-length*))

(defparameter *delay* .0 "Milliseconds to delay.")
(declaim (type single-float *delay*))

(defparameter *delay-increment* .01 "Increment to adjust *delay* by.")
(declaim (type single-float *delay-increment*))

(defun check-for-command ()
  (when (not (zerop *delay*))
    (sleep *delay*))
  (let ((ch (getch)))
    (declare (type fixnum ch))
    (when (> ch 0)
;      (when (and has-resize-key (eql ch :RESIZE))
;	(format t "Resize to ~d ~d~%" *lines* *cols*))
      (case (code-char ch)
	(#\q	 (setf *quit-flag* t))
	(#\s	 (nodelay *stdscr* 0))
	(#\+	 (incf *delay* *delay-increment*))
	(#\-	 (when (not (zerop *delay*))
		   (decf *delay* (if (< *delay* .01) .001 *delay-increment*))))
	(#\=	 (setf *delay* .0))
	(#\space (nodelay *stdscr* 1))))))

(defun initialize-curses (paused)
  (initscr)
  (noecho)
  (cbreak)
  (nonl)
  (curs-set 0)				; turn off cursor
  (nodelay *stdscr* (if paused 0 1))	; don't wait for input

  (if (has-colors)
      (let ((bg +COLOR-BLACK+) (cp 0))
	(declare (type fixnum bg cp))
	(start-color)
	(flet ((set-a-color (num fg)
		 (declare (type fixnum num fg cp))
		 (init-pair (+ num 1) fg bg)
		 ;; here we set the flavors to be colored chars
		 (setf cp (color-pair (1+ num))
		       (aref *flavor* num)
		       (logior (aref *flavor* num)
			       ;; coerce is to shut up sbcl notes
;			       (coerce (color-pair (1+ num)) 'fixnum)
			       cp
			       +a-bold+
			       ))))
	  (set-a-color 0 +color-green+)
	  (set-a-color 1 +color-red+)
	  (set-a-color 2 +color-cyan+)
	  (set-a-color 3 +color-white+)
	  (set-a-color 4 +color-magenta+)
	  (set-a-color 5 +color-blue+)
	  (set-a-color 6 +color-yellow+))))
  (clear))

(defun done ()
  (nodelay *stdscr* 0)
  (endwin))

(defun position-name (x y)
  (declare (type fast-uint x y))
  "Return the name of the position."
  (cond
    ((= x 0)
     (cond
       ((= y 0)				:top-left)
       ((= y (1- (land-height *land*)))	:bottom-left)
       (t 				:left)))
    ((= x (1- (land-width *land*)))
     (cond
       ((= y 0)				:top-right)
       ((= y (1- (land-height *land*)))	:bottom-right)
       (t 				:right)))
    (t
     (cond
       ((= y 0)				:top)
       ((= y (1- (land-height *land*)))	:bottom)
       (t 				:normal)))))

(defmacro rand2 (a b)
  `(if (= (my-random 2) 0) ,a ,b))

(defmacro rand3 (a b c)
  `(elt '(,a ,b ,c) (my-random 3)))

;; This should do the same thing as the opts arrays in worms.c
(defun new-orientation (x y orientation)
  (declare (type fast-uint x y orientation))
  "Return a random orientation valid for the given position and orientation."
  (case (position-name x y)
    (:normal
     (incf orientation (- (my-random 3) 1))
     (if (< orientation 0)
	 (setf orientation 7)
	 (if (> orientation 7)
	     (setf orientation 0)))
     orientation)
    (:top-left
     (case orientation
       (5 3)
       (6 (rand2 1 3))
       (7 1)))
    (:top-right
     (case orientation
       (0 (rand2 3 5))
       (1 3)
       (7 5)))
    (:bottom-left
     (case orientation
       (0 (rand3 7 0 1))		; special case for starting
       (3 1)
       (4 (rand2 1 7))
       (5 7)))
    (:bottom-right
     (case orientation
       (1 7)
       (2 (rand2 5 7))
       (3 5)))
    (:top
     (case orientation
       (0 1)
       (1 (rand2 1 2))
       (5 (rand2 4 5))
       (6 5)
       (7 (rand2 1 5))))
    (:bottom
     (case orientation
       (1 (rand2 0 1))
       (2 1)
       (3 (rand2 1 5))
       (4 5)
       (5 (rand2 5 6))))
    (:left
     (case orientation
       (3 (rand2 2 3))
       (4 3)
       (5 (rand2 3 7))
       (6 7)
       (7 (rand2 7 0))))
    (:right
     (case orientation
       (0 7)
       (1 (rand2 3 7))
       (2 3)
       (3 (rand2 3 4))
       (7 (rand2 6 7))))))

(defun make-worms (len num &optional (start-at :center))
  "Make a bunch-o-worms. Like NUM of them. Make them LEN chars long."
  (flet ((random-x () (the fast-uint (my-random (land-width *land*))))
	 (random-y () (the fast-uint (my-random (land-height *land*))))
	 (random-o () (the fast-uint (my-random 8))) ; XXX THIS IS GONNA FAIL! 
	 (center-x () (the fast-uint (truncate (/ (land-width  *land*) 2.0))))
	 (center-y () (the fast-uint (truncate (/ (land-height *land*) 2.0))))
	 (center-o () (the fast-uint (my-random 8)))
	 (corner-x () (the fast-uint 0))
	 (corner-y () (the fast-uint (1- (land-height *land*))))
	 (corner-o () (the fast-uint 0)))
    (let ((flavors (length *flavor*))
	  (x-func #'random-x) (y-func #'random-y) (o-func #'random-o))
      (declare (type function x-func y-func o-func))
      (case start-at
	(:random (setf x-func #'random-x y-func #'random-y o-func #'random-o))
	(:center (setf x-func #'center-x y-func #'center-y o-func #'center-o))
	(:corner (setf x-func #'corner-x y-func #'corner-y o-func #'corner-o)))
      (setf *worms* (make-array num :element-type 'worm))
      (loop :for i :from 0 :below num :do
	 (let ((x (apply x-func '()))
	       (y (apply y-func '()))
	       (flavor (my-random flavors)))
	   (declare (type fast-uint x y))
	   ;; Put the worm in the worms array.
	   (setf (aref *worms* i)
		 (make-worm :orientation (funcall o-func)
			    :x (make-array len :element-type 'fast-uint
					   :initial-element x)
			    :y (make-array len :element-type 'fast-uint
					   :initial-element y)
			    :length len
			    :flavor flavor))
	   ;; The whole worm is piled on the spot, so increment the counter
	   ;; on the land spot by the length.
	   (incf (aref (land-spot *land*) y x) len)
	   (mvaddch y x (aref *flavor* flavor)))))))

(defmacro wrap-position (x y)
  "Wrap the position around the edges."
  `(progn
    (when (>= ,x (land-width *land*))
      (setf ,x 0))
    (when (< ,x 0)
      (setf ,x (1- (land-width *land*))))
    (when (>= ,y (land-height *land*))
      (setf ,y 0))
    (when (< ,y 0)
      (setf ,y (1- (land-height *land*))))))

(defmacro clamp-position (x y)
  "Clamp the position to the edges."
  `(progn
    (when (>= ,x (land-width *land*))
      (setf ,x (1- (land-width *land*))))
    (when (< ,x 0)
      (setf ,x 0))
    (when (>= ,y (land-height *land*))
      (setf ,y (1- (land-height *land*))))
    (when (< ,y 0)
      (setf ,y 0))))

(defmacro new-random-position (x y orientation)
  "Choose a new position in a random direction. Orientation is ignored."
  (declare (ignore orientation))
  `(progn
    (case (my-random 8)
      (0 (incf ,x) (decf ,y))
      (1 (incf ,x))
      (2 (incf ,x) (incf ,y))
      (3 (incf ,y))
      (4 (decf ,x) (incf ,y))
      (5 (decf ,x))
      (6 (decf ,x) (decf ,y))
      (7 (decf ,y)))))

(defmacro new-valid-position (x y orientation)
  "Choose a new random orientation which is valid based on the current ~
position and orientation."
  `(progn
    ;; 3x3 rectangular cell numbered clockwise starting at top-right = 0
    ;; 6 7 0
    ;; 5   1
    ;; 4 3 2
    (case (setf ,orientation (new-orientation ,x ,y ,orientation))
      (0 (incf ,x) (decf ,y))
      (1 (incf ,x))
      (2 (incf ,x) (incf ,y))
      (3 (incf ,y))
      (4 (decf ,x) (incf ,y))
      (5 (decf ,x))
      (6 (decf ,x) (decf ,y))
      (7 (decf ,y)))))

(defun move-worm (w)
  "Move a worm in some direction."
  (let* ((head   (worm-head w))
	 (tail   (if (= head 0)		; circularly backwards
		     (1- (worm-length w))
		     (1- head)))
	 (x      (aref (worm-x w) head))
	 (y      (aref (worm-y w) head))
	 (tail-x (aref (worm-x w) tail))
	 (tail-y (aref (worm-y w) tail))
	 (new-x  x)
	 (new-y  y))

    ; Get rid of the worm tail here.
    (if (zerop (decf (aref (land-spot *land*) tail-y tail-x)))
 	(mvaddch tail-y tail-x *trail*))

    ;; Worm eats it own tail.
    (setf head tail
	  (worm-head w) tail)

;    (new-random-position new-x new-y (worm-orientation w))
    (new-valid-position new-x new-y (worm-orientation w))
;    (wrap-position new-x new-y)
    (clamp-position new-x new-y)

    ;; Save the new spot
    (setf (aref (worm-x w) head) new-x
	  (aref (worm-y w) head) new-y)
    ;; Put on the screen
    (mvaddch new-y new-x (aref *flavor* (worm-flavor w)))

    ;; Add to count of worms here.
    (incf (aref (land-spot *land*) new-y new-x))))


; (defun draw-worm (w)
;   (loop for i from 0 below (length (worm-x w))
;     do
;     (mvaddch (aref (worm-y w) i)
; 	     (aref (worm-x w) i)
; 	     (aref *flavor* (worm-flavor w)))))

(defvar *debug* nil)

(defun dbg (fmt &rest args)
  (when *debug*
    (let* ((lines *lines*)
	   (lm1 (1- lines)))
      (declare (type fixnum lines lm1))
      (mvaddstr lm1 0 (apply #'format nil fmt args))
      (getch))))

(defun worms (&key (field nil)
		   ((:length worm-length) 5)
		   ((:number worm-number) 10)
		   (trail #\space)
		   (start :center)
		   paused)
  (declare (type (or string null) field)
	   (type number worm-length worm-number)
	   (type character trail))
  (declare (ignore field))

  (when (< worm-length 1)
    (error "Worm length must be at least 1."))
  (when (< worm-number 1)
    (error "Number of worms must be at least 1."))

  (setf *worm-length* (truncate worm-length))
  (setf *trail* (char-code trail))

  (initialize-curses paused)
  (resize-land)
  (make-worms worm-length (truncate worm-number) start)
  (setf *quit-flag* nil)
  (let* ((lines *lines*)
	 (bottom-line (- lines 1)))
    (declare (type fixnum bottom-line lines))
    (loop :with generation fixnum = 0
	  :while (not *quit-flag*) :do
	  (loop :for w :across *worms*
		:do (move-worm w))
	  (mvaddstr bottom-line 0 (format nil "~a" generation))
;	  (mvaddstr 0 0 (format nil "~a" (aref *worms* 0)))
	  (refresh)
	  (check-for-command)
	  (incf generation)))
  (done))

#+lish
(lish:defcommand worms
  ((paused boolean :short-arg #\p
    :help "True to start off paused. Hit 'p' to unpause. Good for timing.")
   (length number :short-arg #\l :default 5
    :help "How long each worm should be.")
   (number number :short-arg #\n :default 10
    :help "The number of worms to create.")
   (trail character :short-arg #\t :default #\space
    :help "A character for the worms to leave as a trail.")
   (field string :short-arg #\f
    :help "A string which is put in the background.")
   (start choice :short-arg #\s
    :choices '(:center :random :corner) :default :center
    :help "Where to start the worms off from: :CENTER :RANDOM :CORNER."))
  "Make worms crawl about on the screen."
  (worms:worms :field field :length length :number number :trail trail
	       :start start :paused paused))

;; notes on worm.c:
;;
;; land (ref)
;;    reference count of worms
;;    if no more worms there, erase on screen
;;
;; Worm head w->head loops thru worm body arrays w->xpos[] w->ypos[]
;;
;; Picks a possible direction to go based on an array which blocks out
;; the walls when the worm is on an edge.
;;
;; Worm has an orientation 0-7, which way it is heading.
;; Normally, worm has 3 choices of equal probability:
;;    can keep going in the same direction
;;    turn it's heading one orientation LEFT
;;    turn it's heading one orientation RIGHT
;; Choices are modifed if it's against a wall.
;;
;; Orientations are numbered:
;;   6 7 0
;;   5   1
;;   4 3 2
;; 


;; Perhaps the speed of the C version comes from using macros for the curses
;; calls instead of function calls?

;; EOF
