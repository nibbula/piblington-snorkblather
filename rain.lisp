;;
;; rain.lisp - Rain on the screen
;;

;; This is like the old thing from a VAX or something.

;;(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
;(declaim (optimize (speed 3) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defpackage :rain
  (:documentation "Rain on the screen.")
  ;; (:shadowing-import-from :curses #:timeout)
  (:use :cl :dlib :terminal)
  (:export
   #:rain
   #:!rain
   #:stars
   #:!stars
   #:snow
   #:!snow
   #:mutrix
   #:!mutrix
   ))
(in-package :rain)

(defparameter *default-density* .5)
(defparameter *default-timeout* 40)

(defclass sky ()
  ((drops
    :initarg :drops :accessor drops #| :initform '() :type list |#
    :documentation "Drops in the sky.")
   (density
    :initarg :density :accessor density
    :initform *default-density* :type single-float
    :documentation "Density of drops.")
   (param
    :initarg :param :accessor sky-param :initform 0
    :documentation "A numeric parameter.")
   (time-out
    :initarg :time-out :accessor time-out
    :initform *default-timeout* #| :type fixnum |#
    :documentation "Time for timer."))
  (:documentation "The roof of the world."))

(defgeneric create-drop (sky)
  (:documentation "Make a drop in the sky."))

(defgeneric draw-background (sky)
  (:documentation "Draw the background of the sky.")
  (:method ((sky sky)) (declare (ignore sky))))

(defgeneric frame-end (sky)
  (:documentation "Do something after the a frame is drawn.")
  (:method ((sky sky)) (declare (ignore sky))))

(defgeneric drop-cycle (sky)
  (:documentation "Cycle the drops in the sky."))

(defgeneric resize-sky (sky)
  (:documentation "The sky was resized.")
  (:method ((sky sky))
    (tt-clear)
    (terminal-get-size *terminal*)))

(defgeneric clear-sky (sky)
  (:documentation "Clear the sky.")
  (:method ((sky sky))
    (setf (drops sky) '())))

(defgeneric precipitate (sky)
  (:documentation "Precipitate in the sky."))

(defclass drop ()
  ((x :initarg :x :accessor drop-x :initform 0 #| :type fixnum |#)
   (y :initarg :y :accessor drop-y :initform 0 #| :type fixnum |#)
   (color :initarg :color :accessor drop-color :type list :initform nil))
  (:documentation "Generic precipitation drop."))

(defgeneric draw-drop (sky drop)
  (:documentation "Draw a drop in the sky."))

(defgeneric draw-drops (sky)
  (:documentation "Draw all the drops in the sky.")
  (:method ((sky sky))
    (loop :for d :in (drops sky) :do (draw-drop sky d))))

(defparameter *drop-colors* #.(vector :blue :cyan :white))
(declaim (type (simple-vector 3) *drop-colors*))

(defun random-color ()
  (cons (elt *drop-colors* (random (length *drop-colors*))) :black))

#| Stolen from wikipedia:

  float x1, x2, w, y1, y2;
  do {
    x1 = 2.0 * ranf() - 1.0;
    x2 = 2.0 * ranf() - 1.0;
    w = x1 * x1 + x2 * x2;
  } while ( w >= 1.0 );

  w = sqrt( (-2.0 * log( w ) ) / w );
  y1 = x1 * w;
  y2 = x2 * w;

OR

#include <math.h>

extern float ranf();         /* ranf() is uniform in 0..1 */

/* normal random variate generator */
float box_muller(float m, float s) /* mean m, standard deviation s */
{
  float x1, x2, w, y1;
  static float y2;
  static int use_last = 0;

  if (use_last)        /* use value from previous call */
  {
    y1 = y2;
    use_last = 0;
  }
  else
  {
    do {
      x1 = 2.0 * ranf() - 1.0;
      x2 = 2.0 * ranf() - 1.0;
      w = x1 * x1 + x2 * x2;
    } while ( w >= 1.0 );

    w = sqrt( (-2.0 * log( w ) ) / w );
    y1 = x1 * w;
    y2 = x2 * w;
    use_last = 1;
  }

  return( m + y1 * s );
}

OR in C#

#define TWO_PI 6.2831853071795864769252866
 
double generateGaussianNoise(const double &variance)
{
  static bool haveSpare = false;
  static double rand1, rand2;
 
  if(haveSpare)
  {
    haveSpare = false;
    return sqrt(variance * rand1) * sin(rand2);
  }
 
  haveSpare = true;
 
  rand1 = rand() / ((double) RAND_MAX);
  if (rand1 < 1e-100) rand1 = 1e-100;
  rand1 = -2 * log(rand1);
  rand2 = (rand() / ((double) RAND_MAX)) * TWO_PI;
 
  return sqrt(variance * rand1) * cos(rand2);
}

or Java

private double nextNextGaussian;
private boolean haveNextNextGaussian = false;
public double nextGaussian() {
  if (haveNextNextGaussian) {
    haveNextNextGaussian = false;
    return nextNextGaussian;
  } else {
    double v1, v2, s;
    do {
      v1 = 2 * nextDouble() - 1;   // between -1.0 and 1.0
      v2 = 2 * nextDouble() - 1;   // between -1.0 and 1.0
      s = v1 * v1 + v2 * v2;
    } while (s >= 1 || s == 0);
    double multiplier = StrictMath.sqrt(-2 * StrictMath.log(s)/s);
    nextNextGaussian = v2 * multiplier;
    haveNextNextGaussian = true;
    return v1 * multiplier;
  }
}

|#

(defparameter *last-one* 0d0)
(declaim (type double-float *last-one*))
(defparameter *has-last-one* nil)
(declaim (type boolean *has-last-one*))

(declaim (ftype (function () double-float) normal-distribution-1))
(defun normal-distribution-1 ()
  "Slow old fashioned normal distribution."
  (declare (optimize (speed 3) (safety 0)))
  (if *has-last-one*
      (progn
	(setf *has-last-one* nil)
	*last-one*)
      (let ((x1 0d0) (x2 0d0) (w 0d0) (y1 0d0))
	(declare (type double-float x1 x2 w y1))
	(loop :do
	   (setf x1 (* 2.0 (- (random 1d0) 1d0))
		 x2 (* 2.0 (- (random 1d0) 1d0))
		 w (+ (* x1 x1) (* x2 x2)))
	   :while (or (> w 1) (= w 0)))
	(setf w (sqrt (/ (* -2d0 (log w)) w))
	      y1 (* x1 w)
	      *last-one* (* x2 w)
	      *has-last-one* t)
	(the double-float y1))))

(defmacro with-slow-normal-dist (() &body body)
  "Wrap around calls of normal-distribution-1 to be thread “safe”."
  `(let ((*last-one* 0d0)
	 (*has-last-one* nil))
     ,@body))

;; Stolen from:
;; https://github.com/tpapp/cl-random/blob/master/src/univariate.lisp
;; Which doesn't seem to have a license.

(defun normal-distribution-2 ()
  "Draw a random number from N(0,1)."
  ;; Method from Leva (1992). This is considered much better/faster than the
  ;; Box-Muller method.
  (declare (optimize (speed 3) (safety 0))
	   #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (tagbody
   top
     (let* ((u (random 1d0))
	    (v (* 1.7156d0 (- (random 1d0) 0.5d0)))
	    (x (- u 0.449871d0))
	    (y (+ (abs v) 0.386595d0))
	    (q (+ (expt x 2) (* y (- (* 0.19600d0 y) (* 0.25472d0 x))))))
       (if (and (> q 0.27597d0)
		(or (> q 0.27846d0)
		    (plusp (+ (expt v 2) (* 4 (expt u 2) (log u))))))
	   (go top)
	   (return-from normal-distribution-2 (/ v u))))))

;; test dists
(defvar aa (make-array '(100) :initial-element 0d0))
(defun clear-aa ()
  (setf aa (make-array '(100) :initial-element 0d0)))
(defun poo ()
  (loop :for i :from 0 :to 99 :do
     (format t "~v,,,va~%" (aref aa i) #\# #\#)))
(defun poo2 ()
  (with-terminal ()
    (tt-clear)
    (loop :for i :from 0 :to 99 :do
       (loop :for j :from 0 :below (aref aa i) :do
	  (tt-move-to (- (tt-height) (+ 1 j)) i)
	  (tt-write-char #\#)))
    (tt-get-char)))

(defun foo1 ()
  (loop :for i :from 1 :to 100 :do
       (incf (aref aa (truncate (+ 50 (* 10 (normal-distribution-1))))))))
(defun foo2 ()
  (loop :for i :from 1 :to 100 :do
       (incf (aref aa (truncate (+ 50 (* 10 (normal-distribution-2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image drops

(defstruct drop-image
  (x      0   :type fixnum)
  (y      0   :type fixnum)
  (width  0   :type fixnum)
  (height 0   :type fixnum)
  (wide   nil :type boolean)
;  (data   #2A((#\x)) :type (simple-array character (* *))))
  data)

(defclass image-drop (drop)
  ((image :initarg :image :accessor image-drop-image :type fixnum))
  (:documentation "Drop with an image."))

(defun wide-string-p (str)
  (loop :for char :across str :do
     (when (> (char-code char) 255)
       (return-from wide-string-p t)))
  nil)

(defun wide-p (str-or-array)
  (if (stringp str-or-array)
      (wide-string-p str-or-array)
      (progn
	(loop :for str :across str-or-array :do
	 (when (wide-p str)
	   (return-from wide-p t)))
	nil)))

(defun init-drop-images (sky init)
  "Initialize the images from lists to structs."
  (with-slots (drop-images) sky
    (when (= (length drop-images) 0)
      (setf drop-images (make-array (length init)
				    :element-type 'drop-image
				    :initial-element (make-drop-image)))
      (loop :with w :and h :and i = 0
	 :for (x y d) :across init
	 :do
	 (setf w (length (aref d 0))
	       h (length d))
	 (setf (aref drop-images i)
	       (make-drop-image :x x :y y :width w :height h
				:data (make-array (list w h)
						  :element-type 'character
						  :initial-contents d)
				:wide (wide-p d)))
	 (incf i)))))

(defun draw-image-drop (drop images)
  (declare (type image-drop drop))
  (with-slots (x y image color) drop
;;; (declare (type fixnum x y))
    (let ((dx (truncate x))
	  (dy (truncate y))
	  (screen-x 0) (screen-y 0)
	  (screen-width (tt-width)) (screen-height (tt-height)))
      (declare (type fixnum screen-x screen-y screen-width screen-height))
      (tt-color (car color) (cdr color))
      (let ((img (aref images image)))
	(with-slots ((cx x) (cy y) width height data wide) img
	  (declare (type fixnum cx cy))
	  (macrolet ((loopy (&body body)
			`(loop :for i :of-type fixnum :from 0 :below height :do
			    (loop :for j :of-type fixnum :from 0 :below width :do
			       (if (char/= (aref data i j) #\space)
				   (progn ,@body))))))
	    (if wide
		(loopy
		   (setf screen-y (+ (- dy cy) i)
			 screen-x (+ (- dx cx) j))
		   (when (and (>= screen-x 0) (< screen-x screen-width)
			      (>= screen-y 0) (< screen-y screen-height))
		     (tt-move-to screen-y screen-x)
		     (tt-write-char (aref data i j))))
		(loopy
		   (setf screen-y (+ (- dy cy) i)
			 screen-x (+ (- dx cx) j))
		   (when (and (>= screen-x 0) (< screen-x screen-width)
			      (>= screen-y 0) (< screen-y screen-height))
		     (tt-move-to screen-y screen-x)
		     (tt-write-char (aref data i j)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rain

(defclass rainy (sky)
  ((drop-images
    :initarg :drop-images :accessor drop-images :initform #()
    :type (simple-array drop-image *)
    :documentation "Images of the drops."))
  (:documentation "A rainy sky."))

(defclass rain-drop (image-drop)
  ()
  (:documentation "Rain drop."))

(defun rain-drop-state (drop)
  (image-drop-image drop))

(defun set-rain-drop-state (drop state)
  (setf (image-drop-image drop) state))

(defsetf rain-drop-state set-rain-drop-state)

(defparameter *drop-images-init*
  #((0 0 #("."))
    (0 0 #("o"))
    (0 0 #("O"))
    (1 1 #(" - "
	   "|.|"
	   " - "))
    (2 2 #("  -  "
	   " / \\ "
	   "| O |"
	   " \\ / "
	   "  -  "))))

;; (defparameter *drop-images* #())
;; (declaim (type (simple-array drop-image *) *drop-images*))

;; (defun init-drop-images (sky)
;;   "Initialize the images from lists to structs."
;;   (with-slots (drop-images) sky
;;     (if (= (length *drop-images*) 0)
;; 	(progn
;; 	  (setf drop-images (make-array (length *drop-images-init*)
;; 					:element-type 'drop-image
;; 					:initial-element (make-drop-image)))
;; 	  (loop :with w :and h :and i = 0
;; 	     :for (x y d) :across *drop-images-init*
;; 	     :do
;; 	     (setf w (length (aref d 0))
;; 		   h (length d))
;; 	     (setf (aref drop-images i)
;; 		   (make-drop-image :x x :y y :width w :height h
;; 				    :data (make-array (list w h)
;; 						      :element-type 'character
;; 						      :initial-contents d)))
;; 	     (incf i))
;; 	  (setf *drop-images* drop-images))
;; 	(setf drop-images *drop-images*))))

(defmethod initialize-instance
    :after ((o rainy) &rest initargs &key &allow-other-keys)
  "Initialize a rainy sky."
  (declare (ignore initargs))
  (init-drop-images o *drop-images-init*))

(defmethod create-drop ((sky rainy))
  "Create a drop in a rainy sky."
  (push
   (make-instance 'rain-drop
		  :x (random (tt-width))
		  :y (random (tt-height))
		  :image 0 :color (random-color))
   (drops sky)))

(defmethod draw-drop ((sky rainy) (drop rain-drop))
  (draw-image-drop drop (drop-images sky)))

;; (defmethod draw-drop ((drop rain-drop))
;;   (declare (type rain-drop drop))
;;   (with-slots (x y state color) drop
;;     (declare (type fixnum x y))
;;     (color-set color (cffi:null-pointer))
;;     (let ((img (aref *drop-images* state)))
;;       (with-slots ((cx x) (cy y) width height data) img
;; 	(declare (type fixnum cx cy))
;; 	(loop :for i :of-type fixnum :from 0 :below height :do
;; 	   (loop :for j :of-type fixnum :from 0 :below width :do
;; 	      (if (char/= (aref data i j) #\space)
;; 		  (mvaddch (+ (- y cy) i)
;; 			   (+ (- x cx) j)
;; 			   (char-code (aref data i j))))))))))

(defmethod drop-cycle ((sky rainy))
  (loop :for d :in (copy-list (drops sky)) :do
     (incf (rain-drop-state d))
     (when (> (rain-drop-state d) (1- (length (drop-images sky))))
       (setf (drops sky) (delete d (drops sky))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snow

(defclass snowy (sky)
  ((snow-pos
    :initarg :snow-pos :accessor snow-pos :initform nil #| :type fixnum |#
    :documentation "Position of the snowbody.")
   (snow-chars
    :initarg :snow-chars :accessor snow-chars
    :documentation "Snow characters to use.")
   (accumulation
    :initarg :accumulation :accessor accumulate :initform 0.0 :type float
    :documentation "Accumulation factor.")))

(defclass snow-drop (drop)
  ((char :initarg :char :accessor snow-drop-char))
  (:documentation "Snow drop."))

(defparameter *unicode-snow-chars*
  #(#.(code-char #x00002603) ; ☃ SNOWMAN
    #\*
    #\.
    #.(code-char #x00002744) ; ❄ SNOWFLAKE
    #.(code-char #x00002744) ; ❄ SNOWFLAKE
    #.(code-char #x00002745) ; ❅ TIGHT_TRIFOLIATE_SNOWFLAKE
    #.(code-char #x00002746) ; ❆ HEAVY_CHEVRON_SNOWFLAKE
    #.(code-char #x00002735) ; ✵ EIGHT_POINTED_PINWHEEL_STAR
    #.(code-char #x00002736) ; ✶ SIX_POINTED_BLACK_STAR
    #.(code-char #x00002737) ; ✷ EIGHT_POINTED_RECTILINEAR_BLACK_STAR
    #.(code-char #x00002738) ; ✸ HEAVY_EIGHT_POINTED_RECTILINEAR_BLACK_STAR
    #.(code-char #x00002739) ; ✹ TWELVE_POINTED_BLACK_STAR
    #.(code-char #x00002731) ; ✱ HEAVY_ASTERISK
    #.(code-char #x00002732) ; ✲ OPEN_CENTRE_ASTERISK
    #.(code-char #x0000274A) ; ❊ EIGHT_TEARDROP-SPOKED_PROPELLER_ASTERISK
;    #.(code-char #x0000FF0A) ; ＊ FULLWIDTH_ASTERISK
    ))

(defparameter *ascii-snow-chars*
  #(#\B #\* #\. #\* #\* #\:))

(defmethod create-drop ((sky snowy))
  "Create a drop in the snowy sky."
  (push
   (make-instance 'snow-drop
		  :x (random (tt-width))
		  :y (random (tt-height))
		  :char (1+ (random (1- (length (snow-chars sky)))))
		  :color (cons :white :black))
   (drops sky)))

(defmethod draw-drop ((sky snowy) (drop snow-drop))
  (declare (type snow-drop drop))
  (with-slots (x y char color) drop
    (declare (type fixnum x y))
    (tt-color (car color) (cdr color))
    (let ((chr (aref (snow-chars sky) char)))
      (when (and (>= y 0) #| they shouldn't go off the bottom |#
		 (>= x 0) (< x (tt-width)))
	(tt-move-to y x)
	(tt-write-char chr)))))

(defmethod drop-cycle ((sky snowy))
  (sleep .06)
  (loop :for d :in (copy-list (drops sky)) :do
     (incf (drop-y d))
     (incf (drop-x d) (- 1 (random 3)))
     (when (> (drop-y d) (1- (tt-height)))
       (setf (drops sky) (delete d (drops sky))))))

(defmethod draw-background ((sky snowy))
  (when (not (snow-pos sky))
    (setf (snow-pos sky) (random (1- (tt-width)))))
  (tt-move-to (1- (tt-height)) (snow-pos sky))
  (tt-write-char (aref (snow-chars sky) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutrix

(defclass mutrix (sky)
  ((bg
    :initarg :bg :accessor mutrix-bg :initform nil
    :documentation "The background.")))

(defmethod initialize-instance
    :after ((o mutrix) &rest initargs &key &allow-other-keys)
  "Initialize a Mutrix."
  (declare (ignore initargs))
  (setf (mutrix-bg o)
	(make-array (list (tt-width) (tt-height))
		    :element-type 'fixnum :initial-element 0)))

(defclass mutrix-drop (drop)
  ((char :initarg :char :accessor mutrix-drop-char))
  (:documentation "Mutrix drop."))

(defun nice-random-char ()
  (loop :with r = (random #xffff)
     :while (not
	     (or
	      (and (>= r #x00021) (<= r #x0007e)) ; ascii
	      (and (>= r #x000a1) (<= r #x00233)) ; extended latin
	      (and (>= r #x00250) (<= r #x002af)) ; ipa
	      (and (>= r #x00390) (<= r #x004f5)) ; greek & cyrillic
;	      (and (>= r #x00905) (<= r #x0097f)) ; devangari
	      (and (>= r #x00e01) (<= r #x00e2f)) ; thai
;	      (and (>= r #x00f00) (<= r #x00fd1)) ; tibetian
	      (and (>= r #x02010) (<= r #x02052)) ; punctuation
	      (and (>= r #x020a0) (<= r #x020b2)) ; currency
	      (and (>= r #x02100) (<= r #x0260a)) ; misc symbols 1
	      (and (>= r #x02700) (<= r #x0285f)) ; misc symbols 1
;	      (and (>= r #x03000) (<= r #x0318e)) ; cjk 1
;	      (and (>= r #x03200) (<= r #x03229)) ; cjk 2
;	      (and (>= r #x03260) (<= r #x0327b)) ; cjk 3
;	      (and (>= r #x03380) (<= r #x033ca)) ; cjk 4
; so many holes
;	      (and (>= r #x04e00) (<= r #x09fbb)) ; cjk
;	      (and (>= r #x0a000) (<= r #x0a4c6)) ; yi
;	      (and (>= r #x0ac00) (<= r #x0d7a3)) ; hangul
	      (and (>= r #x0ff00) (<= r #x0ff9a)) ; half/full kana
	      #|
	      (and (>= r #x1d300) (<= r #x1d356)) ; tai xuan jing
	      (and (>= r #x1d400) (<= r #x1d7ff)) ; math alpha
	      |#
	      ))
     :do (setf r (random #xffff))
     :finally (return r)))

(defmethod create-drop ((sky mutrix))
  "Create a drop in the Mutrix."
  (push
   (make-instance 'mutrix-drop
		  :x (random (tt-width))
		  :y (random (tt-height))
		  :char (if (zerop (random 17)) 0 (nice-random-char))
		  :color (cons :white :black))
   (drops sky)))

(defmethod draw-drop ((sky mutrix) (drop mutrix-drop))
  (declare (type mutrix-drop drop) (ignore sky))
  (with-slots (x y char color) drop
    (declare (type fixnum x y))
    (tt-color (car color) (cdr color))
    (tt-move-to y x)
    (tt-bold t)
    (tt-write-char (code-char char))
    (tt-bold nil)))

(defmethod drop-cycle ((sky mutrix))
  (loop :for d :in (copy-list (drops sky)) :do
     (setf (aref (mutrix-bg sky) (drop-x d) (drop-y d)) (mutrix-drop-char d))
     (incf (drop-y d))
     (when (not (zerop (mutrix-drop-char d)))
       (incf (mutrix-drop-char d)))
     (when (> (drop-y d) (1- (tt-height)))
       (setf (drops sky) (delete d (drops sky))))))

(defmethod draw-background ((sky mutrix))
  "Draw the mutable background."
  (tt-color :green :black)
  (loop
     :with height = (array-dimension (mutrix-bg sky) 1)
     :and width = (array-dimension (mutrix-bg sky) 0)
     :for y :of-type fixnum :from 0 :below height :do
     (loop :for x :of-type fixnum :from 0 :below width :do
	(tt-move-to y x)
	(let ((cc (aref (mutrix-bg sky) x y)))
	  (when (/= cc 0)
	    (tt-write-char (code-char cc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Space

(defclass spacey (sky)
  (
   #| (drops
    :initarg :drops :accessor drops #| :initform #() :type vector |#
    :documentation "Stars in the sky.") |#
   (free-list
    :initarg :free-list :accessor free-list :initform 0 :type fixnum
    :documentation "Index of the first free space in the drops vector.")
   (drop-images
    :initarg :drop-images :accessor drop-images :initform #()
    :type (simple-array star-image *)
    :documentation "Images of the stars.")
   (colorful
    :initarg :colorful :accessor colorful :initform nil :type boolean
    :documentation "True if the stars are colorful.")
   (use-unicode
    :initarg :use-unicode :accessor use-unicode :initform nil :type boolean
    :documentation "True if the stars use unicode characters.")
   (random-star-picker
    :initarg :random-star-picker :accessor random-star-picker
    :documentation "Function to pick a random star.")
   )
  (:documentation "A starry sky."))

;;
;;         /|
;;        / | y = cos(Θ)
;;       /  |
;;   ∠Θ /___|
;;        x = sin(Θ)

(defclass star (image-drop)
  ((x :initarg :x :accessor drop-x :initform 0.0 :type single-float)
   (y :initarg :y :accessor drop-y :initform 0.0 :type single-float)
   (direction
    :initarg :direction :accessor star-direction :initform 0.0 :type single-float
    :documentation "Direction the star is moving.")
   (velocity
    :initarg :velocity :accessor star-velocity :initform 1.0 :type single-float
    :documentation "The rate at which it's moving.")
   (x-inc
    :initarg :x-inc :accessor star-x-inc :initform 0.0 :type single-float
    :documentation "Pre-calculated X increment.")
   (y-inc
    :initarg :y-inc :accessor star-y-inc :initform 0.0 :type single-float
    :documentation "Pre-calculated Y increment.")
   (free
    :initarg :free :accessor star-free :initform -1 :type fixnum
    :documentation "Index to the next free star. -1 if the star is in use."))
  (:documentation "Star you are."))

(defun recalc-increments (star)
  (setf
   (star-x-inc star) (float (* (star-velocity star)
			       (sin (* (star-direction star) (/ pi 180))))
			    (star-x-inc star))
   (star-y-inc star) (float (* (star-velocity star)
			       (cos (* (star-direction star) (/ pi 180))))
			    (star-y-inc star))))
	 
(defmethod initialize-instance
    :after ((o star) &rest initargs &key &allow-other-keys)
  "Initialize a star."
  (declare (ignore initargs))
  (recalc-increments o))

(defmethod (setf star-direction) ((star star) value)
  (setf (slot-value star 'direction) value)
  (recalc-increments star))

(defmethod (setf star-velocity) ((star star) value)
  (setf (slot-value star 'velocity) value)
  (recalc-increments star))

(defstruct star-image
  (x      0   :type fixnum)
  (y      0   :type fixnum)
  (width  0   :type fixnum)
  (height 0   :type fixnum)
;  (data   #2A((#\x)) :type (simple-array character (* *))))
  data)
  
(defparameter *unicode-star-images-init*
  #((0 0 #("."))
    (0 0 #("*"))
    (0 0 #("⭑")) ; BLACK_SMALL_STAR
    (0 0 #("⭒")) ; WHITE_SMALL_STAR
    (0 0 #("✵")) ; EIGHT_POINTED_PINWHEEL_STAR
    (0 0 #("✶")) ; SIX_POINTED_BLACK_STAR
    (0 0 #("✹")) ; TWELVE_POINTED_BLACK_STAR
    (0 0 #("★")) ; BLACK_STAR
    (0 0 #("☆")) ; WHITE_STAR
    (0 0 #("⭐")) ; WHITE_MEDIUM_STAR
    (0 0 #("🌟")) ; GLOWING_STAR
    (0 0 #("✭")) ; OUTLINED_BLACK_STAR
    (0 0 #("✮")) ; HEAVY_OUTLINED_BLACK_STAR
    (0 0 #("✯")) ; PINWHEEL_STAR
    (0 0 #("✰")) ; SHADOWED_WHITE_STAR
    (0 0 #("✴")) ; EIGHT_POINTED_BLACK_STAR
    (1 1 #(" - "
	   "|.|"
	   " - "))
    (1 1 #(" | "
	   "-*-"
	   " | "))
    (1 1 #("\\|/"
	   "-*-"
	   "/|\\"))
    (2 2 #("  |  "
	   " \\|/ "
	   "--o--"
	   " /|\\ "
	   "  |  "))
    (2 2 #("  -  "
	   " / \\ "
	   "| O |"
	   " \\ / "
	   "  -  "))))

(defparameter *ascii-star-images-init*
  #((0 0 #("."))
    (0 0 #("*"))
    (1 1 #(" - "
	   "|.|"
	   " - "))
    (1 1 #(" | "
	   "-*-"
	   " | "))
    (1 1 #("\\|/"
	   "-*-"
	   "/|\\"))
    (2 2 #("  |  "
	   " \\|/ "
	   "--o--"
	   " /|\\ "
	   "  |  "))
    (2 2 #("  -  "
	   " / \\ "
	   "| O |"
	   " \\ / "
	   "  -  "))))

;(defparameter *star-images* #())
;(declaim (type (simple-array drop-image *) *star-images*))

(defun unicode-random-star-image ()
  (let ((r (random 100)))
    (cond
      ((< r 40) 0) 			; .
      ((< r 85) 1)			; *
      ((< r 95) (+ 2 (random 14)))	; fancy stars
      ((< r 100) (+ 14 (random 5))))))	; big stars

(defun ascii-random-star-image ()
  (let ((r (random 100)))
    (cond
      ((< r 40) 0) 			; .
      ((< r 95) 1)			; *
      ((< r 100) (+ 2 (random 5))))))	; big stars

(defmethod initialize-instance
    :after ((o spacey) &rest initargs &key &allow-other-keys)
  "Initialize a rainy sky."
  (declare (ignore initargs))
  (init-drop-images o (if (use-unicode o)
			  *unicode-star-images-init*
			  *ascii-star-images-init*))
  (setf (random-star-picker o) (if (use-unicode o)
				   #'unicode-random-star-image
				   #'ascii-random-star-image)))

(defparameter *star-colors*
  #.(vector :blue :cyan :white :magenta :red :green :yellow))
(declaim (type (simple-vector 7) *star-colors*))

(defun star-color (sky)
  (if (colorful sky)
      (cons (elt *star-colors*
		 (random (length *star-colors*)))
	    :black)
      (cons :white :black)))

(defun random-velocity (sky)
  (let ((r (random 100)))
    (+ .5 (* (sky-param sky) .5)
       (cond
	 ((< r 50) (random 1.5))
	 ((< r 80) (random 2))
	 ((< r 95) (random 4))
	 ((< r 100) (random 7))))))

(defparameter *initial-stars* 512
  "How many stars to make in a clear sky.")

(defmethod clear-sky ((sky spacey))
  (with-slots (free-list drops) sky
    (setf drops (make-array
		 (list *initial-stars*)
		 :element-type 'star
		 :adjustable t
		 :initial-contents
		 (loop :for i :from 1 :to *initial-stars*
		    :collect (make-instance 'star :free i)))
	  free-list 0)))

(defun msg (sky fmt &rest args)
  (declare (ignore sky))		; @@@
  (tt-move-to (1- (tt-height)) 0)
  (tt-format fmt args)
  ;;(timeout -1)
  (tt-get-char)
  ;;(timeout (time-out sky))
  ;;(tt-listen-for (/ time-out 100))
  )

(defmethod create-drop ((sky spacey))
  "Create a star in the sky."
  (with-slots (free-list drops) sky
    (when (= free-list (length drops))
      ;; If there's no free spots left, expand the array.
      ;; (msg sky "REALLLOC ~s" (type-of drops))
      (let* ((old-length (length drops))
	     (new-size (* old-length 2)))
	(adjust-array drops new-size :element-type 'star)
	(loop :for i :from free-list :below new-size
	   :do (setf (aref drops i) (make-instance 'star :free (1+ i))))
	(setf free-list old-length)))

    (let (star (i free-list))
      (setf free-list (star-free (aref drops free-list))
	    star (aref drops i)
	    (drop-x star) (float (/ (tt-width) 2))
	    (drop-y star) (float (/ (tt-height) 2))
	    (drop-color star) (star-color sky)
	    (image-drop-image star) (funcall (random-star-picker sky))
	    (star-x-inc star) 0.0
	    (star-y-inc star) 0.0
	    (star-direction star) (random 360.0)
	    (star-velocity star) (random-velocity sky)
	    (star-free star) -1)
      (recalc-increments star)
      ;; (msg sky "~d ~d ~d ~d ~d" i
      ;; 	   (star-x-inc star) (star-y-inc star)
      ;; 	   (star-direction star) (star-velocity star))
      )))

(defun delete-star (sky i)
  (with-slots (free-list drops) sky
    (setf (star-free (aref drops i)) free-list
	  free-list i)))

(defmethod draw-drop ((sky spacey) (drop star))
  (declare (type star drop))
  (draw-image-drop drop (drop-images sky)))

(defmethod draw-drops ((sky spacey))
  (loop :for d :across (drops sky)
     :do
     (when (minusp (star-free d))
       (draw-drop sky d))))

(defmethod drop-cycle ((sky spacey))
  (with-slots (drops) sky
    (loop :with d
       :for i :from 0 :below (length drops)
       :do
       (setf d (aref drops i))
       ;; (incf (image-drop-image d))
       (when (minusp (star-free d))
	 (incf (drop-x d) (star-x-inc d))
	 (incf (drop-y d) (star-y-inc d))
	 (when (or (> (drop-x d) (tt-width)) (< (drop-x d) 0)
		   (> (drop-y d) (tt-height)) (< (drop-y d) 0))
	   (delete-star sky i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fire

(defclass fire (sky)
  ((wind
    :initarg :wind :accessor fire-wind :initform 0 :type fixnum
    :documentation "Amount of wind.")
   (wind-counter
    :initarg :wind-counter :accessor fire-wind-counter :initform 0 :type fixnum
    :documentation "Countdown until the wind stops."))
  (:default-initargs :param 12)
  (:documentation "Your terminal is on fire."))

(defclass flame (drop)
  ((heat
    :initarg :heat :accessor flame-heat :initform 1f0 :type float
    :documentation "How hot is it, between 1 and 0."))
  (:documentation "A bit of flame."))

(defmethod create-drop ((sky fire))
  (declare (ignore sky))
  #| Don't create random flames based on density. |#
  )

(defun flame-color (n)
  "Return the color given the heat."
  (vector :rgb (if (> n 0) 1.0 0.0) (min n 1.0) (* pi (max 0.0 (- n .8)))))

(defmethod draw-drop ((sky fire) (drop flame))
  (with-slots (x y heat) drop
    (tt-color :black (flame-color heat))
    (when (< -1 x (tt-width))
      (when (not (and (= y (1- (tt-height)))
		      (= x (1- (tt-width)))))
	(tt-write-char-at y x #\space)))))

(defmethod drop-cycle ((sky fire))
  (with-slots (wind wind-counter) sky
    ;; @@@ This wind is crap. We could use much better wind.
    ;; Like with sine undulation and logarithmic decay.
    #|
    (if (zerop wind-counter)
	(when (zerop (random 40))
	  (setf wind (- (random 10) 5)
		wind-counter (+ 10 (random 10))))
	(progn
	  (decf wind-counter)
	  (when (not (zerop wind))
	    (setf wind (+ wind (* -1 (signum wind)))))))
    |#
    (loop :with r
       :for d :in (copy-list (drops sky)) :do
       (decf (drop-y d))
       (setf r (random 100))
       ;; (incf (drop-x d) (cond ((< r 5) -2)
       ;; 			    ((< 5 r 20) 1)
       ;; 			    ((< 10 r 90) 0)
       ;; 			    ((< 80 r 95) 1)
       ;; 			    ((> 5) 2)))
       (incf (drop-x d) (+ (1- (random 3)) wind))
       (decf (flame-heat d) (random (* .01 (max 0.1 (sky-param sky)))))
       ;; (decf (flame-heat d) (* .01 (max 0.1 (sky-param sky))))
       (when (or (<= (drop-y d) 0)
		 (< (flame-heat d) 0.0))
	 (setf (drops sky) (delete d (drops sky)))))))

(defmethod frame-end ((sky fire))
  (with-slots (density) sky
    (loop :for x :from 0 :below (tt-width)
       :do
       (push (make-instance 'flame
			    :x x
			    :y (1- (tt-height))
			    :heat (+ .8 (random 0.2))) (drops sky))
       (push (make-instance 'flame
			    :x x
			    :y (- (tt-height) (random 4))
			    :heat (+ .5 (random 0.5))) (drops sky))
       (when (> density 1)
	 (dotimes (i (ceiling (* density .12)))
	   (push (make-instance 'flame
				:x x
				:y (- (tt-height) (random 4))
				:heat (+ .5 (random 0.5))) (drops sky)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod precipitate ((sky sky))
  (clear-sky sky)
  ;; (with-new-terminal ()
  (with-terminal ()
    (unwind-protect
      (progn
	(tt-cursor-off)
	(tt-clear)
	(tt-enable-events :resize)
	(with-slots (time-out drops density param) sky
	  (let ((last-t-o -1) modeline)
	    ;;(timeout time-out)
	    (loop
	       ;; create drops
	       (if (< density 1)
		   (when (<= (random 1.0) density)
		     (create-drop sky))
		   (dotimes (i (ceiling density))
		     (create-drop sky)))
	       ;; draw drops
	       (tt-home)
	       (tt-erase-below)
	       (draw-background sky)
	       (draw-drops sky)
	       ;; cycle drops
	       (drop-cycle sky)
	       (frame-end sky)
	       (when modeline
		 (tt-color :white :black)
		 (tt-move-to (1- (tt-height)) 0)
		 (tt-format "~a ~a ~a ~a"
			    density time-out param (type-of *terminal*)))
	       ;; check input
	       (tt-finish-output)
	       (when (or (not time-out)
			 (tt-listen-for (/ time-out 1000)))
		 (case (tt-get-key)
		   ((#\q #\Q) (return))
		   (:resize (resize-sky sky))
		   (#\page (resize-sky sky))
		   (#\+ (decf time-out 1))
		   (#\- (incf time-out 1))
		   (#\= (setf time-out *default-timeout*))
		   (#\d (if (> density 1) (incf density) (incf density .1)))
		   (#\D (if (>= density 2) (decf density) (decf density .1)))
		   (#\i (incf (sky-param sky)))
		   (#\I (decf (sky-param sky)))
		   (#\m (setf modeline (not modeline)))
		   (#\p (if (not time-out)
			    (setf time-out last-t-o)
			    (progn (setf last-t-o time-out)
				   ;;(timeout (setf time-out -1))
				   (setf time-out nil))
			    ))))))))
      ;;(timeout -1)
      (tt-cursor-on)
      (tt-move-to (tt-height) 0)
      )))

(defun rain (&key (density 0.5))
  (precipitate (make-instance 'rainy :density density)))

#+lish
(lish:defcommand rain 
  (("density" float :short-arg #\d :default *default-density*))
  "Rain on your screen."
  (rain :density density))

(defun stars (&key (density 3.0) (color nil) (use-unicode t))
  (precipitate (make-instance 'spacey :density density :colorful color
			      :use-unicode use-unicode)))

#+lish
(lish:defcommand stars
    ((density float :short-arg #\d :default 3.0
      :help "Density of stars.")
   (color boolean :short-arg #\c :help "True to make color stars.")
   (ascii boolean :short-arg #\a :help "True to use ASCII stars."))
  "Fly through the stars."
  (stars :density density :color color :use-unicode (not ascii)))

(defun snow (&key (density *default-density*) (unicode t))
  (precipitate (make-instance 'snowy
			      :density density
			      :snow-chars
			      (if unicode
				  *unicode-snow-chars*
				  *ascii-snow-chars*))))

#+lish
(lish:defcommand snow
  ((density float :short-arg #\d :default *default-density*
    :help "How dense the snow is, as a floating point number.")
   (ascii boolean :short-arg #\a :help "True to use ascii snow."))
  "Snow on your screen."
  (snow :density density :unicode (not ascii)))

(defun mutrix (&key (density *default-density*))
  (precipitate (make-instance 'mutrix :density density)))

#+lish
(lish:defcommand mutrix
  ((density float :short-arg #\d :default *default-density*))
  "Peer into the Mutrix."
  (mutrix :density density))

(defun fire (&key (density 10f0))
  (precipitate (make-instance 'fire :density density :time-out 20)))

#+lish
(lish:defcommand fire
  ((density float :short-arg #\d :default 10f0))
  "Set your terminal on fire."
  (fire :density density))

;; EOF
