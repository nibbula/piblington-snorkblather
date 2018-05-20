;;
;; rain.lisp - Rain on the screen
;;

;; This is like the old thing from a VAX or something.

;(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
;(declaim (optimize (speed 3) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defpackage :rain
  (:documentation "Rain on the screen.")
  (:shadowing-import-from :curses #:timeout)
  (:use :cl :curses :dlib :fui :terminal :terminal-curses)
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
    :initarg :parm :accessor sky-param :initform 0
    :documentation "A numeric parameter.")
   (time-out
    :initarg :time-out :accessor time-out
    :initform *default-timeout* :type fixnum
    :documentation "Time for timer."))
  (:documentation "The roof of the world."))

(defgeneric create-drop (sky)
  (:documentation "Make a drop in the sky."))

(defgeneric draw-background (sky)
  (:documentation "Draw the background of the sky.")
  (:method ((sky sky)) (declare (ignore sky))))

(defgeneric drop-cycle (sky)
  (:documentation "Cycle the drops in the sky."))

(defgeneric clear-sky (sky)
  (:documentation "Clear the sky.")
  (:method ((sky sky))
    (setf (drops sky) '())))

(defgeneric precipitate (sky)
  (:documentation "Precipitate in the sky."))

(defclass drop ()
  ((x :initarg :x :accessor drop-x :initform 0 #| :type fixnum |#)
   (y :initarg :y :accessor drop-y :initform 0 #| :type fixnum |#)
   (color :initarg :color :accessor drop-color :type integer :initform 0))
  (:documentation "Generic precipitation drop."))

(defgeneric draw-drop (sky drop)
  (:documentation "Draw a drop in the sky."))

(defgeneric draw-drops (sky)
  (:documentation "Draw all the drops in the sky.")
  (:method ((sky sky))
    (loop :for d :in (drops sky) :do (draw-drop sky d))))

(defparameter *drop-colors*
  #.(vector curses:+COLOR-BLUE+ curses:+COLOR-CYAN+ curses:+COLOR-WHITE+))
(declaim (type (simple-vector 3) *drop-colors*))

(defun random-color ()
  (color-index (elt *drop-colors*
		    (random (length *drop-colors*)))
	       +COLOR-BLACK+))

#+curses-use-wide
(defun add-wide-string (wide-string)
  (declare (type string wide-string))
  (cffi:with-foreign-object (fstr :int (1+ (length wide-string)))
      (loop :with i = 0 :for c :across wide-string :do
	 (setf (cffi:mem-aref fstr :int i) (char-code c))
	 (incf i))
      (setf (cffi:mem-aref fstr :int (length wide-string)) 0)
      (addnwstr fstr (length wide-string))))

(defvar *wide-char* (cffi:foreign-alloc :int :count 2))

#+curses-use-wide
(defun add-wide-char-OLD (wide-char)
  (cffi:with-foreign-object (fstr :int 2)
    (setf (cffi:mem-aref fstr :int 0) wide-char
	  (cffi:mem-aref fstr :int 1) 0)
    (addnwstr fstr 2)))

#+curses-use-wide
(defun add-wide-char (wide-char)
  (setf (cffi:mem-aref *wide-char* :int 0) wide-char
	(cffi:mem-aref *wide-char* :int 1) 0)
    (addnwstr *wide-char* 2))


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
  (with-curses
    (clear)
    (loop :for i :from 0 :to 99 :do
      (loop :for j :from 0 :below (aref aa i) :do
	 (mvaddch (- *lines* (+ 1 j)) i (char-code #\#))))
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
	  (dy (truncate y)))
      (declare (type fixnum dx dy))
    (color-set (or color 0) (cffi:null-pointer))
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
		 (move (+ (- dy cy) i)
		       (+ (- dx cx) j))
		 (add-wide-char (char-code (aref data i j))))
	      (loopy
		 (mvaddch (+ (- dy cy) i)
			  (+ (- dx cx) j)
			  (char-code (aref data i j)))))))))))

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
		  :x (random curses:*cols*)
		  :y (random curses:*lines*)
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
    :documentation "Position of the snowbody.")))

(defclass snow-drop (drop)
  ((char :initarg :char :accessor snow-drop-char))
  (:documentation "Snow drop."))

;; #.(code-char #x00002603) ; ☃ SNOWMAN
(defparameter *snow-chars*
  #(#.(char-code #\*)
    #.(char-code #\.)
    #x00002744 ; ❄ SNOWFLAKE
    #x00002744 ; ❄ SNOWFLAKE
    #x00002745 ; ❅ TIGHT_TRIFOLIATE_SNOWFLAKE
    #x00002746 ; ❆ HEAVY_CHEVRON_SNOWFLAKE
    #x00002735 ; ✵ EIGHT_POINTED_PINWHEEL_STAR
    #x00002736 ; ✶ SIX_POINTED_BLACK_STAR
    #x00002737 ; ✷ EIGHT_POINTED_RECTILINEAR_BLACK_STAR
    #x00002738 ; ✸ HEAVY_EIGHT_POINTED_RECTILINEAR_BLACK_STAR
    #x00002739 ; ✹ TWELVE_POINTED_BLACK_STAR
    #x00002731 ; ✱ HEAVY_ASTERISK
    #x00002732 ; ✲ OPEN_CENTRE_ASTERISK
    #x0000274A ; ❊ EIGHT_TEARDROP-SPOKED_PROPELLER_ASTERISK
;    #x0000FF0A ; ＊ FULLWIDTH_ASTERISK
    ))

(defmethod create-drop ((sky snowy))
  "Create a drop in the snowy sky."
  (push
   (make-instance 'snow-drop
		  :x (random curses:*cols*)
		  :y (random curses:*lines*)
		  :char (random (length *snow-chars*))
		  :color (color-index curses:+COLOR-WHITE+
				      curses:+COLOR-BLACK+))
   (drops sky)))

(defmethod draw-drop ((sky snowy) (drop snow-drop))
  (declare (type snow-drop drop) (ignore sky))
  (with-slots (x y char color) drop
    (declare (type fixnum x y))
    (color-set (or color 0) (cffi:null-pointer))
    (let ((chr (aref *snow-chars* char)))
      (move y x)
      (add-wide-char chr))))

(defmethod drop-cycle ((sky snowy))
  (sleep .06)
  (loop :for d :in (copy-list (drops sky)) :do
     (incf (drop-y d))
     (incf (drop-x d) (- 1 (random 3)))
     (when (> (drop-y d) (1- curses:*lines*))
       (setf (drops sky) (delete d (drops sky))))))

(defmethod draw-background ((sky snowy))
  (when (not (snow-pos sky))
    (setf (snow-pos sky) (random (1- curses:*cols*))))
  (move (1- curses:*lines*) (snow-pos sky))
  (add-wide-string "☃"))

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
	(make-array (list curses:*cols* curses:*lines*)
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
		  :x (random curses:*cols*)
		  :y (random curses:*lines*)
		  :char (if (zerop (random 17)) 0 (nice-random-char))
		  :color (color-index curses:+COLOR-WHITE+
				      curses:+COLOR-BLACK+))
   (drops sky)))

(defmethod draw-drop ((sky mutrix) (drop mutrix-drop))
  (declare (type mutrix-drop drop) (ignore sky))
  (with-slots (x y char color) drop
    (declare (type fixnum x y))
    (color-set (or color 0) (cffi:null-pointer))
    (move y x)
    (attron +a-bold+)
    (add-wide-char char)
    (attroff +a-bold+)))

(defmethod drop-cycle ((sky mutrix))
  (loop :for d :in (copy-list (drops sky)) :do
     (setf (aref (mutrix-bg sky) (drop-x d) (drop-y d)) (mutrix-drop-char d))
     (incf (drop-y d))
     (when (not (zerop (mutrix-drop-char d)))
       (incf (mutrix-drop-char d)))
     (when (> (drop-y d) (1- curses:*lines*))
       (setf (drops sky) (delete d (drops sky))))))

(defmethod draw-background ((sky mutrix))
  "Draw the mutable background."
  (color-set (color-index curses:+COLOR-GREEN+ curses:+COLOR-BLACK+)
             (cffi:null-pointer))
  (loop
     :with height = (array-dimension (mutrix-bg sky) 1)
     :and width = (array-dimension (mutrix-bg sky) 0)
     :for y :of-type fixnum :from 0 :below height :do
     (loop :for x :of-type fixnum :from 0 :below width :do
	(move y x)
	(let ((cc (aref (mutrix-bg sky) x y)))
	  (when (/= cc 0)
	    (add-wide-char cc))))))

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
    :documentation "True if the stars are colorful."))
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
  
(defparameter *star-images-init*
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

;(defparameter *star-images* #())
;(declaim (type (simple-array drop-image *) *star-images*))

(defmethod initialize-instance
    :after ((o spacey) &rest initargs &key &allow-other-keys)
  "Initialize a rainy sky."
  (declare (ignore initargs))
  (init-drop-images o *star-images-init*))

(defparameter *star-colors*
  #.(vector curses:+COLOR-BLUE+ curses:+COLOR-CYAN+ curses:+COLOR-WHITE+
	    curses:+COLOR-MAGENTA+ curses:+COLOR-RED+ curses:+COLOR-GREEN+
	    curses:+COLOR-YELLOW+))
(declaim (type (simple-vector 7) *star-colors*))

(defun star-color (sky)
  (if (colorful sky)
      (color-index (elt *star-colors*
			(random (length *star-colors*)))
		   +COLOR-BLACK+)
      (color-index +COLOR-WHITE+ +COLOR-BLACK+)))

(defun random-star-image ()
  (let ((r (random 100)))
    (cond
      ((< r 40) 0) 			; .
      ((< r 85) 1)			; *
      ((< r 95) (+ 2 (random 14)))	; fancy stars
      ((< r 100) (+ 14 (random 5))))))	; big stars

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
  (mvaddstr (1- curses:*lines*) 0 (apply #'format nil fmt args))
  (timeout -1)
  (tt-get-char)
  (timeout (time-out sky)))

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
	    (drop-x star) (float (/ curses:*cols* 2))
	    (drop-y star) (float (/ curses:*lines* 2))
	    (drop-color star) (star-color sky)
	    (image-drop-image star) (random-star-image)
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
	 (when (or (> (drop-x d) curses:*cols*) (< (drop-x d) 0)
		   (> (drop-y d) curses:*lines*) (< (drop-y d) 0))
	   (delete-star sky i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod precipitate ((sky sky))
  (clear-sky sky)
  (with-curses
    ;;(init-colors)
    (curs-set 0)
    (clear)
    (with-slots (time-out drops density) sky
      (let ((last-t-o -1) modeline)
	(timeout time-out)
	(loop
	   ;; create drops
	   (if (< density 1)
	       (when (<= (random 1.0) density)
		 (create-drop sky))
	       (dotimes (i (ceiling density))
		 (create-drop sky)))
	   ;; draw drops
	   (erase)
	   (draw-background sky)
	   (draw-drops sky)
	   ;; cycle drops
	   (drop-cycle sky)
	   (when modeline
	     (attrset (color-attr +COLOR-WHITE+ +COLOR-BLACK+))
	     (mvaddstr (1- *lines*) 0 (format nil "~a ~a" density time-out)))
	   ;; check input
	   (case (tt-get-char)
	     ((#\q #\Q) (return))
	     (#\+ (timeout (decf time-out 1)))
	     (#\- (timeout (incf time-out 1)))
	     (#\= (timeout (setf time-out *default-timeout*)))
	     (#\d (if (> density 1) (incf density) (incf density .1)))
	     (#\D (if (>= density 2) (decf density) (decf density .1)))
	     (#\i (incf (sky-param sky)))
	     (#\I (decf (sky-param sky)))
	     (#\m (setf modeline (not modeline)))
	     (#\p (if (= -1 time-out)
		      (timeout (setf time-out last-t-o))
		      (progn (setf last-t-o time-out)
			     (timeout (setf time-out -1))))))))
      (timeout -1))))

(defun rain (&key (density 0.5))
  (precipitate (make-instance 'rainy :density density)))

#+lish
(lish:defcommand rain 
  (("density" float :short-arg #\d :default *default-density*))
  "Rain on your screen."
  (rain :density density))

(defun stars (&key (density 3.0) (color nil))
  (precipitate (make-instance 'spacey :density density :colorful color)))

#+lish
(lish:defcommand stars
  (("density" float :short-arg #\d :default 3.0)
   ("color" boolean :short-arg #\c))
  "Fly through the stars."
  (stars :density density :color color))

(defun snow (&key (density *default-density*))
  (precipitate (make-instance 'snowy :density density)))

#+lish
(lish:defcommand snow
  (("density" float :short-arg #\d :default *default-density*))
  "Snow on your screen."
  (snow :density density))

(defun mutrix (&key (density *default-density*))
  (precipitate (make-instance 'mutrix :density density)))

#+lish
(lish:defcommand mutrix
  (("density" float :short-arg #\d :default *default-density*))
  "Peer into the Mutrix."
  (mutrix :density density))

;; EOF
