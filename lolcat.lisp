;;
;; lolcat.lisp - Unfortunately, this doesn't make cats laugh.
;;

(defpackage :lolcat
  (:documentation "Unfortunately, this doesn't make cats laugh.")
  (:use :cl :dlib :fatchar :terminal :color)
  (:export
   #:lolcat
   ))
(in-package :lolcat)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defun mix-color (a b)
  ;;(format t "~s ~s~%" a b)
  (flet ((mix (comp)
	   (setf (color-component a comp)
		 (logand #xff (round (+ (color-component a comp)
					(color-component b comp)) 2)))))
    (mix :red)
    (mix :green)
    (mix :blue)))

(defun rainbow (frequency i)
  (flet ((wave (zerp)
	   (logand #xff (truncate
			 (+ (* (sin (+ (* frequency i) zerp)) 127) 128)))))
    (make-color
     :rgb8
     :red   (wave 0)
     :green (wave (/ (* 2 pi) 3))
     :blue  (wave (/ (* 4 pi) 3)))))

(defun flipper (offset)
  (if (> offset 255)
      (- 512 offset)
      offset))

(defun lolcat (&key files frequency spread)
  (loop :with lol :and lols = (make-fat-string)
     :for f :in (or files (list "-")) :do
     (write-string
      (with-terminal-output-to-string (:ansi)
	(let ((offset (random 255)))
	  (with-lines (line (if (equal f "-") *standard-input* f))
	    (setf lol (process-ansi-colors (make-fatchar-string line)))
	    ;; (lu::fatty lol)
	    (loop
	       :for i :from 0
	       :for c :across lol :do
	       (if (fatchar-fg c)
		   (progn
		     (setf (fatchar-fg c)
			   (convert-color-to
			    (lookup-color (fatchar-fg c)) :rgb8))
		     (mix-color (fatchar-fg c)
				(rainbow frequency
					 (+ (flipper offset)
					    (/ i spread)))))
		   (setf (fatchar-fg c)
			 (rainbow frequency (+ (flipper offset)
					       (/ i spread))))))
	    (setf (fat-string-string lols) lol)
	    (tt-write-string lols)
	    (tt-write-char #\newline)
	    (incf offset)
	    (when (> offset 512)
	      (setf offset 0))))
	(tt-finish-output)))))

#+lish
(lish:defcommand lolcat
  ((files pathname :optional t :repeating t
    :help "Files to concatencolorate.")
   (frequency number :short-arg #\F :default 0.3 :help "Rainnow frequency.")
   (spread number :short-arg #\p :default 8 :help "Rainnow spread."))
  :accepts (:grotty-stream)
  "Cats of many colors!!"
  (lolcat :files files :frequency frequency :spread spread))

;; EOF
