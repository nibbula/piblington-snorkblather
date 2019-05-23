;;
;; scores.lisp - Generic games scores.
;;

(defpackage :scores
  (:documentation
   "Generic games scores. A reminder of how much time you have wasted.

Example usage:

- Make your own score classes: 

  (defclass my-score-v1 (score) ())

  (defclass my-scores (scores)
    ()
    (:default-initargs
     :name \"a-fun-game\"
     :version 3
     :magic \"Fun-Score\"))

- Keep the scores object somehere:

  (defclass my-game ()
    (...
     (the-scores ... :type scores
                     :initform (make-scores 'my-scores 'my-score-v1))
    ...))

- When you want to read the scores:

  (read-scores the-scores)

- When you want to save a score:

  (save-score the-scores (make-instance 'my-score-v1 :n 1234567))

- If you want to change what's saved in the score:

  1. Make new score class:

       (defclass my-score-v2 (score) (..new slots..))

     It's probably best to keep the old one around so you can read or convert
     the old scores.

  2. Make sure to increment the version:

    (defclass my-scores (scores)
      ...    
      (:default-initargs
       :version 5
       ...))

- You could also define you own score file format by making your own
  READ-SCORE-VERSION and WRITE-SCORE-VERSION methods. The default methods just
  read and write the slots of the score class.
")
  (:use :cl :dlib :opsys :dlib-misc)
  (:export
   #:score
   #:score-n
   #:score-name
   #:score-time
   #:scores
   #:scores-name
   #:scores-file
   #:scores-version
   #:scores-magic
   #:scores-score-class
   #:scores-scores
   #:make-scores
   #:read-scores
   #:save-score
   #:read-score-version
   #:write-score-version
   ))
(in-package :scores)

(defclass score ()
  ((n
    :initarg :n :accessor score-n  
    :documentation "The score itself. Usually a number.")
   (name
    :initarg :name :accessor score-name
    :documentation "Name of who got the score.")
   (time
    :initarg :time :accessor score-time
    :documentation "Time the score occured. Usually a universal time."))
  (:default-initargs
   :name (nos:user-name)
   :time (get-universal-time))
  (:documentation "A generic particular score."))

(defmethod print-object ((object score) stream)
  "Print a score to STREAM."
  (with-slots (n name time) object
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~a ~a ~a" n name
	      (format-date "~a-~a-~a" (:year :month :date) :time time)))))

(defclass scores ()
  ((name
    :initarg :name :accessor scores-name :initform nil :type (or null string)
    :documentation "The name of the game.")
   (file
    :initarg :file :accessor scores-file
    :initform nil :type (or null string pathname output-stream)
    :documentation "The file name where the scores are saved.")
   (version
    :initarg :version :accessor scores-version :initform 0 :type integer
    :documentation "A number that defines the format of the scores file. ~
                    This shousl be incremented for every incompatible change.")
   (magic
    :initarg :magic :accessor scores-magic :initform nil
    :documentation "Magic number for identifying the file type.")
   (score-class
    :initarg :score-class :accessor scores-score-class
    :initform nil :type symbol
    :documentation "The class of the individual scores.")
   (scores
    :initarg :scores :accessor scores-scores :initform nil :type list
    :documentation "The actual scores."))
  (:documentation "A generic place to save the scores."))

(defmethod initialize-instance
    :after ((o scores) &rest initargs &key &allow-other-keys)
  "Initialize a scores."
  (declare (ignore initargs))
  (with-slots (name file version magic) o
    (when (not name)
      (error "The name of the scores should be set."))
    (when (not file)
      (setf file (nos:path-append (nos:data-dir name) "scores")))
    (when (< version 1)
      (error "You should probably set the scores version to a positive number."))
    (when (not magic)
      (error "You should probably set a magic number for the scores file."))))

(defun make-scores (class score-class &rest initargs)
  (apply #'make-instance class :score-class score-class initargs))

(defgeneric read-score-version (s score-class stream)
  (:documentation "Read the scores of a particular type.")
  (:method ((s scores) c stream)
    "Default method which just recreates the object from plist of the slots."
    (with-slots (scores score-class) s
      (assert (eq (find-class score-class) c))
      (setf scores nil)
      (loop :with s = nil
	 :while (setf s (read stream nil nil))
	 :do (push (apply #'make-instance score-class s) scores)))))

(defgeneric write-score-version (scores score-class stream)
  (:documentation "Write the scores of particular type.")
  (:method ((s scores) c stream)
    "Default method which just writes a plist for recreating the object."
    (with-slots (scores score-class magic version) s
      (assert (eq (find-class score-class) c))
      (format stream "~a ~w~%" magic version)
      (loop :for s :in scores
	 :do
	   (format stream "~w~%"
		   (loop
		      :for slot :in (mop:class-slots (class-of s))
		      :collect (first (mop:slot-definition-initargs slot))
		      :collect (slot-value
				s (mop:slot-definition-name slot))))))))

(defgeneric read-scores (scores)
  (:documentation "Read the scores.")
  (:method ((s scores))
    (with-slots (file version magic scores score-class) s
      (with-open-file (stream file :direction :input :if-does-not-exist nil)
	(when stream
	  (let ((*read-eval* nil)
		(magic-in (make-string (length magic)))
		version-in)
	    (when (or (/= (read-sequence magic-in stream) (length magic))
		      (string/= magic-in magic))
	      (error "Bad magic number in score file."))
	    (setf version-in (read stream))
	    (when (/= version-in version)
	      (error "The score version is too new?"))
	    (read-score-version s (find-class score-class) stream)))))))

(defgeneric save-score (scores score)
  (:documentation "Save the scores.")
   (:method ((s scores) (score-to-save score))
    (with-slots (scores name file) s
      (when (not (nos:file-exists (nos:data-dir name)))
	(nos:make-directory (nos:data-dir name)))
      (with-locked-file (file)
	(read-scores s)
	(push score-to-save scores)
	(with-open-file (stream file :direction :output
				:if-exists :overwrite
				:if-does-not-exist :create)
	  (write-score-version s (class-of score-to-save) stream))))))

;; EOF
