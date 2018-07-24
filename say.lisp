;;
;; say.lisp - Have critters say stuff.
;;

(defpackage :say
  (:documentation "Have critters say stuff.")
  (:use :cl :dlib :dlib-misc :char-util :glob :opsys :ppcre :grout :fatchar)
  (:export
   #:steal-critters
   #:say
   ))
(in-package :say)

(defparameter *cow-dir* "/usr/share/cowsay/cows"
  "Directory where the cow files live.")

(defun steal-critters (&key (dir *cow-dir*)
			 (output-file "say-critters.lisp"))
  "Steal the critters from the cowsay program."
  (with-open-file (out output-file
		       :direction :output
		       :if-exists :supersede)
    (let (name-list)
      (write-line "(in-package :say)" out)
      (loop :for file :in (glob (s+ dir "/*.cow"))
	 :do
	 (with-open-file (in file)
	   (format out ";; ~a~%" file)
	   (loop :with line :and in-cow :and eoc = "EOC"
	      :and b :and e :and bs :and be
	      :while (setf line (read-line in nil))
	      :do
	      (cond
		((and (begins-with "#" line) (not in-cow))
		 (write-line (substitute #\; #\# line) out))
		((multiple-value-setq (b e bs be)
		   (ppcre:scan "\\$the_cow[ ]+=[ ]+<<[\"]*(\\w+)[\"]*" line))
		 (setf eoc (subseq line (aref bs 0) (aref be 0)))
		 (let ((name (remove-suffix (path-file-name file) ".cow")))
		   (format out "(defparameter *~a*~%\"" name)
		   (push (keywordify name) name-list))
		 (setf in-cow t))
		((string= line eoc)
		 (write-line "\")" out)
		 (setf in-cow nil))
		(t
		 (if in-cow
		     (write-line (replace-subseq "\"" "\\\"" line) out)
		     (format out ";; ~a~%" line)))))))
      (format out "(setf *critters* '~s)~%" name-list))))

(defparameter *critters* nil
  "The critters. Should be set in 'say-critters.lisp'.")

(defparameter *cow-eyes*
  '((:normal   . "oo")
    (:borg     . "==")
    (:dead     . "xx")
    (:greedy   . "$$")
    (:paranoid . "@@")
    (:stoned   . "**")
    (:tired    . "--")
    (:wired    . "OO")
    (:young    . ".."))
  "Various cow eyes.")

(defparameter *tongues*
  '((:normal   . "  ")
    (:stoned   . "U ")
    (:dead     . "U ")
    (:vampire  . "VV")))

(defparameter *borders*
  '((:normal  . #("\\" "/" "\\" "\\" "/" "|" "|" "_" "-" "<" ">"))
    (:thought . #("o" "(" ")" "(" ")" "(" ")" "_" "-" "(" ")"))
    (:unicode . #("╲" "╱" "╲" "╲" "╱" "▏" "▕" "▁" "▔"))))

(defun justify (text width)
  (dlib-misc::justify-text
   (with-output-to-string (str)
     (let ((new-text (untabify text)) (i 0))
       (loop :with len = (1- (length new-text)) :and next-char
	  :while (< i len) :do
	  (cond
	    ((char= (aref new-text i) #\newline)
	     (setf next-char (aref new-text (1+ i)))
	     (case next-char
	       ((#\space #\newline #\tab)
		(write-char #\newline str)
		(incf i)
		(write-char next-char str))
	       (otherwise
		(write-char #\space str))))
	    (t (write-char (aref new-text i) str)))
	  (incf i))
       (write-char (aref new-text i) str)))
   :cols width :stream nil))

(defun get-borders (border)
  (cdr (or (assoc border *borders*) (assoc :normal *borders*))))

(defun thoughts (border)
  (aref (get-borders border) 0))

(defun balloon (text width border no-wrap)
  "Output the ballon with TEXT word justified to WIDTH inside BORDER."
  (let* ((lines (split-sequence #\newline
				(if no-wrap text (justify text width))
				:omit-empty t))
	 (processed-lines (mapcar #'process-ansi-colors
				  (mapcar #'make-fatchar-string lines)))
	 (max-width (reduce #'max processed-lines :key #'display-length))
	 (ba (get-borders border))
	 ;;(thoughts     (aref ba 0))
	 (top-left     (aref ba 1))
	 (top-right    (aref ba 2))
	 (bottom-left  (aref ba 3))
	 (bottom-right (aref ba 4))
	 (left	       (aref ba 5))
	 (right	       (aref ba 6))
	 (top	       (aref ba 7))
	 (bottom       (aref ba 8))
	 (left-1       (aref ba 9))
	 (right-1      (aref ba 10)))
    (flet ((derp-line (char)
	     (write-char #\space)
	     (dotimes (x (+ max-width 1)) (write-string char))
	     (terpri)))
      ;; top line
      (derp-line top)
      ;; text lines
      (if (= 1 (length lines))
	  (format t "~a ~a ~a~%" left-1 (first lines) right-1)
	  (loop :with last = (1- (length lines))
	     :for line :in lines :and i = 0 :then (1+ i)
	     :do
	     (cond
	       ((= i 0)
		(format t "~a ~va ~a~%" top-left max-width line top-right))
	       ((= i last)
		(format t "~a ~va ~a~%" bottom-left max-width line bottom-right))
	       (t
		(format t "~a ~va ~a~%" left max-width line right)))))
		;;(format t "~a ~a.~a~%" left line right)))))
      ;; bottom line
      (derp-line bottom))))

(defparameter *identifier-chars* "_{}")

(defun critter (template vars)
  "Output a critter in TEMPLATE, replacing variables from the property list
VARS. Variables look like: $var or ${var}."
  (let ((i 0))
    (flet ((read-var ()
	     "Return a variable name or NIL."
	     (let ((end (position-if
			 (_ (not (or (alphanumericp _)
				     (position _ *identifier-chars*))))
			 template :start (1+ i)))
		   result)
	       (when (and end (not (zerop end)))
		 (setf result
		       (symbolify
			(remove-if (_ (or (char= #\{ _) (char= #\} _)))
				   (subseq template (1+ i) end))
			:package :say
			)
		       )
		 ;;(format t "var ~s~%" result)
		 )
	       (setf i (1- end))
	       result)))
      (loop :with var :and c :and value
	 :while (< i (length template)) :do
	 (setf c (aref template i))
	 (case c
	   (#\$
	    (setf var (read-var)
		  value (cdr (assoc var vars :test #'equal)))
	    ;;(format t "var ~w ~w = ~w~%" var (type-of var) value)
	    (when (and var value)
	      (write-string value)))
	   (otherwise
	    (write-char c)))
	 (incf i)))))

(defun ensure-stolen-critters (critter-path)
  (when (not (file-exists (quote-filename critter-path)))
    (format t "Stealing critters from ~s...~%" *cow-dir*)
    (finish-output)
    (steal-critters :output-file critter-path)
    (format t "Done.~%")))

(defun critters ()
  (or *critters*
      (let ((critter-path (path-append
			   (asdf/system:system-source-directory
			    (asdf/system:find-system :say))
			   "say-critters.lisp")))
	(ensure-stolen-critters critter-path)
	(format t "Loading critters from ~s...~%" critter-path)
	(finish-output)
	(load critter-path)
	(format t "Done.~%"))))

(defun say (text &key (critter :default) (eyes :normal) (border :normal)
		   tongue (width 40) no-wrap)
  "Have a critter say something."
  (critters)
  (let* ((template (symbol-value
		    (symbolify (s+ "*" critter "*") :package :say)))
	 (eyes-string (cdr (or (assoc eyes *cow-eyes*)
			       (assoc :normal *cow-eyes*))))
	 (tongue-name (or tongue
			  (if (member eyes '(:stoned :dead)) eyes :normal)))
	 (tongue-string (cdr (or (assoc tongue-name *tongues*)
				 (assoc :normal *tongues*)))))
    (balloon text width border no-wrap)
    (critter template `((eyes     . ,eyes-string)
			(tongue   . ,tongue-string)
			(thoughts . ,(thoughts border))))
    (values)))

#+lish
(lish:defcommand say
  ((text string :optional t
    :help "Text for the critter to say.")
   (critter choice :short-arg #\c :default :default :choice-func critters
    :help "Who shall be your spokes-critter?")
   (eyes choice :short-arg #\e :default :normal
    :choices (mapcar #'car *cow-eyes*)
    :help "What to see with.")
   (border choice :short-arg #\b :default :default
    :choices (mapcar #'car *borders*)
    :help "Bubble which the text is in.")
   (tongue choice :short-arg #\t :choices (mapcar #'car *tongues)
    :help "The licky part.")
   (width integer :short-arg #\w :default 40
    :help "The width of the text in the bubble.")
   (no-wrap boolean :short-arg #\n
    :help "True not to wrap the text in the balloon. Useful for preformatted
text, from something like figet."))
  "Have critters say stuff."
  (when (not text)
    (setf text (slurp *standard-input*)))
  (say text
       :critter critter
       :eyes eyes
       :border border
       :tongue tongue
       :width width
       :no-wrap no-wrap))

;; EOF
