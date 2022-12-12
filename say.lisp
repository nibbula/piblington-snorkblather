;;;
;;; say.lisp - Have critters say stuff.
;;;

(defpackage :say
  (:documentation "Have critters say stuff.")
  (:use :cl :dlib :dlib-misc :char-util :glob :opsys :ppcre :grout :fatchar
	:terminal)
  (:export
   #:steal-critters
   #:critters
   #:ponies
   #:all-critters
   #:say
   #:!say
   #:*cow-dir*
   #:*pony-dirs*
   ))
(in-package :say)

(defparameter *cow-dir* "/usr/share/cowsay/cows"
  "Directory where the cow files live.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *system-pony-dirs*
    '("/usr/share/ponysay/ponies"
      "/usr/share/ponysay/extraponies")))

(defparameter *pony-dirs*
  `(,@(or (maybe-refer-to :cl-user '#:*user-pony-dirs*)
	  (maybe-refer-to :lish-user '#:*user-pony-dirs*))
    ,(nos:data-dir "ponysay")
    ,@*system-pony-dirs*)
  "List of directories where the pony files live.")

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

(eval-when (:compile-toplevel :load-toplevel :execute)
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
    '((:normal .
       #("\\" "/"  "\\" "\\" "/"  "|"  "|"  "_"  "-"  "<"  ">"  "/"))
      (:thought .
       #("o"  "("  ")"  "("  ")"  "("  ")"  "_"  "-"  "("  ")"  "o"))
      (:unicode .
       #("╲" "╱" "╲" "╲" "╱" "▏" "▕" "▁" "▔" "(" ")" "╱"))
      (:round .
       #("╲" "╭" "╮" "╰" "╯" "▏" "▕" "▁" "▔" "(" ")" "╱"))
      )))

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

(defun thoughts (border &key (type :left))
  (aref (get-borders border) (if (eq type :left) 0 11)))

(defun balloon (text width border no-wrap)
  "Output the ballon with TEXT word justified to WIDTH inside BORDER."
  (let* ((lines (split-sequence #\newline
				(if no-wrap
				    (untabify text)
				    (justify text width))
				:omit-empty t))
	 (processed-lines (->> lines
			       (mapcar #'make-fatchar-string)
			       (mapcar #'process-ansi-colors)
			       (mapcar (_ (make-fat-string :string _)))))
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
	 (right-1      (aref ba 10))
	 (*print-escape* nil))
    (dbugf :say "max-width = ~s~%" max-width)
    (flet ((derp-line (char)
	     (tt-write-char #\space)
	     (dotimes (x (+ max-width 2)) (tt-write-string char))
	     ;;(terpri)
	     (tt-write-char #\newline)
	     ))
      (write-string
       (with-terminal-output-to-string (:ansi)
	 ;; top line
	 (derp-line top)
	 ;; text lines
	 (if (= 1 (length lines))
	     (tt-format "~a ~a ~a~%" left-1 (first lines) right-1)
	     (loop :with last = (1- (length lines))
		:for line :in processed-lines :and i = 0 :then (1+ i)
		:do
		(cond
		  ((= i 0)
		   (format *terminal* "~a ~v/fatchar-io:print-string/ ~a~%"
			   top-left max-width line top-right)
		   ;;(dbugf :say "line = ~s ~s~%" (type-of line) line)
		   ;;(if-dbugf (:say) (symbol-call :lish-user :fatty line))
		   )
		  ((= i last)
		   (format *terminal* "~a ~v/fatchar-io:print-string/ ~a~%"
			      bottom-left max-width line bottom-right))
		  (t
		   ;;(if-dbugf (:say) (fatty line))
		   (format *terminal* "~a ~v/fatchar-io:print-string/ ~a~%"
			   left max-width line right)))))
	 ;; bottom line
	 (derp-line bottom))))))

(defparameter *identifier-chars* "_{")

(defun critter (template vars &key pony)
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
			:package :say))
		 (dbugf :say "var ~s~%" result))
	       (setf i (if (or pony (char= (char template end) #\}))
			   end
			   (1- end)))
	       result)))
      (loop :with var :and c :and value :and var-start
	 :while (< i (length template)) :do
	 (setf c (aref template i))
	 (case c
	   (#\$
	    (setf var-start i
		  var (read-var)
		  value (cdr (assoc var vars :test #'equal)))
	    ;;(format t "var ~w ~w = ~w~%" var (type-of var) value)
	    (if (and var value)
		(write-string value)
		;; Replace the variable with blanks.
		(progn
		  (dotimes (i (1+ (- i var-start))) (write-char #\space))
		  (dbugf :say "wrote ~s blanks~%" (1+ (- i var-start)))
		  )))
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

(defun critterp (thing)
  (find (keywordify thing) *critters*))

(defparameter *ponies* nil
  "List of pony files.")

(defvar *ponyless* nil
  "Omit the poor little ponies!")

(defun ponies ()
  ;; @@@ ponyless doesn't work when we're called in the command arg parsing!
  ;; (format t "*ponyless* = ~s~%" *ponyless*)
  ;; (break)
  (or *ponies*
      (setf *ponies*
	    (when (not *ponyless*)
	      (loop :for dir :in *pony-dirs*
		 :when (nos:probe-directory dir)
		 :nconc
		 (mapcar (_ (nos:path-file-name (nos:path-snip-ext _)))
			 (glob (nos:path-append dir "*.pony"))))))))

(defun ponyp (thing)
  (find (string thing) *ponies* :test #'equalp))

(defstruct pony
  name
  attributes
  template)

(defun pony-attr (pony attr)
  (gethash attr (pony-attributes pony)))

(defun load-pony (pony)
  (let (result)
    (loop :with file
       :for dir :in *pony-dirs* :do
       (when (probe-file (setf file (nos:path-append dir (s+ pony ".pony"))))
	 (with-open-file (stream file :direction :input)
	   (when (not (begins-with "$$$" (read-line stream)))
	     (error "I don't think this a pony file!"))
	   (setf result (make-pony :name pony
				   :attributes (make-hash-table :test #'equalp)))
	   (loop :with line
	      :while (not (begins-with "$$$" (setf line (read-line stream))))
	      :do
	      (multiple-value-bind (match pieces)
		  (scan-to-strings "\\s*^([A-Z ]+)\\s*:\\s*(.*)$" line)
		(if (not match)
		    (when (not (all-matches "^\\s*$" line)) ; blank line
		      ;; (warn "Malformed meta-data line: ~s" line)
		      )
		    (let ((attr (gethash (aref pieces 0)
					 (pony-attributes result))))
		      (setf (gethash (aref pieces 0)
				     (pony-attributes result))
			    (if attr
				(if (consp attr)
				    (push (aref pieces 1) attr)
				    (aref pieces 1))
				(aref pieces 1)))))))
	   (setf (pony-template result) (slurp stream)))
	 (return)))
    result))

(defparameter *all-critters* nil
  "All the critters we can find.")

(defun all-critters ()
  (or *all-critters*
      (append (critters) (list :random) (ponies))))

(defun get-template (thing)
  (cond
    ((critterp thing)
     (symbol-value (symbolify (s+ "*" thing "*") :package :say)))
    ((ponyp thing)
     (let ((pony (load-pony thing)))
       (or (and pony
		(pony-template pony))
	   (error "I can't find ~s! Try setting *pony-dirs*." thing))))))

(defun list-critters ()
  (critters)
  (format t "Critters:~%")
  (print-columns *critters* :columns (tt-width) :smush t)
  (terpri)
  (ponies)
  (when *ponies*
    (format t "Ponies:~%")
    (print-columns *ponies* :columns (tt-width) :smush t))
  `(,@*critters* ,@*ponies*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun default-border ()
    (or (maybe-refer-to :cl-user '#:*say-default-border*)
	(maybe-refer-to :lish-user '#:*say-default-border*)
	:normal)))

(defun say (text &key (critter :default) (eyes :normal) (border (default-border))
		   tongue (width 40) no-wrap ponyless)
  "Have a critter say something."
  (let ((*ponyless* ponyless))
    (critters)
    (when (eq (keywordify critter) :random)
      (let* ((all (all-critters))
	     (rr (elt all (random (length all)))))
	(setf critter rr)))
    (setf eyes (keywordify eyes)
	  tongue (keywordify tongue)
	  border (keywordify border))
    (let* ((template (get-template critter))
	   (eyes-string (cdr (or (assoc eyes *cow-eyes*)
				 (assoc :normal *cow-eyes*))))
	   (tongue-name (or tongue
			    (if (member eyes '(:stoned :dead)) eyes :normal)))
	   (tongue-string (cdr (or (assoc tongue-name *tongues*)
				   (assoc :normal *tongues*)))))
      (balloon text width border no-wrap)
      (cond
	((critterp critter)
	 (critter template `((eyes     . ,eyes-string)
			     (tongue   . ,tongue-string)
			     (thoughts . ,(thoughts border)))))
	((ponyp critter)
	 (let ((*identifier-chars* "/\\"))
	   (critter template `((|\\|    . ,(thoughts border :type :left))
			       (|/|     . ,(thoughts border :type :right))
			       (||      . "$")
			       ;;(|ballon| . ,(thoughts border))
			       )
		    :pony t))))
      (values))))

#+lish
(lish:defcommand say
  ((text string :optional t
    :help "Text for the critter to say.")
   (ponyless boolean :short-arg #\p :default nil
    :help "Nix the ponies! How could you do such a thing??")
   (critter choice :short-arg #\c :default :default :choice-func 'all-critters
    :help "Who shall be your spokes-critter?")
   (list boolean :short-arg #\l :help "Show a list of critters.")
   (eyes choice :short-arg #\e :default :normal
    :choices (mapcar (_ (string-downcase (car _))) *cow-eyes*)
    :help "What to see with.")
   (border choice :short-arg #\b :default (default-border)
    :choices (mapcar (_ (string-downcase (car _))) *borders*)
    :help "Bubble which the text is in.")
   (tongue choice :short-arg #\t
    :choices (mapcar (_ (string-downcase (car _))) *tongues*)
    :help "The licky part.")
   (width integer :short-arg #\w :default 40
    :help "The width of the text in the bubble.")
   (no-wrap boolean :short-arg #\n
    :help "True not to wrap the text in the balloon. Useful for preformatted
text, from something like figet.")
   (help boolean :short-arg #\? :help "Show this help."))
  :accepts (:grotty-stream :stream string)
  "Have critters say stuff."
  (when (and (not text) (not list) (not help))
    (setf text (or (and (typep lish:*input* '(or stream string fat-string))
			lish:*input*)
		   (slurp *standard-input*))))
  (cond
    (help (lish:print-command-help (lish:command "say")))
    (list
     (let ((*ponyless* ponyless))
       (setf lish:*output* (list-critters))))
    (t
     (say text
	  :critter critter
	  :eyes eyes
	  :border (keywordify border)
	  :tongue tongue
	  :width width
	  :no-wrap no-wrap
	  :ponyless ponyless))))

;; @@@ This foolishly uses things beyond the scope of this palinode.
(lish:defcommand browse-critters ()
  (with-terminal ()
    (critters)
    (ponies)
    (let ((crits (coerce `(,@*critters* ,@*ponies*) 'vector)))
      (loop :with c :and i = 40 :and name
	 :while (not (equal c #\q))
	 :do (tt-clear) (tt-finish-output)
	   (setf name (string-downcase (string (aref crits i))))
	   (lish:! "ff :standard \"" name "\" | lolcat | say -nc " name)
	   (setf c (tt-get-key))
	   (case c
	     (#\q (return nil))
	     (#\n (incf i))
	     (#\p (decf i)))))))

;; EOF
