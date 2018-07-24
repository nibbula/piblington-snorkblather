;;;								-*- Lisp -*-
;;; say.asd - System definition for say
;;;

(defsystem say
    :name               "say"
    :description        "Have a critter say something."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Creatures made of letter art sometimes may want to say various things to
us. This little module enables them to emit words, formed of the very letters
they are made of, even if they are only a filter for the thoughts of others,
as are we, and yet perhaps they can aspire to be, however sidgeon-like a peice
thereof, a conduit for the universal thought."
    :depends-on (:dlib :dlib-misc :char-util :glob :opsys :cl-ppcre :grout
		 :fatchar)
    :components
    ((:file "say")))
