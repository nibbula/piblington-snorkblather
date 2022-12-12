;;;								-*- Lisp -*-
;;; scores.asd - System definition for scores
;;;

(defsystem scores
    :name               "scores"
    :description        "Generic games scores."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Generic games scores."
    :depends-on (:dlib :opsys :dlib-misc)
    :components
    ((:file "scores")))
