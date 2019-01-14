;;;								-*- Lisp -*-
;;; scores.asd - System definition for scores
;;;

(defsystem scores
    :name               "scores"
    :description        "Generic games scores."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Generic games scores."
    :depends-on (:dlib :opsys :dlib-misc)
    :components
    ((:file "scores")))
