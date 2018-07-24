;;;								-*- Lisp -*-
;;; lolcat.asd - System definition for lolcat
;;;

(defsystem lolcat
    :name               "lolcat"
    :description        "The king of cats!"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Now I am indeed the king of cats!"
    :depends-on (:dlib :fatchar :terminal :color :color-names)
    :components
    ((:file "lolcat")))
