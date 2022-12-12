;;;								-*- Lisp -*-
;;; lolcat.asd - System definition for lolcat
;;;

(defsystem lolcat
    :name               "lolcat"
    :description        "The king of cats!"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Now I am indeed the king of cats!"
    :depends-on (:dlib :fatchar :terminal :dcolor :color-names)
    :components
    ((:file "lolcat")))
