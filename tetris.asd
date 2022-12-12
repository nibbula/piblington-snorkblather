;;;								-*- Lisp -*-
;;; tetris.asd - System definition for tetris
;;;

(defsystem tetris
    :name               "tetris"
    :description        "A game you may know."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Waste your time."
    :depends-on (:dlib :dlib-misc :dtime :terminal :inator :terminal-inator :fui
		 :fatchar :keymap :scores)
    :components
    ((:file "tetris")))
