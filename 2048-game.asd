;;;								-*- Lisp -*-
;;; 2048-game.asd - System definition for 2048
;;;

(defsystem 2048-game
    :name               "2048-game"
    :description        "2048 Game"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "2048 Game"
    :depends-on (:dlib :inator :terminal-inator :keymap :scores :terminal-utils
		 :table :table-print :fui :dtime :terminal-table)
    :components
    ((:file "2048-game")))
