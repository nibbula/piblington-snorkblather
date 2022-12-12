;;;								-*- Lisp -*-
;;; worms.asd -- System definition for WORMS package
;;;

(defsystem worms
    :name               "worms"
    :description        "The pure happiness of my worm."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula@yahoo.com>"
    :licence            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Now you too can have some new worms."
    :depends-on (:curses)
    :components
    ((:file "worms")))
