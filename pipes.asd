;;;								-*- Lisp -*-
;;; pipes.asd - System definition for pipes
;;;

(defsystem pipes
    :name               "pipes"
    :description        "A text imitation of the classic pipe screensaver."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "A text imitation of the classic pipe screensaver."
    :depends-on (:dlib :char-util :terminal #+unix :terminal-ansi)
    :components
    ((:file "pipes")))
