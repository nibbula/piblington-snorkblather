;;;								-*- Lisp -*-
;;; robots.asd -- System definition for robots
;;;

(defsystem robots
    :name               "robots"
    :description        "dumb old robots games"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Robots try to get you. You make them crash into each other.
Mostly just to test your tmux window."
    :depends-on (:dlib :opsys :char-util :fui :terminal :scores)
    :components
    ((:file "robots")))
