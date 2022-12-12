;;;								-*- Lisp -*-
;;; rain.asd -- System definition for rain
;;;

(defsystem rain
    :name               "rain"
    :description        "Rain on the screen"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "This is like an old timey program."
    :depends-on (:dlib :terminal)
    :components
    ((:file "rain")))
