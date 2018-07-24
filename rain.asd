;;;								-*- Lisp -*-
;;; rain.asd -- System definition for rain
;;;

(defsystem rain
    :name               "rain"
    :description        "Rain on the screen"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "This is like an old timey program."
    :depends-on (:dlib :terminal)
    :components
    ((:file "rain")))
