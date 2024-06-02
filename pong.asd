;;;								-*- Lisp -*-
;;; pong.asd - System definition for pong
;;;

(defsystem pong
  :name               "pong"
  :description        "Poorly Optimized Nerd Game"
  :version            "0.1.0"
  :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
  :license            "GPL-3.0-only"
  :source-control     :git
  :long-description   "Poorly Optimized Nerd Game"
  :depends-on (:dlib :char-util :terminal :fui)
  :components
  ((:file "pong")))
