; -*- mode:common-lisp -*-

(defsystem irc-message-parser
  :depends-on ("esrap")
  :components ((:file "packages")
               (:file "main")))
