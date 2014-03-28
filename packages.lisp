(in-package #:common-lisp)

(defpackage #:irc-message-parser
  (:use #:common-lisp #:esrap)
  (:export #:parse-irc-message
           #:parse-mirc-markup))
