(in-package #:irc-message-parser)

;;;; IRC message parser


;;;; A few utilities used when parsing.

(defun flatten-text (form)
  "Convert a parse tree into text"
  (with-output-to-string (s)
    (labels ((recure (x)
               (etypecase x
                 (character (princ x s))
                 (string (princ x s))
                 (atom)
                 (list (map nil #'recure x)))))
      (recure form))))

(defun make-keyword (text)
  (intern (string-upcase text) (symbol-package :keyword)))

(defparameter *space-saver* (make-hash-table :test #'equal))

(defun intern-form (form)
  (or (gethash form *space-saver*)
      (setf (gethash form *space-saver*) form)))




;;;; Parse IRC messages

;;; Based on grammar in http://tools.ietf.org/html/rfc1459.html#section-2.3.1

;; see TBD

(defrule <message> (and (? (and #\: <prefix> <SPACE>)) <command> <params> <crlf>)
  (:destructure (user? command params z)
                (declare (ignore z))
                (let* ((z (nreverse params))
                       (msg (pop z))
                       (info (nreverse z)))
                  `(,command :from ,(second user?) :info ,info :msg ,msg))))

(defrule <prefix>  (or (and <nick>
                            (? (and #\! <user>)) 
                            (? (and #\@ <host> )))
                       <servername>)
  (:lambda (x)
    (typecase x
      (string `(:host x))
      (list
       (intern-form
       `(:nick ,(first x) :user ,(second (second x)) :host ,(second (third x))))))))

(defrule <command> (or (+ <letter>)
                       (and <number> <number> <number>))
  (:lambda (x) 
    (if (digit-char-p(first x))
        (parse-integer (flatten-text x))
        (make-keyword (flatten-text x)))))
                   

(defrule <space> (+ #\space))

(defrule <params> (and <SPACE> (? (or (and #\: <trailing>)
                                      (and <middle> <params>))))
  (:lambda (x)
    (let* ((meat (second x))
           (kind (first meat))
           (body (second meat)))
      (cond
        ((null kind) (break))
        ((string= ":" kind) `(,body))
        (t `(,kind ,@body))))))

(defrule <middle> (and (and (! (or #\: #\space #\null #\return #\newline)) character)
                       (* (and (! (or #\space #\null #\return #\newline)) character)))
  ;; i.e. <Any *non-empty* sequence of octets not including SPACE
  ;;       or NUL or CR or LF, the first of which may not be #\:>
  (:lambda (x) (flatten-text x)))

(defrule <trailing> (* (and (! (or #\null #\return #\newline)) character))
    ;; i.e. <Any, possibly *empty*, sequence of octets not including
    ;;       NUL or CR or LF>
  (:lambda (x) (flatten-text x)))

(defrule <crlf> (or (and #\return #\newline)
                    #\newline
                    #\return))

(defrule <target> (and <to> (? (and #\, <target>))))

(defrule <to> (or <channel> (and <user> #\@ <servername>) <nick> <mask>))

(defrule <channel> (and (or #\# #\&) <chstring>))

(defrule <servername> <host>)

(defrule <host> (+ (or <letter> <number> #\. #\-))  ; TBD
  (:lambda (x) (flatten-text x)))

(defrule <nick> (and <letter> (* (or <letter> <number> <special>)))
  (:lambda (x) (flatten-text x)))

(defrule <mask> (and (or #\# #\$) <chstring>))

(defrule <chstring> (+ (and (! (or #\space #\bell #\null #\return #\newline #\,)) character))
  ;; i.e. <any 8bit code except SPACE, BELL, NUL, CR, LF and comma (#\,)
  (:lambda (x) (flatten-text x)))

(defrule <user> (+ (or <letter> <number> <special> #\~))
  (:lambda (x) (flatten-text x)))

;(defrule <nonwhite> (! (or #\space #\tab #\newline #\return)))

(defrule <letter> (character-ranges (#\a #\z) (#\A #\Z)))

(defrule <number> (character-ranges (#\0 #\9)))

(defrule <special> (character-ranges  #\- #\[ #\] #\\ #\` #\^ #\{ #\}  #\~ #\.))





;;;; Parse the user's message exposing the MIRC formating.

;;; I have yet to find a spec for this markup.  So, this only does the
;;; cases I've encountered.

(defun parse-mirc-markup (text)
  (let ((pad (make-array '(0) :element-type 'base-char
                         :fill-pointer 0 :adjustable t))
        (result ()))
    (flet ((accum ()
             (unless (zerop (fill-pointer pad))
               (push (format nil "~A" pad) result)
               (setf (fill-pointer pad) 0))))
      (loop
         with colors = #(:white :black :blue :green
                         :red :brown :purple :orange 
                         :yellow :light :teal :light-cyan
                         :light-blue :pink :grey :light-grey)
         with len = (length text)
         for i from 0 below len
         as c = (char text i)
         do (case c
              (#\03
               (accum)
               (flet ((safe-char (j)
                        (and (< j len)
                             (char text j))))
                 (let ((d1? (safe-char (1+ i))))
                   (incf i)
                   (cond
                     ((and d1? (digit-char-p d1?))
                      (let ((n (digit-char-p d1?))
                            (d2? (safe-char (1+ i))))
                        (when (and d2? (digit-char-p d2?))
                          (incf i)
                          (setf n (+ (* 10 n) (digit-char-p d2?))))
                        (push (svref colors n) result)))
                     (t
                      (push :end-color result))))))
              (#\02
               (accum)
               (push :toggle-bold result))
              (#\037
               (accum)
               (push :toggle-underline result))
              ;; other escapes at TBD
              (otherwise (vector-push-extend c pad))))
      (accum)
      (nreverse result))))

#|

;;; Wikimedia provides an colorful IRC channel where every edit is
;;; reported, so let's get one.

(defun get-example-message (user-name)
  (block nil
    (let ((c (irc:connect :nickname user-name
                          :server "irc.wikimedia.org")))
      (unwind-protect
           (progn
             (irc:join c "#en.wikipedia")
             (irc:add-hook c 'irc:irc-privmsg-message
                           #'(lambda (msg)
                               (return (irc:raw-message-string msg))))
               (irc:read-message-loop c))
             (irc:quit c)))))

(defparameter *irc-user-name* (car (last (pathname-directory (user-homedir-pathname)))))

;; the following is slow and noisy
(defparameter *example-message* (get-example-message *irc-user-name*))

(parse-mirc-markup *example-message*)

|#
