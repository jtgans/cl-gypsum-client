;;;; package.lisp

(defpackage #:gypsum-client
  (:use #:cl #:sb-bsd-sockets #:sb-thread)
  (:documentation "Gypsum protocol drawing primitives and client."))
