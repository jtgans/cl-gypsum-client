;;;; gypsum-client.asd

(asdf:defsystem #:cl-gypsum-client
    :description "Client-side implementation of the Gypsum protocol."
    :author "June Tate-Gans <june@theonelab.com>"
    :license "Simplified BSD License"
    :serial t
    :depends-on (#:alexandria
                 #:binary-types
                 #:cl-evdev
                 #:log4cl)
    :components ((:file "package")
                 (:file "client")
                 (:file "event")
                 (:file "handler")
                 (:file "rect")
                 (:file "view")
                 (:file "windowmanager")))
