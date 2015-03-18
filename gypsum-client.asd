;;;; gypsum-client.asd

(asdf:defsystem #:gypsum-client
    :description "Client-side implementation of the Gypsum protocol."
    :author "June Tate-Gans <june@theonelab.com>"
    :license "Commercial"
    :serial t
    :depends (:alexandria
              :binary-types
              :evdev
              :log4cl)
    :components ((:file "package")
                 (:file "client")
                 (:file "event")
                 (:file "handler")
                 (:file "rect")
                 (:file "view")
                 (:file "windowmanager")))
