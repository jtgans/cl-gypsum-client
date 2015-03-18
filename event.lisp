(in-package :gypsum-client)

(defclass event ()
  ((timestamp
    :initform (get-universal-time)
    :initarg :time
    :type float
    :reader event-time))
  (:documentation "A class that contains various event information."))

(defclass draw-event (event)
  ()
  (:documentation "An event that represents a redraw request."))

(defclass assign-event (event)
  ((frame :initarg :frame
          :type frame
          :reader assign-event-frame))
  (:documentation "An event that represents a frame assignment request."))

(defclass keyboard-event (event)
  ((code :initarg :code
         :type integer
         :reader keyboard-event-code)
   (name :initarg :name
         :type string
         :reader keyboard-event-name)
   (glyph :initarg :glyph
          :type character
          :reader keyboard-event-character)
   (state :initarg :state
          :type symbol
          :reader keyboard-event-state))
  (:documentation "An event that represents a keyboard input event."))

(defclass ping-event (event)
  ()
  (:documentation "Simplistic ping event to notify of a working queue."))

(defclass quit-event (event)
  ()
  (:documentation "An event to shutdown a handler."))
