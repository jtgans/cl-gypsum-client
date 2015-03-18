(in-package :gypsum-client)

(defclass window ()
  ((title      :initarg :title  :accessor window-title)
   (x          :initarg :x      :accessor window-x)
   (y          :initarg :y      :accessor window-y)
   (width      :initarg :width  :accessor window-width)
   (height     :initarg :height :accessor window-height)
   (frame      :initarg :frame  :accessor window-frame)
   (fullscreen :initform nil    :accessor window-fullscreen)
   (marked     :initform nil    :accessor window-marked)))

(defmethod print-object ((object window) stream)
  (format stream "#<~a :TITLE ~s>" (type-of object) (window-title object)))

;;; Window API

(defgeneric handle-event (window event)
  (:documentation "General event handler entry point."))
(defgeneric handle-draw-event (window)
  (:documentation "Event handler for draw events."))
(defgeneric handle-assign-event (window frame)
  (:documentation "Event handler for frame assignment events (attached to a frame)."))
(defgeneric handle-keyboard-event (window state key)
  (:documentation "Event handler for keyboard input events."))

(defgeneric draw-window (window)
  (:documentation "Draws the window to the screen."))

;; General event handling and window drawing routines.

(define-condition unknown-event (error)
  ((event :initarg :event
          :reader unknown-event-event))
  (:report (lambda (condition stream)
             (format stream "Unhandled event type ~A." (unknown-event-event condition)))))

(defmethod handle-event (window event)
  "Standard event handling dispatch method."
  (cond ((eq (event-type event) :draw) (handle-draw-event window))
        ((eq (event-type event) :assign) (handle-assign-event window))
        ((eq (event-type event) :keyboard) (handle-keyboard-event window event))
        (t (error (make-condition 'unknown-event :event event)))))

(defmethod handle-draw-event (window)
  "Event handler for draw events. Doesn't do anything at all.")

(defmethod handle-assign-event (window frame)
  "Event handler for frame assignment events (when the window is attached to a frame).")

(defmethod handle-keyboard-event (window event)
  "Event handler for keyboard input events. Does nothing.")
