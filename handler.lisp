(in-package :gypsum-client)

(defclass handler ()
  ((name
    :initform nil
    :type string
    :initarg :name
    :documentation "The name of this handler.")
   (event-thread
    :initform nil
    :type thread
    :initarg :event-thread
    :documentation "The thread used for event dispatch.")
   (event-waitqueue
    :initform nil
    :type waitqueue
    :documentation "Waitqueue for signaling when a new event has come in.")
   (queue-lock
    :initform nil
    :type mutex
    :documentation "Lock to control access to the event-queue.")
   (event-queue
    :initform nil
    :type list
    :documentation "A list containing incoming events to handle."))
  (:documentation "A handler is an event handling object that spawns a thread to
dispatch events placed on its queue. Events are sent to the handler via the
`post-event' generic method, and are executed via the `handle-event' generic
method."))

;;;; Interface

(defgeneric post-event (handler event)
  (:documentation "Posts EVENT to HANDLER's queue."))
(defgeneric handle-event (handler event)
  (:documentation "Handles an EVENT on HANDLER's queue from HANDLER's
event-thread context. The default method does nothing."))
(defgeneric start-handler (handler)
  (:documentation "Starts HANDLER's event loop if it is not running already."))

;;;; Default methods and functions

(defmethod print-object ((object handler) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name event-thread event-queue) object
      (format stream ":NAME ~a :EVENT-THREAD ~a :EVENT-QUEUE ~a"
              name event-thread event-queue))))

(defmethod post-event ((handler handler) (event event))
  (with-slots (event-waitqueue queue-lock event-queue) handler
    (with-mutex (queue-lock)
      (setf event-queue (nconc (list event) event-queue))
      (condition-notify event-waitqueue))))

(defmethod handle-event ((handler handler) (event event))
  (log:warn "Unhandled event for ~a: ~a" handler event)
  (values))

(defmethod handle-event ((handler handler) (event ping-event))
  (log:info "PONG from ~a" handler)
  (values))

(defmethod handle-event ((handler handler) (event quit-event))
  (log:info "Quit received -- terminating thread.")
  (terminate-thread (slot-value handler 'event-thread))
  (values))

(defmethod initialize-instance :after ((handler handler) &key name)
  (with-slots (event-thread event-waitqueue queue-lock) handler
    (setf event-waitqueue (make-waitqueue :name (concatenate 'string name "-waitqueue"))
          queue-lock (make-mutex :name (concatenate 'string name "-queuemutex")))))

(defmethod start-handler ((handler handler))
  (with-slots (event-thread name) handler
    (when (or (null event-thread) (not (thread-alive-p event-thread))))
      (setf event-thread (make-thread #'(lambda () (handler-runloop handler))
                                      :name name))
      (log:info "Event loop started for handler ~a" handler)))

(defun handler-runloop (handler)
  "Handles each event on the event queue by popping the end of calling `handle-event' on each event
in the queue."
  (with-slots (event-waitqueue queue-lock event-queue) handler
      (with-mutex (queue-lock)
        (loop
           (loop
              (unless event-queue (return))
              (let ((event (first (last event-queue))))
                (setf event-queue (nbutlast event-queue))
                (handle-event handler event)))
           (condition-wait event-waitqueue queue-lock)))))
