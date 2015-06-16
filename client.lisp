(in-package :cl-gypsum-client)

(export '(connect
          disconnect
          with-gypsum-context
          box
          text
          clip
          reset-clip
          commit))

(defvar *command-stream* nil
  "The stream connected to Gypsum where commands are sent.")

(defvar *tcp-socket* nil
  "The TCP socket that may be open if a previous call to `connect-via-tcp' or
`connect' was made using the :TCP method.")

(defvar *cached-params-hash-table* (make-hash-table)
  "The locally-cached parameters hash table. Used to deduplicate parameters for
the lifetime of a connection. Reset on `connect' or on `reset'.")

(defvar *write-lock* (make-mutex :name "Gypsum display write lock")
  "Write lock used to serialize calls to `with-gypsum-context'.")

(defun connect (method &key
                         (device "/dev/rfcomm")
                         (host "localhost")
                         (port 8888))
  "Connects to a Gypsum server. Method must be one of :TCP or :RFCOMM. If
:RFCOMM is specified, then the client will connect via BlueZ's rfcomm mechanisms
as done via `connect-via-rfcomm'. If :TCP is specified, then the client will
connect via TCP, as per `connect-via-tcp'.

Keyword flags defined are:
  :device
    The device to connect via RFCOMM with. Defaults to /dev/rfcomm0.

  :host
    The hostname to connect to via TCP. Defaults to localhost.

  :port
    The port to connect to via TCP. Defaults to 8888.

Returns the newly opened command stream."
  (clrhash *cached-params-hash-table*)
  (setf *command-stream*
        (cond ((eq method :tcp) (connect-via-tcp :host host :port port))
              ((eq method :rfcomm) (connect-via-rfcomm :device device))
              (t (error "Expected either :TCP or :RFCOMM as a connect method.")))))

(defun connect-via-rfcomm (&key (device "/dev/rfcomm0"))
  "Connects to a Gypsum server via a BlueZ RFCOMM device. If DEVICE is
unspecified, it defaults to /dev/rfcomm0. Returns the newly created command
stream."
  (open device :direction :io :if-exists :append))

(defun connect-via-tcp (&key (host "localhost") (port 8888))
  "Connects to a Gypsum server via TCP on the given HOST and PORT. If HOST or
PORT are unspecified, they default to localhost and 8888, respectively. Returns
the newly created command stream."
  (setf *tcp-socket* (make-instance 'inet-socket :type :stream :protocol :tcp))
  (socket-connect *tcp-socket*
                  (host-ent-address (get-host-by-name host))
                  port)
  (socket-make-stream *tcp-socket*
                      :input t
                      :output t
                      :auto-close t))

(defun disconnect ()
  "Closes and disconnects the Gypsum command stream and (if connected) TCP
socket."
  (when *command-stream*
    (close *command-stream*)
    (setf *command-stream* nil))
  (when *tcp-socket*
    (socket-close *tcp-socket*)
    (setf *tcp-socket* nil)))

(defun draw-it (stream &rest forms)
  "Sends a set of FORMS to the given Gypsum-connected STREAM for drawing."
  (format stream "~{~S~%~}" (deduplicate-params-in-body forms))
  (finish-output stream)
  (values))

(defun deduplicate-params-in-form (form)
  "Iterates through the a Gypsum parameter list of the form (:field value
:field2 value), removing fields with cached values."
  (loop while form nconcing
       (let* ((name (pop form))
              (arg  (pop form))
              (previous-value (gethash name *cached-params-hash-table*)))
         (cond ((equal previous-value arg) nil)
               (t (setf (gethash name *cached-params-hash-table*) arg)
                  `(,name ,arg))))))

(defun deduplicate-params-in-body (body)
  "Iterates through a list of Gypsum drawing primitives and commands, calling
`deduplicate-params-in-form' to remove cached params."
  (print (loop while body collecting
       (let* ((form (pop body))
              (command (pop form)))
         ;; Wipe out the hash table to prevent gobbling of prior values on
         ;; reset.
         (when (eq command 'reset)
           (clrhash *cached-params-hash-table*))
         `(,command ,@(deduplicate-params-in-form form))))))

(defmacro with-gypsum-context (&body body)
  "Converts and sends a BODY of Gypsum drawing primitives to the Gypsum command
stream via `draw-it'. Multiple concurrent calls to `with-gypsum-context' are
serialized via an encapsulated write lock to prevent contexts from overwriting
one another, so calls should be made as short as possible."
  `(with-mutex (*write-lock*)
     (draw-it *command-stream* ,@body)))

(defun box (&key start end (filled nil filled-supplied-p) color)
  "Generates a Gypsum drawing primitive to draw a box with the given parameters.
Valid parameters include:

  :start
     Coordinate. A cons cell containing the x and y coordinate of the upper left
     corner of the box.

  :end
     Coordinate. A cons cell containing the x and y coordinate of the lower
     right corner of the box.

  :filled
     Boolean. Determines if the box will be filled or just a rect.

  :color
     A color string. Either HTML-style #RRGGBB or #AARRGGBB, or a color name."
  `(box
    ,@(when start (list :start start))
    ,@(when end (list :end end))
    ,@(when filled-supplied-p (list :filled filled))
    ,@(when color (list :color color))))

(defun commit ()
  "Generates a Gypsum drawing primitive to commit the existing batched
primitives to the screen."
  '(commit))

(defun text (&key text start font size color (filled nil filled-supplied-p))
  "Generates a Gypsum drawing primitive to draw text with the given parameters.
Valid parameters include:

  :text
    String. The text to display.

  :start
    Coordinate. A cons cell containing the x and y coordinate of the lower left
    corner of the text box.

  :font
    String. The name of the font family to draw. Useful fonts include sans,
    serif, monospace, and envy_code_r.

  :size
    Integer. The size in scaled pixels of the font to draw.

  :color
    A color string. Either HTML-style #RRGGBB or #AARRGGBB, or a color name."
  `(text
    ,@(when text (list :text text))
    ,@(when start (list :start start))
    ,@(when font (list :font font))
    ,@(when size (list :size size))
    ,@(when color (list :color color))
    ,@(when filled-supplied-p (list :filled filled))))

(defun clip (&key start end)
  "Generates a Gypsum drawing primitive to set the clipping region with the
given parameters. Valid parameters include:

  :start
    Coordinate. A cons cell containing the x and y coordinate of the upper left
    corner of the clipping rectangle.

  :end
    Coordinate. A cons cell containing the x and y coordinates of the lower
    right corner of the clipping rectangle."
  `(clip
    ,@(when start (list :start start))
    ,@(when end (list :end end))))

(defun reset-clip ()
  "Generates a Gypsum drawing primitive to reset the clipping region."
  '(reset-clip))

(defun reset ()
  '(reset))

(defun draw-dialog (x y width height title)
  "Draws a dialog to Gypsum's display."
  (let ((end-x (+ x width))
        (end-y (+ y height))
        (titlebar-end (+ y 20))
        (text-x (+ x 2))
        (text-y (+ y 18)))
    (with-gypsum-context
      (box :start (cons x y)
           :end (cons end-x end-y)
           :filled t
           :color "white")
      (box :start (cons (+ x 1) (+ titlebar-end 1))
           :end (cons (- end-x 1) (- end-y 1))
           :filled t
           :color "black")
      (box :start (cons (- x 1) (- y 1))
           :end (cons (+ end-x 1) (+ end-y 1))
           :filled nil)
      (clip :start (cons x y)
            :end (cons end-x titlebar-end))
      (text :start (cons text-x text-y)
            :size 20
            :text title
            :font "envy_code_r"
            :filled t)
      (reset-clip)
      (commit))))

(defun dialog-test ()
  (loop
     (let ((x (random 500)) (y (random 500)))
       (draw-dialog x y 100 100 "Blah"))))
