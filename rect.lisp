(in-package :gypsum-client)

(export '(rect
          rect-top
          rect-right
          rect-bottom
          rect-left
          rect-width
          rect-height
          rect-x
          rect-y
          subdivide-rect))

(defclass rect ()
  ((top :initform 0
        :type integer
        :initarg :top
        :initarg :y
        :accessor rect-top
        :accessor rect-y
        :documentation "The top edge of the rect. Can be used as the Y offset of the rect.")
   (right :initform 0
          :type integer
          :initarg :right
          :initarg :width
          :accessor rect-right
          :documentation "The right edge of the rect.")
   (bottom :initform 0
           :type integer
           :initarg :bottom
           :initarg :height
           :accessor rect-bottom
           :documentation "The bottom edge of the rect.")
   (left :initform 0
         :type integer
         :initarg :left
         :initarg :x
         :accessor rect-left
         :accessor rect-x
         :documentation "The left edge of the rect. Can be used as the X offset of the rect."))
  (:documentation "Represents a rectangular region."))

(defmethod print-object ((object rect) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (top right bottom left) object
      (format stream ":TOP ~d :RIGHT ~d :BOTTOM ~d :LEFT ~d"
            top right bottom left))))

(defmacro make-rect (&rest args)
  "Constructs a rect instance using the same args as would be passed to `make-instance'."
  `(make-instance 'rect ,@args))

(defun rect-width (rect)
  "Returns the width of RECT."
  (check-type rect rect)
  (- (rect-right rect) (rect-left rect)))

(defun rect-height (rect)
  "Returns the height of R."
  (check-type rect rect)
  (- (rect-bottom rect) (rect-top rect)))

(defun subdivide-rect-h (rect ratio gutter)
  "Subdivides a RECT into two new rects horizontally using the given RATIO and
GUTTER, where one is stacked above the other. RATIO is used to determine where
the integer-valued division point is, and GUTTER is used to determine how much
space to place between the two new rects. Returns the new instances, top and
bottom as multiple values."
  (with-slots (top right left bottom) rect
    (let* ((height (rect-height rect))
           (division-point (+ top (truncate (* ratio height))))
           (top (make-instance 'rect
                               :top    top
                               :right  right
                               :left   left
                               :bottom division-point))
           (bottom (make-instance 'rect
                                  :top    (+ division-point 1)
                                  :right  right
                                  :left   left
                                  :bottom bottom)))
      (values top bottom))))

(defun subdivide-rect-v (rect ratio gutter)
  "Subdivides a RECT into two new rects vertically, where one is beside another.
RATIO is used to determine where the integer-valued division point is, and
GUTTER is used to determine how much space to place between the two new rects.
Returns the new instances, left and right as multiple values."
  (with-slots (top right left bottom) rect
    (let* ((width (rect-width rect))
           (division-point (+ left (truncate (* ratio width))))
           (left (make-instance 'rect
                                :top    top
                                :right  division-point
                                :left   left
                                :bottom bottom))
           (right (make-instance 'rect
                                 :top    top
                                 :right  right
                                 :left   (+ division-point 1)
                                 :bottom bottom)))
      (values left right))))

(defun subdivide-rect (rect direction &key (ratio 1/2) (gutter 1))
  "Subdivides the given RECT into two new rects. DIRECTION must be either
:HORIZONTAL or :VERTICAL. GUTTER must be at least 1."
  (check-type rect rect)
  (check-type ratio ratio)
  (check-type gutter integer)
  (cond ((eq direction :horizontal) (subdivide-rect-h rect ratio gutter))
        ((eq direction :vertical)   (subdivide-rect-v rect ratio gutter))
        (t (error "DIVISION must be one of :HORIZONTAL or :VERTICAL"))))
