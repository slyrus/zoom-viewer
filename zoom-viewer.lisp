
(defpackage #:zoom-viewer
  (:use #:clim #:clim-lisp #:clim-extensions #:mcclim-bezier))

(in-package #:zoom-viewer)

(defclass zoom-pane (application-pane)
  ((zoom-x-level :initform 1.0d0 :accessor zoom-x-level)
   (zoom-y-level :initform 1.0d0 :accessor zoom-y-level)))

(defun zoom-x-callback (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'app)))
      (setf (zoom-x-level pane) scale)
      (repaint-sheet pane +everywhere+))))

(defun zoom-y-callback (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'app)))
      (setf (zoom-y-level pane) scale)
      (repaint-sheet pane +everywhere+))))

(define-application-frame zoom-viewer-app ()
  ()
  (:panes
   (app zoom-pane
        :height 4096 :width 4096
        :display-function 'display-zoom-viewer)
   (int :interactor :height 200 :width 600)
   (zoom-x :slider
         :min-value 0.1
         :max-value 10
         :decimal-places 2
         :value 1.0d0
         :show-value-p t
         :orientation :horizontal
         :drag-callback 'zoom-x-callback
         :value-changed-callback 'zoom-x-callback)
   (zoom-y :slider
         :min-value 0.1
         :max-value 10
         :decimal-places 2
         :value 1.0d0
         :show-value-p t
         :orientation :horizontal
         :drag-callback 'zoom-y-callback
         :value-changed-callback 'zoom-y-callback))
  (:layouts
   (default (vertically ()
              app
              (labelling (:label "Zoom X")
                zoom-x)
              (labelling (:label "Zoom Y")
                zoom-y)
              int))))


(defun remove-keyword-arg (key args)
  (loop for (k v) on args by #'cddr
     unless (eql key k)
     append (list k v)))

(defmethod replay-output-record :around (record (pane zoom-pane)
                                 &optional (region +everywhere+)
                                           (x-offset 0) (y-offset 0))
  (declare (optimize (debug 3)))
  (let* ((tr (make-scaling-transformation (zoom-x-level pane) (zoom-y-level pane)))
         (original-transform (sheet-native-transformation pane)))
    (climi::%%set-sheet-native-transformation tr pane)
    (call-next-method)
    (climi::%%set-sheet-native-transformation original-transform pane)))

(defun draw-zoom-polygon (sheet x y sides radius
                             &rest args
                             &key (angle 0)
                                  (filled t) ink clipping-region
                                  transformation line-style line-thickness
                                  line-unit line-dashes line-joint-shape line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness filled
		   line-unit line-dashes line-joint-shape line-cap-shape))
  (let* ((coords (loop for i below sides
                    with theta1 = angle
                    for x1 = (+ x (* radius (sin theta1)))
                    for y1 = (+ y (* radius (cos theta1)))
                    do (incf theta1 (/ (* 2 pi) sides))
                    collect (list x1 y1)))
         (points (mapcar (lambda (x) (apply #'clim:make-point x))
                         coords)))
    (apply #'draw-polygon sheet points (remove-keyword-arg :angle args))))

(defun display-zoom-viewer (frame pane)
  (declare (ignore frame))
  
  (draw-rectangle* pane 10 10 100 30 :ink +green+)
  (draw-zoom-polygon pane 50 50 6 20 :angle (/ pi 2) :ink +orange+)
  (let* ((c1-coords (relative-to-absolute-coord-seq (list 50 70 30 -60 -20 -30 100 -15)))
         (c1 (make-bezier-curve* c1-coords)))
    (draw-oval* pane 100 200 30 50 :ink +red+)
    (draw-bezier-design* pane c1 :line-thickness 5 :ink +blue+)
    (destructuring-bind (arrow-y arrow-x &rest args)
        (reverse c1-coords)
      (declare (ignore args))
      (draw-arrow* pane (+ arrow-x 3) (+ arrow-y 4) (+ arrow-x 3) (+ arrow-y 4) :ink +blue+
                   :line-thickness 5
                   :angle (* -128 (/ pi 180))
                   :head-width 10)
      (draw-text* pane "ZOOMABILITY!" 200 200))))

(defun zoom-viewer-main ()
  (let ((frame (make-application-frame 'zoom-viewer-app)))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))

