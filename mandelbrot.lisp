#|| Program taken from
    https://rosettacode.org/wiki/Mandelbrot_set#Common_Lisp
    and modified to my own liking (for better understanding)
 ||#

;;; pack everything neatly into a package called 'mandelbrot'
;;; (unsing cl = common lisp)
(defpackage #:mandelbrot
    (:use #:cl))
 
(in-package #:mandelbrot)
 

;;; define a pixel datatype and image
(deftype pixel () '(unsigned-byte 8))
(deftype image () '(array pixel))

 
;;; define function to write a 2D pixel array as a PGM file
(defun write-pgm (image fname)
    (declare (image image))  ; I don't get this yet...
    (format t "Writing image to ~s~%" fname)
    (with-open-file (s fname
                     :direction :output      ; we'll write, not read
                     :element-type 'pixel    ; we'll write pixels / unsigned-bytes
                     :if-exists :supersede)  ; like overwrite, but destroy old
                                             ; file only after done writing
        ; create the local variables sequentially,
        ; because they are used for `header`
        (let* ((width  (array-dimension image 1))
               (height (array-dimension image 0))
               (header (format nil "P5~%~D ~D~%255~%" width height)))
            ; write header of PGM file
            (loop for c across header do (write-byte (char-code c) s))
            ; loop over the 2D array and write pixels
            (dotimes (row height)
                (dotimes (col width)
                    (write-byte (aref image row col) s)))))
    (format t "Done.~%")
)

 
;;; calculate and write an mandelbrot image
(defun mandelbrot (
    &optional &key
    (x-dim 256)
    (y-dim 256)
    (iter-max 100)
    (extent '(-2 1 -1.5 1.5))
)
    ;(declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
    (format t "Calculate Mandelbrot set with:~%")
    (format t "  dim:        (~d x ~d)~%" x-dim y-dim)
    (format t "  max. iter:  ~,d~%" iter-max)
    (format t "  extent:     ~A~%" extent)
    (format t "  extent:     x = [~e ... ~e]~%" (car extent) (cadr extent))
    (format t "              y = [~e ... ~e]~%" (elt extent 2) (elt extent 3))
    (let* ((cx-min (car extent))
           (cy-min (caddr extent))
           (dx (/ (- (elt extent 1) cx-min) x-dim))
           (dy (/ (- (elt extent 3) cy-min) y-dim))
           (image (make-array (list y-dim x-dim)
                   :element-type 'pixel
                   :initial-element 0)))
        (loop for y from 0 below y-dim
              for cy from cy-min by dy
            do (loop for x from 0 below x-dim
                     for cx from cx-min by dx
                     for n = (loop with c = (complex cx cy)
                                   for i from 0 below iter-max
                                   for z = c then (+ (* z z) c)
                                   while (< (abs z) 2)
                                   finally (return i))
                     for value = (expt (min 1 (/ (log (+ 1 n) ) 5)) 2)
                     for pixel = (round (* 255 (- 1 value)))
                do (setf (aref image y x) pixel)))
    image)
)


;;; run some code...
(let ((image (mandelbrot
                :iter-max 200
                :x-dim 1024
                :y-dim 1024)))
    (write-pgm image "mndbrt.pgm"))

