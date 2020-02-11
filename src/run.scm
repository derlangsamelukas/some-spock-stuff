(define (plain a b)
  (list
   (* -0.707 a)
   (* 1 b)
   (* 0.707 a)))


(define (x-z-plain a b)
  (list
   a
   0
   b))

(define (control-camera keys-down camera ship)
  (with-down
   keys-down
   `(;; (w ,(lambda () (3d:rot:x:set! camera (+ 0.1 (-x (3d:rot:ref camera))))))
     ;; (a ,(lambda () (3d:rot:y:set! camera (+ 0.1 (-y (3d:rot:ref camera))))))
     ;; (s ,(lambda () (3d:rot:x:set! camera (+ -0.1 (-x (3d:rot:ref camera))))))
     ;; (d ,(lambda () (3d:rot:y:set! camera (+ -0.1 (-y (3d:rot:ref camera))))))
     ;; (v ,(lambda () (print (3d:rot:ref camera))))
     (a ,(lambda () (3d:rot:y:set! ship (+ 0.1 (-y (3d:rot:ref ship))))))
     (d ,(lambda () (3d:rot:y:set! ship (+ -0.1 (-y (3d:rot:ref ship))))))

     ;; (ArrowUp ,(lambda ()
     ;;             (rotate! ship
     ;;                      (math:+
     ;;                       (math:* 0.1 (list
     ;;                                    (cos (-x (3d:rot:ref ship)))
     ;;                                    0
     ;;                                    (sin (-z (3d:rot:ref ship)))))
     ;;                       (3d:rot:ref ship)))))
     
     (w ,(lambda ()
           (let ((angle (-y (3d:rot:ref ship)))
                 (y-angle #f))
             (move! ship
                    (math:+
                     (math:* 0.1 (x-z-plain (sin angle)
                                            (cos angle)))
                     (3d:position:ref ship)))
             (camera-look-at! camera (3d:position:ref ship))))))))

(define (main-loop camera moon ship)
  (lambda (angle events keys-down cc)
    ;; (3d:rot:y:set! test (+ 0.1 (-y (3d:rot:ref test))))
    (control-camera keys-down camera ship)
    (let ((length (math:length (3d:position:ref moon))))
      (move! moon
             (math:+
              (3d:position:ref ship)
              (plain (* 2 (cos angle))
                     (* 2 (sin angle))))))
    (cc (+ 0.04 angle))))

(define (make-ship)
  (let ((top (make-plane 1 1))
        (bottom (make-plane 1 1))
        (left (make-plane 1 1))
        (right (make-plane 1 1))
        (front (make-plane 1 1 (make-material-basic #xFF0000)))
        (back (make-plane 1 1)))
    (move-y! top 0.5)
    (rotate-x! top (- (/ Math.PI 2)))
    (move-y! bottom -0.5)
    (rotate-x! bottom (/ Math.PI 2))
    (move-x! left 0.5)
    (rotate-y! left (/ Math.PI 2))
    (move-x! right -0.5)
    (rotate-y! right (- (/ Math.PI 2)))
    (move-z! front 0.5)
    (move-z! back -0.5)
    (rotate-x! back Math.PI)
    (make-group (list top bottom left right front back))))

(define (run)
  (let ((scene (make-scene-with-axis))
        (renderer (make-renderer (js:select "#screen")))
        (camera (at '(3 3 3)
                    make-camera 1))
        (light (at '(50 50 50)
                   make-point-light #xFFFFFF 1))
        (light2 (at '(-50 50 -50)
                    make-point-light #xFFFFFF 0.5))
        (cube (at '(0 0 0)
                  make-cube 1))
        (moon (at '(0 2 0)
                  make-sphere 0.1 2))
        (test (at '(0 0 0)
                  make-plane 10 10))
        (ship (make-ship)))
    (scene-add-all scene (list moon light light2 ship))
    (3d:rot:x:set! test (- (/ Math.PI 2)))
    (js:set! "cam" (js:window) camera)
    (camera-look-at! camera '(0 0 0))
    (let ((loop (main-loop camera moon ship)))
      (main-frame (/ Math.PI 2)
                  (lambda (x y z cc)
                    (loop x
                          y
                          z
                          (lambda (x)
                            (renderer-render! renderer scene camera)
                            (cc x))))))))

(js:add-listener
 (js:window)
 "load"
 run)
