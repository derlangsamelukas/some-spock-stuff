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

(define (rotate-by e angle)
  (let ((m (- 1 (cos angle)))
        (cosa (cos angle))
        (sina (sin angle))
        (e1 (-x e))
        (e2 (-y e))
        (e3 (-z e)))
    (lambda (v)
      (list
       (+ (* (+ (* e1 e1 m)        cosa) (-x v))
          (* (- (* e1 e2 m) (* e3 sina)) (-y v))
          (* (+ (* e1 e3 m) (* e2 sina)) (-z v)))
       
       (+ (* (+ (* e2 e1 m) (* e3 sina)) (-x v))
          (* (+ (* e2 e2 m)        cosa) (-y v))
          (* (- (* e2 e3 m) (* e1 sina)) (-z v)))
       
       (+ (* (- (* e3 e1 m) (* e2 sina)) (-x v))
          (* (+ (* e3 e2 m) (* e1 sina)) (-y v))
          (* (+ (* e3 e3 m)        cosa) (-z v)))))))

(define (rotate v rotation)
  (let ((tx (rotate-by '(1 0 0) (-x rotation)))
        (ty (rotate-by '(0 1 0) (-y rotation)))
        (tz (rotate-by '(0 0 1) (-z rotation))))
    ((o tz ty tx) v)))

(define (angle-between v1 v2)
  (acos
   (/ (+ (* (-x v1) (-x v2))
         (* (-y v1) (-y v2))
         (* (-z v1) (-z v2)))
      (* (math:length v1)
         (math:length v2)))))

(define (rotation-of-vector v)
  (list
   (angle-between v '(1 0 0))
   (angle-between v '(0 1 0))
   (angle-between v '(0 0 1))))

;; (print (rotation-of-vector '(0 0 1)))

(define (control-camera keys-down camera ship)
  (with-down
   keys-down
   `(;; (w ,(lambda () (3d:rot:x:set! camera (+ 0.1 (-x (3d:rot:ref camera))))))
     ;; (a ,(lambda () (3d:rot:y:set! camera (+ 0.1 (-y (3d:rot:ref camera))))))
     ;; (s ,(lambda () (3d:rot:x:set! camera (+ -0.1 (-x (3d:rot:ref camera))))))
     ;; (d ,(lambda () (3d:rot:y:set! camera (+ -0.1 (-y (3d:rot:ref camera))))))
     ;; (v ,(lambda () (print (3d:rot:ref camera))))
     (a ,(lambda () (3d:rot:y:set! ship (+ 0.1 (-y (3d:rot:ref ship))))))
     ;; (d ,(lambda () (3d:rot:y:set! ship (+ -0.1 (-y (3d:rot:ref ship))))))
     (b ,(lambda () (print (3d:rot:ref ship))))

     (ArrowUp ,(lambda ()
                 ;; (rotate! ship
                 ;;          (math:+
                 ;;           (rotation-of-vector
                 ;;            ((rotate-by (rotate '(0 0 1) (3d:rot:ref ship)) (/ Math.PI 2)) '(0 0 1)))
                 ;;           '(0 0 0)))
                 (3d:rot:x:set! ship (+ 0.1 (-x (3d:rot:ref ship))))
                 ;; (rotate! ship
                 ;;          (math:+
                 ;;           '(-0.1 0 0)
                 ;;           (3d:rot:ref ship)))
                 ))

     ;; (r ,(lambda () (rotate! ship '(0 0 0))))
     ;; (x ,(lambda () (js:method "rotateX" ship 0.1)))
     ;; (y ,(lambda () (js:method "rotateY" ship 0.1)))
     ;; (z ,(lambda () (js:method "rotateZ" ship 0.1)))
     ;; (x ,(lambda () (rotate-x! ship (+ 0.1 (-x (3d:rot:ref ship))))))
     ;; (y ,(lambda () (rotate-y! ship (+ 0.1 (-y (3d:rot:ref ship))))))
     ;; (z ,(lambda () (rotate-z! ship (+ 0.1 (-z (3d:rot:ref ship))))))

     ;; (x ,(lambda ()
     ;;       (let* ((q (js:ref "quaternion" ship))
     ;;              (q* (q->s (js:ref "w" q)
     ;;                        (list (js:ref "x" q)
     ;;                              (js:ref "y" q)
     ;;                              (js:ref "z" q)))))
     ;;         (js:method "setRotationFromQuaternion" (make-quaternion (angle-between ))))))
     
     (w ,(lambda ()
           (move! ship (math:+
                        (rotate '(0 0 0.1) (3d:rot:ref ship))
                        (3d:position:ref ship)))
           (camera-look-at! camera (3d:position:ref ship)))))))

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
  (let ((top (make-plane 1 1 (make-material-basic #x00FF00)))
        (bottom (make-plane 1 1))
        (left (make-plane 1 1 (make-material-basic #x0000FF)))
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
    (js:set! "ship" (js:window) ship)
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
