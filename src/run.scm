(define (plain a b)
  (list
   (* -0.707 a)
   (* 1 b)
   (* 0.707 a)))

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

(define (apply-rotation rotate-me ship)
  (let* ((rot (3d:rot:ref ship))
         (first (rotate-by '(1 0 0) (-x rot)))
         (second (rotate-by (first '(0 1 0)) (-y rot)))
         (third (rotate-by (second (first '(0 0 1))) (-z rot))))
    (third (second (first rotate-me)))))

(define (camera-adjust-third-persion camera ship)
  (let ((rot (3d:rot:ref ship))
        (angle (+ 0.2 Math.PI)))
    (move!
     camera
     (math:+ (apply-rotation ((rotate-by '(1 0 0) angle) '(0 0 3)) ship)
             (3d:position:ref ship)))
    (rotate! camera rot)
    (js:method "rotateX" camera angle)
    (js:method "rotateZ" camera Math.PI))
  ;; (camera-look-at! camera (3d:position:ref ship))
  )

(define (control-camera keys-down camera ship)
  (let ((camera-adjust-third-persion
         ;; camera-adjust-third-persion
         void
         ))
    (with-down
     keys-down
     `(;; (x ,(lambda () (js:method "rotateX" ship 0.1)))
       ;; (a ,(lambda () (js:set! "x" (js:ref "rotation" ship) (+ 0.1 (-x (3d:rot:ref ship))))))
       (a ,(lambda () (js:method "rotateY" ship 0.1) (camera-adjust-third-persion camera ship)))
       (d ,(lambda () (js:method "rotateY" ship -0.1) (camera-adjust-third-persion camera ship)))
       (w ,(lambda () (js:method "rotateX" ship -0.1) (camera-adjust-third-persion camera ship)))
       (s ,(lambda () (js:method "rotateX" ship 0.1) (camera-adjust-third-persion camera ship)))
       (| |
        ,(lambda ()
           (move!
            ship
            (math:+ (math:* 0.1 (apply-rotation '(0 0 1) ship))
                    (3d:position:ref ship)))
           (camera-adjust-third-persion camera ship)))
       ))))

(define (main-loop camera moon ship)
  (lambda (angle events keys-down cc)
    ;; (3d:rot:y:set! test (+ 0.1 (-y (3d:rot:ref test))))
    (control-camera keys-down camera ship)
    ;; (camera-look-at! camera ship)
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
  (let ((screen (js:select "#screen")))
    (js:set! "width" screen (js:ref "innerHeight" (js:window)))
    (js:set! "height" screen (js:ref "innerHeight" (js:window)))
    (let ((scene (make-scene-with-axis))
          (renderer (make-renderer screen))
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
          ;; (box (make-ship))
          ;; (ship (make-ship))
          )
      (scene-add-all scene (list moon light light2))
      (loadit
       (lambda (ship)
         (scene-add scene ship)
         (3d:rot:x:set! test (- (/ Math.PI 2)))
         (js:set! "ship" (js:window) ship)
         (js:set! "cam" (js:window) camera)
         ;; (camera-look-at! camera '(0 0 0))
         (camera-adjust-third-persion camera ship)
         (let ((loop (main-loop camera moon ship)))
           (main-frame (/ Math.PI 2)
                       (lambda (x y z cc)
                         (loop x
                               y
                               z
                               (lambda (x)
                                 (renderer-render! renderer scene camera)
                                 (cc x)))))))))))

(js:add-listener
 (js:window)
 "load"
 run)

(define (loadit cc)
  (js:method
   "then"
   (js:method
    "then"
    (js:method "fetch" (js:window) "monkey.obj")
    (callback
     (lambda (result)
       (js:method "text" result))))
   (callback
    (lambda (obj-data)
      (cc (obj-data->mesh obj-data))))))

