(define (plain a b)
  (list
   (* -0.707 a)
   (* 1 b)
   (* 0.707 a)))



;; (print (rotation-of-vector '(0 0 1)))

(define (camera-adjust-third-persion camera ship game-state)
  (let ((rot (3d:rot:ref ship))
        ;; (camera-pos (apply-rotation '(0 0 -3) camera))
        (angle (+ 0.5 Math.PI)))
    (when (game-state-lock-camera game-state)
      (move!
       camera
       (math:+ (apply-rotation ((make-rotation-matrix '(1 0 0) angle) '(0 0 3)) ship)
               (3d:position:ref ship)))
      (rotate! camera rot)
      (js:method "rotateX" camera angle)
      (js:method "rotateZ" camera Math.PI)))
  ;; (camera-look-at! camera (3d:position:ref ship))
  )

(define (control-camera keys-down camera ship shield game-state)
  (let ((speed 0.05)
        (camera-adjust-third-persion
         camera-adjust-third-persion
         ;; void
         ))
    (with-down
     keys-down
     `(;; (x ,(lambda () (js:method "rotateX" ship 0.1)))
       ;; (a ,(lambda () (js:set! "x" (js:ref "rotation" ship) (+ 0.1 (-x (3d:rot:ref ship))))))
       (a ,(lambda () (js:method "rotateZ" ship (- speed)) (camera-adjust-third-persion camera ship game-state)))
       (d ,(lambda () (js:method "rotateZ" ship speed) (camera-adjust-third-persion camera ship game-state)))
       (s ,(lambda () (js:method "rotateX" ship (- speed)) (camera-adjust-third-persion camera ship game-state)))
       (w ,(lambda () (js:method "rotateX" ship speed) (camera-adjust-third-persion camera ship game-state)))
       (x ,(lambda () (game-state-player-velocity! game-state (math:* 0.95 (game-state-player-velocity game-state)))))
       (| |
        ,(lambda ()
           (game-state-player-velocity! game-state
             (math:+ (math:* 0.0005 (apply-rotation '(0 0 1) ship))
                     (game-state-player-velocity game-state)))
           ;; (move!
           ;;  ship
           ;;  (math:+ (math:* 0.1 (apply-rotation '(0 0 1) ship))
           ;;          (3d:position:ref ship)))
           ;; (camera-adjust-third-persion camera ship)
           ))))))

(define (main-loop camera moon ship shield)
  (lambda (game-state events keys-down cc)
    ;; (3d:rot:y:set! test (+ 0.1 (-y (3d:rot:ref test))))
    (control-camera keys-down camera ship shield game-state)
    ;; (camera-look-at! camera ship)
    (let ((length (math:length (3d:position:ref moon))))
      (move! moon
             (math:+
              (3d:position:ref ship)
              (plain (* 2 (cos (game-state-moon-angle game-state)))
                     (* 2 (sin (game-state-moon-angle game-state))))))
      (move! ship (math:+ (game-state-player-velocity game-state)
                          (3d:position:ref ship)))
      (move! shield (3d:position:ref ship))
      (rotate! shield (3d:rot:ref ship))
      (camera-adjust-third-persion camera ship game-state))
    (game-state-moon-angle! game-state (+ 0.04 (game-state-moon-angle game-state)))
    (cc game-state)))

(define (light-up-at positions)
  (map
   (lambda (pos)
     (at pos make-point-light #xFFFFFF 0.3))
   positions))

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
          (lights (light-up-at '((50 50 50)
                                 (-50 50 -50)
                                 (0 50 0)
                                 (-50 220 120)
                                 (100 -100 -280))))
          (cube (at '(0 0 0)
                    make-cube 1))
          (moon (at '(0 2 0)
                    make-sphere 0.1 2 (new THREE.MeshStandardMaterial (% "shading" THREE.FlatShading "color" #x0088FF))))
          (test (at '(0 -2 0)
                    make-plane 10 10 (new THREE.MeshStandardMaterial (% "side" THREE.DoubleSide))))
          (mars (at '(0 0 100)
                    make-sphere 10 1 (new THREE.MeshStandardMaterial (% "shading" THREE.FlatShading "color" #xFF0000))))
          (merkur (at '(-50 200 100)
                      make-sphere 7 1 (new THREE.MeshStandardMaterial (% "shading" THREE.FlatShading "color" #xFF8800))))
          (neptun (at '(50 -50 -200)
                      make-sphere 20 1 (new THREE.MeshStandardMaterial (% "shading" THREE.FlatShading "color" #x00FF00))))
          (shield (at '(0 0 0)
                      make-sphere 1.2 2 (new THREE.MeshStandardMaterial (% "shading" THREE.FlatShading "color" #x0000FF "opacity" 0 "transparent" #t))))
          ;; (box (make-ship))
          ;; (ship (make-ship))
          (game-state (make-game-state)))
      (scene-add-all scene (append (list moon test mars merkur neptun shield)
                                   lights))
      (js:add-listener
       (js:window)
       "keyup"
       (lambda (e)
         (when (equal? "t" (js:ref "key" e))
           (game-state-lock-camera! game-state (not (game-state-lock-camera game-state))))
         (when (equal? "o" (js:ref "key" e))
           (js:set! "opacity" (js:ref "material" shield) (if (= 0 (js:ref "opacity" (js:ref "material" shield))) 0.1 0)))))
      (loadit
       (lambda (ship)
         (mouse-listener ship)
         (scene-add scene ship)
         (3d:rot:x:set! test (- (/ Math.PI 2)))
         (js:set! "ship" (js:window) ship)
         (js:set! "cam" (js:window) camera)
         ;; (camera-look-at! camera '(0 0 0))
         (camera-adjust-third-persion camera ship game-state)
         (let ((loop (main-loop camera moon ship shield)))
           (main-frame game-state
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
   "catch"
   (js:method
    "then"
    (js:method
     "then"
     (js:method "fetch" (js:window) "spaceship.obj")
     (callback
      (lambda (result)
        (js:method "text" result))))
    (callback
     (lambda (obj-data)
       (cc (obj-data->mesh obj-data)))))
   (callback (lambda (x) ((native window.alert) x)))))

(define (mouse-listener ship)
  (let ((start #f))
    (js:add-listener
     (js:window)
     "mousedown"
     (lambda (event)
       (set! start
         (list (js:ref "clientX" event)
               (js:ref "clientY" event)))))
    (js:add-listener (js:window) "mouseup" (lambda () (set! start #f)))
    (js:add-listener
     (js:window)
     "mousemove"
     (lambda (event)
       (when start
         (let ((l (list (js:ref "clientX" event)
                        (js:ref "clientY" event))))
           (js:method "rotateOnAxis"
                      ship
                      (new THREE.Vector3 1 0 0)
                      (* -0.01 (- (-y start)
                                      (-y l))))
           (js:method "rotateOnAxis"
                      ship
                      (new THREE.Vector3 0 1 0)
                      (* 0.01 (- (-x start)
                                  (-x l))))
           (set! start l)))))
    (js:add-listener
     (js:window)
     "deviceready"
     (lambda ()
       (js:method "getCurrentAcceleration"
                  navigator.accelerometer
                  (callback
                   (lambda (e)
                     (js:method "alert" (js:window) (js:ref "x" e))))
                  (callback (lambda () (js:method "alert" (js:window) "error"))))))))
