
(define (-width sprite)
  (car sprite))

(define (-height sprite)
  (cadr sprite))

(define (render context x y sprite)
  (let ((width (-width sprite))
        (height (-height sprite)))
    (js:context:stroke-rect context (- x (/ width 2)) (- y (/ height 2)) width height)))

(define (next-frame model fn)
  (js:window:request-animation-frame (lambda () (fn model))))

(define (push-end x lst)
  (if (null? lst)
      (list x)
      (cons (car lst) (push-end x (cdr lst)))))

(define (recursive fn . args)
  (define (loop . args)
    (apply fn loop args))
  (apply loop args))

(define (foldl f z lst)
  (recursive
   (lambda (cc z lst)
     (if (null? lst)
         z
         (cc (f z (car lst)) (cdr lst))))
   z
   lst))

(define (add-to-list x lst)
  (recursive
   (lambda (cc lst)
     (if (null? lst)
         (list x)
         (if (equal? x (car lst))
             lst
             (cons (car lst) (cc (cdr lst))))))
   lst))

(define (remove-from-list x lst)
  (recursive
   (lambda (cc lst)
     (if (null? lst)
         '()
         (if (equal? x (car lst))
             (cdr lst)
             (cons (car lst) (remove-from-list x (cdr lst))))))
   lst))

(define (main-frame model idle)
  (let ((event-queue '())
        (keys-down '()))
    (define (cc model)
      (js:window:request-animation-frame
       (lambda ()
         (let ((model (idle model event-queue keys-down cc)))
           (set! event-queue '())
           model))))
    (js:add-listener
     (js:window)
     "keydown"
     (lambda (event)
       (set! event-queue (push-end event event-queue))
       (set! keys-down (add-to-list (js:keyboard:key event) keys-down))
       ))
    (js:add-listener
     (js:window)
     "keyup"
     (lambda (event)
       (set! event-queue (push-end event event-queue))
       (set! keys-down (remove-from-list (js:keyboard:key event) keys-down))
       ))
    (idle model event-queue keys-down cc)))

(define (handle-events model event)
  ;; (cond
  ;;  ((equal? "a" (js:keyboard:key event)) (- model 5))
  ;;  (else model))
  model)

(define (exists? x lst)
  (recursive
   (lambda (cc lst)
     (if (null? lst)
         #f
         (if (equal? x (car lst))
             #t
             (cc (cdr lst)))))
   lst))

;; (define (run)
;;   (let* ((screen (js:select "#screen"))
;;          (context (js:canvas:get-context screen "2d"))
;;          (width (js:canvas:width screen))
;;          (height (js:canvas:height screen)))
;;     (define (mainloop model events keys-down cc)
;;       (let ((model (foldl handle-events model events)))
;;         (let ((model (cond
;;                       ((exists? "a" keys-down)
;;                        (- model 5))
;;                       ((exists? "d" keys-down)
;;                        (+ model 5))
;;                       (else model))))
;;           (js:context:fill-style context "#000000")
;;           (js:context:stroke-style context "#FFFFFF")
;;           (js:context:clear context)
;;           ;; (js:context:stroke-rect context 0 0 100 100)
;;           (render context model 400 '(30 50))
;;           (cc model))))
;;     (main-frame 250 mainloop)))

;; (print
;;  (match '(a b c)
;;    ((x . _) x)
;;    (y 'numb)))

(define (3d:line color from to)
  (let* ((material (new THREE.LineBasicMaterial (% "color" color)))
         (points
          (vector
           (new THREE.Vector3 (car from) (cadr from) (caddr from))
           (new THREE.Vector3 (car to) (cadr to) (caddr to))))
         (geometry (js:method "setFromPoints" (new THREE.BufferGeometry) points)))
    (new THREE.Line geometry material)))

(define (3d:position:ref object)
  (let ((pos (js:ref "position" object)))
    (list
     (js:ref "x" pos)
     (js:ref "y" pos)
     (js:ref "z" pos))))

(define (3d:position:set! object x y z)
  (js:method "set" (js:ref "position" object) x y z))

(define (js:incr! object path by)
  (define (loop object path)
    (if (null? (cdr path))
        (js:set! (car path) object (+ (js:ref (car path) object) by))
        (loop (js:ref (car path) object) (cdr path))))
  (loop object path))

(define (pow x exp)
  (define (loop exp)
    (if (<= exp 0)
        1
        (* x (loop (- exp 1)))))
  (loop exp))

(define (run)
  (let* ((canvas (js:select "#screen"))
         (context (js:method "getContext" canvas (jstring "webgl2") (% "alpha" #f))))
    (let ((scene (new THREE.Scene))
          (camera (new THREE.PerspectiveCamera 75 1 0.1 1000))
          (renderer (new THREE.WebGLRenderer (% "canvas" canvas "context" context)))
          (geometry (new THREE.BoxGeometry))
          ;; (geometry (new THREE.DodecahedronGeometry 1 2))
          ;; (material (new THREE.MeshBasicMaterial (% "color" #x00ff00)))
          (material (new THREE.MeshStandardMaterial (%)))
          (light (new THREE.PointLight #x0f0f0)))
      (let ((cube (new THREE.Mesh geometry material))
            (x-axis (3d:line #xFF0000 '(-100 0 0) '(100 0 0)))
            (y-axis (3d:line #x00FF00 '(0 -100 0) '(0 100 0)))
            (z-axis (3d:line #x0000FF '(0 0 -100) '(0 0 100))))
        ;; (js:method "setSize" renderer 500 500)
        ;; (js:method "appendChild" (js:select "#app") (js:ref "domElement" renderer))
        (js:method "add" scene light)
        (js:method "add" scene x-axis)
        (js:method "add" scene y-axis)
        (js:method "add" scene z-axis)
        (js:method "add" scene cube)
        (3d:position:set! light 50 50 50)
        (3d:position:set! camera 2 2 2)
        (js:method "lookAt" camera 0 0 0)
        ;; (js:method "set" (js:ref "position" light) 50 50 50)
        ;; (js:set! "z" (js:ref "position" camera) 5)
        (main-frame
         0
         (lambda (model events keys-down cc)
           (let ((plain (lambda (a b)
                          (match (3d:position:ref camera)
                            ((x y z)
                             (let ((deg (acos
                                         (/ (+ (* x x) (* y y) (* z 0))
                                            (* (sqrt (+ (pow x 2) (pow y 2) (pow z 2)))
                                               (sqrt (+ (pow x 2) (pow y 2) 0)))))))
                               (list
                                (* 0.3 a)
                                (* 0.3 b)
                                (* 0.3 a))))))))
             ;; (js:set! "x" (js:ref "rotation" cube) (+ 0.01 (js:ref "x" (js:ref "rotation" cube))))
             ;; (js:set! "y" (js:ref "rotation" cube) (+ 0.01 (js:ref "y" (js:ref "rotation" cube))))
             ;; (js:set! "y" (js:ref "rotation" cube) (+ 0.01 (js:ref "z" (js:ref "rotation" cube))))
             (cond
              ((exists? "x" keys-down)
               ;; (js:incr! camera '("position" "x") 0.02)
               (set! model (+ 0.01 model))
               (apply 3d:position:set! camera (plain (* 10 (cos model)) (* 10 (sin model))))
               ;; (js:set! "x" (js:ref "position" camera) (* 2 (cos model)))
               ;; (js:set! "y" (js:ref "position" camera) (* 2 (sin model)))
               (js:method "lookAt" camera 0 0 0))
              ((exists? "y" keys-down)
               (js:incr! camera '("position" "y") -0.02)
               (js:method "lookAt" camera 0 0 0))
              ((exists? "z" keys-down)
               (js:incr! camera '("position" "z") 0.02)
               (js:method "lookAt" camera 0 0 0))))
           (js:method "render" renderer scene camera)
           (cc model)))))))

(js:add-listener
 (js:window)
 "load"
 run)
