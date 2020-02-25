(define (-width sprite)
  (car sprite))

(define (-height sprite)
  (cadr sprite))

(define -x car)
(define -y cadr)
(define -z caddr)

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

(define (make-material-basic color)
  (new THREE.MeshBasicMaterial (% "color" color)))

(define (make-material-standard)
  (new THREE.MeshStandardMaterial))

(define (make-material-wireframe)
  (new THREE.MeshBasicMaterial (% "wireframe" #t)))

(define (make-quaternion angle v)
  (let ((quaternion (new THREE.Quaternion)))
    (js:method "setFromAxisAngle" quaternion (new THREE.Vector3 (-x v) (-y v) (-z v)) angle)
    quaternion))

(define (q->s a ijk)
  (let* ((angle (acos a))
         (sina (sin angle)))
    (if (zero? sina)
        (list (* angle 2) 0 0 0)
        (cons (* angle 2)
              (map (cut / <> sina)
                   ijk)))))

(define (s->q angle v)
  (let ((angle (/ angle 2)))
    (cons
     (cos angle)
     (map (cut * (sin angle) <>)
          v))))

;; (js:log (make-quaternion (/ Math.PI 2) '(0 0 1)))
;; (let ((x (s->q (/ Math.PI 2) '(0 0 1))))
;;   (print (q->s (car x) (cdr x))))

(define (3d:line color from to material)
  (let* ((material (if (void? material) (new THREE.LineBasicMaterial (% "color" color)) material))
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

(define (move-x! object x) (js:set! "x" (js:ref "position" object) x))
(define (move-y! object y) (js:set! "y" (js:ref "position" object) y))
(define (move-z! object z) (js:set! "z" (js:ref "position" object) z))

(define (3d:rot:set! object x y z)
  (js:method "set" (js:ref "rotation" object) x y z))

(define (3d:rot:x:set! object x) (js:set! "x" (js:ref "rotation" object) x))
(define (3d:rot:y:set! object y) (js:set! "y" (js:ref "rotation" object) y))
(define (3d:rot:z:set! object z) (js:set! "z" (js:ref "rotation" object) z))

(define (rotate! object rotation)
  (apply 3d:rot:set! object rotation))

(define rotate-x! 3d:rot:x:set!)
(define rotate-y! 3d:rot:y:set!)
(define rotate-z! 3d:rot:z:set!)

(define (3d:rot:ref object)
  (let ((pos (js:ref "rotation" object)))
    (list
     (js:ref "x" pos)
     (js:ref "y" pos)
     (js:ref "z" pos))))

(define (js:incr! object path by)
  (define (loop object path)
    (if (null? (cdr path))
        (js:set! (car path) object (+ (js:ref (car path) object) by))
        (loop (js:ref (car path) object (cdr path)))))
  (loop object path))

(define (pow x exp)
  (define (loop exp)
    (if (<= exp 0)
        1
        (* x (loop (- exp 1)))))
  (loop exp))

(define (math:length list)
  (sqrt
   (foldl
    (lambda (l x)
      (+ l (* x x)))
    0
    list)))

(define (math:* m list)
  (map (cut * m <>)
       list))

(define (math:+ list1 list2)
  (map +
       list1
       list2))

(define (make-renderer canvas)
  (let ((context (js:method "getContext" canvas (jstring "webgl2") (% "alpha" #f))))
    (when (null? context)
      ((native window.alert) (jstring "context is null")))
    (new THREE.WebGLRenderer (% "canvas" canvas "context" context))))

;; (define (make-mesh vertices)
;;   (let ((geometry (new THREE.BufferGeometry))
;;         (normals (new window.Float32Array (list->vector
;;                                            (foldl (lambda (l) `(,@l 0 0 1))
;;                                                   '()
;;                                                   vertices))))
;;         (vertices (new window.Float32Array (list->vector
;;                                             (foldl (lambda (l point) `(,@l ,@point))
;;                                                    '()
;;                                                    vertices))))
;;         ;; (material (new THREE.MeshBasicMaterial (% "color" #xFFFF00)))
;;         (material (new THREE.MeshStandardMaterial (%)))
;;         )
;;     (js:log vertices)
;;     (js:method "setAttribute" geometry (jstring "position") (new THREE.BufferAttribute vertices 3))
;;     (js:method "setAttribute" geometry (jstring "normal") (new THREE.BufferAttribute normals 3))
;;     (new THREE.Mesh geometry material)))

;; (make-mesh '((-1.0 -1.0 1.0)
;;             ( 1.0 -1.0 1.0)
;;             ( 1.0  1.0 1.0)
;;             ( 1.0  1.0 1.0)
;;             (-1.0  1.0 1.0)
;;             (-1.0 -1.0 1.0)

;;             ( 1.0  1.0 -1.0)
;;             ( 1.0 -1.0 -1.0)
;;             (-1.0 -1.0 -1.0)
;;             ( 1.0  1.0 -1.0)
;;             (-1.0 -1.0 -1.0)
;;             (-1.0  1.0 -1.0)))

(define (make-plane width height material)
  (let ((geometry (new THREE.PlaneBufferGeometry width height))
        (material (if (void? material) (new THREE.MeshStandardMaterial (%)) material)))
    (new THREE.Mesh geometry material)))

(define (make-cube radius material)
  (let ((geometry (new THREE.BoxGeometry radius))
        (material (if (void? material) (new THREE.MeshStandardMaterial (%)) material)))
    (new THREE.Mesh geometry material)))

(define (make-sphere radius vertice-density material)
  (new THREE.Mesh
       (new THREE.DodecahedronGeometry radius vertice-density)
       (if (void? material) (new THREE.MeshStandardMaterial (%)) material)))

(define (make-scene)
  (new THREE.Scene))

(define (scene-add scene object)
  (js:method "add" scene object))

(define (scene-add-all scene objects)
  (map (cut scene-add scene <>)
       objects)
  (void))

(define (make-scene-with-axis)
  (let* ((scene (make-scene))
         (x-axis (3d:line #xFF0000 '(-500 0 0) '(500 0 0)))
         (y-axis (3d:line #x00FF00 '(0 -500 0) '(0 500 0)))
         (z-axis (3d:line #x0000FF '(0 0 -500) '(0 0 500))))
    (scene-add-all scene (list x-axis y-axis z-axis))
    scene))

(define (make-point-light color lumen)
  (let ((light (new THREE.PointLight color lumen)))
    light))

(define (at pos create . args)
  (let ((object (apply create args)))
    (apply 3d:position:set! object pos)
    object))

(define (make-camera ratio)
  (new THREE.PerspectiveCamera 75 ratio 0.1 1000))

(define (camera-look-at! camera pos)
  (apply js:method "lookAt" camera pos))

(define (renderer-render! renderer scene camera)
  (js:method "render" renderer scene camera))

(define (move! object pos)
  (apply 3d:position:set! object pos))

(define (with-down keys-down alist)
  (map
   (lambda (pair)
     (when (exists? (symbol->string (car pair)) keys-down)
       ((cadr pair))))
   alist))

(define (make-group objects)
  (let ((group (new THREE.Group)))
    (scene-add-all group objects)
    group))

(define (obj-data->mesh obj-data)
  (let ((lines (string-split "\n" obj-data))
        (vertices '())
        (normals '())
        (faces '())
        (face-color #xFFFFFF))
    (define (number-list-of-line line)
      (map string->number (cdr (vector->list (string-split " " line)))))
    (define (prepare-points list)
      (list->vector
       (map
        (lambda (point)
          (new THREE.Vector3 (-x point) (-y point) (-z point)))
        list)))
    (define (add-face-color face)
      (append face (list face-color)))
    (define (loop index)
      (when (< index (vector-length lines))
        (let* ((line (js:method "trim" (new window.String (jstring (vector-ref lines index)))))
               (result (js:method "match" (new window.String line) (new window.RegExp (jstring "(^#)|(^v )|(^vn )|(^f )|(^usemtl )")))))
          (cond
           ((null? result)
            ;;(print "dunno")
            #f)
           ((not (void? (vector-ref result 1)))
            ;; (print "comment line")
            #f)
           ((not (void? (vector-ref result 2)))
            (set! vertices (cons (number-list-of-line line) vertices)))
           ((not (void? (vector-ref result 3)))
            (set! normals (cons (number-list-of-line line) normals)))
           ((not (void? (vector-ref result 4)))
            (let ((definition (map
                         (lambda (string) (- (string->number (vector-ref (string-split "/" string) 0)) 1))
                         (cdr (vector->list (string-split " " line))))))
              (cond
               ((= 4 (length definition))
                (set! faces (append (list (add-face-color (list (car definition) (cadr definition) (caddr definition)))
                                          (add-face-color (list (caddr definition) (cadddr definition) (car definition))))
                                    faces))
                ;; (set! faces (cons definition
                ;;                   (cons (list (caddr definition) (cadddr definition) (car definition))
                ;;                         faces)))
                )
               ((= 3 (length definition))
                (set! faces (cons (add-face-color definition) faces)))
               ((= 5 (length definition))
                (set! faces (append (list (add-face-color (list (car definition) (cadr definition) (caddr definition)))
                                          (add-face-color (list (caddr definition) (cadddr definition) (car definition)))
                                          (add-face-color (list (cadddr definition) (cadddr (cdr definition)) (car definition))))
                                    faces))
                ;; (set! faces (cons definition
                ;;                   (cons (list (caddr definition) (cadddr definition) (car definition))
                ;;                         (cons (list (cadddr definition) (cadddr (cdr definition)) (car definition))
                ;;                               faces))))
                )
               (#t (thiswillcauseaerrorbecauseidontexist))))
            ;;(print "face line")
            #f)
           ((not (void? (vector-ref result 5)))
            (set! face-color
              (if (equal? "cockpit" (vector-ref (string-split " " line) 1))
                  #x3B64E7
                  #xFFFFFF)))
           (#t (print "dunno")))
          (loop (+ 1 index)))))
    (loop 0)
    (let ((geometry (new THREE.Geometry))
          (vertices (prepare-points (reverse vertices)))
          ;; (normals (prepare-points (reverse normals)))
          (material (new THREE.MeshStandardMaterial (% "roughness" 0 "metalness" 0 "shading" THREE.FlatShading "vertexColors" THREE.FaceColors)))
          )
      ;; (js:method "setAttribute" geometry (jstring "position") (new THREE.BufferAttribute vertices 3))
      ;; (js:method "setAttribute" geometry (jstring "normal") (new THREE.BufferAttribute normals 3))
      (map
       (lambda (face)
         (js:method "push" (js:ref "faces" geometry) (new THREE.Face3 (car face) (cadr face) (caddr face) (void) (new THREE.Color (cadddr face)))))
       (reverse faces))
      (js:method "setFromPoints" geometry vertices)
      (js:method "computeVertexNormals" geometry)
      (js:method "normalize" geometry)
      (new THREE.Mesh geometry material))))

(define (make-rotation-matrix e angle)
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

(define (apply-rotation rotate-me object)
  (let* ((rot (3d:rot:ref object))
         (first (make-rotation-matrix '(1 0 0) (-x rot)))
         (second (make-rotation-matrix (first '(0 1 0)) (-y rot)))
         (third (make-rotation-matrix (second (first '(0 0 1))) (-z rot))))
    (third (second (first rotate-me)))))
