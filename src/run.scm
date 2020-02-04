
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

(define (run)
  (let* ((screen (js:select "#screen"))
         (context (js:canvas:get-context screen "2d"))
         (width (js:canvas:width screen))
         (height (js:canvas:height screen)))
    (define (mainloop model events keys-down cc)
      (let ((model (foldl handle-events model events)))
        (let ((model (cond
                      ((exists? "a" keys-down)
                       (- model 5))
                      ((exists? "d" keys-down)
                       (+ model 5))
                      (else model))))
          (js:context:fill-style context "#000000")
          (js:context:stroke-style context "#FFFFFF")
          (js:context:clear context)
          ;; (js:context:stroke-rect context 0 0 100 100)
          (render context model 400 '(30 50))
          (cc model))))
    (main-frame 250 mainloop)))

;; (print
;;  (match '(a b c)
;;    ((x . _) x)
;;    (y 'numb)))

(js:add-listener
 (js:window)
 "load"
 run)
