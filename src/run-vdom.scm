
(define (header model)
  `(div
    (class "header")
    (children
     (div
      (class "item")
      (text "Hejoo")))))

(define (body model)
  `(div
    (class "body")
    (children
     ;; (img (src "images/ship.png"))
     (div
      (class "list")
      (children . ,(map (lambda (item) `(div (class "item") (text ,item) (on (click remove ,item)))) model)))
     (button (text "click") (on (click add))))))

(define (render model)
  `(div
    (children
     ,(header model)
     ,(body model))))

(define run
  (lambda ()
    (define handle
      (lambda (model event data dispatch)
        (match event
          ('add `(,@model ,((native (jref "prompt" (js-window))) (jstring "Name: "))))
          ('remove (remove-if (cut equal? data <>) model))
          (_ model))))
    (let ((root (query-selector window.document "#app")))
      (vdom-create root '() render handle)
      (display "ok\n"))))

(set! .onload (callback run))
