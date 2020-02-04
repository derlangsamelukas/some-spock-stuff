
(define run
  (lambda ()
    (define render
      (lambda (model)
        '(div (text "hej"))))
    (define handle
      (lambda (model)
        model))
    (let ((root (query-selector window.document "#app")))
      (vdom-create root '() render handle)
      (display "hej"))))

(set! .onload (callback run))
