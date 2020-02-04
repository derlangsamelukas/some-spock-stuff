
(define run
  (lambda ()
    (define render
      (lambda (model)
        (let ((on
               (match model
                 ("naaaa" '((on (click naa "nu"))))
                 ("nu" '((on (click naa "ni"))))
                 (x '()))))
          `(div ,@on (text ,model)))))
    (define handle
      (lambda (model event data dispatch)
        (match event
          ('naa data)
          (_ model))))
    (let ((root (query-selector window.document "#app")))
      (vdom-create root "naaaa" render handle)
      (display "ok\n"))))

(set! .onload (callback run))
