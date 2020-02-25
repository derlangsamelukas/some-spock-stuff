(lambda ()
  (define (define-structure name slots)
    (let* ((name (symbol->string name))
           (make-body (map (lambda (slot) (if (symbol? slot) #f (cadr slot))) slots))
           (getters
            (let ((i -1))
              (map (lambda (slot)
                     (let ((slot (if (symbol? slot) slot (car slot))))
                       (set! i (+ 1 i))
                       `(define (,(string->symbol (string-append name "-" (symbol->string slot))) struct)
                          (list-ref struct ,i))))
                   slots)))
           (setters
            (let ((i -1))
              (map (lambda (slot)
                     (let ((slot (if (symbol? slot) slot (car slot))))
                       (set! i (+ 1 i))
                       `(define (,(string->symbol (string-append name "-" (symbol->string slot) "!")) struct value)
                          (set-car! (list-tail struct ,i) value))))
                   slots))))
      `(begin
         (define (,(string->symbol (string-append "make-" name)))
           (list ,@make-body))
         ,@getters
         ,@setters)))

  (define-structure 'game-state
    '((moon-angle (/ Math.PI 2)) (player-velocity '(0 0 0)) (lock-camera #t))))

