(define set-handlers
  (lambda (node dispatch)
    (let ((dataset (%property-ref dataset node)))
      (let ((imonit
             (lambda (name)
               (print (jref name dataset))
               (unless (void? (jref name dataset))
                 (let ((data (if (void? (jref (string-append name "data"))) #f (jref (string-append name "data")))))
                   (dispatch (string->symbol (jref name dataset)) data))))))
        ;; (imonit "click")
        ;; (imonit "input")
        #f
        )
      ;; (on node "click" (lambda ()
      ;;                    (unless (void? (jref "click" dataset))
      ;;                      (let ((data (if (void? (%property-ref clickdata dataset)) #f (string->expr (%property-ref clickdata dataset)))))
      ;;                        (dispatch (string->symbol (%property-ref click dataset)) data)))))
      ;; (on node "input" (lambda ()
      ;;                    (unless (void? (%property-ref input dataset))
      ;;                      (dispatch (string->symbol (%property-ref input dataset)) (%property-ref value node)))))
      )
    ;; (on node "input" (lambda (e) (print (%property-ref target e))))
    ))

(define diff
  (lambda (new old cc)
    (let ((remove old)
          ;; (change '())
          (add '()))
      (letrec ((loop
                (lambda (new)
                  (match new
                    (()
                     ;; (set! remove old)
                     #f)
                    (((k . v) . new)
                     (let ((kv* (assq k old)))
                       (if kv*
                           (if (equal? v (cdr kv*))
                               (set! remove (remove-if (lambda (pair) (equal? k (car pair))) remove))
                               (set! add (cons (cons k v) add)))
                           (set! add (cons (cons k v) add)))
                       (loop new)))))))
        (loop new)
        (cc remove add)))))

(define map-on-handlers
  (lambda (node on-new on-old dispatch)
    (let ((ons (jref "on" node)))
      (diff
       on-new
       on-old
       (lambda (remove add)
         (map
          (lambda (pair)
            (let* ((event (symbol->string (car pair)))
                   (handler (jref event ons)))
              (off node event handler)))
          remove)
         (map
          (lambda (pair)
            (let ((event (symbol->string (car pair)))
                  (handler (callback (lambda () (dispatch (cadr pair) (caddr pair))))))
              (on* node event handler)
              (jset! event ons handler)))
          add))))))

(define create-node-from-attrs
  (lambda (attrs dispatch)
    (let ((node (create-node (symbol->string (car attrs)))))
      (set-handlers node dispatch)
      (check-attrs node '() (cdr attrs) dispatch)
      (jset! "on" node (%))
      node)))

(define assoc-list
  (lambda (key lst)
    (cdr (or (assoc key lst) '(#f)))))

(define check-attrs
  (lambda (node old-attrs new-attrs dispatch)
    (let ((remove-specials (lambda (lst) (remove-if (lambda (attr) (assoc (car attr) '((children) (on) (text)))) lst))))
      (let ((children-new (assoc-list 'children new-attrs))
            (children-old (assoc-list 'children old-attrs))
            (on-new (assoc-list 'on new-attrs))
            (on-old (assoc-list 'on old-attrs))
            (text-new (assoc-list 'text new-attrs))
            (text-old (assoc-list 'text old-attrs))
            (new-attrs (remove-specials new-attrs))
            (old-attrs (remove-specials old-attrs)))
        (letrec ((map-attrs
                  (lambda (new-attrs attr-to-remove)
                    (if (equal? new-attrs '())
                        (map (lambda (attr) (remove-attr! node (symbol->string (car attr)))) attr-to-remove)
                        (unless (and (assoc (caar new-attrs) old-attrs)
                                     (equal? (car new-attrs) (assoc (caar new-attrs) old-attrs)))
                          (if (assoc (caar new-attrs) '((type) (value) (href)))
                              ((native (%property-ref set (%host-ref Reflect))) node (jstring (symbol->string (caar new-attrs))) (jstring (cadar new-attrs)))
                              (attr-set! node (symbol->string (caar new-attrs)) (jstring (cadar new-attrs))))
                          (map-attrs (cdr new-attrs)
                                     (remove-if (lambda (attr) (equal? (caar new-attrs) (car attr))) attr-to-remove)))))))
          (map-attrs new-attrs old-attrs)
          (map-on-handlers node on-new on-old dispatch)
          ;; (map (lambda (on)
          ;;        ((native (%property-ref set (%host-ref Reflect)))
          ;;         (%property-ref dataset node)
          ;;         (jstring (symbol->string (car on)))
          ;;         (jstring (symbol->string (cadr on))))
          ;;        ((native (%property-ref set (%host-ref Reflect)))
          ;;         (%property-ref dataset node)
          ;;         (jstring (string-append (symbol->string (car on)) "data"))
          ;;         (jstring (expr->string (if (not (null? (cddr on))) (caddr on) #f)))))
          ;;      on-new)
          ;; check children
          (if (>= (length children-new) (length children-old))
              (begin (map (lambda (old new index) (map-tree (vector-ref (%property-ref children node) index) node old new)) children-old children-new (range 0 (length children-old)))
                     (map (lambda (attr) (append-child node (create-node-from-attrs attr dispatch))) (nth-cdr (length children-old) children-new)))
              (begin (map (lambda (new old index) (map-tree (vector-ref (%property-ref children node) index) node new old)) children-new children-old (range 0 (length children-new)))
                     (map (lambda () (remove-node (vector-ref (%property-ref children node) 0)))
                          (range 0 (- (length children-old) (length children-new))))))
          ;; check textContent
          (unless (equal? text-old text-new)
            (%property-set! textContent node (jstring (if (equal? text-new '()) "" (car text-new))))))))))

(define map-tree
  (lambda (node parent old new dispatch)
    (cond
     ((equal? old new) #f)
     ((equal? (car old) (car new))
      (check-attrs node (cdr old) (cdr new) dispatch))
     (#t (replace parent (create-node-from-attrs new dispatch) node)))))

(define vdom-create
  (lambda (node model render handle-event)
    (letrec ((vdom '(div))
             (update-vdom
              (lambda (new-model)
                (let ((new-vdom (render new-model)))
                  (map-tree node (parent node) vdom new-vdom dispatch)
                  (set! vdom new-vdom))))
             (dispatch
              (lambda (event data)
                (letrec ((new-model (handle-event model event data dispatch)))
                  (unless (equal? model new-model)
                    (set! model new-model)
                    (update-vdom new-model))))))
      (jset! "on" node (%))
      (update-vdom model)
      dispatch)))

(define with-vdom-app
  (lambda (selector model render handle-event init-fn)
    (set! .onload
      (callback (lambda ()
                  (init-fn
                   (vdom-create (query-selector window.document selector)
                                model
                                render
                                handle-event)))))))
