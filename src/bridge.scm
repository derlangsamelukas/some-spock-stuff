(define then
  (let ((then (native-method Promise.prototype.then)))
    (lambda (p fn)
      (then p (callback fn)))))

(define catch
  (let ((catch (native-method Promise.prototype.catch)))
    (lambda (p fn)
      (catch p (callback fn)))))

(define loggit (native console.log))

(define append-child (native-method document.appendChild))
(define query-selector (native-method document.querySelector))
(define create-node
  (lambda (name)
    ((native-method document.createElement) (%host-ref document) (jstring name))))
(define remove-node
  (lambda (node)
    ((native-method (%property-ref remove node)) node)))
(define replace (native-method document.replaceChild))

(define attr-set!
  (lambda (node key value)
    ((native-method (%property-ref setAttribute node)) node (jstring key) (jstring value))))

(define remove-attr!
  (lambda (node key)
    ((native-method (%property-ref removeAttribute node)) node (jstring key))))

(define attr-ref
  (lambda (node key)
    ((native-method (%property-ref getAttribute node)) node (jstring key))))

(define expr->string
  (lambda (expr)
    (with-output-to-string (lambda () (write expr)))))

(define string->expr
  (lambda (text)
    (with-input-from-string text read)))

(define fetchit
  (lambda (url data cc)
    (let ((p ((native (%host-ref fetch))
              url
              (if (or (void? data) (void? cc))
                  (%)
                  (% "method" "POST" "body" (jstring data))))))
      (then p (if (void? cc) data cc)))))

(define send-list
  (lambda (url lst cc)
    (fetchit url (with-output-to-string (lambda () (write lst)))
             (lambda (x)
               (then ((native-method (%property-ref text x)) x)
                     (lambda (text)
                       (with-input-from-string text (o cc read))))))))

(define fetch-list
  (lambda (url cc)
    (fetchit
     url
     (lambda (response)
       (if (= 200 (jref "status" response))
           ((js-method
              "then"
              ((js-method "text" response)))
            (lambda (text)
              (with-input-from-string text (o cc read))))
           (cc #f))))))

(define div
  (lambda (class)
    (let ((div (create-node "div")))
      (%property-set! className div (if (void? class) "" class))
      div)))

(define text
  (lambda (text)
    (let ((div (div "text")))
      (%property-set! textContent div (jstring text))
      div)))

(define title
  (lambda (title)
    (let ((div (text title)))
      (%property-set! className div "title")
      div)))

(define on*
  (lambda (node name native-fn)
    ((native-method (%property-ref addEventListener node)) node (jstring name) native-fn)))

(define on
  (lambda (node name fn)
    (on* node name (callback fn))))

(define off
  (lambda (node name native-fn)
    ((native-method (%property-ref removeEventListener node)) node (jstring name) native-fn)))

(define parent
  (lambda (node)
    (%property-ref parentNode node)))

(define nth-cdr
  (lambda (x list)
    (if (or (zero? x) (equal? '() list))
        list
        (nth-cdr (- x 1) (cdr list)))))

(define range
  (lambda (start end)
    (letrec ((f (lambda (x)
                (if (= x end)
                    '()
                    (cons x (f (+ x 1)))))))
      (f start))))

(define fold
  (lambda (fn z lst)
    (if (equal? '() lst)
        z
        (fold fn (fn z (car lst)) (cdr lst)))))

(define remove-if
  (lambda (fn list)
    (reverse (fold (lambda (list item) (if (fn item) list (cons item list))) '() list))))

(define remove-if-not
  (lambda (fn list)
    (remove-if (compl fn) list)))

(define replace-assoc
  (lambda (key lst value)
    (cons (list key value) (remove-if (lambda (x) (equal? key (car x))) lst))))

(define-syntax λ
  (syntax-rules ()
    ((λ params . body)
     (lambda params . body))))

(define timeout
  (lambda (fn delay)
    ((native (%host-ref setTimeout)) (callback fn) delay)))

(define interval
  (lambda (fn delay)
    ((native (%host-ref setInterval)) (callback fn) delay)))

(define jref
  (lambda (key object)
    ((native (%property-ref get (%host-ref Reflect))) object (jstring key))))

(define jset!
  (lambda (key object value)
    ((native (%property-ref set (%host-ref Reflect))) object (jstring key) value)))

(define js-method
  (lambda (method object)
    (lambda args (apply (native-method (jref (jstring method) object)) (cons object args)))))
