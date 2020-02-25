(import spiffy)
(import intarweb)
(import uri-common)
(import matchable)
(import (chicken irregex))
(import spock)
(import (chicken file))
(import (chicken port))
(import sxml-transforms)
(import sxml-serializer)
(import (chicken file))

(define (find-file name)
  (string-append "src/" name ".scm"))

(define (compile-file file)
  (with-output-to-string
    (lambda () (spock file))))

(define (bundle-spock-files files)
  (with-output-to-file "tmp.scm"
    (lambda ()
      (let* ((file (open-input-file "transform.scm"))
             (thunk (read file)))
        (close-input-port file)
        (write ((eval thunk))))))
  (with-output-to-string
    (lambda ()
      (apply
       spock
       (cons "tmp.scm" (map find-file files))))))

(define (render-index-page)
  (let* ((file (open-input-file "index.lisp"))
         (sxml (read file)))
    (close-input-port file)
    (string-append
     "<!DOCTYPE html>\n"
     (serialize-sxml
      (pre-post-order*
       sxml
       `((bundle . ,(lambda (x xs) (bundle-spock-files xs)))
         ,@alist-conv-rules*))
      method: 'html))))

(define (send-js body)
  (send-response
   headers: '((content-type "text/javascript"))
   status: 'ok
   body: body))

(define (with-js-file string true false)
  (let ((match-data (irregex-match (string->irregex "(.*)\\.js$") string)))
    (if (irregex-match-data? match-data)
        (true (irregex-match-substring match-data 1))
        (false))))

(define (with-existence file true false)
  (if (and (file-exists? file)
           (not (directory-exists? file)))
      (true file)
      (false)))

(define (try-compilation cc)
  (lambda (name)
    (with-existence
     (find-file name)
     (lambda (file) (send-js (compile-file file)))
     cc)))

(define (js-handler cc)
  (with-js-file
   (or (car (reverse (uri-path (request-uri (current-request))))) "")
   (try-compilation cc)
   cc))

(define (handler cc)
  (match (uri-path (request-uri (current-request)))
    ((/ "")
     (send-response
      status: 'ok
      body: (render-index-page)
      headers: '((content-type "text/html"))))
    (else
     (js-handler cc))))

(parameterize
    ((server-port 4300)
     (root-path "./public")
     (vhost-map `((".*" . ,handler))))
  (start-server))
