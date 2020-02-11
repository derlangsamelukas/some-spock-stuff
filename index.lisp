(html
 (head
  (title "Hej")
  (meta (@ (charset "utf-8")))
  (meta (@ (name "viewport") (content "user-scalable=no, initial-scale=1, maximum-scale=1, minimum-scale=1, width=device-width")))
  ;; (link (@ (href "vdom-test.css") (type "text/css") (rel "stylesheet")))
  (link (@ (href "canvas.css") (type "text/css") (rel "stylesheet"))))
 (body
  (canvas (@ (id screen) (width 500) (height 500)) "")
  (div (@ (id "app")) "")
  (script (@ (src "spock-runtime.js") (type "text/javascript")) "")
  (script (@ (src "three.min.js") (type "text/javascript")) "")
  (script (@ (type "text/javascript")) (bundle "match" "js" "main-loop" "run"))
  ;; (script (@ (type "text/javascript")) (bundle "match" "bridge" "vdom" "run-vdom"))
  ))
