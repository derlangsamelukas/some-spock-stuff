
(define (js:log x)
  ((native console.log) x))

(define (js:select selector)
  ((native-method document.querySelector) window.document selector))

(define (js:add-listener node event fn)
  ((native-method document.addEventListener) node (jstring event) (callback fn)))

(define (js:document)
  window.document)

(define (js:window)
  (%host-ref window))

(define (js:canvas:get-context canvas type)
  ((native-method HTMLCanvasElement.prototype.getContext) canvas (jstring type)))

(define (js:canvas:width canvas)
  (%property-ref width canvas))

(define (js:canvas:height canvas)
  (%property-ref height canvas))

(define (js:context:fill-style context color)
  (%property-set! fillStyle context (jstring color)))

(define (js:context:stroke-style context color)
  (%property-set! strokeStyle context (jstring color)))

(define (js:context:fill context)
  ((native-method CanvasRenderingContext2D.prototype.fill) context))

(define (js:context:fill-rect context x y width height)
  ((native-method CanvasRenderingContext2D.prototype.fillRect) context x y width height))

(define (js:context:stroke-rect context x y width height)
  ((native-method CanvasRenderingContext2D.prototype.strokeRect) context x y width height))

(define (js:context:clear-rect context x y width height)
  ((native-method CanvasRenderingContext2D.prototype.clearRect) context x y width height))

(define (js:context:clear context)
  (let ((canvas (%property-ref canvas context)))
    (js:context:clear-rect context 0 0 (js:canvas:width canvas) (js:canvas:height canvas))))

(define (js:window:request-animation-frame fn)
  ((native-method window.requestAnimationFrame) (js:window) (callback fn)))

(define (js:keyboard:key event)
  (%property-ref key event))
