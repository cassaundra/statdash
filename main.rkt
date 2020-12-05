#lang racket

(require lux
         raart/lux-chaos
         "view.rkt")

(define (with-clean-handle proc)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (system "stty -raw && clear")
                     (displayln exn))])
    (proc)))

(module+ main
  (void (with-clean-handle
          (thunk (call-with-chaos
                  (make-raart)
                  (thunk
                   (fiat-lux (make-terminal-view))))))))
