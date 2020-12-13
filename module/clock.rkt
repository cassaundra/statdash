#lang racket

(require "module.rkt"
         gregor
         gregor/time
         raart)

;; (define (seven-segment-shapes)
;;   #((list #t #t #t #t #t #t #f)
;;     (list #f #t #t #f #f #f #f)
;;     (list #t #t #f #t #t #f #t)
;;     (list #t #t #t #t #f #f #t)
;;     (list #t #f #t #t #f #t #t)
;;     (list #t #f #t #t #t #t #t)
;;     (list #t #t #t #f #f #f #f)
;;     (list #t #t #t #t #t #t #t)
;;     (list #t #t #t #f #f #t #t)))

(define clock%
  (class* object% (module-interface)
    (super-new)

    (define current-timer #f)

    (define/public (render size)
      (draw-clock))

    (define/public (handle-input evt)
      void)
    ))

(define (draw-clock)
  (vappend #:halign 'center
           (text (~t (current-time) "HH:mm:ss"))
           (text "✨🌙 ")))

;; (define (start-timer state)
;;   (send state interact
;;         (lambda (ia)
;;           (let* ([value (send ia read-string "Duration (in minutes)?")]
;;                  [minutes (min (sub1 1440) (string->number value))])
;;             (set! current-timer (+ minutes (current-clock)))))))
;; (define (stop-timer)
;;   (set! current-timer #f))

(provide clock%)
