#lang racket

(require "input.rkt"
         "buffer/clock.rkt"
         "state.rkt"
         lux
         raart)

(define (make-terminal-view)
  (let ([state (new state%)])
    (send state push-buffer (new clock%))
    (terminal-view state #f)))

(struct terminal-view (state size)
  #:methods gen:word
  [(define (word-fps w)
     30.0)
   (define (word-label w ft)
     "statdash")
   (define (word-event w evt)
     (match-define (terminal-view state size) w)
     (match evt
       [(struct screen-size-report (height width))
        (terminal-view state (cons width height))]
       [else (begin
               (send state handle-input evt)
               (and (send state is-alive) w))]))

   (define (word-tick w)
     ;; (for ([buffer (send (terminal-view-state w) get-buffers)])
     ;;   (send buffer tick))
     (send (terminal-view-state w) tick)
     w)
   (define (word-output w)
     (match-define (terminal-view state size) w)
     (if size
         (send state render size)
         (blank)))
   (define (word-return w)
     w)])

;; (define-syntax (bind-draw stx)
;;   (syntax-case stx ()
;;     [(_ [value-name value] proc)
;;      #'(let ([value-name value])
;;          (if value-name
;;             ((lambda (value-name) proc) value-name)
;;             (blank)))]))

;; (define (draw-home state)
;;   (define size (xor (void? state) (send state get-size)))
;;   (bind-draw [size size]
;;              (without-cursor
;;               (place-at (matte (car size) (cdr size)
;;                                (vappend #:halign 'center
;;                                         (send (send state get-focused-buffer) render size)))
;;                         0 0 (draw-interaction state size))))
;;   )

;; (define (draw-interaction state size)
;;   (bind-draw [ia (send state current-interaction)]
;;              (send ia render size)))

(provide make-terminal-view)
