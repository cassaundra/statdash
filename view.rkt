#lang racket

(require "input.rkt"
         "buffer/clock.rkt"
         "state.rkt"
         lux
         raart)

(define (make-terminal-view)
  (let ([state (new state%)])
    (send state push-buffer (new clock%))
    (terminal-view state)))

(struct terminal-view (state)
  #:methods gen:word
  [(define (word-fps w)
     60.0)
   (define (word-label w ft)
     "statdash")
   (define (word-event w e)
     (let ([state (terminal-view-state w)])
       (parameterize ([current-state state])
         (handle-input e))
       (and (send state is-alive) (terminal-view state)))
     )

   (define (word-tick w)
     (for ([buffer (send (terminal-view-state w) get-buffers)])
       (send buffer tick))
     w)
   (define (word-output w)
     ;; (match-define (state size _) (terminal-view-state w))
     (define state (terminal-view-state w))
     ;; (define size (send state get-size))

     (parameterize ([current-state state])
       (draw-home state))
       ;; (without-cursor
       ;;  (place-at (matte (car size) (cdr size)
       ;;                   (vappend #:halign 'center
       ;;                            (draw-clock)))
       ;;            0 0 (text "hi")))
       )
   (define (word-return w)
     w)])

(define-syntax (bind-draw stx)
  (syntax-case stx ()
    [(_ [value-name value] proc)
     #'(let ([value-name value])
         (if value-name
            ((lambda (value-name) proc) value-name)
            (blank)))]))

(define (draw-home state)
  (define size (xor (void? state) (send state get-size)))
  (bind-draw [size size]
             (without-cursor
              (place-at (matte (car size) (cdr size)
                               (vappend #:halign 'center
                                        (send (send state get-focused-buffer) render size)))
                        0 0 (draw-interaction state size))))
  )

(define (draw-interaction state size)
  (bind-draw [ia (send state current-interaction)]
             (send ia render size)))

(provide make-terminal-view)
