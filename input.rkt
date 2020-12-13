#lang racket

(require raart/lux-chaos
         "state.rkt")

;; (define (handle-input state evt)
;;   (match evt
;;     ["C-C" #f]
;;     [(struct screen-size-report (height width))
;;      (send state set-size (cons width height))]
;;     [_
;;      #:when (not (eq? #f (send state get-reader)))
;;      (let* ([reader (send state get-reader)]
;;             [value (reader-value reader)])
;;        (set-reader-value! reader (string-append value evt))
;;        state)]
;;     [" " (f state)]
;;     [_ state]))

(define (handle-input evt)
  (let ([state (current-state)])
    (match evt
     [(struct screen-size-report (height width))
      (send state set-size (cons width height))]
     ["C-C" (send state kill)]
     ["=" (f)]
     [else
      (cond
        [(send state current-interaction)
         (handle-input/interaction (send state current-interaction) evt)]
        ;; [(send state current-module)]
        )]
     )))

(define (handle-input/interaction ia evt)
  (let ([reader (send ia get-reader)])
    (when reader
      (cond
        [(string=? evt "C-M") (complete-interaction)]
        [(= 1 (string-length evt)) (set-reader-value! reader (string-append (reader-value reader) evt))])
      ))
  )

(define (f)
  (interactive
   (λ ()
     (define name (read-string "Name?"))
     (system (format "espeak-ng 'Your name is ~a'" name)))))

(provide handle-input)
