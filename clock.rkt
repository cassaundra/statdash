#lang racket

(require gregor
         gregor/time)

(define (timestamp)
  (~t (current-time) "HH:mm:ss"))

(provide timestamp)
