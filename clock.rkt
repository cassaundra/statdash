#lang racket

(require gregor
         gregor/time)

(define (timestamp)
  (~t (current-time) "hh:mm:ss"))

(provide timestamp)
