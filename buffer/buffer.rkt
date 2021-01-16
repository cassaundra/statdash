#lang racket

(define buffer<%>
  (interface () tick render handle-input))

(provide buffer<%>)
