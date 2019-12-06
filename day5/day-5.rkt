#lang racket

(define (read-input-to-int-vector input)
  (list->vector (map string->number (string-split (car (file->lines input)) ","))))

; Recursive function to process ops and then return value of 0th position upon termination
(define (process-op state-vec op-pos first-pos second-pos dest-pos)
  (let* ([op-code (vector-ref state-vec op-pos)]
         [first-num (vector-ref state-vec (vector-ref state-vec first-pos))]
         [second-num (vector-ref state-vec (vector-ref state-vec second-pos))])
    (cond [(= op-code 1) (vector-set! state-vec (vector-ref state-vec dest-pos) (+ first-num second-num)) (process-op state-vec (+ op-pos 4) (+ op-pos 5) (+ op-pos 6) (+ op-pos 7))]
          [(= op-code 2) (vector-set! state-vec (vector-ref state-vec dest-pos) (* first-num second-num)) (process-op state-vec (+ op-pos 4) (+ op-pos 5) (+ op-pos 6) (+ op-pos 7))]
          [(= op-code 3) #f]
          [(= op-code 4) #f]
          [(= op-code 99) (vector-ref state-vec 0)])))

; Simulates program from a filename
(define (simulate-program input-filename)
  (let* ([program-vec (read-input-to-int-vector input-filename)])
    (process-op program-vec 0 1 2 3)))