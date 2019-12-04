#lang racket

; For part 1 of day 1
(define (compute-fuel-for-module mass)
  (- (floor (/ mass 3)) 2))

; For part 2 of day 1
(define (compute-nested-fuel-for-module mass)
  (let ([fuel (- (floor (/ mass 3)) 2)])
    (cond [(<= fuel 0) 0]
          [else (+ fuel (compute-nested-fuel-for-module fuel))])))

; Takes input and function that should be used to compute fuel for each module
(define (compute-total-fuel fuel-computer input-name)
  (let* ([mass-list (map string->number (file->lines input-name))]
         [fuel-list (map fuel-computer mass-list)])
    (foldl + 0 fuel-list)))

; Part 1 Solution
(compute-total-fuel compute-fuel-for-module "input.txt")

; Part 2 Solution
(compute-total-fuel compute-nested-fuel-for-module "input.txt")

