#lang racket

(struct operation (op-code parameter-modes))

(define (read-input-to-int-vector input)
  (list->vector (map string->number (string-split (car (file->lines input)) ","))))

(define (parse-full-op-code-into-operation full-op-code)
  (let ([op-as-list (string->list (number->string full-op-code))])
    (cond [(<= (length op-as-list) 2)
           (operation full-op-code (make-list (get-op-code-arity full-op-code) 0))]
          [else
           (let ([op-code (string->number (list->string (list-tail op-as-list (- (length op-as-list) 2))))])
             (operation
              op-code
              (cond [(= (- (length op-as-list) 2) (get-op-code-arity op-code))
                     (map (lambda (num-char) (string->number (string num-char))) (reverse (take op-as-list (- (length op-as-list) 2))))]
                    [else
                     (append (map (lambda (num-char) (string->number (string num-char))) (reverse (take op-as-list (- (length op-as-list) 2))))
                             (make-list (- (get-op-code-arity op-code) (- (length op-as-list) 2)) 0))])))])))

(define (get-op-code-arity op-code)
  (cond [(= op-code 1) 3]
        [(= op-code 2) 3]
        [(= op-code 3) 1]
        [(= op-code 4) 1]
        [(= op-code 5) 2]
        [(= op-code 6) 2]
        [(= op-code 7) 3]
        [(= op-code 8) 3]
        [(= op-code 99) 0]))

(define (process-op state-vec op-pos)
  (let* ([op (parse-full-op-code-into-operation (vector-ref state-vec op-pos))]
         [op-code (operation-op-code op)]
         [parameter-modes (operation-parameter-modes op)])
    (cond [(= op-code 1)
           (let* ([first-num (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                  [second-num (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))]
                  [destination-address (vector-ref state-vec (+ op-pos 3))])
           (vector-set! state-vec destination-address (+ first-num second-num))
           (process-op state-vec (+ op-pos 4)))]
          [(= op-code 2)
           (let* ([first-num (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                  [second-num (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))]
                  [destination-address (vector-ref state-vec (+ op-pos 3))])
           (vector-set! state-vec destination-address (* first-num second-num))
           (process-op state-vec (+ op-pos 4)))]
          [(= op-code 3)
           (let ([write-address (vector-ref state-vec (+ op-pos 1))])
             (vector-set! state-vec write-address (string->number (read-line)))
             (process-op state-vec (+ op-pos 2)))]
          [(= op-code 4)
           (let ([read-address (vector-ref state-vec (+ op-pos 1))])
             (displayln (vector-ref state-vec read-address))
             (process-op state-vec (+ op-pos 2)))]
          [(= op-code 5)
           (let ([num-to-check (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                 [instruction-pointer-dest (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))])
             (cond [(not (= num-to-check 0))
                    (process-op state-vec instruction-pointer-dest)]
                   [else
                    (process-op state-vec (+ op-pos 3))]))]
          [(= op-code 6)
           (let ([num-to-check (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                 [instruction-pointer-dest (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))])
             (cond [(= num-to-check 0)
                    (process-op state-vec instruction-pointer-dest)]
                   [else
                    (process-op state-vec (+ op-pos 3))]))]
          [(= op-code 7)
           (let* ([first-num (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                  [second-num (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))]
                  [destination-address (vector-ref state-vec (+ op-pos 3))])
             (vector-set! state-vec destination-address (if (< first-num second-num) 1 0))
             (process-op state-vec (+ op-pos 4)))]
          [(= op-code 8)
           (let* ([first-num (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                  [second-num (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))]
                  [destination-address (vector-ref state-vec (+ op-pos 3))])
             (vector-set! state-vec destination-address (if (= first-num second-num) 1 0))
             (process-op state-vec (+ op-pos 4)))]
          [(= op-code 99) (displayln "Done")])))

; Simulates program from a filename
(define (simulate-program input-filename)
  (let* ([program-vec (read-input-to-int-vector input-filename)])
    (process-op program-vec 0)))

(define (solve-day5)
  (simulate-program "input.txt"))
