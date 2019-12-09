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

(define (process-op state-vec op-pos inputs output-signal)
  (let* ([op (parse-full-op-code-into-operation (vector-ref state-vec op-pos))]
         [op-code (operation-op-code op)]
         [parameter-modes (operation-parameter-modes op)])
    (cond [(= op-code 1)
           (let* ([first-num (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                  [second-num (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))]
                  [destination-address (vector-ref state-vec (+ op-pos 3))])
           (vector-set! state-vec destination-address (+ first-num second-num))
           (process-op state-vec (+ op-pos 4) inputs output-signal))]
          [(= op-code 2)
           (let* ([first-num (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                  [second-num (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))]
                  [destination-address (vector-ref state-vec (+ op-pos 3))])
           (vector-set! state-vec destination-address (* first-num second-num))
           (process-op state-vec (+ op-pos 4) inputs output-signal))]
          [(= op-code 3)
           (let ([write-address (vector-ref state-vec (+ op-pos 1))])
             (vector-set! state-vec write-address (first inputs))
             (if (> (length inputs) 1) (set! inputs (list (second inputs))) #f)
             (process-op state-vec (+ op-pos 2) inputs output-signal))]
          [(= op-code 4)
           (let ([read-address (vector-ref state-vec (+ op-pos 1))])
             (list #t (vector-ref state-vec read-address) (+ op-pos 2)))]
          [(= op-code 5)
           (let ([num-to-check (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                 [instruction-pointer-dest (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))])
             (cond [(not (= num-to-check 0))
                    (process-op state-vec instruction-pointer-dest inputs output-signal)]
                   [else
                    (process-op state-vec (+ op-pos 3) inputs output-signal)]))]
          [(= op-code 6)
           (let ([num-to-check (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                 [instruction-pointer-dest (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))])
             (cond [(= num-to-check 0)
                    (process-op state-vec instruction-pointer-dest inputs output-signal)]
                   [else
                    (process-op state-vec (+ op-pos 3) inputs output-signal)]))]
          [(= op-code 7)
           (let* ([first-num (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                  [second-num (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))]
                  [destination-address (vector-ref state-vec (+ op-pos 3))])
             (vector-set! state-vec destination-address (if (< first-num second-num) 1 0))
             (process-op state-vec (+ op-pos 4) inputs output-signal))]
          [(= op-code 8)
           (let* ([first-num (if (= (first parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 1))) (vector-ref state-vec (+ op-pos 1)))]
                  [second-num (if (= (second parameter-modes) 0) (vector-ref state-vec (vector-ref state-vec (+ op-pos 2))) (vector-ref state-vec (+ op-pos 2)))]
                  [destination-address (vector-ref state-vec (+ op-pos 3))])
             (vector-set! state-vec destination-address (if (= first-num second-num) 1 0))
             (process-op state-vec (+ op-pos 4) inputs output-signal))]
          [(= op-code 99) (list #f output-signal)])))

; Simulates program from a filename
(define (simulate-program input-filename)
  (let* ([program-vec (read-input-to-int-vector input-filename)])
    (process-op program-vec 0)))

(define (get-max-phase-setting)
  (let* ([possible-phase-sequences (permutations (list 0 1 2 3 4))]
         [base-state-vec (read-input-to-int-vector "input.txt")]
         [max-signal 0])
    (for-each (lambda (phase-sequence)
                (let* ([current-input-signal 0])
                  (for-each (lambda (phase)
                              (set! current-input-signal (process-op (vector-copy base-state-vec) 0 (list phase current-input-signal))))
                            phase-sequence)
                  (if (> current-input-signal max-signal) (set! max-signal current-input-signal) #f)))
              possible-phase-sequences)
    max-signal))

(define (get-looped-max-phase-setting)
  (let* ([possible-phase-sequences (permutations (list 5 6 7 8 9))]
         [base-state-vec (read-input-to-int-vector "input.txt")]
         [max-signal 0])
    (for-each (lambda (phase-sequence)
                (let ([signal (get-signal-for-sequence phase-sequence base-state-vec)])
                  (if (> signal max-signal) (set! max-signal signal) #f)))
              possible-phase-sequences)
    max-signal))

(define (get-signal phase-sequence machine-states instruction-pointers phase-pos current-signal)
  (let* ([inputs (if (= (list-ref instruction-pointers phase-pos) 0) (list (list-ref phase-sequence phase-pos) current-signal) (list current-signal))]
         [signal (process-op (list-ref machine-states phase-pos) (list-ref instruction-pointers phase-pos) inputs current-signal)]
         [old-phase-pos phase-pos])
    (set! phase-pos (if (= (+ 1 phase-pos) (length phase-sequence)) 0 (+ 1 phase-pos)))
    (cond [(eqv? #f (first signal)) (second signal)]
          [else (get-signal phase-sequence machine-states (list-set instruction-pointers old-phase-pos (third signal)) phase-pos (second signal))])))

(define (get-signal-for-sequence phase-sequence base-state-vec)
  (let* ([current-signal 0]
         [machine-states (make-list 5 (vector-copy base-state-vec))]
         [instruction-pointers (make-list 5 0)])
    (get-signal phase-sequence machine-states instruction-pointers 0 current-signal)))
  
(define (solve-day7-part1)
  (get-max-phase-setting))

(define (solve-day7-part2)
  (get-looped-max-phase-setting))
