#lang racket

(define (is-password-valid? p)
  (let ([contains-double #f]
        [p-as-numbers (map char->integer (string->list p))])
    (define (check-neighboring-chars current rest)
      (if (empty? rest)
          #t 
          (let ([next (car rest)])
            (cond 
              [(= next current)
               (set! contains-double #t)
               (check-neighboring-chars next (cdr rest))]
              [(< next current) #f]
              [else (check-neighboring-chars next (cdr rest))]))))
      (if (check-neighboring-chars (car p-as-numbers) (cdr p-as-numbers)) contains-double #f)))

(define (is-password-valid-v2? p)
  (let ([contains-double #f]
        [p-as-numbers (map char->integer (string->list p))])
    (define (check-neighboring-chars current rest)
      (if (empty? rest)
          #t 
          (let ([next (car rest)])
            (cond 
              [(and (= next current) (= (length (filter (lambda (x) (= x current)) p-as-numbers)) 2))
               (set! contains-double #t)
               (check-neighboring-chars next (cdr rest))]
              [(< next current) #f]
              [else (check-neighboring-chars next (cdr rest))]))))
    (if (check-neighboring-chars (car p-as-numbers) (cdr p-as-numbers)) contains-double #f)))

(define (count-valid-passwords passwords validation-proc)
  (let ([valid-passwords 0])
    (for-each (lambda (password)
                (cond [(validation-proc password)
                       (set! valid-passwords (+ 1 valid-passwords))]))
              passwords)
    valid-passwords))

(define (solve-day4-part1)
  (count-valid-passwords (map number->string (range 206938 679129)) is-password-valid?))

(define (solve-day4-part2)
  (count-valid-passwords (map number->string (range 206938 679129)) is-password-valid-v2?))