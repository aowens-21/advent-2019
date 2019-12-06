#lang racket

(require racket/set)

(define (parse-map-into-orbit-table filename)
  (let* ([orbit-strings (file->lines filename)]
         [orbit-table (make-hash)])
    (for-each (lambda (orbit-string)
                (let* ([orbit (string-split orbit-string ")")]
                      [orbited-object (first orbit)]
                      [orbital-object (second orbit)])
                      (hash-set! orbit-table orbital-object orbited-object)))
              orbit-strings)
    orbit-table))

; Goes through each object and recursively counts its indirect and direct orbits
(define (sum-all-orbits orbit-table)
  (define (sum-orbits object total)
    (if (string=? object "COM")
        (+ total 1)
        (sum-orbits (hash-ref orbit-table object) (+ total 1))))
  (let ([sum 0])
    (for-each (lambda (object)
                (set! sum (+ sum (sum-orbits (hash-ref orbit-table object) 0))))
              (hash-keys orbit-table))
    sum))

(define (get-list-of-orbits object orbit-table init-list)
  (if (string=? object "COM")
      (append init-list (list "COM"))
      (get-list-of-orbits (hash-ref orbit-table object) orbit-table (append init-list (list object)))))

(define (get-dist-to-orbited-object current-object destination orbit-table dist)
  (if (string=? current-object destination)
      dist
      (get-dist-to-orbited-object (hash-ref orbit-table current-object) destination orbit-table (+ 1 dist))))

(define (find-transfer-count-between object1 object2 orbit-table)
  (let* ([object1-orbits (get-list-of-orbits object1 orbit-table (list))]
         [object2-orbits (get-list-of-orbits object2 orbit-table (list))]
         [common-orbits (filter (lambda (object) (member object object2-orbits)) object1-orbits)]
         [least-common-orbit (first common-orbits)])
    (+ (get-dist-to-orbited-object object1 least-common-orbit orbit-table 0) (get-dist-to-orbited-object object2 least-common-orbit orbit-table 0))))

(define (solve-day6-part1)
  (sum-all-orbits (parse-map-into-orbit-table "input.txt")))

(define (solve-day6-part2)
  ; Subtract 2 because its quicker than finding each one's adjacent orbited object
  (- (find-transfer-count-between "YOU" "SAN" (parse-map-into-orbit-table "input.txt")) 2)
  