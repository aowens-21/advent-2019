#lang racket

(require racket/set)

; Represents a single movement like R34 as direction=R and distance=34
(struct wire-movement (direction distance))

; Represents a single line in a wire
(struct line (p1 p2 length))

; Parse a list of movement strings into a list of movement structures
(define (parse-wire-movements movement-string)
  (map
   (lambda (single-movement)
     (let ([movement-char-list (string->list single-movement)])
       (wire-movement (car movement-char-list) (string->number (list->string (cdr movement-char-list))))))
   (string-split movement-string ",")))

; Get movement structure lists from input
(define (load-wire-movements-from-file filename)
  (map parse-wire-movements (file->lines filename)))

; Logs each position from a line in the wire into a given hash table 
(define (log-to-hash-table! table wire-num start-pos end-pos)
  (cond [(= (first start-pos) (first end-pos)) ; x is the same so loop over y changes
         (let* ([x (first start-pos)]
                [start-y (min (second start-pos) (second end-pos))]
                [end-y (max (second start-pos) (second end-pos))])
           (for ([current-y (in-range start-y (+ end-y 1))])
             (if (and (hash-has-key? table (list x current-y))
                      (not (and (= current-y 0) (= x 0))))
                 (hash-set! table (list x current-y) (set-add (hash-ref table (list x current-y)) wire-num))
                 (hash-set! table (list x current-y) (set wire-num)))))]
        [(= (second start-pos) (second end-pos)) ; y is the same so loop over x changes
         (let* ([y (second start-pos)]
                [start-x (min (first start-pos) (first end-pos))]
                [end-x (max (first start-pos) (first end-pos))])
           (for ([current-x (in-range start-x (+ end-x 1))])
             (if (and (hash-has-key? table (list current-x y))
                      (not (and (= current-x 0) (= y 0))))
                 (hash-set! table (list current-x y) (set-add (hash-ref table (list current-x y)) wire-num))
                 (hash-set! table (list current-x y) (set wire-num)))))]))

; Build hash table with points and which lines have crossed them
(define (build-wire-hash-table)
  (let* ([wires (load-wire-movements-from-file "input.txt")]
         [position-table (make-hash)])
    (for-each (lambda (wire wire-num)
                (let ([current-pos (list 0 0)])
                  (for-each (lambda (movement)
                              (let* ([start-pos current-pos]
                                     [end-pos (cond [(char=? (wire-movement-direction movement) #\U)
                                                     (list (first start-pos) (+ (wire-movement-distance movement) (second start-pos)))]
                                                    [(char=? (wire-movement-direction movement) #\D)
                                                     (list (first start-pos) (- (second start-pos) (wire-movement-distance movement)))]
                                                    [(char=? (wire-movement-direction movement) #\L)
                                                     (list (- (first start-pos) (wire-movement-distance movement)) (second start-pos))]
                                                    [(char=? (wire-movement-direction movement) #\R)
                                                     (list (+ (first start-pos) (wire-movement-distance movement)) (second start-pos))])])
                                (log-to-hash-table! position-table wire-num start-pos end-pos)
                                (set! current-pos end-pos)))
                            wire)))
              wires (build-list (length wires) values))
    position-table))

(define (get-manhattan-dist-between p1 p2)
  (+ (abs (- (first p1) (first p2))) (abs (- (second p1) (second p2)))))

(define (get-intersections position-table)
  (map (lambda (point-wire-pair)
                               (car point-wire-pair))
                             (filter (lambda (point-wire-pair)
                                       (set=? (cdr point-wire-pair) (set 0 1)))
                                     (hash->list position-table))))

; Find smallest intersection point from the center
(define (find-closest-intersection-to-center position-table)
  (let* ([intersections (get-intersections position-table)]
         [closest-dist 100000000])
    (for-each (lambda (position)
                (let ([dist (get-manhattan-dist-between position (list 0 0))])
                  (if (<= dist closest-dist) (set! closest-dist dist) #f)))
              intersections)
    closest-dist))

; Turn list of wire movements into list of lines
(define (build-lines-from-wires wires)
  (map (lambda (wire)
              (let ([current-pos (list 0 0)])
                (map (lambda (movement)
                              (let* ([start-pos current-pos]
                                     [end-pos (cond [(char=? (wire-movement-direction movement) #\U)
                                                     (list (first start-pos) (+ (wire-movement-distance movement) (second start-pos)))]
                                                    [(char=? (wire-movement-direction movement) #\D)
                                                     (list (first start-pos) (- (second start-pos) (wire-movement-distance movement)))]
                                                    [(char=? (wire-movement-direction movement) #\L)
                                                     (list (- (first start-pos) (wire-movement-distance movement)) (second start-pos))]
                                                    [(char=? (wire-movement-direction movement) #\R)
                                                     (list (+ (first start-pos) (wire-movement-distance movement)) (second start-pos))])])
                                (set! current-pos end-pos)
                                (line start-pos end-pos (wire-movement-distance movement))))
                            wire)))
            wires))

(define (point-on-line? point line)
  (let* ([start-line (line-p1 line)]
         [end-line (line-p2 line)])
    (cond [(= (first start-line) (first end-line))
           (and (= (first point) (first start-line))
                (and (>= (second point) (min (second start-line) (second end-line)))
                     (< (second point) (max (second start-line) (second end-line)))))]
          [(= (second start-line) (second end-line))
           (and (= (second point) (second start-line))
                (and (>= (first point) (min (first start-line) (first end-line)))
                     (< (first point) (max (first start-line) (first end-line)))))])))

(define (count-steps-to-point point wire-as-lines)
  (define (count-steps line remaining-lines total)
    (cond [(point-on-line? point line)
           (+ total (get-manhattan-dist-between point (line-p1 line)))] ; Hacky, can only use manhattan dist because we know they're straight lines, otherwise this would fail
          [else (count-steps (car remaining-lines) (cdr remaining-lines) (+ total (line-length line)))]))
  (count-steps (car wire-as-lines) (cdr wire-as-lines) 0))

; Find intersection with lowest combined steps
(define (find-least-effort-intersection wires-as-lines intersections)
  (let ([least-combined-steps 100000000])
    (for-each (lambda (intersection)
                (let ([combined-steps (foldl + 0 (map (curry count-steps-to-point intersection) wires-as-lines))])
                  (if (<= combined-steps least-combined-steps)
                      (set! least-combined-steps combined-steps)
                      #f)))
              intersections)
    least-combined-steps))

(define (solve-day3-part1)
  (find-closest-intersection-to-center (build-wire-hash-table)))

(define (solve-day3-part2)
  (find-least-effort-intersection
   (build-lines-from-wires (load-wire-movements-from-file "input.txt"))
   (get-intersections (build-wire-hash-table))))