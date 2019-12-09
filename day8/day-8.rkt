#lang racket

(require pict)

(define (partition-list full size partitions)
  (if (= (length full) 0)
      partitions
      (partition-list (list-tail full size) size (append partitions (list (take full size))))))

(define (get-partitioned-list full-list partition-size)
  (partition-list full-list partition-size (list)))

(define (find-part1-solution input image-width image-height)
  (let* ([entire-image (string->list (first (file->lines input)))]
         [layers (get-partitioned-list entire-image (* image-width image-height))]
         [fewest-zeroes (* image-width image-height)]
         [solution 0])
    (for-each (lambda (layer)
                (let ([zeroes (count (lambda (num) (char=? num #\0)) layer)])
                  (cond [(< zeroes fewest-zeroes)
                         (set! solution (* (count (lambda (num) (char=? num #\1)) layer) (count (lambda (num) (char=? num #\2)) layer)))
                         (set! fewest-zeroes zeroes)])))
              layers)
    solution))

(define (solve-day8-part1)
  (find-part1-solution "input.txt" 25 6))

(define (solve-day8-part2)
  (get-partitioned-list (decode-image "input.txt" 25 6) 25))

(define (decode-image image image-width image-height)
  (let ([layer-table (build-layer-table (map (lambda (c) (string->number (string c))) (string->list (first (file->lines image)))) image-width image-height)]
        [decoded-image (list)])
    (for-each (lambda (pixel)
                (set! decoded-image (append decoded-image (list (get-first-visible-pixel (hash-ref layer-table pixel))))))
              (hash-keys layer-table))
    decoded-image))

(define (build-layer-table image image-width image-height)
  (let ([pixel-count (* image-width image-height)]
        [layer-table (make-hash)])
    (for-each (lambda (pixel-pos)
                (hash-set! layer-table pixel-pos (build-layer (list) image pixel-count pixel-pos)))
              (build-list pixel-count values))
    layer-table))

(define (build-layer layer-list remaining-image pixel-count pixel-pos)
  (if (= (length remaining-image) 0)
      layer-list
      (build-layer (append layer-list (list (list-ref remaining-image pixel-pos))) (list-tail remaining-image pixel-count) pixel-count pixel-pos)))

(define (get-first-visible-pixel layer)
  (if (not (= 2 (first layer)))
      (first layer)
      (get-first-visible-pixel (cdr layer))))

(define (pixel v)
  (if (zero? v) (filled-rectangle 4 4) (rectangle 4 4)))

(define (pixel-row lst)
  (apply hc-append (map pixel lst)))

(define (show-image layers)
  (apply vc-append (map pixel-row layers)))
  
(show-image (solve-day8-part2))