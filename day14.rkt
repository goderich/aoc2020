#lang racket

(require threading)

(struct mem (address value))

(define (read-num line)
  (mem
   (~>> line
        (string-split)
        (first)
        (string->list)
        (filter char-numeric?)
        (list->string)
        (string->number))
   (~>> line
        (string-split)
        (third)
        (string->number))))

(define (pad numstr)
    (define zeros (make-string (- 36 (string-length numstr)) #\0))
    (format "~a~a" zeros numstr))

(define (apply-mask num mask)
  (define (masked-charlist numstr)
    (for/list ((n (in-string numstr))
               (m (in-string mask)))
      (if (eq? m #\X) n m)))
  (~>> num
       (number->string _ 2)
       (pad)
       (masked-charlist)
       (list->string)
       (string->number _ 2)))

(for/fold ((dict (make-immutable-hasheq))
           (mask "")
           #:result (apply + (hash-values dict)))
          ((line (file->lines "inputs/day14.txt")))
  (cond
   ((string-prefix? line "mask")
    (values dict (third (string-split line))))
   (else
    (define memcell (read-num line))
    (define key (mem-address memcell))
    (define val (apply-mask (mem-value memcell) mask))
    (values (hash-set dict key val) mask))))

;; Part 2

(define (decode-memory-addresses num mask)
  (define (memory-addresses numstr)
    (for/fold ((acc '(())))
              ((n (in-string numstr))
               (m (in-string mask)))
      (match m
        (#\0 (map (curry cons n) acc))
        (#\1 (map (curry cons #\1) acc))
        (#\X (append (map (curry cons #\0) acc)
                     (map (curry cons #\1) acc))))))
  (define (from-binstr str)
    (string->number str 2))
  (~>> num
       (number->string _ 2)
       (pad)
       (memory-addresses)
       (map (compose from-binstr list->string reverse))))

(for/fold ((dict (make-hasheq))
           (mask "")
           #:result (apply + (hash-values dict)))
          ((line (file->lines "inputs/day14.txt")))
  (cond
    ((string-prefix? line "mask")
     (values dict (third (string-split line))))
    (else
     (define memcell (read-num line))
     (define keys (decode-memory-addresses (mem-address memcell) mask))
     (define val (mem-value memcell))
     (for ((key keys))
       (hash-set! dict key val))
     (values dict mask))))
