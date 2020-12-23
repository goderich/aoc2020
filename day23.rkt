#lang racket

(require threading)

(define input
  (map string->number (drop-right (rest (string-split "368195742" "")) 1)))

(for/fold ((lst input)
           (curr (list-ref input 0)))
          ((_ (in-range 10)))
  (define curr-ind (index-of lst curr))
  (define picked-up
    (for/list ((i (in-range (add1 curr-ind) (+ 4 curr-ind))))
      (list-ref lst (remainder i (length lst)))))
  (define remaining
    (for/list ((cup lst)
               #:unless (member cup picked-up))
      cup))
  (define (find-destination-cup label)
    (cond
      ((zero? label) (find-destination-cup 9))
      ((member label picked-up) (find-destination-cup (sub1 label)))
      (else label)))
  (define destination-cup (find-destination-cup (sub1 curr)))
  (define-values (beg end)
    (~>> destination-cup
         (index-of remaining)
         (add1)
         (split-at remaining)))
  (define new-list (append beg picked-up end))
  (define new-curr
    (list-ref new-list (remainder (add1 (index-of new-list curr)) (length lst))))
  (values new-list new-curr))

(define mega-input
  (list->vector (append input (range 10 1000001))))

(define hm
  (make-hasheq
   (for/list ((i (vector-length mega-input)))
     (cons (vector-ref mega-input i)
           (if (= i 999999)
               (vector-ref mega-input 0)
               (vector-ref mega-input (add1 i)))))))

(define (print-hm)
  (for/fold ((lab 1)
             (acc '())
             #:result (reverse acc))
            ((_ (in-range (sub1 (hash-count hm))))
             #:break (= 1 (hash-ref hm lab)))
    (define next (hash-ref hm lab))
    (values next
            (cons next acc))))

(define (game dict)
  (for/fold ((curr 3)
             #:result (* (hash-ref dict 1)
                         (hash-ref dict (hash-ref dict 1))))
            ((_ (in-range 10000000)))
    (define a (hash-ref dict curr))
    (define b (hash-ref dict a))
    (define c (hash-ref dict b))
    (define (find-destination-cup label)
      (cond
        ((zero? label) (find-destination-cup (hash-count dict)))
        ((member label (list a b c)) (find-destination-cup (sub1 label)))
        (else label)))
    (define destination-cup (find-destination-cup (sub1 curr)))
    (define cup-old-tl (hash-ref dict destination-cup))
    (hash-set! dict destination-cup a)
    (hash-set! dict curr (hash-ref dict c))
    (hash-set! dict c cup-old-tl)
    (hash-ref dict curr)))

(game hm)
