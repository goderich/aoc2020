#lang racket

(require threading)

(define (parse-foreign line)
  (~>> line
       (string-split _ "(contains ")
       (first)
       (string-split)))

(define (parse-allergens line)
    (~>> line
         (string-split _ "(contains ")
         (second)
         (string-trim _ ")")
         (string-split _ ", ")))

(define allergen-dict
  (for*/fold ((dict (make-immutable-hash)))
             ((line (file->lines "inputs/day21.txt"))
              (allergen (parse-allergens line)))
    (define foreign (parse-foreign line))
    (if (hash-has-key? dict allergen)
        (hash-update dict allergen (lambda (v) (set-intersect v foreign)))
        (hash-set dict allergen foreign))))

(define allergens
  (~>> allergen-dict
       (hash-values)
       (flatten)
       (remove-duplicates)))

(define words
  (~>> "inputs/day21.txt"
       (file->lines)
       (map parse-foreign)
       (flatten)))

(count (λ (word) (not (member word allergens))) words)

;; Part 2
;;
(define (remove-from-dict dict singleton)
  (for/hash (((key val) dict))
    (values
     key
     (if (not (list? val))
         val
         (remove singleton val)))))

(define (prune-candidates dict)
  (define new-dict
    (for/first (((key val) dict)
                #:when (and (list? val)
                            (= 1 (length val))))
      (define singleton (car val))
      (remove-from-dict
       (hash-set dict key singleton)
       singleton)))
  (cond
    ((false? new-dict) dict)
    (else (prune-candidates new-dict))))

(define translations
  (prune-candidates allergen-dict))

(~>>
 (for/list ((allergen (sort (hash-keys translations) string<?)))
   (hash-ref translations allergen))
 (string-join _ ","))
