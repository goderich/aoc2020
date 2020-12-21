#lang racket

(require threading)

(define (parse-line str)
  (match-define (list word-str allergen-str) (string-split str "(contains "))
  (define words (string-split word-str))
  (define allergens
    (~>> allergen-str
         (substring _ 0 (sub1 (string-length allergen-str)))
         (string-split _ ", ")))
  (values allergens words))

(define words
  (flatten
   (for/list ((line (file->lines "inputs/day21.txt")))
     (~>> line
          (string-split _ "(contains")
          (first)
          (string-split)))))

(define lines
  (for/hash ((line (file->lines "inputs/day21.txt")))
    (parse-line line)))

(define d
  (for*/fold ((dict (make-immutable-hash)))
             (((alls wds) lines)
              (all alls))
    (if (hash-has-key? dict all)
        (hash-update dict all (lambda (v) (set-intersect v wds)))
        (hash-set dict all wds))))

(define allergens (remove-duplicates (flatten (hash-values d))))

(for/fold ((acc 0))
          ((word words)
           #:unless (member word allergens))
  (add1 acc))

(define (calc-alls dict)
  (for/first (((k v) dict)
              ((k2 v2) dict)
              #:when (for/and ((all v))
                       (member all v2)))
  (hash-set dict k2 (set-subtract v2 v))
  ))

;; '#hash(("dairy" . ("tsqpn" "zqzmzl" "rcqb"))
;;        ("eggs" . ("cltx" "rcqb"))
;;        ("fish" . ("nrl"))
;;        ("nuts" . ("qjvvcvz"))
;;        ("peanuts" . ("xhnk" "tsqpn" "cltx" "tfqsb"))
;;        ("sesame" . ("xhnk" "tsqpn"))
;;        ("shellfish" . ("zqzmzl" "tfqsb" "rcqb" "cltx"))
;;        ("wheat" . ("zqzmzl" "xhnk")))

;; (~>>
;; "zqzmzl"
;;  '("cltx" "nrl" "qjvvcvz" "tfqsb" "tsqpn" "rcqb" "xhnk")
;;  (string-join _ ","))
