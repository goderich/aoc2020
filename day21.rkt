#lang racket

(require threading)

;; We use separate functions to extract "foreign" words from a line, and the
;; list of allergens in English. Separating the two makes looping over the
;; input a lot easier.

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

;; We loop over each allergen in each line, collecting them into a hash map.
;; The value for individual allergens in the intersection of the sets of
;; "foreign" words on lines where the allergen appears.
(define allergen-dict
  (for*/fold ((dict (make-immutable-hash)))
             ((line (file->lines "inputs/day21.txt"))
              (allergen (parse-allergens line)))
    (define foreign (parse-foreign line))
    (if (hash-has-key? dict allergen)
        (hash-update dict allergen (curry set-intersect foreign))
        (hash-set dict allergen foreign))))

;; To get the answer to part 1, we need to know which "foreign" words must
;; be allergens, and count the number of words out of the total that are NOT
;; in this list.

(define foreign-allergens
  (~>> allergen-dict
       (hash-values)
       (flatten)
       (remove-duplicates)))

(define foreign-words
  (~>> "inputs/day21.txt"
       (file->lines)
       (map parse-foreign)
       (flatten)))

(count (λ (word) (not (member word foreign-allergens))) foreign-words)

;; Part 2
;;
;; Since we already have a list of candidates for correct allergen translations,
;; we need to go over it to eliminate impossible candidates, until we have
;; correct translations for all allergens.
;;
;; This helper function removes a single value (string) from all lists in the dict.
;; It ignores everything that's not a list.
(define (remove-from-dict dict singleton)
  (for/hash (((key val) dict))
    (values
     key
     (if (list? val)
         (remove singleton val)
         val))))

;; In order to prune the candidates dict, we look for a list with just one
;; candidate, and remove it from all other lists (before we do that, we need
;; to extract it from the list, or we will end up with an empty list for this
;; candidate). We repeat the process until there are no more lists in DICT,
;; at which point the `for/first` loop will return #f and we can return the
;; pruned version.
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

;; Even though dicts are printed with their keys in order (at least in my
;; REPL), Racket doesn't actually guarantee their ordering, especially
;; if we try to extract the values directly with `hash-values`. Thus we
;; make sure to go over the sorted list of keys in TRANSLATIONS to get
;; the correctly ordered list of "foreign" allergens, and join them with
;; commas to get the answer to part 2.
(~>>
 (for/list ((allergen (sort (hash-keys translations) string<?)))
   (hash-ref translations allergen))
 (string-join _ ","))
