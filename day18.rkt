#lang racket

;; We use the Megaparsack library and friends again today, and this time
;; they are especially helpful.
(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either
         threading)

;; We parse the + and * operators as functions. We don't have to, but we
;; do so because we can, and also because functions are first-class values
;; in Racket, and can be passed around like variables. We'll be using this
;; a lot.
(define add/p
  (do (char/p #\+)
      (pure +)))

(define multiply/p
  (do (char/p #\*)
      (pure *)))

(define operator/p
  (or/p add/p multiply/p))

;; An expression is defined as several of the following: integers, operators,
;; and other expressions in parentheses, all optionally separated by at most
;; one space. We parse each line recursively, and preserve the nested structure
;; of the input.
(define expression/p
  (many/p (do (many/p space/p #:max 1)
              (or/p integer/p
                    operator/p
                    (do (char/p #\()
                        (inner-exp <- expression/p)
                        (char/p #\))
                        (pure inner-exp))))))

;; We parse all input lines and save them to a variable to be used with
;; both part 1 and part 2. Megaparsack is quite powerful, but it is slow,
;; so it's a good idea to parse the input only once.
(define inputs
  (for/list ((line (file->lines "inputs/day18.txt")))
    (from-success #f (parse-string expression/p line))))

;; This function evaluates inner-level expressions, i.e. expressions which
;; contain only numbers and operators, but not other parenthetical expressions.
;; We build the calculation from the outside in, starting at the right hand
;; side and stopping when we reach a list of length 3: the innermost expression.
(define (eval-inner-part1 lst)
  (cond
    ((= 3 (length lst))
     (match-define (list n1 op n2) lst)
     (op n1 n2))
    (else
     (match-define (list op num) (take-right lst 2))
     (op num (eval-inner-part1 (drop-right lst 2))))))

;; This function is abstracted for use with both part 1 and part 2. It simply
;; checks if there are any lists in LST and recursively calls INNER-F on them
;; until they are all evaluated, finally calling INNER-F one last time on the
;; remaining list.
(define (eval-exp inner-f lst)
  (define list-index (index-where lst list?))
  (cond
    ((not list-index) (inner-f lst))
    (else (~>> (list-ref lst list-index)
               (eval-exp inner-f)
               (list-set lst list-index)
               (eval-exp inner-f)))))

;; The answer to part 1.
(for/sum ((line inputs))
  (eval-exp eval-inner-part1 line))

;; For part 2, the only thing that's different is the inner expression
;; evaluator. Here we mostly ignore the operators and instead work with
;; lists. We split the INPUT list on each *, so that the following:
;; 1 + 2 * 3 + 4 + 5 * 6
;; becomes this:
;; ((1 2) (3 4 5) (6))
;; [NB. The lists are actually all reversed, but since we're only adding
;; and multiplying it doesn't matter.]
;; Then all that remains is to take the sum of each inner list, and then
;; multiply the sums together.
(define (eval-inner-part2 input)
  (for/fold ((acc '())
             (curr-lst '())
             #:result (~>> acc
                           (cons curr-lst)
                           (map (curry apply +))
                           (apply *)))
            ((item input)
             #:when (not (eq? item +)))
    (cond
      ((number? item) (values acc (cons item curr-lst)))
      (else (values (cons curr-lst acc) '())))))

;; The answer to part 2.
(for/sum ((line inputs))
  (eval-exp eval-inner-part2 line))
