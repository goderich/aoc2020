#lang racket

;; This day's puzzle works great with parser
;; combinators. I am using Alexis King's
;; megaparsack library, because it's pleasant to
;; work with and well documented.
;;
;; The imports below are all needed for the
;; parser.

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either)

;; We start by defining simple parsers to be
;; combined later on.

(define word/p
  (do (word <- (many/p letter/p))
      (pure (list->string word))))

(define color/p
  (do (w1 <- word/p)
      space/p
      (w2 <- word/p)
      (pure (string-join (list w1 w2)))))

;; When a bag of a particular colour contains
;; other bags, the string begins with a number,
;; followed by the colour of the contained bag,
;; the word "bag" or "bags", then either a comma
;; and another colour or a full stop.
;; The result of this parser is a cons cell of
;; an integer (number of bags) and a string
;; (bag colour).

(define contained-single/p
  (do (num <- integer/p)
      space/p
      (color <- color/p)
      (string/p " bag")
      (many/p (char/p #\s) #:max 1)
      (or/p (string/p ", ")
            (do (char/p #\.)
                eof/p))
      (pure (cons num color))))

;; To find out all the bags contained in a given
;; bag, we just run the above parser with many/p,
;; which is a Kleene star. If it cannot parse a
;; single colour (i.e. in the case of "no other
;; bags"), it returns an empty list.

(define contained-all/p
  (do (contained <- (many/p contained-single/p))
      (pure contained)))

(define line/p
  (do (color <- color/p)
      (string/p " bags contain ")
      (contained <- contained-all/p)
      (pure (cons color contained))))

;; We parse each line and store the results in a hash
;; map, where the keys are the colours of outer bags,
;; and the values are lists of bags contained inside
;; them, with each item of the list being a pair of
;; int and string, i.e. the number and colour of
;; contained bags. The hash map is then used for both
;; part 1 and part 2.

(define dict
  (for/hash ((line (file->lines "inputs/day07.txt")))
    (define result (from-success #f (parse-string line/p line)))
    (values (car result) (cdr result))))

;; Part 1
;;
;; A bag can hold a shiny gold bag either if it
;; contains one directly, or if any of the bags
;; it contains can hold one, recursively.

(define (has-shiny-gold? color)
  (define contained (map cdr (hash-ref dict color)))
  (or (ormap (curry equal? "shiny gold") contained)
      (ormap has-shiny-gold? contained)))

;; The answer to part 1 of the puzzle is the amount
;; of colours that can contain a shiny gold bag.

(count has-shiny-gold? (hash-keys dict))

;; Part 2
;;
;; For the second answer we recursively count the
;; number of bags contained in the shiny gold bag
;; and each of its constituent bags.

(define (numbags-contained color)
  (define contained (hash-ref dict color))
  (for/sum ((bagnum contained))
    (define n (car bagnum))
    (define color (cdr bagnum))
    (+ n (* n (numbags-contained color)))))

(numbags-contained "shiny gold")
