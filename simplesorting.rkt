;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simplesorting) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; constants for testing

(define UNSORTEDLIST (list 6 8 3 0 9 1 4 7 2 5))
(define SORTEDLIST (list 0 1 2 3 4 5 6 7 8 9))
(define REVSORTEDLIST (list 9 8 7 6 5 4 3 2 1 0))



; data definitions


; [ListOf X] is one of
;     - '()
;     - (cons X [ListOf X])
; where X is atomic
#;
(define (fn-on-lst lst)
  (cond
    [(empty? lst) ...]
    [else ... (first lst) ... (fn-on-lst (rest lst))]))
          


; abstract functions


(define (abstract-sort pred lst)
  ; [X X -> Boolean] [ListOf X] -> [ListOf X]
  (cond
    [(empty? lst) '()]
    [else (local (
                  (define (insert val lst)
                    ; X [ListOf X] -> [ListOf X]
                    ; an helper function on abstract-sort that
                    ; prevents exponential recursion growth.
                    ; It inserts a value into a sorted list of the
                    ; same type, preserving the sort order
                    (cond
                      [(empty? lst) (cons val '())]
                      [(pred val (first lst)) (cons val lst)]
                      [else (cons (first lst) (insert val (rest lst)))])))
            ; - IN -
            (insert (first lst) (abstract-sort pred (rest lst))))]))


(define (list-of-x? lst pred)
  ; [ListOf X] [X -> Boolean] -> Boolean
  ; determines if lst is in fact a list of the
  ; appropriate type as determined by pred
  (and
   (list? lst)
   (or
    (empty? lst)
    (and
     (pred (first lst))
     (list-of-x? (rest lst) pred)))))


(define (sorted? pred)
  ; [X X -> Boolean] -> [[ListOf X] -> Boolean]
  (lambda (lst)
    (local (
            (define (destro? rlst)
              (or
               (empty? (rest rlst))
               (and
                (or
                 (pred (first rlst) (first (rest rlst)))
                 (equal? (first rlst) (first (rest rlst))))
                (destro? (rest rlst))))))
      ; - IN -
      (destro? lst))))


; typed functions


(define (sort< lon)
  ; ListOfNumbers -> ListOfNumbers
  ; sort a list of numbers in ascending order using insertion sort algorithm
  (cond
    [(not (list-of-numbers? lon)) (error "not a list of numbers")]
    [else (abstract-sort < lon)]))
; checks
(check-error (sort< "apple") "not a list of numbers")
(check-satisfied (sort< (list 1 0)) (sorted? <))
(check-satisfied (sort< UNSORTEDLIST) (sorted? <))
(check-satisfied (sort< (list 0 2 1 2)) (sorted? <))


(define (sort> lon)
  ; ListOfNumbers -> ListOfNumbers
  ; sort a list of numbers in descending order using insertion sort algorithm
  (cond
    [(not (list-of-numbers? lon)) (error "not a list of numbers")]
    [else (abstract-sort > lon)]))
; checks
(check-satisfied (sort> UNSORTEDLIST) (sorted? >))


(define (sort-string< los)
  ; [ListOf String] -> [ListOf String]
  ; sort a list of strings in ascending order using insertion sort algorithm
  (cond
    [(not (list-of-string? los)) (error "not a list of strings")]
    [else (abstract-sort string<? los)]))
; checks
(check-satisfied (sort-string< '("donut" "jerry" "apple")) (sorted? string<?))


(define (sort-string> los)
  ; [ListOf String] -> [ListOf String]
  ; sort a list of strings in descending order using insertion sort algorithm
  (cond
    [(not (list-of-string? los)) (error "not a list of strings")]
    [else (abstract-sort string>? los)]))
; checks
(check-satisfied (sort-string> '("donut" "jerry" "apple")) (sorted? string>?))


(define (list-of-numbers? lst)
  ; [ListOf Any] -> Boolean
  (list-of-x? lst number?))
; checks
(check-expect (list-of-numbers? (list)) #t)
(check-expect (list-of-numbers? '()) #t)
(check-expect (list-of-numbers? "apple") #f)
(check-expect (list-of-numbers? UNSORTEDLIST) #t)
(check-expect (list-of-numbers? (list 1 "p")) #f)
(check-expect (list-of-numbers? (list 1 UNSORTEDLIST)) #f)


(define (list-of-string? lst)
  ; [ListOf Any] -> Boolean
  (list-of-x? lst string?))
; checks
(check-expect (list-of-string? "apple") #f)
(check-expect (list-of-string? '("donut" "jerry")) #t)