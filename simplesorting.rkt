;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simplesorting) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; constants for testing

(define UNSORTEDLIST (list 6 8 3 0 9 1 4 7 2 5))
(define SORTEDLIST (list 0 1 2 3 4 5 6 7 8 9))
(define REVSORTEDLIST (list 9 8 7 6 5 4 3 2 1 0))

          

; ==================================
; abstract functions

(define (abstract-sort pred lst algo)
  ; [X X -> Boolean] [ListOf X] [[X X -> Boolean] [ListOf X] -> [ListOf X]]
  ; -> [ListOf X]
  (algo pred lst))


(define (insertion-sort pred lst)
  ; [X X -> Boolean] [ListOf X] -> [ListOf X]
  ; slow, O(n^2) insertion sorting
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
            (insert (first lst) (insertion-sort pred (rest lst))))]))


(define (quick-sort lst)
  ; [ListOf X] -> [ListOf X]
  ; fast, O(n lg n) merge sorting with pivot
  ; can't be supplied with a predicate it's current form
  (cond
    [(< (length lst) 2) lst]
    [else
     (local (
             (define pivot (list-ref lst (quotient (length lst) 2)))
             (define-struct quickness [lesser equality greater])
             (define (divide l q)
               ; [ListOf X] Quickness -> Quickness
               (cond
                 [(empty? l) q]
                 [else
                  (divide (rest l)
                          (make-quickness    
                           (if (< (first l) pivot)
                               (cons (first l) (quickness-lesser q))
                               (quickness-lesser q))
                           (if (= (first l) pivot)
                               (cons (first l) (quickness-equality q))
                               (quickness-equality q))                 
                           (if (> (first l) pivot)
                               (cons (first l) (quickness-greater q))
                               (quickness-greater q))))]))
             (define conquered (divide lst (make-quickness '() '() '()))))
       ; - IN -
       (append (quick-sort (quickness-lesser conquered))
               (quickness-equality conquered)
               (quick-sort (quickness-greater conquered))))]))


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
                 (pred (first rlst) (second rlst))
                 (equal? (first rlst) (second rlst)))
                (destro? (rest rlst))))))
      ; - IN -
      (destro? lst))))


; =============================
; concrete functions


(define (isort< lon)
  ; ListOfNumbers -> ListOfNumbers
  ; sort a list of numbers in ascending order using insertion sort algorithm
  (cond
    [(not (list-of-numbers? lon)) (error "not a list of numbers")]
    [else (insertion-sort < lon)]))


(define (qsort< lon)
  ; ListOfNumbers -> ListOfNumbers
  ; sort a list of numbers in ascending order using insertion sort algorithm
  (cond
    [(not (list-of-numbers? lon)) (error "not a list of numbers")]
    [else (quick-sort lon)]))


(define (isort> lon)
  ; ListOfNumbers -> ListOfNumbers
  ; sort a list of numbers in descending order using insertion sort algorithm
  (cond
    [(not (list-of-numbers? lon)) (error "not a list of numbers")]
    [else (insertion-sort > lon)]))


(define (qsort> lon)
  ; ListOfNumbers -> ListOfNumbers
  ; sort a list of numbers in descending order using insertion sort algorithm
  (cond
    [(not (list-of-numbers? lon)) (error "not a list of numbers")]
    [else (reverse (quick-sort lon))]))


(define (isort-string< los)
  ; [ListOf String] -> [ListOf String]
  ; sort a list of strings in ascending order using insertion sort algorithm
  (cond
    [(not (list-of-string? los)) (error "not a list of strings")]
    [else (insertion-sort string<? los)]))


(define (isort-string> los)
  ; [ListOf String] -> [ListOf String]
  ; sort a list of strings in descending order using insertion sort algorithm
  (cond
    [(not (list-of-string? los)) (error "not a list of strings")]
    [else (insertion-sort string>? los)]))


(define (list-of-numbers? lst)
  ; [ListOf Any] -> Boolean
  (list-of-x? lst number?))


(define (list-of-string? lst)
  ; [ListOf Any] -> Boolean
  (list-of-x? lst string?))



; ==========================
; checks

(check-error (isort< "apple") "not a list of numbers")
(check-satisfied (isort< (list 1 0)) (sorted? <))
(check-satisfied (isort< UNSORTEDLIST) (sorted? <))
(check-satisfied (isort< (list 0 2 1 2)) (sorted? <))
(check-error (qsort< "apple") "not a list of numbers")
(check-satisfied (qsort< (list 1 0)) (sorted? <))
(check-satisfied (qsort< UNSORTEDLIST) (sorted? <))
(check-satisfied (qsort< (list 0 2 1 2)) (sorted? <))
(check-satisfied (isort> UNSORTEDLIST) (sorted? >))
(check-satisfied (qsort> UNSORTEDLIST) (sorted? >))
(check-satisfied (isort-string< '("donut" "jerry" "apple")) (sorted? string<?))
(check-satisfied (isort-string> '("donut" "jerry" "apple")) (sorted? string>?))
(check-expect (list-of-numbers? (list)) #t)
(check-expect (list-of-numbers? '()) #t)
(check-expect (list-of-numbers? "apple") #f)
(check-expect (list-of-numbers? UNSORTEDLIST) #t)
(check-expect (list-of-numbers? (list 1 "p")) #f)
(check-expect (list-of-numbers? (list 1 UNSORTEDLIST)) #f)
(check-expect (list-of-string? "apple") #f)
(check-expect (list-of-string? '("donut" "jerry")) #t)