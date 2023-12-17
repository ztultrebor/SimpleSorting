;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname simplesorting) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; constants for testing

(define UNSORTEDLIST (list 6 8 3 0 9 1 4 7 2 5))
(define SORTEDLIST (list 0 1 2 3 4 5 6 7 8 9))
(define REVSORTEDLIST (list 9 8 7 6 5 4 3 2 1 0))



; data definitions

; A list of numbers is one of
;    - '()
;    - (cons Number ListOfNumbers
(define (list-of-numbers? lon)
  (and
   (list? lon)
   (or
    (empty? lon)
    (and
     (number? (first lon))
     (list-of-numbers? (rest lon))))))
; checks
(check-expect (list-of-numbers? (list)) #t)
(check-expect (list-of-numbers? '()) #t)
(check-expect (list-of-numbers? "apple") #f)
(check-expect (list-of-numbers? UNSORTEDLIST) #t)
(check-expect (list-of-numbers? (list 1 "p")) #f)
(check-expect (list-of-numbers? UNSORTEDLIST) #t)
(check-expect (list-of-numbers? (list 1 UNSORTEDLIST)) #f)
#;
(define (fn-on-lon lon)
  (cond
    [(not (list-of-numbers? lon)) (error "not a list of numbers")]
    [(empty? lon) ...]
    [else ... (first lon) ... (fn-on-lon (rest lon))]))
          


; functions

(define (sort< lon)
; ListOfNumbers -> ListOfNumbers
; sort a list of numbers in ascending order using insertion sort algorithm
  (cond
    [(not (list-of-numbers? lon)) (error "not a list of numbers")]
    [(empty? lon) '()]
    [else (insert (first lon) (sort< (rest lon)))]))
; checks
(check-error (sort< "apple") "not a list of numbers")
(check-satisfied (sort< (list 1 0)) sorted<?)
(check-satisfied (sort< UNSORTEDLIST) sorted<?)
(check-satisfied (sort< (list 0 2 1 2)) sorted<?)


(define (sort> lon)
; ListOfNumbers -> ListOfNumbers
; sort a list of numbers in descending order
  (reverse (sort< lon)))
; checks
(check-satisfied (sort> UNSORTEDLIST) sorted>?)


(define (insert n lon)
; Number ListOfNumbers -> ListOfNumbers
; insert a number into a sorted list, preserving the sort order
  (cond
    [(empty? lon) (cons n '())]
    [(not (sorted<? lon)) (error "list is not sorted in ascending order")]
    [(< n (first lon)) (cons n lon)]
    [else (cons (first lon) (insert n (rest lon)))]))
; checks
(check-expect (insert -1 SORTEDLIST) (cons -1 SORTEDLIST))
(check-expect (insert 10 SORTEDLIST) (list 0 1 2 3 4 5 6 7 8 9 10))
(check-expect (insert 5 SORTEDLIST) (list 0 1 2 3 4 5 5 6 7 8 9))
(check-error (insert 5 REVSORTEDLIST) "list is not sorted in ascending order")


(define (sorted<? lon)
; ListOfNumbers -> Boolean
; determine if a ListOfNumbers is sorted in ascending order
  (and
   (list-of-numbers? lon)
   (or
    (empty? (rest lon))
    (and
     (<= (first lon) (first (rest lon)))
     (sorted<? (rest lon))))))
; checks
(check-expect (sorted<? UNSORTEDLIST) #f)
(check-expect (sorted<? SORTEDLIST) #t)
(check-expect (sorted<? REVSORTEDLIST) #f)
(check-expect (sorted<? (list 1 2 2 3)) #t)


(define (sorted>? lon)
; ListOfNumbers -> Boolean
; determine if a ListOfNumbers is sorted in descending order
  (sorted<? (reverse lon)))
(check-expect (sorted>? UNSORTEDLIST) #f)
(check-expect (sorted>? SORTEDLIST) #f)
(check-expect (sorted>? REVSORTEDLIST) #t)
(check-expect (sorted>? (list 3 2 2 1)) #t)