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

; ListOfNumbers -> ListOfNumbers
; sort a list of numbers in ascending order
(define (sort< lon)
  (cond
    [(not (list-of-numbers? lon)) (error "not a list of numbers")]
    [(empty? lon) '()]
    [else (insert (first lon) (sort< (rest lon)))]))
; checks
(check-error (sort< "apple") "not a list of numbers")
(check-expect (sort< (list 1 0)) (list 0 1))
(check-expect (sort< UNSORTEDLIST) SORTEDLIST)
(check-expect (sort< (list 0 2 1 2)) (list 0 1 2 2))


; ListOfNumbers -> ListOfNumbers
; sort a list of numbers in descending order
(define (sort> lon)
  (reverse (sort< lon)))
; checks
(check-expect (sort> UNSORTEDLIST) REVSORTEDLIST)

  
; Number ListOfNumbers -> ListOfNumbers
; insert a number into a sorted list, preserving the sort order
(define (insert n lon)
  (cond
    [(empty? lon) (cons n '())]
    [(< n (first lon)) (cons n lon)]
    [else (cons (first lon) (insert n (rest lon)))]))
; checks
(check-expect (insert -1 SORTEDLIST) (cons -1 SORTEDLIST))
(check-expect (insert 10 SORTEDLIST) (list 0 1 2 3 4 5 6 7 8 9 10))
(check-expect (insert 5 SORTEDLIST) (list 0 1 2 3 4 5 5 6 7 8 9))