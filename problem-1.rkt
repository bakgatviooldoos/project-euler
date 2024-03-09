#lang racket

(require math/number-theory
         rackunit)

#|
Multiples of 3 or 5
-------------------

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get and 3, 5, 6 and 9.
The sum of these multiples is 23.

Find the sum of all the multiples of or below 1000.
|#

; naive brute-force: guaranteed to be correct, terrible runtime
(define (find-sum-of-multiples:naive below . factors)
  (define (count? n)
    (ormap (lambda (factor) (divides? factor n)) factors))
  (for/sum ([n (in-range (apply min factors) below)] #:when (count? n))
    n))

; number-theoretic: better runtime, more complicated to explain
(define (find-sum-of-multiples:better below . factors)
  (define products-and-ωs
    ; we calculate all of the combinations of the factors, i.e.
    ; for factors=(3,5,7), we have 3,5,7,(3,5),(3,7),(5,7),(3,5,7)
    ; and then multiply the factors of each combination
    (for/list ([prod (in-combinations factors)]
               ; we discard the empty product, i.e. 1
               #:unless (empty? prod))
      ; we also keep track of how *many* factors are in a combination
      ; because we use this for the inclusion/exclusion principle
      (list (apply * prod) (length prod))))
  
  (define bounds-per-product
    ; we calculate the "bounds" of the products above, which count the total number
    ; of multiples of each of the products below our upper-bound
    (let ([the-product first])
      (map (compose (curry quotient (- below 1)) the-product)
           products-and-ωs)))
  
  (for/sum ([product/ω products-and-ωs]
            [bound     bounds-per-product])
    ; for each of the product/length pairs
    (match-define (list product ω) product/ω)
    ; we multiply
    (* ; the product
       product
       ; the sign of the inclusion/exclusion
       ; for an odd  number of factors, we add the product
       ; for an even number of factors, we subtract the product
       ; -1 = -1
       ; +1 = -1 * -1
       ; -1 = -1 * -1 * -1, etc.
       (expt -1 (+ ω 1))
       ; the sum of the positive integers up to the bound of the product,
       ; i.e triangular number
       (triangle-number bound))))

; > (find-sum-of-multiples:naive 1000 3 5)
; 233168

(check-equal? (find-sum-of-multiples:naive  1000 3 5)
              (find-sum-of-multiples:better 1000 3 5))
