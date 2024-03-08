#lang racket

(require math/number-theory)

#|
Multiples of 3 or 5
-------------------

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get and 3, 5, 6 and 9.
The sum of these multiples is 23.

Find the sum of all the multiples of or below 1000.
|#

; brute-force
(define (find-sum-of-multiples below . factors)
  (define (count? n)
    (ormap (lambda (factor) (divides? factor n)) factors))
  (for/sum ([n (in-range 1 below)] #:when (count? n)) n))

; number-theoretic
(define (find-sum-of-multiples* below . factors)
  (define products-and-ωs
    ; we calculate all of the combinations of the factors, i.e.
    ; for factors=(3,5,7), we have 3,5,7,(3,5),(3,7),(5,7),(3,5,7)
    (for/list ([prod (in-combinations factors)]
               ; we discard the empty product
               #:unless (empty? prod))
      ; we also keep track of how *many* factors are in a combination
      ; because we use this for the inclusion/exclusion principle
      (cons (apply * prod) (length prod))))
  
  (define bounds-per-product
    ; we calculate the "bounds" of the products above, which count the total number
    ; of multiples of each of the products below our upper-bound
    (map (compose (curry quotient (- below 1))
                  first)
         products-and-ωs))
  
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

; 233168
(list 1000
      (find-sum-of-multiples  1000 3 5)
      (find-sum-of-multiples* 1000 3 5))
