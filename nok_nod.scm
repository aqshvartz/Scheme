
(define (my-gcd a b) 
 (cond ((= a 0) b)
        ((= b 0) a)
        ((< a b) (my-gcd (remainder b a) a))
        ((> a b) (my-gcd (remainder a b) b))
        )
  )


(define (my-lcm a b)
  (quotient (abs (* a b)) (my-gcd a b))
  )

(define (qor ch) (sqrt ch)
  )



(define (qor ch) (sqrt ch))

(define (prime? n) 
  (define (pr? n i) 
    (or (> i (qor n))
           (and 
            (not (= (remainder n i) 0)) (pr? n (+ i 1)))))
           (pr? n 2))
