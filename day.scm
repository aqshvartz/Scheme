(define (day-of-week den mes god)  
  (let* 
      (( a (quotient (- 14 mes) 12))
       ( b (- god a))
       ( c (- (+ mes (* 12 a)) 2))
       (n (+(-(quotient b 4) (quotient b 100)) (quotient b 400)))
       (k (quotient (* 31 c) 12))
       (d (+ 7000 den b n k)))
    (remainder d 7)))
