(define (pack xs)
  (define (pack-element e xs)
    (or
     (and (or (null? xs) (not (equal? e (car xs)))) '())
     (cons e (pack-element e (cdr xs)))))
  
  (if (not (null? xs))
      (let ((v (pack-element (car xs) xs)))
        (cons v (pack (list-tail xs (length v)))))
      '()))

(define (encode xs)
  (let loop ((v (pack xs)))
    (or
     (and (null? v) '())
     (cons (list (caar v) (length (car v))) (loop (cdr v))))))

(define (unpack xs)
  (define (build-list e k)
    (or
     (and (zero? k) '())
     (cons e (build-list e (- k 1)))))
  
  (or
   (and (null? xs) '())
   (cons (build-list (caar xs) (cadar xs)) (unpack (cdr xs)))))

(define (decode xs)
  (let loop ((v (unpack xs)))
    (or
     (and (null? v) '())
     (append (car v) (loop (cdr v))))))

