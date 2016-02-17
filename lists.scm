(define (my-range a b d)
  (define (helper a xs )
    (if (< a b)
        (cons a (helper (+ a d) xs))
        xs))
  (helper a '()))

(define (my-flatten xs)
  (cond ((null? xs) '())
        ((pair? xs) (append (my-flatten (car xs))
                            (my-flatten (cdr xs))))
        (else (list xs))))

(define (my-element? x xs)
  (if (null? xs)
      (= 1 0)
      (if (equal? x (car xs)) (= 1 1)
          (my-element? x (cdr xs)))))

(define (my-filter p? xs)
  (cond ((null? xs) '())
        ((p? (car xs)) (cons (car xs) (my-filter p? (cdr xs))))
        (else (my-filter p? (cdr xs)))))

(define (my-fold-left op xs)
  (if (null? (cdr xs)) (car xs)
      (let* ((l (op (car xs) (cadr xs)))
             (xs (cons l (cdr (cdr xs)))))
        (my-fold-left op xs))))

;если "перевернуть" список для левоассоциативной свертки, получится правоассоциативная. 
(define (my-fold-right op xs)
  (right-reverse op (reverse xs)))

(define (right-reverse op xs)
  (if (null? (cdr xs)) (car xs) (let* ((x (op (cadr xs) (car xs)))
                                       (xs (cons x (cddr xs))))
                                  (right-reverse op xs))))
