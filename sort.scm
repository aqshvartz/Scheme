(define (selection-sort pred? xs)
  (define (find v iterable)
    (or
     (and (null? iterable) v)
     (if (pred? (car iterable) v)
         (find (car iterable) (cdr iterable))
         (find v (cdr iterable)))))
  
  (define (remove e iterable)
    (or
     (and (equal? e (car iterable)) (cdr iterable))
     (cons (car iterable) (remove e (cdr iterable)))))
  
  (if (null? xs)
      '()
      (let ((v (find (car xs) xs)))
        (cons (find (car xs) xs) (selection-sort pred? (remove v xs))))))

(define (insertion-sort pred? xs)
  (define (insert pred? e xs)
    (or
     (and (null? xs) (list e))
     (if (pred? e (car xs))
         (cons e xs)
         (cons (car xs) (insert pred? e (cdr xs))))))
  (if (null? xs)
      '()
      (insert pred? (car xs) (insertion-sort pred? (cdr xs)))))
