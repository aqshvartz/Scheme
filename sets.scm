(define (f x xs)
  (if (null? xs) '()
      (if (equal? x (car xs))
          (f x (cdr xs))
          (cons (car xs) (f x (cdr xs))))))

(define (list->set xs)
  (if (null? xs) '()
      (cons (car xs)
            (f (car xs) (list->set (cdr xs))))))

(define (asd x xs)
  (if (null? xs) (= 1 1)
      (if (equal? x (car xs)) (= 1 0)
          (asd x (cdr xs)))))

(define (set? xs)
  (if (null? xs) (= 1 1)
      (and (asd (car xs) (cdr xs)) (set? (cdr xs)))))

(define (union xs ys)
  (if (null? xs) ys
      (if (asd (car xs) ys) 
          (union (cdr xs) (append (cons (car xs) '())
                                  ys))
          (union (cdr xs) ys))))

(define (intersection xs ys)
  (if (null? xs) '()
      (if (asd (car xs) ys) (intersection (cdr xs) ys)
          (cons (car xs) (intersection (cdr xs) ys)))))

(define (difference xs ys)
  (if (null? ys) xs
      (if (asd (car ys) xs) (difference xs (cdr ys))
          (difference (f (car ys) xs) (cdr ys)))))

(define (symmetric-difference xs ys)
  (difference (union xs ys) (intersection xs ys)))

(define (set-eq? xs ys)
  (if (equal? xs ys) (= 1 1) 
      (if (or (null? xs) (null? ys)) (= 1 0)
          (set-eq? (cdr xs) (f (car xs) ys)))))
