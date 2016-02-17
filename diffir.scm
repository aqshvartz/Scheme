(define (derivative xs)
  (if (not (list? xs))
      (or (and (number? xs) 0) 1)
      (cond
        ((equal? '- (car xs)) (sub xs))
        ((equal? '+ (car xs)) (add xs))
        ((equal? '* (car xs)) (mul xs))
        ((equal? '/ (car xs)) (div xs))
        ((equal? 'exp (car xs)) (list '* xs (derivative (cadr xs))))
        ((equal? 'expt (car xs)) (my-expt xs))
        ((equal? 'sin (car xs)) (list '* (list 'cos (cadr xs)) (derivative (cadr xs))))
        ((equal? 'cos (car xs)) (list '* -1 (list 'sin (cadr xs)) (derivative (cadr xs))))
        ((equal? 'log (car xs)) (list '/ (derivative (cadr xs)) (cadr xs))))))

(define (sub xs)
  (if (= (length xs) 2)
      (if (equal? (cadr xs) 'x)
          (- (derivative (cadr xs)))
          (derivative (cadr xs)))
      (list '- (derivative (cadr xs)) (derivative (cons '- (cddr xs))))))

(define (add xs)
  (if (= (length xs) 2)
      (list '+ (derivative (cadr xs)))
      (list '+ (derivative (cadr xs)) (derivative (cons '+ (cddr xs))))))

(define (mul xs)
  (let ((a (cadr xs))
        (b (cddr xs)))
    (if (= (length xs) 3)
        (if (and (number? a) (equal? (car b) 'x))
            a
            (list '+ (list '* a (derivative (car b))) (list '* (car b) (derivative a))))
        (list '+ (list '* a (derivative (cons '* b))) (list '* (cons '* b) (derivative a))))))

(define (div xs)
  (let ((a (cadr xs))
        (b (cddr xs)))
    (if (= (length xs) 3)
        (list '/
              (list '-
                    (list '* (car b) (derivative a))
                    (list '* a (derivative (car b))))
              (list '* (car b) (car b)))
        (list '/
              (list '-
                    (list '* (cons '/ b) (derivative a))
                    (list '* a (derivative (cons '/ b))))
              (list '* (cons '/ b) (cons '/ b))))))

(define (my-expt xs)
  (if (number? (caddr xs))
      (list '* (caddr xs) (list 'expt (cadr xs) (- (caddr xs) 1)) (derivative (cadr xs)))
      (list '* xs (list 'log (cadr xs)) (derivative (caddr xs)))))
