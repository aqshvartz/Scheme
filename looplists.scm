(use-syntax (ice-9 syncase))

(define-syntax lazy-cons
  (syntax-rules ()   
    ((lazy-cons a b) (cons a (delay b)))))

(define (naturals-list n)
  (lazy-cons  n (naturals-list (+ n 1))))

(define naturals
  (naturals-list 0))

(define lazy-car car)

(define (lazy-cdr ls)
  (force (cdr ls)))

(define (lazy-head lst n)
  (if (= n 0)
      '()
      (cons (lazy-car lst) (lazy-head (lazy-cdr lst) (- n 1)))))

(define (lazy-filter pred lst)
  (if (null? lst)
      '()
      (let ((obj (lazy-car lst)))
        (if (pred obj)
            (lazy-cons obj (lazy-filter pred (lazy-cdr lst)))
            (lazy-filter pred (lazy-cdr lst))))))

(define (lazy-map proc . lss)
     (if (memq '() lss)
          '()
       (lazy-cons (apply proc (map lazy-car lss))
                   (apply lazy-map proc (map lazy-cdr lss)))))
