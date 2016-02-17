(define ok -1)
(define (fold op xs)
  (or
   (and (null? xs) 0)
   (and (null? (cdr xs)) (car xs))
   (fold op (cons (op (car xs) (cadr xs)) (cddr xs)))))

(define (make-multi-vector sizes . args)
  (let ((v (make-vector (fold * sizes))))
    (if (not (null? args))
        (vector-fill! v (car args)))
    (list->vector (cons sizes (vector->list v)))))

(define (multi-vector? v)
  (and
   (vector? v)
   (let ((l (vector->list v)))
     (and
      (not (null? l))
      (list? (car l))
      (= (fold * (car l)) (- (vector-length v) 1))))))

(define (searcher xs sizes a b)
  (if (null? xs)
      (+ b 1)
      (let* ((a (/ a (car sizes)))
             (b (+ b (* a (car xs)))))
        (searcher (cdr xs) (cdr sizes) a b))))

(define (multi-vector-ref v indices)
  (vector-ref v (searcher indices (vector-ref v 0) (- (vector-length v) 1) 0)))

(define (multi-vector-set! v indices e)
  (vector-set! 
   v
   (searcher indices (vector-ref v 0) (- (vector-length v) 1) 0)
   e))
