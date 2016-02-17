(define (char->number c)
  (or
   (and (char>=? c #\a) (char<=? c #\z) (- (char->integer c) 87))
   (and (char>=? c #\A) (char<=? c #\Z) (- (char->integer c) 55))
   (- (char->integer c) 48)))

(define (number->char n)
  (or
   (and (< n 10) (integer->char (+ n 48)))
   (integer->char (+ n 87))))

(define (correct-number? s base)
  (define (helper xs)
    (or
     (null? xs)
     (< (char->number (car xs)) base)))
  
  (helper (string->list s)))


(define (certain->decimal s base)
  (define (helper xs p)
    (or
     (and (null? xs) 0)
     (+ (* (char->number (car xs)) p) (helper (cdr xs) (* p base)))))
  
  (if (correct-number? s base)
      (helper (reverse (string->list s)) 1)
      'number-conversion-error))

(define (decimal->certain d base)
  (define (helper n)
    (or
     (and (< n 1) '())
     (cons (number->char (remainder n base)) (helper (quotient n base)))))
  
  (list->string (reverse (helper d))))
