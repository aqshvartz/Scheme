(define (string-trim-helper xs)
  (or
   (and (or (null? xs) (not (char-whitespace? (car xs)))) xs)
   (string-trim-helper (cdr xs))))

(define (string-trim-left s) 
  (list->string (string-trim-helper (string->list s))))

(define (string-trim-right s)
  (list->string (reverse (string-trim-helper (reverse (string->list s))))))

(define (string-trim s)
  (string-trim-right (string-trim-left s)))

(define (string-prefix? prefix s)
  (equal? prefix (substring s 0 (min (string-length prefix) (string-length s)))))

(define (string-suffix? suffix s)
  (equal? suffix (substring s (max (- (string-length s) (string-length suffix)) 0))))

(define (string-infix? infix s)
  (and
   (<= (string-length infix) (string-length s))
   (or (string-prefix? infix s) (string-infix? infix (substring s 1)))))

(define (string-split s sep)
  ;
  (define (string-null? str) (zero? (string-length str)))
  
  (let loop ((ans "")
             (substr s)
             (length (string-length sep)))
    (or
     (and (string-null? substr) (if (not (string-null? ans)) `(,ans) '()))
     (and
      (string-prefix? sep substr)
      (if (not (string-null? ans))
          (cons ans (string-split (substring substr length) sep))
          (string-split (substring substr length) sep)))
     (loop
      (string-append ans (string (string-ref substr 0)))
      (substring substr 1)
      length))))
