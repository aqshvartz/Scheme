(define (read-words)
  (let reaad ((word '()))
    (let ((mem (read-char)))
      (if (not (eof-object? mem))
          (if (or (equal? mem #\space) (equal? mem #\newline) (equal? mem #\tab))
              (if (null? word)
                  (reaad '())
                  (cons (list->string word) (reaad '())))
              (reaad (append word (list mem))))
          (if (null? word)
              '()
              (list (list->string word)))))))