(use-syntax (ice-9 syncase))

(define-syntax my-let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))))

(define-syntax define-memoized
  (syntax-rules ()
    ((define-memoized (func args ...) body)
     (define func (memoized (eval (list 'lambda (list 'args ...) 'body) (interaction-environment)))))
    ((define-memoized func body)
     (define func (memoized (eval 'body (interaction-environment)))))))

(define memoized (lambda (func)
                   (my-let ((memo '()))
                     (lambda args
                       (my-let ((match (assoc args memo)))
                         (if match
                             (cadr match)                  
                             (my-let ((value (apply func args)))
                               (set! memo (cons (list args value) memo))
                               value)))))))
