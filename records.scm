(use-syntax (ice-9 syncase))

(define-syntax define-struct
  (syntax-rules () 
    ((_ name (fields ...)) 
     (begin
       (build-constructor name (fields ...))
       (build-predicate name)
       (build-getters name (fields ...))
       (build-setters name (fields ...))))))

(define-syntax build-constructor
  (syntax-rules()
    ((_ name (fields ...))
       (let ((header (cons (string->symbol (string-append "make-" (symbol->string 'name))) 'args))
             (body '(let loop ((res '())
                               (values args)
                               (keys '(fields ...)))
                      (if (null? values)
                          (cons 'name res)
                          (loop
                           (cons (list (car keys) (car values)) res)
                           (cdr values)
                           (cdr keys))))))
         
         (eval (list 'define header body) (interaction-environment))))))

(define-syntax build-predicate
  (syntax-rules()
    ((_ name)
       (let ((header (list (string->symbol (string-append (symbol->string 'name) "?")) 's))
             (body '(and (list? s) (equal? (car s) 'name))))
         
         (eval (list 'define header body) (interaction-environment))))))

(define-syntax build-getters
  (syntax-rules()
    ((_ name (field)) (build-get name field))
    ((_ name (field fields ...))
     (begin
       (build-get name field)
       (build-getters name (fields ...))))))

(define-syntax build-get
  (syntax-rules()
    ((_ name field)
       (let
           ((header (list (string->symbol (string-append (symbol->string 'name) "-" (symbol->string 'field))) 'p))
            (body '(cadr (assoc 'field (cdr p)))))

         (eval (list 'define header body) (interaction-environment))))))

(define-syntax build-setters
  (syntax-rules()
    ((_ name (field)) (build-set name field))
    ((_ name (field fields ...))
     (begin
       (build-set name field)
       (build-setters name (fields ...))))))

(define-syntax build-set
  (syntax-rules()
    ((_ name field)
       (let
           ((header (list (string->symbol (string-append "set-" (symbol->string 'name) "-" (symbol->string 'field) "!")) 'p 'v))
            (body '(set-car! (cdr (assoc 'field (cdr p))) v)))

         (eval (list 'define header body) (interaction-environment))))))
 
