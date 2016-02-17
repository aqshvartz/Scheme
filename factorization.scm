;чтобы не писать equal много раз
(define :? equal?)
;сраниваем степень и знак с известными формулами разложения на множители,"раскрываем" их
(define (factorize expr)
;a^2-b^2 = (a-b)(a+b)
  (cond ((and (:? (car expr) '-) (:? (caddr (cadr expr)) 2)) 
         (list '* 
               (list '- (cadr (cadr expr)) (cadr (caddr expr))) 
               (list '+ (cadr (cadr expr)) (cadr (caddr expr)))))
;a^3-b^3 = (a-b)(a^2+ab+b^2)
        ((and (:? (car expr) '-) (:? (caddr (cadr expr)) 3)) 
         (list '* 
               (list '- (cadr (cadr expr)) (cadr (caddr expr))) 
               (list '+ 
                     (list 'expt (cadr (cadr expr)) 2)
                     (list '* (cadr (cadr expr)) (cadr (caddr expr)))
                     (list 'expt (cadr (caddr expr)) 2))))
;a^3+b^3 = (a+b)(a^2-ab+b^2)
        ((and (:? (car expr) '+) (:? (caddr (cadr expr)) 3)) 
         (list '* 
               (list '+ (cadr (cadr expr)) (cadr (caddr expr))) 
               (list '+ 
                     (list 'expt (cadr (cadr expr)) 2)
                     (list '- (list '* (cadr (cadr expr)) (cadr (caddr expr))))
                     (list 'expt (cadr (caddr expr)) 2))))))


;(factorize '(- (expt x 2) (expt y 2)))
;(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
;(factorize '(- (expt x 3) (expt y 3)))
;(factorize '(+ (expt x 3) (expt y 3)))
