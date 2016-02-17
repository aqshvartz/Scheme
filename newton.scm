;func - функция, pro - производная
(define (newton func pro x0 eps)
  (if (< (abs (func x0 )) eps)
      x0
      (let ((x0 (- x0 (/ (func x0) (pro x0)))))
        (newton func pro x0 eps))))


;const - пропорция золотого сечения
(define const (/ (+ (sqrt 5) 1) 2))
;func - функция, x0, x1 - интервал, eps - точность
(define (golden func x0  x1 eps)
  (if (not (> (abs (- x1 x0)) eps))
      (/ (+ x0 x1) 2)
      (if (not (< (func (- x1 (/ (- x1 x0) const))) (func (+ x0 (/ (- x1 x0) const)))))
          (golden func (- x1 (/ (- x1 x0) const)) x1 eps)
          (golden func x0 (+ x0 (/ (- x1 x0) const)) eps))))
           
