;вспомогательная процедура, определяющая, одинаковы ли знаки на концах отрезка
(define (signum si)
  (if (> si 0) 1 0))
;сама процедура
(define (bisection function a b epsilon)
  (if (<= (abs (function (/ (+ b a) 2))) epsilon)
      (/ (+ b a) 2)
      (if (not (= (signum (function a)) (signum (function (/ (+ b a) 2)))))
      (bisection function a (/ (+ b a) 2) epsilon)
      (bisection function (/ (+ b a) 2) b epsilon))))
