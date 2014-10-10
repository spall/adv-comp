; Unchurchification.
(define (succ n) (+ n 1))

(define (natify church-numeral)
  ((church-numeral succ) 0))

(define (boolify church-boolean)
  ((church-boolean (λ (_) #t)) (λ (_) #f)))

(define (listify f church-list)
  ((church-list
    (λ (car) (λ (cdr) (cons (f car) (listify f cdr)))))
   (λ (_) '())))

