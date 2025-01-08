(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (succ n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define five (add three two))

(define _nil '())
(define (f _)
  (display "🐱"))

((two f) _nil)
(display " + ")
((three f) _nil)
(display " = ")
((five f) _nil)
(newline)
