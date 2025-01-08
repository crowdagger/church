;; Booleans
(define _true
  (lambda (x)
    (lambda (y)
      x)))

(define _false
  (lambda (x)
    (lambda (y)
      y)))

(define (_if p a b)
  ((p a) b))

(define (_and p q)
  ((p q) p))


; Numbers

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

(define (mul a b)
  (lambda (f)
    (b (a f))))

(define (pow a b)
  (lambda (f)
    ((b a) f)))

;; Copy/pasted from https://gist.github.com/nicky-zs/8296596 
(define (pred   n)
  (lambda (f)
    (lambda (x)
      (((n (lambda (g)
             (lambda (h) (h (g f)))))
        (lambda (u) x))
       (lambda (u) u)))))


(define (sub a b)
  ((b pred) a))

(define (zero? n)
  ((n (lambda (x)
       _false))
   _true))

(define (_leq? a b)
  (zero? (sub a b)))

(define (_eq? a b)
  (_and (_leq? a b)
        (_leq? b a)))

(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))

;; Examples 
(define other-three (pred four))
(define other-two (sub three one))

(define (inc x)
   (+ 1 x))


(display one)

(newline)

(display two)

(newline)

(display ((two inc) 0))
(newline)


(display ((four inc) 0))
(newline)


(display ((other-three inc) 0))
(newline)


(display ((other-two inc) 0))
(newline)


(display ((_true 1) 0))
(newline)


(display ((_false 1) 0))
(newline)

(define b (((_and _false _true) "true") "false"))
(display b)
(newline)


(define b(_if (zero? one) "true" "false"))
(display b)
(newline)

(define b (_if (_leq? three two) "true" "false"))
(display b)
(newline)


(define b (_if (_eq? (add two two) four) "True" "False"))
(display b)
(newline)

(define (show a b op emoji op-sym)
  (define (f _)
    (display emoji))
  ((a f) '())
  (display op-sym)
  ((b f) '())
  (display " = ")
  (((op a b) f) '())
  (newline))

(show two four add "🐱" " + ")
(show three four mul "🐶" " X ")
(show two three pow "🐮" " ^ ")
