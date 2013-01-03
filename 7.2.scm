
  
(define stream-nth
  (lambda (k s)
    (cond
      [(one? k) (head s)]
      [else (stream-nth (sub1 k) (tail s))])))
  
(define stream-map
  (lambda (f s)
    (cond
      [(empty-stream? s) the-empty-stream]
      [else (cons-stream
              (f (head s))
              (stream-map f (tail s)))])))
  
(define wholes
  (lambda (n)
    (cons-stream n (wholes (add1 n)))))
  
(define w (wholes 0))
  
(define naturals (wholes 1))
  
(define multiples-stream
  (lambda (m)
    (multiples-stream-help m 0)))
  
(define multiples-stream-help
  (lambda (m n)
    (cons-stream
      (+ m n) (multiples-stream-help m (+ m n)))))
  
(define fib-stream-maker
  (lambda (a b)
    (cons-stream b
                 (fib-stream-maker b (+ a b)))))
  
(define stream-add
  (lambda (r s)
    (cond
      [(empty-stream? r) s]
      [(empty-stream? s) r]
      [else (cons-stream
              (+ (head r) (head s))
              (stream-add (tail r) (tail s)))])))
  
(define two-up(tail naturals))
  
(define stream-seive
  (lambda (str)
    (cond
      [(empty-stream? str) the-empty-stream]
      [else (cons-stream
              (head str)
              (stream-seive
                (stream-filter
                  (lambda (x)
                    (not (divides? (head str) x)))
                  (tail str))))])))
  
  