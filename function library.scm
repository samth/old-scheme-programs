(define second
  (lambda (a)
    (first (rest a))))

(define xor
  (lambda (a b)
    (not (equal? a b))))

(define inor
  (lambda (a b)
    (cond
      [a a]
      [else b])))

(define nor
  (lambda (a b)
    (not (inor a  b))))

(define iff
  (lambda (a b)
    (cond
      [a b]
      [else (not b)])))

(define nand
  (lambda (a b)
    (not (and a b))))

(define imp
  (lambda (a b)
    (cond
      [a b]
      [else #t])))

(define lat?
  (lambda (a)
    (cond
      [(null? a) #t]
      [(atom? (first a)) (lat? (rest a))]
      [else #f])))

(define member?
  (lambda (a s)
    (cond
      [(null? s) #f]
      [(equal? a (first s)) #t]
      [else (member? a (rest s))])))

(define atom-occur?
  (lambda (a s)
    (cond
      [(null? s) #f]
      [(equal? (first s) a) #t]
      [(atom? (first s)) (atom-occur? (rest s))]
      [else (inor (atom-occur? (first s)) (atom-occur? (rest s)))])))

(define rember
  (lambda (a s)
    (cond
      [(null? s) '()]
      [(equal? (first s) a) (rest s)]
      [else (cons (first s) (rember a (rest s)))])))

(define insertl
  (lambda (a b s)
    (cond
      [(null? s) '()]
      [(equal? (first s) a) (cons b s)]
      [else (cons
              (first s)
              (insertl a b (rest s)))])))

(define insertr
  (lambda (a b s)
    (cond
      [(null? s) '()]
      [(equal? (first s) a) (cons (first s) (cons b ( rest s)))]
      [else (cons
              (first s)
              (insertr a b (rest s)))])))

(define subst
  (lambda (a b s)
    (cond
      [(null? s) '()]
      [(equal? (first s) a) (cons b (rest s))]
      [else (cons
              (first s)
              (subst a b (rest s)))])))

(define one?
  (lambda (a)
    (equal? a 1)))

(define nth
  (lambda (k s)
    (cond
      [(one? k) (first s)]
      [else (nth (sub1 k) (rest s))])))

(define square
  (lambda (a)
    (* a a)))

(define factorial
  (lambda (a)
    (cond
      [(zero? a) 1]
      [else (* a (factorial (sub1 a)))])))

(define power
  (lambda (x y)
    (cond
      [(one? y) x]
      [(even? y) (power (square x) (quotient y 2))]
      [else (* x (power x (sub1 y)))])))

(define doubleton
  (lambda (a b)
    (cons a (cons b '()))))

(define lookup
  (lambda (s plist)
    (cond
      [(equal? s (first (first plist)))
       (second (first plist))]
      [else (lookup s (rest plist))])))

(define make-plist
  (lambda (r s)
    (cond
      [(null? r) '()]
      [else (cons
              (doubleton (first r) (first s))
              (make plist (rest r) (rest s)))])))

(define remove
  (lambda (s plist)
    (rember (doubleton s (lookup s plist)) plist)))

  
  
  