(define interval
  (lambda (m n)
    (cond
      [(< n m) '()]
      [else (cons m
                  (interval (add1 m) n))])))

(define fib
  (lambda (n)
    (cond
      [(or (one? n) (one? (sub1 n))) 1]
      [else (+
              (fib (sub1 n))
              (fib (sub1 (sub1 n))))])))

(define map
  (lambda (f nums)
    (cond
      [(null? nums) '()]
      [else (cons (f (first nums))
                  (map f (rest nums)))])))

(define fibnums
  (lambda (nums)
    (map fib nums)))

(define filter
  (lambda (f nums)
    (cond
      [(null? nums) '()]
      [(f (first nums)) (cons (first nums) (filter f (rest nums)))]
      [else (filter f (rest nums))])))

(define pick-evens
  (lambda (nums)
    (filter even? nums)))

(define seive
  (lambda (nums)
    (cond
      [(null? nums) '()]
      [else (cons
              (first nums)
              (seive
                (filter
                  (lambda (x)
                    (not (divides? (first nums) x)))
                  (rest nums))))])))

(define prime-div-list
  (lambda (n)
    (prime-div-help n '() 2)))

(define prime-div-help
  (lambda (n acc div)
    (cond
      [(= n div) (cons div acc)]
      [(divides? div n) (prime-div-help
                          (quotient n div) (cons div acc) div)]
      [else (prime-div-help
              n acc (add1 div))])))

(define prime1?
  (lambda (n)
    (cond
      [(or (one? n) (zero? n)) #f]
      [else (null? (rest (prime-div-list n)))])))

(define prime?
  (lambda (n)
    (cond
      [(< n 2) #f]
      [(even? n) (= n 2)]
      [else (prime?-help n 3)])))

(define prime?-help
  (lambda (n div)
    (cond
      [(< n (square div)) #t]
      [(divides? div n) #f]
      [else (prime?-help n (+ 2 div))])))

  
  