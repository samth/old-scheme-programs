(define second
  (lambda (n) (first (rest n))))
(define base-object-class
  (lambda ()
    (lambda (msg)
      (error (cons 'Message (cons msg
                                  '(not understood by object))))))))
(define last
  (lambda (s)
    (cond
      [(null? (rest s)) (first s)]
      [else (last (rest s))])))

(define subst
  (lambda (a b s)
    (cond
      [(null? s) '()]
      [(equal? (first s) a) (cons b (rest s))]
      [else (cons
              (first s)
              (subst a b (rest s)))])))
(define bucket-class
  (lambda (contents)
    (let ((base-object (base-object-class)))
      (lambda (msg)
        (cond
          [(equal? msg 'get) contents]
          [(equal? msg 'set) (lambda (x)
                               (begin (set! contents x) x))]
          [else (base-object msg)])))))

(define counter-class
  (lambda ()
    (let ((counter (bucket-class 0)))
      (lambda (msg)
        (cond
          [(equal? msg 'inc)
           ((counter 'set) (add1 (counter 'get)))]
          [(equal? msg 'dec)
           ((counter 'set) (sub1 (counter 'get)))]
          [else (counter msg)])))))
(define container-class
  (lambda ()
    (let ((contents (bucket-class '())))
      (lambda (msg)
        (cond
          [(equal? msg 'empty?)
           (null? (contents 'get))]
          [(equal? msg 'flush)
           ((contents 'set) '())]
          [(equal? msg 'member?)
           (lambda (x) (member? x (contents 'get)))]
          [else (contents msg)])))))

(define make-list
  (lambda (n r)
    (make-list-help n r '())))
(define fourth
  (lambda (s)
    (first (rest (rest (rest s))))))
(define make-list-help
  (lambda (n r acc)
    (cond
      [(zero? n) acc]
      [else (make-list-help (sub1 n) r (cons r acc))])))
(define member?
  (lambda (a s)
    (cond
      [(null? s) #f]
      [(equal? a (first s)) #t]
      [else (member? a (rest s))])))
;(load "tsginit.scm")
(define ascii '((! 33) (#\# 35) ($ 36) (* 42) (+ 43) (- 45) (/ 47) (0 48) (1 49) (2 50) (3 51) (4 52)
                (5 53) (6 54)
                (7 55) (8 56) (9 57) (= 61) (? 63) (A 65) (B 66) (C 67)
                (D 68) (E 69) (F 70) (G 71) (H 72) (I 73) (J 74)
                (K 75) (L 76) (M 77) (N 78) (O 79) (P 80) (Q 81)
                (R 82) (S 83) (T 84) (U 85) (V 86) (W 87) (X 88)
                (Y 89) (Z 90) (a 97) (b 98) (c 99)
                (d 100) (e 101) (f 102) (g 103) (h 104) (i 105) (j 106) (k 107)
                (l 108) (m 109) (n 110) (o 111) (p 112) (q 113) (r 114) (s 115)
                (t 116)
                (u 117) (v 118) (w 119) (x 120) (y 121) (z 122)))
(define divides?
  (lambda (a b)
    (zero? (remainder b a))))
(define square
  (lambda (n)
    (* n n)))
;(load "Stm_win.scm")
(define third
  (lambda (n) (first (rest (rest n)))))
(define lat?
  (lambda (l)
    (cond
      [(null? a ) #t]
      [(atom? (first a)) (lat? (rest a))]
      [else #f])))
(define rember
  (lambda (a s)
    (cond
      [(null? s) '()]
      [(equal? (first s) a) (rest s)]
      [else (cons (first s) (rember a (rest s)))])))
(define one?
  (lambda (z)
    (zero? (sub1 z))))
(define lookup
  (lambda (s plist)
    (cond
      [(equal? s (first (first plist))) (second (first plist))]
      [else (lookup s (rest plist))])))
(define accumulate
  (lambda (op init r)
    (cond
      [(null? r) init]
      [else (op (first r)
                (accumulate op init (rest r)))])))
(define product
  (lambda (x)
    (accumulate * 1 x)))
(define nth
  (lambda (k s)
    (cond
      [(null? s) #f]
      [(one? k) (first s)]
      [else (nth (sub1 k) (rest s))])))
(define map-curry
  (lambda (f)
    (lambda (r)
      (cond
        [(null? r) '()]
        [else (cons (f (first r))
                    ((map-curry f) (rest r)))]))))
(define getfirsts (map-curry first))
(define cutfirsts (map-curry rest))
(define list-checker
  (lambda (s f)
    (cond
      [(null? s) #t]
      [(f (first s)) (list-checker (rest s) f)]
      [else #f])))
(define null-list?
  (lambda (a)
    (equal? a '())))
(define lnl?
  (lambda (s)
    (list-checker s null-list?)))
(define cons-to-end
  (lambda (a s)
    (append s (cons a '()))))
(define butlast
  (lambda (s)
    (reverse (rest (reverse s)))))
(define replace-nth
  (lambda (k a r)
    (cond
      [(one? k) (cons a (rest r))]
      [else (cons (first r) (replace-nth (sub1 k) a (rest r)))])))
(define position
  (lambda (a r)
    (position-help a r 1)))
(define position-help
  (lambda (a r n)
    (cond
      [(equal? a (first r)) n]
      [else (position-help a (rest r) (add1 n))])))

(define numlist?
  (lambda (a)
    (list-checker a number?)))

(define modify
  (lambda (f old new r)
    (cond
      [(null? r) '()]
      [(equal? old (first r)) (f old new (rest r))]
      [else (cons (first r)
                  (modify f old new (rest r)))])))

(define filter
  (lambda (f s)
    (cond
      [(null? s) '()]
      [(f (first s)) (cons (first s) (filter f (rest s)))]
      [else (filter f (rest s))])))

(define filter-curry
  (lambda (f)
    (lambda (s)
      (cond
        [(null? s) '()]
        [(f (first s)) (cons (first s) (filter-curry f (rest s)))]
        [else (filter-curry f (rest s))]))))

(define compose
  (lambda (f g)
    (lambda (x)
      (g (f x)))))

(define assoc-f
  (lambda (r plist succ fail)
    (cond
      [(null? plist) (fail r)]
      [(equal? r (first (first plist)))
       (succ (first plist))]
      [else (assoc-f r (rest plist) succ fail)])))

(define interval
  (lambda (m n)
    (cond
      [(= m n) '()]
      [else (cons m (interval (add1 m) n))])))

(define fib
  (lambda (n)
    (cond
      [(or (one? n) (one? (sub1 n))) 1]
      [else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))])))

(define sieve
  (lambda (nums)
    (cond
      [(null? nums) '()]
      [else (cons
              (first nums)
              (sieve (filter
                       (lambda (x)
                         (not (divides? (first nums) x))) (rest nums))))])))

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
(define less?
  (lambda (a b)
    (< a b)))

;(include "STREAMS.SCM")

(define doubleton
  (lambda (a b)
    (cons a (cons b '()))))


(define pairing-class
  (lambda ()
    (let ((plist (container-class)))
      (lambda (msg)
        (cond
          [(equal? msg 'new-pair)
           (lambda (key value)
             ((plist 'set)
              (cons
                (doubleton key value)
                (plist 'get))))]
          [(equal? msg 'get-pair)
           (lambda (key) (assoc key (plist 'get)))]
          [(equal? msg 'remove-pair)
           (lambda (key)
             (let ((pairs (plist 'get)))
               ((plist 'set)
                (rember (assoc key pairs) pairs))))]
          [(equal? msg 'lookup)
           (lambda (key)
             (lookup
               key (plist 'get)))]
          [else (plist msg)])))))

(define num-in-list
  (lambda (s)
    (nil-help s 0)))

(define nil-help
  (lambda (s acc)
    (cond
      [(null? s)  acc]
      [else (nil-help (rest s) (add1 acc))])))




