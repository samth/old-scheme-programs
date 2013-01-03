(define switch
  (lambda (plist)
    (cond
      [(null? plist) '()]
      [else (cons
              (doubleton (second (first plist)) (first (first plist)))
              (switch (rest plist))]))))
(define sum
  (lambda (nums)
    (sum-help nums 0)))

(define sum-help
  (lambda (nums acc)
    (cond
      [(null? nums) acc]
      [else (sum-help
              (rest nums)
              (+ acc (first nums)))])))
(define one?
  (lambda (z)
    (zero? (sub1 z))))


(define nth
  (lambda (k s)
    (cond
      [(one? k) (first s)]
      [else (nth (sub1 k) (rest s))])))

(define sum-nth-row
  (lambda (k matrix)
    (sum (nth k matrix))))

(define nth-column
  (lambda (k mat)
    (cond
      [(null? mat) '()]
      [else (cons (nth k (first mat))
                  (nth-column k (rest mat)))])))
(define sum-nth-column
  (lambda (k matrix)
    (sum (nth-column k matrix))))
(define sum-matrix
  (lambda (mat)
    (cond
      [(null? mat) 0]
      [else (+ (sum (first mat)) (sum-matrix (rest mat)))])))

(define min-num
  (lambda (nums)
    (min-num-help (rest nums) (first nums))))

(define min-num-help
  (lambda (nums acc)
    (cond
      [(null? nums) acc]
      [(< (first nums) acc) (min-num-help (rest nums) (first nums))]
      [else (min-num-help (rest nums) acc)])))

(define rember
  (lambda (a s)
    (cond
      [(null? s) '()]
      [(equal? (first s) a) (rest s)]
      [else (cons (first s) (rember a (rest s)))])))

(define max-num
  (lambda (nums)
    (max-num-help nums 0)))

(define max-num-help
  (lambda (nums acc)
    (cond
      [(null? nums) acc]
      [(< acc (first nums)) (max-num-help (rest nums) (first nums))]
      [else (max-num-help (rest nums) acc)])))


(define next-please
  (lambda (nums)
    (next-please-help nums (max-num nums))))
(define next-please-help
  (lambda (nums biggest)
    (cons biggest (rember biggest nums))))

(define selection-sort
  (lambda (nums)
    (cond
      [(null? nums) '()]
      [else (cons (first (next-please nums))
                  (selection-sort (rest (next-please nums))))])))

(define array-ref
  (lambda (a idx-list)
    (cond
      [(null? (rest idx-list))
       (list-ref a (first idx-list))]
      [else (array-ref
              (list-ref
                a (first idx-list))
              (rest idx-list))])))

(define lat?
  (lambda (l)
    (cond
      [(null? a ) #t]
      [(atom? (first a)) (lat? (rest a))]
      [else #f])))

(define ages
  (lambda (a)
    (cond
      [(lat? a) (list-ref a 3)]
      [else (append (ages (first a)) (ages (rest a)))])))





(define replace-entry
  (lambda (a idx-list new)
    (subst (array-ref a idx-list) new (array-ref a (butlast idx-list)))))
