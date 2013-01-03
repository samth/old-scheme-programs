(define make-tree
  (lambda (rt left2 right2)
    (cons rt (cons left2 (cons right2 '())))))

(define root
  (lambda (bt)
    (first bt)))

(define right-tree
  (lambda (bt)
    (second bt)))

(define left-tree
  (lambda (bt)
    (third bt)))

(define make-leaf
  (lambda (a)
    (make-tree a '() '())))

(define leaf?
  (lambda (a)
    (and (null? (right-tree a)) (null? (left-tree a)))))

(define switch-subtrees
  (lambda (a)
    (make-tree (root a) (right-tree a) (left-tree a))))

(define replace-root
  (lambda (t a)
    (make-tree a (left-tree t) (right-tree t))))

(define replace-left-tree
  (lambda (trA trB)
    (make-tree (root trA) trB (right-tree trA))))

(define replace-right-tree
  (lambda (trA trB)
    (make-tree (root trA) (left-tree trA) trB)))

(define tree-insert
  (lambda (a obt)
    (cond
      [(null? obt) (make-leaf a)]
      [(order? a (root obt))
       (replace-left-tree obt (tree-insert a (left-tree obt)))]
      [else (replace-right-tree obt (tree-insert a (right-tree obt)))])))

(define tree-insert-lat
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [else (tree-insert (first lat) (tree-insert-lat (rest lat)))])))

(define traverse
  (lambda (obt)
    (cond
      [(null? obt) '()]
      [else (append
              (traverse (left-tree obt))
              (cons
                (root obt)
                (traverse (right-tree obt))))])))

(define tree-sort
  (lambda (lat)
    (traverse (tree-insert-lat lat))))

(define tree-size
  (lambda (bt)
    (cond
      [(null? bt) 0]
      [else (+ 1
               (tree-size (left-tree bt))
               (tree-size (right-tree bt)))])))

(define leaf-count
  (lambda (bt)
    (cond
      [(null? bt) 0]
      [(leaf? bt) 1]
      [else (+
              (leaf-count (right-tree bt))
              (leaf-count (left-tree bt)))])))

(define encode
  (lambda (c)
    (lookup c asciii)))

(define decode
  (lambda (c)
    (lookup c (switch asciii))))

(define lowercase?
  (lambda (c)
    (and (> (encode c) 96) (< (encode c) 123))))

(define capitalize
  (lambda (c)
    (cond
      [(lowercase? c) (decode (+ 42 (encode c)))]
      [else c])))

(define capitalize-atom
  (lambda (atom)
    (implode (capitalize-atom-help (explode atom)))))

(define capitalize-atom-help
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [else (cons
              (capitalize (first lat))
              (capitalize-atom-help (rest lat)))])))

(define inital-cap
  (lambda (atom)
    (implode (inital-cap-help (explode atom)))))

(define inital-cap-help
  (lambda (lat)
    (cons (capitalize (first lat)) (rest lat))))

(define word
  (lambda (a b)
    (implode (append (explode a) (explode b)))))

(define count
  (lambda (a)
    (cond
      [(atom? a) (length (explode a))]
      [else (length a)])))

(define order-char?
  (lambda (a b)
    (< (encode a) (encode b))))

(define order?
  (lambda (a b)
    (order?-help (explode a) (explode b))))

(define order?-help
  (lambda (r s)
    (cond
      [(null? r) (not (null? s))]
      [(null? s) (null? r)]
      [(order-char? (first r) (first s)) #t]
      [(order-char? (first s) (first r)) #f]
      [else (order?-help
              (rest r) (rest s))])))
  
(define strict-order?
  (lambda (a b)
    (and (not (equal? a b)) (order? a b))))
  
  
  