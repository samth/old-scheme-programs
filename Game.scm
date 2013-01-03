(define make-grid
  (lambda ()
    (make-list 4 '(- - - -))))

(define column-full?
  (lambda (pos col)
    (not (member? '- (nth col pos)))))

(define tie?
  (lambda (pos)
    (and (column-full? pos 1)
         (column-full? pos 2)
         (column-full? pos 3)
         (column-full? pos 4))))

(define play
  (lambda (pos col char)
    (replace-nth col (replace-nth (position '- (nth col pos)) char (nth col pos)) pos)))

(define display-list
  (lambda (s)
    (cond
      [(null? s) 'done]
      [else (begin (display (first s)) (newline) (display-list (rest s)))])))

(define transpose
  (lambda (s)
    (cond
      [(lnl? s) '()]
      [else (cons (getfirsts s) (transpose (cutfirsts s)))])))

(define transform-game
  (lambda (pos)
    (cons '(1 2 3 4) (reverse (transpose pos)))))

(define display-game
  (lambda (pos)
    (display-list (transform-game pos))))

(define winner?
  (lambda (pos char)
    (or (win-column? pos char)
        (win-row? pos char)
        (win-riser? pos char)
        (win-sinker? pos char))))

(define win-in-this-column?
  (lambda (col char n)
    (cond
      [(= n 3) #t]
      [(null? col) #f]
      [(equal? (first col) char) (win-in-this-column? (rest col) char (add1 n))]
      [else (win-in-this-column? (rest col) char 0)])))

(define win-column?
  (lambda (pos char)
    (or (win-in-this-column? (first pos) char 0)
        (win-in-this-column? (second pos) char 0)
        (win-in-this-column? (third pos) char 0)
        (win-in-this-column? (fourth pos) char 0))))

(define win-row?
  (lambda (pos char)
    (win-column? (transpose pos) char)))

(define adjust-column
  (lambda (col n)
    (cond
      [(zero? n) (cons '- (butlast col))]
      [(one? n) col]
      [else (adjust-column
              (cons-to-end '- (rest col))
              (sub1 n))])))

(define adjust-game
  (lambda (pos)
    (adjust-game-help pos 0)))

(define adjust-game-help
  (lambda (pos n)
    (cond
      [(null? pos) '()]
      [else (cons
              (adjust-column (first pos) n)
              (adjust-game-help
                (rest pos)
                (add1 n)))])))

(define win-riser?
  (lambda (pos char)
    (win-row? (adjust-game pos) char)))

( define win-sinker?
  (lambda (pos char)
    (win-row? (adjust-game (reverse pos)) char)))

(define winning-move?
  (lambda (pos char n)
    (winner? (play pos n char) char)))

(define exists-winning-move?
  (lambda (pos char)
    (exists-help pos char 1)))

(define exists-help
  (lambda (pos char n)
    (cond
      [(column-full? pos n) (exists-help pos char (add1 n))]
      [(winning-move? pos char n) #t]
      [(= n 4) #f]
      [(and (= n 3) (column-full? pos 4)) #f]
      [else (exists-help pos char (add1 n))])))


(define losing-move?
  (lambda (pos ch1 ch2 n)
    (exists-winning-move? (play pos n ch1) ch2)))

(define game-report
  (lambda (game-pos ch1 ch2)
    (game-report-help game-pos ch1 ch2 4 '(ok ok ok ok))))

(define game-report-help
  (lambda (pos ch1 ch2 n report)
    (cond
      [(zero? n) report]
      [(column-full? pos n)
       (game-report-help pos ch1 ch2 (sub1 n) (replace-nth n 'full report))]
      [(winning-move? pos ch1 n)
       (game-report-help
         pos ch1 ch2 (sub1 n)
         (replace-nth n 'win report))]
      [(losing-move? pos ch1 ch2 n)
       (game-report-help
         pos ch1 ch2 (sub1 n)
         (replace-nth n 'lose report))]
      [else (game-report-help
              pos ch1 ch2 (sub1 n) report)])))

(define pick-move
  (lambda (report)
    (cond
      [(member? 'win report) (position 'win report)]
      [(member? 'ok report) (pick (av-moves report ))]
      [else (position 'lose report)])))

(define player-class
  (lambda (my-name)
    (let ((my-char (bucket-class #f))
          (your-char (bucket-class  #f))
          (won (counter-class)) (lost (counter-class))
          (base-object (base-object-class)))
      (lambda (msg)
        (cond
          [(equal? msg 'name) my-name]
          [(equal? msg 'char) (my-char 'get)]
          [(equal? msg 'other-char) (your-char 'get)]
          [(equal? msg 'wins) (won 'get)]
          [(equal? msg 'losses) (lost 'get)]
          [(equal? msg 'winner) (won 'inc)]
          [(equal? msg 'loser) (lost 'inc)]
          [(equal? msg 'new-game)
           (lambda (ch1 ch2)
             (begin
               ((my-char 'set) ch1)
               ((your-char 'set) ch2)))]
          [else (base-object msg)])))))

(define game-over
  (lambda (winner loser)
    (begin
      (winner 'winner)
      (loser 'loser)
      (display (cons (winner 'name) '(won that game))) (newline)
      (display (cons (winner 'name) (append '(has won) (cons (winner 'wins) '(games))))) (newline)
      (display (cons (loser 'name) (append '(has won) (cons (loser 'wins) '(games))))) (newline)
      'done)))

(define play-game
  (lambda (pos next-player prev-player)
    (cond
      [(winner? pos (prev-player 'char))
       (game-over prev-player next-player)]
      [(tie? pos) '(The game is a tie)]
      [else
        (let (( new-game-pos
                (play pos ((next-player 'move) pos) (next-player 'char))))
          (begin
            (display-game new-game-pos)
            (play-game new-game-pos prev-player next-player)))])))

(define play-new-game
  (lambda (player1 player2)
    (begin
      (display-game (make-grid))
      ((player1 'new-game) 'o 'x)
      ((player2 'new-game) 'x 'o)
      (play-game (make-grid) player1 player2))))

(define human-player-class
  (lambda (my-name)
    (let ((me (player-class my-name)))
      (lambda (msg)
        (cond
          [(equal? msg 'move)
           (lambda (pos)
             (begin
               (display
                 (cons (me 'name)
                       '(type a number from 1 to 4)))
               (read)))]
          [else (me msg)])))))

(define computer-player-class
  (lambda (my-name)
    (let ((me (player-class my-name)))
      (lambda (msg)
        (cond
          [(equal? msg 'move)
           (lambda (game-pos)
             (pick-move
               (game-report
                 game-pos
                 (me 'char)
                 (me 'other-char))))]
          [else (me msg)])))))

(define pick
  (lambda (av-list)
    (let ((a (add1 (random (num-in-list av-list)))))
      ;(cond
      ;[(equal? (nth num report) sym) num]
      ;[else (pick sym report (add1 (random 4)))])))
      (cond
        [(and (< 0 a) (< a (num-in-list av-list))) (nth a av-list)]
        [else (nth (num-in-list av-list) av-list)]))))


(define quicker-player-class
  (lambda (my-name)
    (let ((me (player-class my-name))
          (knowledge (pairing-class)))
      (lambda (msg)
        (cond
          [(equal? msg 'recall) knowledge]
          [(equal? msg 'move)
           (lambda (game-pos)
             (begin
               (update-knowledge knowledge game-pos me)
               (pick-move ((knowledge 'lookup) game-pos))))]
          [else (me msg)])))))

(define update-knowledge
  (lambda (plist pos me)
    (cond
      [(not (equal? #f (assoc pos plist))) plist]
      [else ((plist 'new-pair) pos
             (game-report pos (me 'char) (me 'other-char)))])))

(define play-lots
  (lambda (player1 player2 k)
    (cond
      [(and (< (player1 'wins) k) (< (player2 'wins) k))
       (begin
         (play-new-game player1 player2)
         (play-lots player2 player1 k))]
      [else 'done])))

(define av-moves
  (lambda  (report)
    (av-moves-help report 1)))

(define av-moves-help
  (lambda (report acc)
    (cond
      [(null? report) '()]
      [(equal? 'ok (first report)) (cons acc (av-moves-help (rest report) (add1 acc)))]
      [else (av-moves-help (rest report) (add1 acc))])))