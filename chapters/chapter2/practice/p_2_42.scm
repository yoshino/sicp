(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (queens board-size)
   (define (queen-cols k)
     (if (= k 0)
         (list empty-board)
         (filter
          (lambda (positions) (safe? k positions))
          (flatmap
           (lambda (rest-of-queens)
             (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
           (queen-cols (- k 1))))))
   (queen-cols board-size))

(define (make-position row col)
  (cons row col))

(define (position-row position)
  (car position))

(define (position-col position)
  (cdr position))

(define empty-board ())

(define (adjoin-position row col positions)
  (append positions (list (make-position row col))))

(define (safe? col positions)
   (let ((kth-queen (list-ref positions (- col 1)))
         (other-queens (filter (lambda (q) (not (= col (position-col q)))) positions)))

   (define (attacks? q1 q2)
     (or (= (position-row q1) (position-row q2))
         (= (abs (- (position-row q1) (position-row q2)))
            (abs (- (position-col q1) (position-col q2))))))

   (define (iter q board)
     (or (null? board)
         (and (not (attacks? q (car board)))
              (iter q (cdr board)))))
   (iter kth-queen other-queens)))

;gosh> (list-ref (list 1 2 3 4 5) 3)
;4

;gosh> (queens 2)
;()
;gosh> (queens 3)
;()
;gosh> (queens 4)
;(((2 . 1) (4 . 2) (1 . 3) (3 . 4)) ((3 . 1) (1 . 2) (4 . 3) (2 . 4)))
;gosh> (queens 5)
;(((1 . 1) (3 . 2) (5 . 3) (2 . 4) (4 . 5)) ((1 . 1) (4 . 2) (2 . 3) (5 . 4) (3 . 5)) ((2 . 1) (4 . 2) (1 . 3) (3 . 4) (5 . 5)) ((2 . 1) (5 . 2) (3 . 3) (1 . 4) (4 . 5)) ((3 . 1) (1 . 2) (4 . 3) (2 . 4) (5 . 5)) ((3 . 1) (5 . 2) (2 . 3) (4 . 4) (1 . 5)) ((4 . 1) (1 . 2) (3 . 3) (5 . 4) (2 . 5)) ((4 . 1) (2 . 2) (5 . 3) (3 . 4) (1 . 5)) ((5 . 1) (2 . 2) (4 . 3) (1 . 4) (3 . 5)) ((5 . 1) (3 . 2) (1 . 3) (4 . 4) (2 . 5)))
