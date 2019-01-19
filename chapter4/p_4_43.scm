(define (distinct? ls)
     (cond
       ((null? ls) #t)
       ((memq (car ls) (cdr ls)) #f)
       (else (distinct? (cdr ls)))))

; 排他的論理和
(define (xor cond1 cond2)
  (if (or (and cond1 (not cond2))
          (and (not cond1) cond2))
      #t
      #f))


; father     daughter    cluiser
;--------------------------------
; dowing     ?           melissa
; hall       ?           rosalind
; barnacle   melissa     gabrielle
; parker     ?           mary
; moore      mary        lorna

; normal
(define (daughters? daughters)
    (define (daughter ls) (car ls))
    (define (cluiser ls) (cadr ls))

    (define (not_own_daughter father)
      (not (eq? (daughter father) (cluiser father))))

    (define (father_from_daughter fathers dname)
      (if (eq? (daughter (car fathers)) dname)
          (car fathers)
          (father_from_daughter (cdr fathers) dname)))

    (let (
          (dowing (list (car daughters) 'melissa))
          (hall (list (cadr daughters) 'rosalind))
          (barnacle (list (caddr daughters) 'gabrielle))
          (parker (list (cadddr daughters) 'mary))
          (moore (list (cadddr (cdr daughters)) 'lorna)))
      (if (and
            (distinct? daughters)
            (eq? (daughter barnacle) 'melissa)
            (eq? (daughter moore) 'mary)
            (not_own_daughter dowing)
            (not_own_daughter hall)
            (not_own_daughter barnacle)
            (not_own_daughter parker)
            (not_own_daughter moore)
            (eq? (cluiser (father_from_daughter (list dowing hall barnacle parker moore) 'gabrielle))
                 (daughter parker)))
          (list dowing hall barnacle parker moore)
          #f)))

(define d1 '(melissa rosalind gabrielle mary lorna))
(define res1 (daughters? d1))

;            dowing hall     barnacle parker  moore
(define d2 '(lorna gabrielle melissa rosalind mary))
(define res2 (daughters? d2))

(define (daughter ls) (car ls))
(define (cluiser ls) (cadr ls))

(define (not_own_daughter father)
  (not (eq? (daughter father) (cluiser father))))

(define (cluiser_name_from_daugher daughter_name)
  (cond
    ((eq? (cluiser dowing) daughter_name) (cluiser dowing))
    ((eq? (cluiser hall) daughter_name) (cluiser hall))
    ((eq? (cluiser barnacle) daughter_name) (cluiser barnacle))
    ((eq? (cluiser parker) daughter_name) (cluiser parker))
    ((eq? (cluiser moore) daughter_name) (cluiser moore))))

; amd
;(define (multiple-dwelling)
;  (define (daughter ls) (car ls))
;  (define (cluiser ls) (cadr ls))
;
;  (define (not_own_daughter fahter)
;    (not (eq? (daughter father) (cluiser father))))
;
;  (define (cluiser_name_from_daugher daughter_name)
;    (cond
;      ((eq? (cluiser dowing) daughter_name) (cluiser dowing))
;      ((eq? (cluiser hall) daughter_name) (cluiser hall))
;      ((eq? (cluiser barnacle) daughter_name) (cluiser barnacle))
;      ((eq? (cluiser parker) daughter_name) (cluiser parker))
;      ((eq? (cluiser moore) daughter_name) (cluiser moore))))
;
;
;  (let (
;        (dowing (list (amb 'melissa 'rosalind 'gabrielle 'lorna 'mary) 'melissa))
;        (hall (list (amb 'melissa 'rosalind 'gabrielle 'lorna 'mary) 'rosalind))
;        (barnacle (list (amb 'melissa 'rosalind 'gabrielle 'lorna 'mary) 'gabrielle))
;        (parker (list (amb 'melissa 'rosalind 'gabrielle 'lorna 'mary) 'lorna))
;        (moore (list (amb 'melissa 'rosalind 'gabrielle 'lorna 'mary) 'mary)))
;
;    (require (distinct? (list (daughter dowing) (daughter hall) (daughter barnacle) (daughter parker) (daughter moore))))
;    (require (eq? (daughter barnacle) 'melissa))
;    (require (eq? (daughter moore) 'mary))
;    (require (not_own_daughter dowing))
;    (require (not_own_daughter hall))
;    (require (not_own_daughter barnacle))
;    (require (not_own_daughter parker))
;    (require (not_own_daughter moore))
;    (require (eq? (cluiser_name_from_daugher 'gabrielle) (daugher parker)))
;    (list dowing hall barnacle parker moore)))
