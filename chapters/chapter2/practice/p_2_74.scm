; a
; 本部が各事業所へ向けて従業員情報をget
; 一意に決まるものをparameterとしてgetする
(define (get-record employee_id)
  ((get 'get-record employee_id)))

(define (id record) (car record))

; (list (list 'id 1) (list 'id 2) (list 'id 3))

; [ {id: 1, ... },
;   {id: 2, ... },
;   {id: 3, ... },
;   {id: 4, ... }
  ]

(define (id record) (car record))

; b
; 事業部へ給与情報をGETする
(define (get-salary employee_id)
  ((get 'get-salary employee_id)))

; (list (list 'id 1) (list 'salary 3000))

; [ {id: 1, salary: 3000 ... },
;   {id: 2, salary: 5000... },
;   {id: 3, ... },
;   {id: 4, ... }
  ]

(define (salary return_id record)
  (if (eq? (id record) return_id)
    (car (cdr record))))


; c
; 前事業所の中から従業員のレコードを探す

(define (find-emploee-reocord name files)
  ; 1つの事業所の中でnameを探す
  (get-reord name (car files))

  ; なければ別の事業所を再帰的に探していく
  (find-emploee-reocord name (cdr files))

; d
; 新しい会社を組み入れた場合、本部での変更はしないでOK。
; 新しい会社のデータ構造が正しければ良い。
