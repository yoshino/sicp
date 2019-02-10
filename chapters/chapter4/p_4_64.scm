; 地位が上かどうかを判定する
; 元々の実装:再帰を使っている
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

; Louis Reaoner
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss) ; 関係のないmiddle-managerのbossが出力される
               (supervisor ?staff-person ?middle-manager))))

(outranked-by (Bitdiddle Ben) ?who)
; orの代替の方を評価する
; (outranked-by ?middle-manager ?who)
; (supervisor ?middle-manager ?who)
; orの代替の方を評価する
; 無限ループ!
; (outranked-by ?middle-manager ?who) ;未規定のものを残して次のループへ行く場合は無限ループ

