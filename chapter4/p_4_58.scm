;ある人が、自分の勤める部署に勤める監督者がいない場合、
;その人を “big shot”(重要人物) であるとする規則を定義せよ。

(rule (big-shot? ?person)
      (and
        (job ?person ?job-kind)
        (job ?same-job-kind-poople ?job-kind)
        (supervispr ?person ?same-job-kind-poople)
        (same ?same-job-kind-poople ()))) ; 空配列と同じ


; bossがいないか、
; bossが他部署にいるか
; の２つの条件が当てはまるみたい
 rule:
 (assert! (rule (bigshot ?person ?division)
                (and (job ?person (?division . ?rest))
                     (or (not (supervisor ?person ?boss))
                         (and (supervisor ?person ?boss)
                              (not (job ?boss (?division . ?r))))))))
