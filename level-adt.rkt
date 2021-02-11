;;          -----------
;; --------  Level ADT  ---------
;;          -----------

(load "alienvloot-adt.rkt")

(define (maak-level aantal-cellen-breedte aantal-cellen-hoogte)
  (let* ((raket-start-positie
          (maak-positie 0 (- aantal-cellen-hoogte 8)))
         (raket-adt (maak-raket raket-start-positie))
         (alienvloot-adt (maak-alienvloot))
         (vloot-tijd 0))

    ;; beweeg-raket! : symbol -> /
    (define (beweeg-raket! toets)
      (cond ((eq? toets 'left)
             ((raket-adt 'beweeg!) 'links))
            ((eq? toets 'right)
             ((raket-adt 'beweeg!) 'rechts))))
      

    ;; beweeg-vloot! : / -> /
    (define (beweeg-vloot!)
      ((alienvloot-adt 'beweeg)))
    

    ;; update! : number -> /
    (define (update! tijdsverschil)
      (set! vloot-tijd (+ vloot-tijd 10))
      (if (= vloot-tijd snelheid-vloot)
          (begin (beweeg-vloot!)
                 (set! vloot-tijd 0))))
    
    ;; toets! : any -> /
    (define (toets! toets)
      (beweeg-raket! toets))
    
    ; Dispatch
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'toets!) toets!)
            ((eq? msg 'raket) raket-adt)
            ((eq? msg 'alienvloot) alienvloot-adt)))

    dispatch-level))
            

    
    