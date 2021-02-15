;;          -----------
;; --------  Level ADT  ---------
;;          -----------

(load "alienvloot-adt.rkt")

(define (maak-level aantal-cellen-breedte aantal-cellen-hoogte)
  (let* ((raket-start-positie
          (maak-positie 0 (- aantal-cellen-hoogte 8)))
         (raket-adt (maak-raket raket-start-positie))
         (alienvloot-adt (maak-alienvloot))
         (kogels (maak-kogels-lijst))
         (vloot-tijd 0)
         (kogel-tijd 0))

    ;; beweeg-raket! : symbol -> /
    (define (beweeg-raket! toets)
      (cond ((eq? toets 'left)
             ((raket-adt 'beweeg!) 'links))
            ((eq? toets 'right)
             ((raket-adt 'beweeg!) 'rechts))))
      

    ;; beweeg-vloot! : / -> /
    (define (beweeg-vloot!)
      ((alienvloot-adt 'beweeg)))


    ;; vuur-kogel-af! : symbol -> /
    (define (vuur-kogel-af! toets)
      (if (eq? toets '#\space)
          ((kogels 'voeg-kogel-toe!) (maak-kogel (raket-adt 'positie)))))


    ;; beweeg-kogel! : / -> /
    (define (beweeg-kogels!)
      (let ((lijst-kogels (kogels 'kogels-lijst)))
        (if (not (null? lijst-kogels))
            ((kogels 'voor-alle-kogels) roep-beweeg-op))))

    (define (roep-beweeg-op kogel-adt)
      ((kogel-adt 'beweeg!)))
    

    ;; update! : number -> /
    (define (update! tijdsverschil)
      (set! vloot-tijd (+ vloot-tijd 10))
      (if (= vloot-tijd snelheid-vloot)
          (begin (beweeg-vloot!)
                 (set! vloot-tijd 0)))
      ; Als er een kogel bestaat dan beweeg je hem
      (set! kogel-tijd (+ kogel-tijd 10))
      (if (and (not (null? (kogels 'kogels-lijst)))
               (>= kogel-tijd snelheid-kogel))
          (begin (beweeg-kogels!)
                 (set! kogel-tijd 0))))
    
    
    ;; toets! : any -> /
    (define (toets! toets)
      (beweeg-raket! toets)
      (vuur-kogel-af! toets))
    
    
    ; Dispatch
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'toets!) toets!)
            ((eq? msg 'raket) raket-adt)
            ((eq? msg 'kogels) kogels)
            ((eq? msg 'alienvloot) alienvloot-adt)
            (else (display "ongeldige boodschap"))))

    dispatch-level))
            

    
    