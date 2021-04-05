;;          -----------
;; --------  Level ADT  ---------
;;          -----------

(load "alienvloot-adt.rkt")
(#%require (only random))

(define (maak-level aantal-cellen-breedte aantal-cellen-hoogte)
  (let* ((raket-start-positie
          (maak-positie 0 (- aantal-cellen-hoogte 8)))
         (raket-adt (maak-raket raket-start-positie))
         (alienvloot-adt (maak-alienvloot))
         (kogels-adt (maak-kogels-adt))
         (vloot-tijd 0)
         (kogel-tijd 0)
         (alien-schiettijd 0))

    ;; beweeg-raket! : symbol -> /
    (define (beweeg-raket! toets)
      (cond ((eq? toets 'left)
             ((raket-adt 'beweeg!) 'links))
            ((eq? toets 'right)
             ((raket-adt 'beweeg!) 'rechts))))
      

    ;; beweeg-vloot! : / -> /
    (define (beweeg-vloot!)
      (if (>= vloot-tijd snelheid-vloot)
          (begin
            ((alienvloot-adt 'beweeg))
            (set! vloot-tijd 0))))
    

    ;; schiet-raketkogel! : symbol -> /
    (define (schiet-raketkogel! toets)
      (if (eq? toets '#\space)
          (let ((nieuwe_kogel (maak-kogel (maak-positie ((raket-adt 'positie) 'x)
                                                        ((raket-adt 'positie) 'y))
                                          'raket)))
            ((kogels-adt 'voeg-kogel-toe!) nieuwe_kogel))))

    
    ;; schiet-alienkogel! : / -> /
    ;(define (schiet-alienkogel!)
      


    ;; beweeg-kogel! : / -> /
    (define (beweeg-kogels!)
      (if (and (not (null? (kogels-adt 'kogels-lijst)))
               (>= kogel-tijd snelheid-kogel))
          (let ((lijst-kogels (kogels-adt 'kogels-lijst)))
            (begin
              ((kogels-adt 'voor-alle-kogels) roep-beweeg-op)
              (set! kogel-tijd 0)))))
    

    (define (roep-beweeg-op kogel-adt)
      (let* ((y ((kogel-adt 'positie) 'y))
             (raakt-rand? (< y 0)))
        (if (not raakt-rand?)
            ((kogel-adt 'beweeg!))
            ((kogel-adt 'stop)))))


    (define (check-geraakt kogels-adt alienvloot-adt teken-adt)
      (let ((kogels-lijst (kogels-adt 'kogels-lijst))
            (aliens-lijst (alienvloot-adt 'schepen)))
        ; itereren over alle kogels
        (define (iter kogels-lijst aliens-lijst)
          (if (not (null? kogels-lijst))
              (let ((kogel (car kogels-lijst)))
                ; positie-kogel = positie-alien + levens-alien = 1? -> verwijder het schip
                ; positie-kogel = positie-alien + levens-alien not 1 -> trek 1 leven af van alien
                ((alienvloot-adt 'voor-alle-schepen)
                 (lambda (alien) (cond ((and (((alien 'positie) 'gelijk?) (kogel 'positie))
                                             (= (alien 'levens) 1))
                                        (begin (verwijder-alienschip! alien)
                                               (verwijder-kogel! kogel)
                                               ((teken-adt 'verwijder-kogel!) kogel)))
                                       ((((alien 'positie) 'gelijk?) (kogel 'positie))
                                        ((alien 'levens!) (- (alien 'levens) 1))
                                        (verwijder-kogel! kogel)
                                        ((teken-adt 'verwijder-kogel!) kogel)))))
                (iter (cdr kogels-lijst) aliens-lijst))))
        (iter kogels-lijst aliens-lijst)))
    


    ; individueel alienschip verwijderen van scherm
    (define (verwijder-alienschip! alienschip-adt)
      ((alienvloot-adt 'verwijder-schip!) alienschip-adt))
    

    ; individuele kogel verwijderen van scherm
    (define (verwijder-kogel! kogel-adt)
      ((kogels-adt 'verwijder-kogel!) kogel-adt))

    

    ;; update! : number -> /
    (define (update! tijdsverschil teken-adt)
      (set! vloot-tijd (+ vloot-tijd tijdsverschil))
      (beweeg-vloot!)
      (set! kogel-tijd (+ kogel-tijd tijdsverschil))
      (beweeg-kogels!)
      (set! alien-schiettijd (+ alien-schiettijd tijdsverschil))
      (schiet-alienkogel!)
      (check-geraakt kogels-adt alienvloot-adt teken-adt))
    
    
    ;; toets! : any -> /
    (define (toets! toets)
      (beweeg-raket! toets)
      (schiet-raketkogel! toets))
    
    
    ; Dispatch
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'toets!) toets!)
            ((eq? msg 'raket) raket-adt)
            ((eq? msg 'kogels) kogels-adt)
            ((eq? msg 'alienvloot) alienvloot-adt)
            (else (display "ongeldige boodschap"))))

    dispatch-level))
            

    
