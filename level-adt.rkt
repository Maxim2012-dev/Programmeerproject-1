;;          -----------
;; --------  Level ADT  ---------
;;          -----------

(load "alienvloot-adt.rkt")
(#%require srfi/27)

(define (maak-level aantal-cellen-breedte aantal-cellen-hoogte)
  (let* ((raket-start-positie
          (maak-positie raket-start-x raket-start-y))
         (raket (maak-raket raket-start-positie))
         (alienvloot (maak-alienvloot))
         (kogels (maak-kogels-adt))
         (score (maak-score))
         (alien-schiettijd 0)
         (vloot-tijd 0)
         (kogel-tijd 0))


    ;; --------------- BEWEEG - OPERATIES ---------------
    
    ;; beweeg-raket! : symbol -> /
    (define (beweeg-raket! toets)
      (cond ((eq? toets 'left)
             ((raket 'beweeg!) 'links))
            ((eq? toets 'right)
             ((raket 'beweeg!) 'rechts))))
      

    ;; beweeg-vloot! : / -> /
    (define (beweeg-vloot!)
      (if (>= vloot-tijd snelheid-vloot)
          (begin
            ((alienvloot 'beweeg))
            (set! vloot-tijd 0))))


     ;; beweeg-kogel! : / -> /
    (define (beweeg-kogels!)
      (if (and (not (null? (kogels 'kogels-lijst)))
               (>= kogel-tijd snelheid-kogel))
          (let ((lijst-kogels (kogels 'kogels-lijst)))
            (begin
              ((kogels 'voor-alle-kogels) roep-beweeg-op)
              (set! kogel-tijd 0)))))


     ;; roep-beweeg-op : kogel-adt -> /
    (define (roep-beweeg-op kogel-adt)
      (let* ((y ((kogel-adt 'positie) 'y))
             (raakt-rand? (((kogel-adt 'positie) 'rand-verticaal?)))
             (type-kogel (kogel-adt 'type)))
        (if (not raakt-rand?)
            ((kogel-adt 'beweeg!) type-kogel)
            ((kogels 'verwijder-kogel!) kogel-adt))))
    


    ;; --------------- SCHIET - OPERATIES ---------------
    

    ;; schiet-raketkogel! : symbol -> /
    (define (schiet-raketkogel! toets)
      (if (eq? toets '#\space)
          (let ((nieuwe_kogel (maak-kogel (maak-positie ((raket 'positie) 'x)
                                                        (- ((raket 'positie) 'y) 1))
                                          'raket)))
            ((kogels 'voeg-kogel-toe!) nieuwe_kogel))))

    
    ;; schiet-alienkogel! : / -> /
    (define (schiet-alienkogel!)
      (if (>= alien-schiettijd delay-alienschot)
          ; willekeurige alien uit het vloot kiezen
          (let* ((alien-matrix (alienvloot 'schepen))
                 (inner-vector (vector-ref alien-matrix (random-integer aantal-rijen-aliens)))
                 (schietende-alien (vector-ref inner-vector (random-integer aantal-aliens-per-rij)))
                 (nieuwe_kogel (maak-kogel (maak-positie ((schietende-alien 'positie) 'x)
                                                         ((schietende-alien 'positie) 'y))
                                           'alien)))
            ((kogels 'voeg-kogel-toe!) nieuwe_kogel)
            (set! alien-schiettijd 0))))
    
    

    ;; Dit is de procedure die constant checkt of één van de kogels
    ;; ofwel een alien ofwel de raket raakt.
    ;; check-geraakt : Teken-adt -> /
    (define (check-geraakt teken-adt)
      (let ((kogels-lijst (kogels 'kogels-lijst)))
        ; itereren over alle kogels
        (define (iter kogels-lijst)
          (if (not (null? kogels-lijst))
              (let* ((kogel (car kogels-lijst))
                     (raket-kogel? (eq? (kogel 'type) 'raket)))
                (if raket-kogel?
                    ; Als raket-kogel? dan...
                    (begin ((alienvloot 'voor-alle-schepen)
                            (lambda (alien)
                              ; kogel van RAKET raakt een alien + alien heeft 1 leven
                              (cond ((and (((alien 'positie) 'gelijk?) (kogel 'positie))
                                          (eq? (kogel 'type) 'raket)
                                          (= (alien 'levens) 1))
                                     (bepaal-score alien teken-adt)
                                     (verwijder-alienschip! alien)
                                     (verwijder-kogel! kogel)
                                     ((teken-adt 'verwijder-kogel!) kogel))
                                    ; kogel van RAKET raakt een alien + alien heeft meer dan 1 leven
                                    ((and (((alien 'positie) 'gelijk?) (kogel 'positie))
                                          (eq? (kogel 'type) 'raket))
                                     ((alien 'levens!) (- (alien 'levens) 1))
                                     (verwijder-kogel! kogel)
                                     ((teken-adt 'verwijder-kogel!) kogel)))))
                           (iter (cdr kogels-lijst)))
                    ; Anders doe dit...
                    ; kogel van ALIEN raakt de raket + raket heeft 1 leven
                    (cond ((and (((raket 'positie) 'gelijk?) (kogel 'positie))
                                (= (raket 'levens) 1))
                           (display "raket dood"))
                          ; kogel van ALIEN raakt de raket + raket heeft meer dan 1 leven
                          ((((raket 'positie) 'gelijk?) (kogel 'positie))
                           ((raket 'verminder-levens!))
                           ((kogels 'verwijder-kogel!) kogel)
                           ((teken-adt 'verwijder-kogel!) kogel)
                           ((teken-adt 'teken-levens!) raket)
                           (iter (cdr kogels-lijst)))))
                (iter (cdr kogels-lijst)))))
        (iter kogels-lijst)))


    ;; --------------- SCORE - OPERATIES ---------------


;    (define (check-score)
;      (let ((huidige-score (score 'huidige-score))
;            (hoogste-score (score 'hoogste-score)))


    ; score wordt bepaald op basis van de alien die werd neergeschoten
    ; bepaal-score : Alien -> /
    (define (bepaal-score alien teken-adt)
      (let ((soort-alien (alien 'kleur)))
        (cond ((eq? soort-alien 'blauw) ((score 'verhoog-score!) 10))
              ((eq? soort-alien 'geel) ((score 'verhoog-score!) 20))
              ((eq? soort-alien 'paars) ((score 'verhoog-score!) 30)))
        ((teken-adt 'teken-score) score)))


    ;; --------------- VERWIJDER - FUNCTIES ---------------
    

    ; individueel alienschip verwijderen van scherm
    (define (verwijder-alienschip! alienschip-adt)
      ((alienvloot 'verwijder-schip!) alienschip-adt))
    

    ; individuele kogel verwijderen van scherm
    (define (verwijder-kogel! kogel-adt)
      ((kogels 'verwijder-kogel!) kogel-adt))


    ;; --------------- CALLBACKS ---------------
    

    ;; update! : number -> /
    (define (update! tijdsverschil teken-adt)
      (set! vloot-tijd (+ vloot-tijd tijdsverschil))
      (beweeg-vloot!)
      (set! kogel-tijd (+ kogel-tijd tijdsverschil))
      (beweeg-kogels!)
      (set! alien-schiettijd (+ alien-schiettijd tijdsverschil))
      (schiet-alienkogel!)
      (check-geraakt teken-adt))
    
    
    ;; toets! : any -> /
    (define (toets! toets)
      (beweeg-raket! toets)
      (schiet-raketkogel! toets))
    
    
    ; Dispatch
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'toets!) toets!)
            ((eq? msg 'raket) raket)
            ((eq? msg 'kogels) kogels)
            ((eq? msg 'alienvloot) alienvloot)
            (else (display "ongeldige boodschap"))))

    dispatch-level))
            

    
