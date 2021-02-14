;;            -----------
;; ----------  Raket ADT  ----------
;;            -----------


; constructor om raket aan te maken
; de raket begint altijd op een gegeven positie
(define (maak-raket positie)


  ; de beweeg! functie die de beweeg van positie gaat aanroepen
  ; om zo de raket naar links of rechts te laten bewegen afhankelijk van het richtingsargument
  (define (beweeg! richting)
    (if (not (rand-geraakt?))
        ((positie 'beweeg!) richting)
        (let ((x (positie 'x)))
                 ; linkerrand geraakt (dan mag je alleen naar rechts)
          (cond ((and (= x 0) (eq? richting 'rechts))
                 ((positie 'beweeg!) richting))
                 ; rechterrand geraakt (dan mag je alleen naar links)
                ((and (= x (- spel-breedte 1)) (eq? richting 'links))
                 ((positie 'beweeg!) richting))))))

  
  ; rand-geraakt? : / -> /
  (define (rand-geraakt?)
    ((positie 'rand?)))

  ; schiet! operatie gaat een nieuwe kogel aanmaken op de positie van de raket
  (define (schiet!)
    (maak-kogel positie))

  ; de dispatch functie
  (define (dispatch-raket msg)
    (cond((eq? msg 'beweeg!) beweeg!)
         ((eq? msg 'positie) positie)
         ((eq? msg 'schiet!) schiet!)
         (else (display "ongeldige boodschap"))))
  dispatch-raket)

  

  

  
  