;;              --------------  
;;  -----------  Positie ADT   ------------
;;              --------------

; constructor om instantie van Positie ADT te maken
; (number number -> Positie)
(define (maak-positie x y)
  

  ; destructieve operatie om x-coördinaat van positie object aan te passen
  ; (number -> /)
  (define (x! nieuwe-x)
    (set! x nieuwe-x))

  ; destructieve operatie om y-coördinaat van positie object aan te passen
  ; (number -> /)
  (define (y! nieuwe-y)
    (set! y nieuwe-y))
  

  ; Predicaat dat x- en y-coördinaat van twee posities vergelijkt
  ; en #t teruggeeft indien ze gelijk zijn
  ; (Positie -> boolean)
  (define (gelijk? positie)
    (and (= x (positie 'x)) (= y (positie 'y))))


  ; Checken of de x-coördinaten niet buiten het speelveld treden
  (define (rand?)
    (or (< x 0) (> x spel-breedte)))

  ; Procedure die een positieverandering gaat doen door
  ; een aanpassing aan de x- of y-coördinaat afhankelijk van de meegegeven richting
  ; (symbol -> Positie)
  (define (beweeg! richting)
    (cond
      ((eq? richting 'omhoog) (y! (- y 1)))
      ((eq? richting 'omlaag) (y! (+ y 1)))
      ((eq? richting 'links) (x! (- x 1)))
      ((eq? richting 'rechts) (x! (+ x 1)))
      (else (display "geen geldige richting"))))

  ; de overeenkomstige dispatch procedure waarmee er
  ; 'berichten' kunnen verzonden worden naar een object om zo een
  ; bepaalde operatie uit te voeren
  (define (dispatch-positie msg)
    (cond
      ((eq? msg 'x) x)
      ((eq? msg 'y) y)
      ((eq? msg 'x!) x!)
      ((eq? msg 'y!) y!)
      ((eq? msg 'gelijk?) gelijk?)
      ((eq? msg 'beweeg!) beweeg!)
      ((eq? msg 'rand?) rand?)))
  dispatch-positie)
      

  

  
    

  

  