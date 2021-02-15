;;            ----------------
;; ----------  Alienvloot ADT  ----------
;;            ----------------

(load "matrix-adt.rkt")
(load "alienschip-adt.rkt")
(load "positie-adt.rkt")

(define (maak-alienvloot)
  (let* ((schepen (maak-matrix))
         (richting 'rechts)
         (size (vector-length schepen)))


    (define (afstand-tussen-rijen idx) (* 1 idx))
    (define (afstand-tussen-kolommen idx) (* 1 idx))

    ; Elke plaats in de vector opvullen met een alienschip object
    (define (vul-vloot)
      (let outer-loop
        ((outer-idx 0))
        (if (< outer-idx size)
            (let inner-loop
              ((inner-idx 0)
               (inner-vector (vector-ref schepen outer-idx)))
              ; inner-idx + 1 (vermijden dat x op nul start)
              (vector-set! inner-vector inner-idx (maak-alienschip (maak-positie
                                                                    (afstand-tussen-kolommen (+ inner-idx 1))
                                                                    (afstand-tussen-rijen outer-idx))))
              (if (< (+ inner-idx 1) aantal-aliens-per-rij)
                  (inner-loop (+ inner-idx 1) inner-vector)
                  (outer-loop (+ outer-idx 1)))))))
    
    (vul-vloot)

    ; Een for-each om op elk alienschip een functie los te laten
    (define (voor-alle-schepen fun)
      (let outer-loop
        ((outer-idx 0))
        (if (< outer-idx size)
            (let inner-loop
              ((inner-idx 0)
               (inner-vector (vector-ref schepen outer-idx)))
              (fun (vector-ref inner-vector inner-idx))
              (if (< (+ inner-idx 1) aantal-aliens-per-rij)
                  (inner-loop (+ inner-idx 1) inner-vector)
                  (outer-loop (+ outer-idx 1)))))))
    

    ; switch! : / -> /
    ; Het alterneren van de richting van het alienvloot
    (define (switch!)
      (if (eq? richting 'rechts)
          (set! richting 'links)
          (set! richting 'rechts)))

    
    ; beweeg! : / -> /
    (define (beweeg!)
      ; rand checken
      (let ((test-list '()))
        (voor-alle-schepen (lambda (schip)
                             (set! test-list (cons ((schip 'rand-geraakt?)) test-list))))
        (if (not (member #t test-list))
            (voor-alle-schepen roep-beweeg-op)
            (begin (switch!)
                   (voor-alle-schepen (lambda (schip)
                                        ((schip 'beweeg) 'omlaag)))
                   (voor-alle-schepen roep-beweeg-op)))))


    ; roep-beweeg-op : alienschip-adt -> /
    (define (roep-beweeg-op schip-adt)
      ((schip-adt 'beweeg) richting))


    ;; dispatch-functie
    (define (dispatch-alienvloot msg)
      (cond ((eq? msg 'beweeg) beweeg!)
            ((eq? msg 'schepen) schepen)
            ((eq? msg 'voor-alle-schepen) voor-alle-schepen)
            (else "verkeerde boodschap - alienvloot")))
    dispatch-alienvloot))
                  
              
              
        
