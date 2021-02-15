;;            -----------
;; ----------  Kogel ADT  ----------
;;            -----------

(define (maak-kogel positie)
  (let ((afgeschoten? #f))

    ;; beweeg! : / -> /
    (define (beweeg!)
      ((positie 'beweeg!) 'omhoog))


    (define (stop)
      ((positie 'beweeg!) 'stop))
    
    ;; dispatch functie
    (define (dispatch-kogel msg)
      (cond ((eq? msg 'positie) positie)
            ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'stop) stop)
            (else (display "ongeldige boodschap - kogel"))))

    dispatch-kogel))