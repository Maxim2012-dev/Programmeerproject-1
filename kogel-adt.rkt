;;            -----------
;; ----------  Kogel ADT  ----------
;;            -----------

(define (maak-kogel positie type-kogel)
  (let ((type type-kogel))

    ;; beweeg! : symbol -> /
    (define (beweeg! type)
      (cond ((eq? type 'raket)
             ((positie 'beweeg!) 'omhoog))
            ((eq? type 'alien)
             ((positie 'beweeg!) 'omlaag))))

    ;; stop! : / > /
    (define (stop)
      ((positie 'beweeg!) 'stop))
    
    ;; dispatch functie
    (define (dispatch-kogel msg)
      (cond ((eq? msg 'positie) positie)
            ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'stop) stop)
            ((eq? msg 'type) type)
            (else (display "ongeldige boodschap - kogel"))))

    dispatch-kogel))