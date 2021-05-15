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


    ;; toggle-type : / -> /
    (define (toggle-type!)
      (if (eq? type 'raket)
          (set! type 'alien)
          (set! type 'raket)))
    
    ;; dispatch functie
    (define (dispatch-kogel msg)
      (cond ((eq? msg 'positie) positie)
            ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'toggle-type!) toggle-type!)
            ((eq? msg 'stop) stop)
            ((eq? msg 'type) type)
            (else (display "ongeldige boodschap - kogel"))))

    dispatch-kogel))