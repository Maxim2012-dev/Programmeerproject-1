;;            ------------
;; ----------  Matrix ADT  ----------
;;            ------------

(define (maak-matrix)
  (let ((vector (make-vector aantal-rijen-aliens)))

    ; Deze procedure zal de vector in de 'let' initialiseren met
    ; vectoren van gelijke grootte
    (define (vul-vector)
      (vector-set! vector 0 (make-vector aantal-aliens-per-rij))
      (vector-set! vector 1 (make-vector aantal-aliens-per-rij))
      (vector-set! vector 2 (make-vector aantal-aliens-per-rij)))

    (vul-vector)
    vector))


    