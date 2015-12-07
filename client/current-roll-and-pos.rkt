#lang racket
(require pict3d rackunit pict3d/universe mzlib/string "structures.rkt" "variables.rkt" "rotate-dir.rkt" "landscape.rkt" "collision-detection.rkt")
(provide current-pos current-roll key-velocity)
;;this is where all the magic happens.

(define (current-roll o t)
  (define ms (orb-movekeys o))
  (adjust-ang (orb-roll o) ms (- t (orb-time o))))

(module+ test (check-equal?
               (current-roll TESTORB 50)
               0))

(module+ test (check-equal?
               (current-roll (struct-copy orb TESTORB
                                         [movekeys (list (movekey "q" STARTING-SPEED))])
                            6)
               (- (* ROTATION-SPEED-MULTIPLIER STARTING-SPEED))))
(module+ test (check-equal?
               (current-roll (struct-copy orb TESTORB
                                         [movekeys empty])
                            6)
               0))

(define (adjust-ang ang ms dt)
  (cond
    [(empty? ms)
     ang]
    [else
     (adjust-ang (adjust-one-ang-key ang (first ms) dt) (rest ms) dt)]))

(define (adjust-one-ang-key ang m dt)
  (cond
    [(equal? (movekey-key m) "q")
     (- ang (* dt (movekey-speed m) ROTATION-SPEED-MULTIPLIER))]
    [(equal? (movekey-key m) "e")
     (+ ang (* dt (movekey-speed m) ROTATION-SPEED-MULTIPLIER))]
    [else ang]))

;;takes an orb and time and gives current position of the orb
(define (current-pos o t)
  (adjust-pos (orb-pos o) (orb-vel o) (- t (orb-time o))))

(module+ test (check-equal?
               (current-pos TESTORB 8)
               (pos 1 1 1)))
(module+ test (check-equal?
               (current-pos (struct-copy orb TESTORB
                                         [movekeys (list (movekey "w" 1/2))]
                                         [pos (pos 5 1 1)]
                                         [time 5]) 7)
               (pos 4 1 1)))
(module+ test (check-equal?
               (current-pos (struct-copy orb TESTORB
                                         [movekeys (list (movekey "s" 1/2))]
                                         [pos (pos 5 1 1)]
                                         [time 5]) 7)
               (pos 6 1 1)))
(module+ test (check-equal?
               (round-pos
                (current-pos (struct-copy orb TESTORB
                                          [movekeys (list (movekey "a" 1/2))]
                                          [pos (pos 1 1 1)]
                                          [time 5]) 7))
               (pos 1 0 1)))
(module+ test (check-equal?
               (round-pos
                (current-pos (struct-copy orb TESTORB
                                          [movekeys (list (movekey "d" 1/2))]
                                          [pos (pos 1 1 1)]
                                          [time 5]) 7))
               (pos 1 2 1)))
(module+ test (check-equal?
               (round-pos
                (current-pos (struct-copy orb TESTORB
                                          [movekeys (list (movekey " " 1/2))]
                                          [pos (pos 1 1 1)]
                                          [time 5]) 7))
               (pos 1 1 2)))
(module+ test (check-equal?
               (round-pos
                (current-pos (struct-copy orb TESTORB
                                          [movekeys (list (movekey "shift" 1/2))]
                                          [pos (pos 1 1 1)]
                                          [time 5]) 7))
               (pos 1 1 0)))

;;pos, vel, and delta-time -> ajusted position of the orb
(define (adjust-pos pos vel dt)
  (cond [(= 0 (dir-dx vel) (dir-dy vel) (dir-dz vel)) pos]
        [else
         (move-with-collision*
          pos
          vel
          dt
          FINAL-LANDSCAPE)]))

;; key, speed, dir, and angle -> velocity
;; pd is d adjusted by 90 degrees for pitch, and yd is adjusted 90 degrees for
;; yaw, and then they are adjusted for the angle the camera is turned
(define (key-velocity k s d ang)
  (define-values (yaw pitch) (dir->angles d))
  (define pd (rotate-around-dir (rotate-up d) d ang))
  (define yd (angles->dir (+ 90 yaw) ang))
  (cond
    [(equal? k "w")
     (dir-scale d s)]
    [(equal? k "s")
     (dir-scale (dir-negate d) s)]
    [(equal? k "a")
     (dir-scale yd s)]
    [(equal? k "d")
     (dir-scale (dir-negate yd) s)]
    [(equal? k "shift")
     (dir-scale (dir-negate pd) s)]
    [(equal? k " ")
     (dir-scale pd s)]
    [else
     (error 'key-velocity "unrecognized key: ~v" k)]))
