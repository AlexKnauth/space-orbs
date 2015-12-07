#lang racket
(require rackunit lens pict3d "structures.rkt" "current-roll-and-pos.rkt" "variables.rkt" "big-crunch.rkt" "on-frame.rkt")
(provide on-key on-release)

(define (find-this-movekey key ms)
  (cond
    [(empty? ms)
     #f]
    [(equal? (movekey-key (first ms)) key)
     (first ms)]
    [else (find-this-movekey key (rest ms))]))

(module+ test (check-equal? (find-this-movekey "w" '())
                           #f))
(module+ test (check-equal? (find-this-movekey "a" (list (movekey "w" 1/2) (movekey "a" 1/2)))
                            (movekey "a" 1/2)))

(define (on-key g n ot key)
  (define t (- ot MASTER-TIME-OFFSET))
  (define lkey (string-foldcase key))
  (cond
    [(equal? key "escape")
     (struct-copy game g
                  [exit? #t])]
    [(equal? key "\t")
     (struct-copy game g
                  [scores? #t])]
    [(find-this-movekey lkey (orb-movekeys (orbs-player (game-orbs g))))
     g]
    [(member lkey '("w" "a" "s" "d" " " "shift" "q" "e"))
     (struct-copy game g
                  [orbs (on-orbs-key (game-orbs g) n t key)]
                  [mt t])]
    [else g]))

(define (on-orbs-key os n t key)
  (struct-copy orbs os
               [player (on-player-key (orbs-player os) n t key)]))

(define (on-player-key o n t key)
  (define lkey (string-foldcase key))
  (define result
    (on-player-key-result o n t lkey))
  (send-orb* result 'key t)
  result)

(define (on-player-key-result o n t lkey)
  (define roll (current-roll o t))
  (cond
    [(member lkey '("w" "a" "s" "d" " " "shift"))
     (struct-copy orb o
                  [pos (current-pos o t)]
                  [time t]
                  [vel (dir+ (orb-vel o)
                             (key-velocity lkey STARTING-SPEED (orb-dir o) roll))]
                  [roll roll])]
    [else
     (struct-copy orb o
                  [pos (current-pos o t)]
                  [time t]
                  [movekeys (cons (movekey lkey STARTING-SPEED)  (orb-movekeys o))]
                  [roll roll])]))

(module+ test
  (check-equal? (lens-transform
                 orb-vel-lens
                 (on-player-key (struct-copy orb TESTORB [pos (pos 2 2 2)] [dir -x]) "n" 5 "shift")
                 (lambda (vel)
                   (match (round-dir vel)
                     [(dir x y z)
                      (dir (if (zero? x) 0 x)
                           (if (zero? y) 0 y)
                           (if (zero? z) 0 z))])))
                (struct-copy orb TESTORB [pos (pos 2 2 2)] [dir -x] [vel (dir 0 0 (- STARTING-SPEED))])))

;#############################################################################################################################################################################################

(define (on-release g n ot key)
  (define t (- ot MASTER-TIME-OFFSET))
  (define lkey (string-foldcase key))
  (cond
    [(equal? key "\t")
     (struct-copy game g
                  [scores? #f])]
    [(member lkey '("w" "a" "s" "d" " " "shift" "q" "e"))
     (struct-copy game g
                  [orbs (on-orbs-release (game-orbs g) n t key)]
                  [mt t])]
    [else g]))

(define (on-orbs-release os n t key)
    (struct-copy orbs os
               [player (on-player-release (orbs-player os) n t key)]))

(define (on-player-release o n t key)
  (define lkey (string-foldcase key))
  (define result
    (on-player-release-result o n t lkey))
  (send-orb* result 'key-release t)
  result)

(define (on-player-release-result o n t lkey)
  (define roll (current-roll o t))
  (cond
    [(member lkey '("w" "a" "s" "d" " " "shift"))
     (struct-copy orb o
                  [pos (current-pos o t)]
                  [time t]
                  [vel (dir- (orb-vel o)
                             (key-velocity lkey STARTING-SPEED (orb-dir o) roll))]
                  [roll roll])]
    [else
     (struct-copy orb o
                  [pos (current-pos o t)]
                  [time t]
                  [movekeys (remove-this-movekey (orb-movekeys o) lkey)]
                  [roll roll])]))

(module+ test
  (check-equal? (on-player-release (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys (list (movekey "a" 1))]) "n" 5 "a")
                (struct-copy orb TESTORB [pos (pos 2 2 2)] [movekeys empty])))

(define (remove-this-movekey ms key)
  (cond
    [(empty? ms) ms]
    [(equal? key (movekey-key (first ms)))
     (remove-this-movekey (rest ms) key)]
    [else
     (cons (first ms) (remove-this-movekey (rest ms) key))]))