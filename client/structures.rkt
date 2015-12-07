#lang racket
(require pict3d lens unstable/lens)
(provide (all-defined-out))

;; key and the speed it is moving in that direction
(struct/lens movekey (key speed) #:prefab)
;corner 1 and 2 are pos for drawing and pos is where to move it
;time is time in milliseconds when it was shot
(struct/lens shot (corner1 corner2 pos yaw pitch time) #:prefab)
;;position, velocity, and time at last key change.list of moves.
;;Pos is a list of 3 coordinates that is current. (continue on next line)
;;Dir is direction it is pointing and roll is how much the camera is rotated. mx and my are mouse coordinates
;;shots is a list of shots to draw and reload-time is time the player shot last
;;name and color are strings
(struct/lens orb (pos vel time movekeys dir roll roll-vel shots reload-time name color hostname port kills deaths) #:prefab)
;;player is a orb and enemys is a list of orbs
(struct/lens orbs (player enemys) #:transparent);;enemys includes teammates, confusing!
;orbs is an orbs and exit? is wheather or not to stop the state and close the window
;;scores? is whether or not tab is pressed, to show scores ect.
;;mt is the time in milliseconds at last update and send of state
;;held-keys is a set containing the keys that are currently pressed down, and
;;  that we don't want to react to until they are released
(struct/lens game (mode orbs exit? scores? mt held-keys) #:transparent)

(define-nested-lenses [game-orbs game-orbs-lens]
  [enemys orbs-enemys-lens]
  [player orbs-player-lens
    [pos orb-pos-lens]
    [vel orb-vel-lens]
    [time orb-time-lens]
    [shots orb-shots-lens]
    [deaths orb-deaths-lens]
    [kills orb-kills-lens]])

(struct/lens client (hostname port last-message-time) #:prefab);;used by server to keep track of clients
(struct/lens message (name data) #:prefab);;this is what is sent between clients and server
(struct/lens orbdefine (name color hostname port) #:prefab);;used for sending info about an orb for kills, new orbs, ect.

(struct/lens mypos (x y z) #:prefab)
(struct/lens mydir (dx dy dz) #:prefab)
(struct/lens mycube (pos scale color) #:prefab)

(define/match (pos->mypos p)
  [[(pos x y z)]
   (mypos x y z)])
(define/match (mypos->pos p)
  [[(mypos x y z)]
   (pos x y z)])

(define/match (dir->mydir d)
  [[(dir x y z)]
   (mydir x y z)])
(define/match (mydir->dir d)
  [[(mydir x y z)]
   (dir x y z)])

(define (round-10000th x)
  (/ (round (* x 10000)) 10000))

(define/match (round-pos p)
  [[(pos x y z)]
   (pos (round-10000th x)
        (round-10000th y)
        (round-10000th z))])

(define/match (round-dir d)
  [[(dir dx dy dz)]
   (dir (round-10000th dx)
        (round-10000th dy)
        (round-10000th dz))])

(define (round-orbs-dir o)
  (struct-copy orb o [dir (round-dir (orb-dir o))]))

(define/match (pos->dir p)
  [[(pos x y z)]
   (dir x y z)])
