;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname downloadsnake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./snake_lib.rkt")

; a game is...
; - (make-game snake (listof posn) (listof posn) number)
; (define-struct game (snake food obstacles ticks))

; a direction is one of...
; - 'up
; - 'down
; - 'left
; - 'right
; If this type looks new to you, its just a symbol.
; That is ‘up is a symbol and “up” is a string.
; Symbols are like strings without spaces. 


; a snake is...
; - (make-snake direction (listof posn))
; (define-struct snake (heading segments))

; segments is either
; - (cons posn empty)
; - (cons posn segments)
; That is, segments is a non-empty list of posns. 
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.

; food is either
; - empty
; - (cons posn food)
; That is, food is a list of posns.

; obstacles is either
; - empty
; - (cons posn obstacles)
; Obstacles is also a list of posns.

; add-food : game posn -> game
; Given a game and posn, returns a new game (so you want to call make-game here)
; where food has been added
; at that posn. 
(define (add-food g p)
  (make-game (game-snake g)
             (cons p (game-food g))
             (game-obstacles g)
             (game-ticks g)))

(check-expect
 (add-food (make-game (make-snake 'up (list (make-posn 1 2)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      5)
           (make-posn 6 7))
 (make-game (make-snake 'up (list (make-posn 1 2)))
            (list (make-posn 6 7) (make-posn 3 4))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            5))
(check-expect
 (add-food (make-game (make-snake 'up (list (make-posn 32 2) (make-posn 32 3) (make-posn 32 4)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      5)
           (make-posn 49 12))
 (make-game (make-snake 'up (list (make-posn 32 2) (make-posn 32 3) (make-posn 32 4)))
            (list (make-posn 49 12) (make-posn 3 4))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            5))

; change-direction : game direction -> game
; Given a game and direction, returns a new game where the snake
;   is now headed in the provided direction. 
(define (change-direction g d)
  (make-game (make-snake d (snake-segments (game-snake g)))
             (game-food g)
             (game-obstacles g)
             (game-ticks g)))

(check-expect
 (change-direction
  (make-game (make-snake 'down (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  'left)
 (make-game (make-snake 'left (list (make-posn 1 2)))
            (list (make-posn 3 4))
            empty
            5))
(check-expect
 (change-direction
  (make-game (make-snake 'down (list (make-posn 32 2) (make-posn 32 3) (make-posn 32 4)))
             (list (make-posn 14 50))
             (list (make-posn 12 50))
             5)
  'right)
 (make-game (make-snake 'right (list (make-posn 32 2) (make-posn 32 3) (make-posn 32 4)))
            (list (make-posn 14 50))
            (list (make-posn 12 50))
            5))

; game-score : game -> number
; Given a game, returns a score (as a number)
(define (game-score g)
  (length (snake-segments (game-snake g))))

(check-expect
 (game-score
  (make-game (make-snake 'down (list (make-posn 32 2) (make-posn 32 3) (make-posn 32 4)))
             (list (make-posn 14 50))
             (list (make-posn 12 50))
             5))
 3)
(check-expect
 (game-score
  (make-game (make-snake 'down (list (make-posn 32 2) (make-posn 32 3) (make-posn 32 4) (make-posn 32 5) (make-posn 32 6) (make-posn 33 6) (make-posn 33 5)))
             (list (make-posn 14 50))
             (list (make-posn 12 50))
             5))
 7)

; no tests are provided for game-score because it is open-ended
; feel free to implement it however you would like to

; game-over? : game -> boolean
; Given a game, returns true if that snake has died and false otherwise.
; We strongly recommend writing helper functions for this question!
(define (self-collision g)
  (segment-collision (first(snake-segments (game-snake g))) (rest(snake-segments (game-snake g)))))

(define (segment-collision head body)
  (cond [(empty? body) false]
        [(and (= (posn-x head) (posn-x (first body))) (= (posn-y head) (posn-y (first body)))) true]
        [else (segment-collision head (rest body))]))

(define (obstacle-collision g)
  (segment-collision (first(snake-segments (game-snake g))) (game-obstacles g)))



(define (game-over? g)
  (cond [(boolean=? (self-collision g) true) true]

        [(> (posn-x(first(snake-segments (game-snake g)))) 50) true]
        [(> (posn-y(first(snake-segments (game-snake g)))) 50) true]
        [(< (posn-x(first(snake-segments (game-snake g)))) 1) true]
        [(< (posn-y(first(snake-segments (game-snake g)))) 1) true]

        [(boolean=? (obstacle-collision g) true) true]

        [else false]
        ))

(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1))) empty empty 5))
 false)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn -1 1))) empty empty 5))
 true)

; advance-game : game -> game
; Takes a game as input and advances the game one tick. The snake
;  moves forward one segment and eats or not.
(define (eating g)
  (member? (first(snake-segments (game-snake g))) (game-food g)))

(define (snake-head g)
  (first (snake-segments (game-snake g))))

(define (next-snake-head g)
  (cond [(symbol=? 'left (snake-heading (game-snake g)))
         (make-posn (- (posn-x (snake-head g)) 1)
                    (posn-y (snake-head g)))]
        [(symbol=? 'right (snake-heading (game-snake g)))
         (make-posn (+ 1 (posn-x (snake-head g)))
                    (posn-y (snake-head g)))]
        [(symbol=? 'down (snake-heading (game-snake g)))
         (make-posn (posn-x (snake-head g))
                    (- (posn-y (snake-head g)) 1))]
        [(symbol=? 'up (snake-heading (game-snake g)))
         (make-posn (posn-x (snake-head g))
                    (+ (posn-y (snake-head g)) 1))]))

(define (snake-grow g)
  (make-snake (snake-heading (game-snake g))
              (cons (next-snake-head g) (snake-segments (game-snake g)))))

(define (snake-move g)
  (make-snake (snake-heading (game-snake g))
              (cons (next-snake-head g) (remove-last (snake-segments (game-snake g))))))

(define (remove-last segments)
  (cond [(empty? (rest segments)) '()]
        [else (cons (first segments) (remove-last (rest segments)))]))

(define (advance-game g)
  (if (boolean=? (eating g) true)
      (make-game (snake-grow g)
                 (remove (snake-head g) (game-food g))
                 (game-obstacles g)
                 (+ 1 (game-ticks g)))
      (make-game (snake-move g)
                 (game-food g)
                 (game-obstacles g)
                 (+ 1 (game-ticks g)))))

(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             empty
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)))
            empty
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))
(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             (list (make-posn 2 1) (make-posn 8 9))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)
                                    (make-posn 3 3)))
            (list (make-posn 8 9))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))



; a starting game to experiment with
(define game-start
  (make-game (make-snake 'up (list (make-posn 12 12)))
             (list (make-posn 2 2) 
                   (make-posn 5 20)
                   (make-posn 15 15)
                   (make-posn 24 24))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             0))

;; play : game -> game
(define (play initial-game)
  (play-game initial-game advance-game add-food change-direction game-score game-over?))

;to start a game
(play game-start)

;to make sure all procedures are defined with no typos
(check-expect (procedure? add-food) #true)
(check-expect (procedure? change-direction) #true)
(check-expect (procedure? game-score) #true)
(check-expect (procedure? game-over?) #true)
(check-expect (procedure? advance-game) #true)