#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

;; We'll use the same version of Some and Optional that we used on Homework 5.
(define-struct (Some X)
  ([value : X]))

(define-type (Optional X)
  (U 'none (Some X)))

;; The game has two players, who we'll call 'black and 'white. You can choose
;; any color or pattern you would like for the pieces, but we'll still call
;; the players by these names.
(define-type Player (U 'black 'white))

;; (Pos row col) represents a position on the game board. The lower-left corner
;; is at row 0, column 0. The lower-right corner is at row 0, column 6. The
;; upper-left corner is at row 5, column 0. The upper-right corner is at row 5,
;; column 6.
(define-struct Pos
  ([row : Integer]   ;; integer between 0 and 5 inclusive
   [col : Integer])) ;; integer between 0 and 6 inclusive

;; (Stack height pieces) represents one full column of the game board. The
;; integer height is the number of pieces currently in that column. The
;; list pieces is a list of the pieces in the column (or more precisely, a list
;; of the players who placed the pieces). The last element of the list is the
;; bottom piece in the stack, while the first element of the list is the top
;; piece in the stack (so far). The value of height should alway match the
;; length of the list pieces.
(define-struct Stack
  ([height : Integer]
   [pieces : (Listof Player)]))

(define-struct TempStack
  ([height : Integer]
   [pieces : (Listof (Optional Player))]))
  

;; (Board stacks) represents a game board. The list stacks will always have
;; seven elements, representing the seven columns of the game board. The first
;; element of stacks represents the leftmost column of the board.
(define-struct Board
  ([stacks : (Listof Stack)]))

(define-struct TempBoard
  ([stacks : (Listof TempStack)]))

;; (Game board next) represents the state of the game at a given moment of time.
;; The current state of the game board is saved in board. The player whose turn
;; it is currently is stored in next.
(define-struct Game
  ([board : Board]
   [next : Player]))

(define-struct TempGame
  ([board : TempBoard]
   [next : Player]))

;; If a player has won the game by creating a line of four, then
;; (Winning-Line player start end) can be used to keep track of which player
;; created the line of four, and where that line is (it goes from start to end).
;; Generally, if a player's winning move creates more than one line of four, we
;; won't care which one gets represented.
(define-struct Winning-Line
  ([player : Player]
   [start : Pos]
   [end : Pos]))

;; strategies ot be used by AI
(define-type Strategy (Game -> Integer))

;; part 2 structs
(define-struct Human
  ([name : (U String Symbol)]))

(define-struct Bot
  ([name : (U String Symbol)]
   [strategy : Strategy]))

(define-type Controller (U Human Bot))

(define-struct World
  ([player1 : Controller]
   [player2 : Controller]
   [game : Game]
   [spacing : Integer]
   [status : (U Winning-Line 'tie 'ongoing 'invalidmove)]))

(: new-game : Game)
(define new-game
  (Game (Board (make-list 7 (Stack 0 '()))) 'black))

;; tester definitions for parts 1 and 2
(define b1 (Board (make-list 7 (Stack 4 (list 'black 'white 'black 'white)))))
(define b2 (Board (list (Stack 0 '())
                        (Stack 1 (list 'white))
                        (Stack 2 (list 'white 'black))
                        (Stack 3 (list 'white 'black 'black))
                        (Stack 5 (list 'black 'white 'black 'black 'white))
                        (Stack 6 (list 'black 'white
                                       'white 'black 'black 'white))
                        (Stack 2 (list 'white 'white)))))
(define b3 (Board (list (Stack 1 (list 'white))
                        (Stack 1 (list 'white))
                        (Stack 2 (list 'white 'black))
                        (Stack 3 (list 'white 'black 'black))
                        (Stack 5 (list 'black 'white 'black 'black 'white))
                        (Stack 6 (list 'black 'white
                                       'white 'black 'black 'white))
                        (Stack 2 (list 'white 'white)))))
(define b4 (Board (list (Stack 6 (list 'white 'white
                                       'black 'black
                                       'white 'white))
                        (Stack 6 (list 'white 'white
                                       'black 'black
                                       'white 'white))
                        (Stack 6 (list 'white 'white
                                       'black 'black
                                       'white 'white))
                        (Stack 6 (list 'black 'black
                                       'white 'white
                                       'black 'black))
                        (Stack 6 (list 'white 'white
                                       'black 'black
                                       'black 'white))
                        (Stack 6 (list 'black 'white
                                       'black 'black
                                       'white 'white))
                        (Stack 6 (list 'black 'white
                                       'white 'black
                                       'white 'white)))))
(define b5 (Board (reverse (Board-stacks b3))))
(define b6 (Board (list (Stack 4 (list 'white 'white 'white 'white))
                        (Stack 0 '())
                        (Stack 0 '())
                        (Stack 0 '())
                        (Stack 0 '())
                        (Stack 0 '())
                        (Stack 0 '()))))
(define w1 (World (Human 'b) (Human 'a) new-game 50 'ongoing))
(define w1m1 (World (Human 'b) (Human 'a)
                    (Game (Board (list (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 1 (list 'black))
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())))
                          'white) 50 'ongoing))
(define w1m2 (World (Human 'b) (Human 'a)
                    (Game (Board (list (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 2 (list 'white 'black))
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())))
                          'black) 50 'ongoing))
(define w3 (World (Human 'a) (Human 'b) (Game b2 'white) 30 'ongoing))
(define w3invalid (World (Human 'a) (Human 'b)
                         (Game b2 'white) 30 'invalidmove))
(define w4 (World (Human 'b) (Human 'a) (Game b2 'white) 50
                  (Winning-Line 'white (Pos 0 1) (Pos 3 4))))
(define wtie (World (Human 'a) (Human 'b) (Game b4 'white) 30 'tie))
(define wywin (World (Human 'y) (Human 'r) (Game b1 'black) 20
                     (Winning-Line 'black (Pos 3 0) (Pos 3 3))))

(define gbwin (Game (Board (list (Stack 3 (list 'black 'black 'black))
                                 (Stack 0 '())
                                 (Stack 1 (list 'white))
                                 (Stack 0 '())
                                 (Stack 1 (list 'white))
                                 (Stack 0 '())
                                 (Stack 1 (list 'white))))
                    'black))
(define gbwon (Game (Board (list (Stack 4 (list 'black 'black 'black 'black))
                                 (Stack 0 '())
                                 (Stack 1 (list 'white))
                                 (Stack 0 '())
                                 (Stack 1 (list 'white))
                                 (Stack 0 '())
                                 (Stack 1 (list 'white))))
                    'white))
(define gwwinx2 (Game (Board (list (Stack 3 (list 'white 'white 'black))
                                   (Stack 3 (list 'white 'white 'black))
                                   (Stack 3 (list 'white 'white 'black))
                                   (Stack 0 '())
                                   (Stack 2 (list 'black 'black))
                                   (Stack 1 (list 'black))
                                   (Stack 0 '())))
                      'white))
                                   
;; part 3 structs 
(define-type Heuristic (Game -> Integer))

;; start of functions and body of the code

;; part 3 functions
(: did-I-win? : Heuristic)
;; assigns 1 if black won, and -1 if white did
;; no other info (0) is given if the game is not won
;;
(define (did-I-win? game)
  (match (outcome game)
    [(Winning-Line 'black _ _) 1]
    [(Winning-Line 'white _ _) -1]
    [_ 0]))

(check-expect (did-I-win? new-game) 0)
(check-expect (did-I-win? (Game b3 'white)) -1)
(check-expect (did-I-win? (Game b1 'black)) 1)

(: pos=? : Pos Pos -> Boolean)
;; determines if two positions are the same
;;
(define (pos=? p1 p2)
  (and (= (Pos-row p1) (Pos-row p2)) (= (Pos-col p1) (Pos-col p2))))

(check-expect (pos=? (Pos 1 1) (Pos 1 1)) #t)
(check-expect (pos=? (Pos 1 2) (Pos 1 1)) #f)
(check-expect (pos=? (Pos 1 1) (Pos 3 1)) #f)

(: maketempgame : Pos Game (Optional Player) -> TempGame)
;; creates a game with a piece "hovering" in the given position
;;
(define (maketempgame position g p)
  (local
    {(define b (Game-board g))
     (: s-aux : Pos -> (Listof (Optional Player)))
     ;; goes through each stack and adds it to the tempstack
     ;;
     (define (s-aux currentpos)
       (match*  (currentpos (board-ref b currentpos))
         [((Pos 0 _) 'none) (if (pos=? currentpos position)
                                (list p)
                                (list 'none))]
         [((Pos r c) 'none) (if (pos=? currentpos position)
                                (cons p (s-aux (Pos (- r 1) c)))
                                (cons 'none (s-aux (Pos (- r 1) c))))]
         [((Pos 0 _) cv) (list cv)]
         [((Pos r c) cv) (cons cv (s-aux (Pos (- r 1) c)))]))
     (: b-aux : Integer (Listof Stack) -> (Listof TempStack))
     ;; goes through the board stack by stack to send to s-aux to produce
     ;; the temp board
     ;;
     (define (b-aux c stacks)
       (match stacks
         ['() '()]
         [(cons (Stack h ps) sr) (cons (TempStack 6 (s-aux (Pos 5 c)))
                                       (b-aux (+ 1 c) sr))]))}
    (TempGame (TempBoard (b-aux 0 (Board-stacks b))) (Game-next g))))

(check-expect (TempGame-board (maketempgame (Pos 3 1) gbwin 'none))
              (TempBoard (list (TempStack 6 (list 'none 'none 'none
                                                  (Some 'black) (Some 'black)
                                                  (Some 'black)))
                               (TempStack 6 (make-list 6 'none))
                               (TempStack 6 (list 'none 'none 'none
                                                  'none 'none (Some 'white)))
                               (TempStack 6 (make-list 6 'none))
                               (TempStack 6 (list 'none 'none 'none
                                                  'none 'none (Some 'white)))
                               (TempStack 6 (make-list 6 'none))
                               (TempStack 6 (list 'none 'none 'none
                                                  'none 'none (Some 'white))))))
(check-expect (TempGame-board (maketempgame (Pos 3 2) gbwin (Some 'black)))
              (TempBoard (list (TempStack 6 (list 'none 'none 'none
                                                  (Some 'black) (Some 'black)
                                                  (Some 'black)))
                               (TempStack 6 (make-list 6 'none))
                               (TempStack 6 (list 'none 'none
                                                  (Some 'black) 'none 'none
                                                  (Some 'white)))
                               (TempStack 6 (make-list 6 'none))
                               (TempStack 6 (list 'none 'none 'none
                                                  'none 'none (Some 'white)))
                               (TempStack 6 (make-list 6 'none))
                               (TempStack 6 (list 'none 'none 'none
                                                  'none 'none (Some 'white))))))
(check-expect (TempGame-board (maketempgame (Pos 2 0) gbwin (Some 'white)))
              (TempBoard (list (TempStack 6 (list 'none 'none 'none
                                                  (Some 'black) (Some 'black)
                                                  (Some 'black)))
                               (TempStack 6 (make-list 6 'none))
                               (TempStack 6 (list 'none 'none 'none
                                                  'none 'none (Some 'white)))
                               (TempStack 6 (make-list 6 'none))
                               (TempStack 6 (list 'none 'none 'none
                                                  'none 'none (Some 'white)))
                               (TempStack 6 (make-list 6 'none))
                               (TempStack 6 (list 'none 'none 'none
                                                  'none 'none (Some 'white))))))

(: count-winning-positions : Heuristic)
;; counts the number of winning positions for black minus the
;; nubmer of winning positions for white
(define (count-winning-positions g)
  (local
    {(define b (Game-board g))
     (: cwp-aux : Pos Integer Player -> Integer)
     ;; goes through each board position and checks if its a winning one
     ;;
     (define (cwp-aux p wpos cp)
       (match* (p (board-ref b p))
         [((Pos 0 6) 'none) (if (not (symbol? (outcome
                                               (maketempgame p g (Some cp)))))
                                (+ 1 wpos)
                                wpos)]
         [((Pos 0 c) 'none) (if (not (symbol? (outcome
                                               (maketempgame p g (Some cp)))))
                                (cwp-aux (Pos 5 (+ 1 c)) (+ 1 wpos) cp)
                                (cwp-aux (Pos 5 (+ 1 c)) wpos cp))]
         [((Pos r c) 'none) (if (not (symbol? (outcome
                                               (maketempgame p g (Some cp)))))
                                (cwp-aux (Pos (- r 1) c) (+ 1 wpos) cp)
                                (cwp-aux (Pos (- r 1) c) wpos cp))]
         [((Pos 0 6) _) wpos]
         [((Pos 0 c) _) (cwp-aux (Pos 5 (+ 1 c)) wpos cp)]
         [((Pos r c) _) (cwp-aux (Pos (- r 1) c) wpos cp)]))}
    (match (outcome g)
      [(Winning-Line 'black _ _) 1000]
      [(Winning-Line 'white _ _) -1000]
      [_  (- (cwp-aux (Pos 5 0) 0 'black) (cwp-aux (Pos 5 0) 0 'white))])))
           
(check-expect (count-winning-positions gbwin) 1)
(check-expect (count-winning-positions (Game b4 'white)) 0)
(check-expect (count-winning-positions (Game b1 'white)) 1000)
(check-expect (count-winning-positions (Game b2 'black)) -1000)
(check-expect (count-winning-positions gwwinx2) -1)

(: minimax-eval : Heuristic Integer Game -> Integer)
;;
(define (minimax-eval h ply gamestate)
  (local
    {(: m-eval-aux : Game -> Integer)
     ;; auxillary function to use for mapping
     ;;
     (define (m-eval-aux g)
       (minimax-eval h (- ply 1) g))} 
    (match* (ply (outcome gamestate) (Game-next gamestate))
      [(0 _ _) (h gamestate)]
      [(_ 'ongoing 'black) (foldl max -1000
                                  (map m-eval-aux (make-next-state gamestate)))]
      [(_ 'ongoing 'white) (foldl min 1000
                                  (map m-eval-aux (make-next-state gamestate)))]
      [(p _ _) (h gamestate)])))

(check-expect (minimax-eval count-winning-positions 3 new-game) 0)
(check-expect (minimax-eval count-winning-positions 3 (Game b4 'black)) 0)
(check-expect (minimax-eval count-winning-positions 3 (Game b2 'white)) -1000)
(check-expect (minimax-eval count-winning-positions 3 gbwin) 1000)
(check-expect (minimax-eval count-winning-positions 2 gbwon) 1000)
(check-expect (minimax-eval count-winning-positions 2 gwwinx2) -2)
(check-expect (minimax-eval count-winning-positions 3 gwwinx2) -1000)
(check-expect (minimax-eval did-I-win? 3 gwwinx2) -1)

(: make-next-state : Game -> (Listof Game))
;; makes a list of all the next possible games
;;
(define (make-next-state g)
  (local
    {(define p (Game-next g))
     (: aux : Integer -> (Listof Game))
     ;; auxillary function to go through each column and make a list of all the
     ;; possible next game states
     ;;
     (define (aux c)
       (match c
         [ 6 (if (valid-move? g p 6)
                 (list (Game (add-one (Game-board g) 6 p) (next-player p)))
                 '())]
         [i (if (valid-move? g p i)
                (cons (Game (add-one (Game-board g) i p) (next-player p))
                      (aux (+ i 1)))
                (aux (+ i 1)))]))}
    (aux 0)))

(check-expect (make-next-state new-game)
              (reverse (list (Game (Board (list (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 1 (list 'black))))
                          'white)
                    (Game (Board (list (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 1 (list 'black))
                                       (Stack 0 '())))
                          'white)
                    (Game (Board (list (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 1 (list 'black))
                                       (Stack 0 '())
                                       (Stack 0 '())))
                          'white)
                    (Game (Board (list (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 1 (list 'black))
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())))
                          'white)
                    (Game (Board (list (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 1 (list 'black))
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())))
                          'white)
                    (Game (Board (list (Stack 0 '())
                                       (Stack 1 (list 'black))
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())))
                          'white)
                    (Game (Board (list (Stack 1 (list 'black))
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())))
                          'white))))

(check-expect (make-next-state (Game b4 'white)) '())
(check-expect (first (make-next-state gbwin)) gbwon)

(: make-minimax-strategy : Heuristic Integer -> Strategy)
;; makes a strategy that uses the heurisic and ply to determine the best move
;;
(define (make-minimax-strategy h ply)
  (local
    {(: make-strat : Strategy)
     ;; defines the strategy to return
     ;;
     (define (make-strat g)
       (local
         {(define next-states (make-next-state g))
          (define best-indexes (find-best (map (lambda ([gs : Game])
                                                 (minimax-eval h ply gs))
                                               next-states)
                                           (Game-next g)))}
         (match best-indexes
           ['() (error "game is over")]
           [(cons i ir) (get-c g (list-ref next-states i))])))}
    make-strat))

(: teststrat1 : Strategy)
;; tester for make-minimax-strategy
;;
(define teststrat1 (make-minimax-strategy count-winning-positions 3))

(check-expect (teststrat1 new-game) 0)
(check-expect (teststrat1 gbwin) 0)
(check-expect (teststrat1 gwwinx2) 3)
(check-error (teststrat1 (Game b2 'white)) "game is over")
(check-error (teststrat1 gbwon) "game is over")

(: find-best : (Listof Integer) Player -> (Listof Integer))
;; finds the indexes of the best minimax-evals for the current player
;;
(define (find-best evals pl)
  (local
    {(define best (if (symbol=? pl 'black)
                      (foldr max -1000 evals)
                      (foldr min 1000 evals)))
     (: find-aux : Integer (Listof Integer) -> (Listof Integer))
     ;; goes through and finds the idexes where each best value is
     ;;
     (define (find-aux index is)
       (match is
         ['() '()]
         [(cons i ir) (if (= i best)
                          (cons index (find-aux (+ index 1) ir))
                          (find-aux (+ index 1) ir))]))}
    (find-aux 0 evals)))

(check-expect (find-best (list -1 -2 0 5 5 2) 'black) (list 3 4))
(check-expect (find-best (list -3 4 5 0 -3 -3 7) 'white) (list 0 4 5))
(check-expect (find-best '() 'white) '())

(: get-c : Game Game -> Integer)
;; determines which column a piece was added to, assumes game 2
;; is the next gamestate and not a completely different game
;;
(define (get-c g1 g2)
  (local
    {(define b1 (Game-board g1))
     (define plnext (Game-next g1))
     (define b2 (Game-board g2))
     (: get-aux : Pos -> Integer)
     ;; recursively finds the pos a piece was added
     ;;
     (define (get-aux p)
       (match* (p (board-ref b1 p) (board-ref b2 p))      
         [((Pos r c) 'none (Some plnext)) c]
         [((Pos 0 c) a a) (get-aux (Pos 5 (+ 1 c)))]
         [((Pos r c) a a) (get-aux (Pos (- r 1) c))]))}
    (get-aux (Pos 5 0))))

(check-expect (get-c new-game (Game (Board (list (Stack 0 '())
                                                 (Stack 0 '())
                                                 (Stack 0 '())
                                                 (Stack 0 '())
                                                 (Stack 1 (list 'black))
                                                 (Stack 0 '())
                                                 (Stack 0 '())))
                                    'white))
              4)
(check-expect (get-c gbwin gbwon) 0)



;; part 1 functions 
(: board-ref : (U TempBoard Board) Pos -> (Optional Player))
;; If there is a piece at the given position, belonging to the player pl,
;; then board-ref returns (Some pl). Otherwise, it returns 'none.
;;
(define (board-ref b pl)
  (local
    {(: c : Integer)
     (define c (Pos-col pl))
     (: r : Integer)
     (define r (Pos-row pl))
     (define ss (match b
                  [(Board s) s]
                  [(TempBoard s) s]))}
    (match (list-ref ss c)
      ['() 'none]
      [(Stack h ps) (if (and (not (= h 0)) (< r h))
                        (Some (list-ref ps (- h 1 r)))
                        'none)]
      [(TempStack h ps) (list-ref ps (- h 1 r))])))

(check-expect (board-ref (Game-board new-game) (Pos 3 4)) 'none)
(check-expect (board-ref b1 (Pos 2 3)) (Some 'white))
(check-expect (board-ref b2 (Pos 1 4)) (Some 'black))
(check-expect (board-ref b1 (Pos 5 0)) 'none)

(: valid-move? : Game Player Integer -> Boolean)
;; true if it is given player's turn, and the column value is valid and not full
;;
(define (valid-move? g p c)
  (and (symbol=? (Game-next g) p)
       (<= c 6) (>= c 0)
       (< (Stack-height (list-ref (Board-stacks (Game-board g)) c))
          6)
       (symbol? (outcome g))))

(check-expect (valid-move? (Game b1 'black) 'black 3) #f)
(check-expect (valid-move? (Game b1 'black) 'white 3) #f)
(check-expect (valid-move? (Game b1 'black) 'black 7) #f)
(check-expect (valid-move? (Game b1 'black) 'black -1) #f)
(check-expect (valid-move? (Game (Board
                                  (make-list 7 (Stack 6 (make-list 6 'black))))
                                 'black) 'black 3) #f)
(check-expect (valid-move? (Game b2 'white) 'white 5) #f)

(: apply-move : World Player Integer -> World)
;; given a game, player and column number, move is done if valid and returns
;; the updated game, else an error is raised
;;
(define (apply-move w p c)
  (local
    {(define g (World-game w))
     (define newg (if (valid-move? g p c)
                      (Game (add-one (Game-board g) c p)
                            (next-player p))
                      g))}
    (if (symbol=? (Game-next newg) (Game-next g))
        (World (World-player1 w) (World-player2 w) newg
               (World-spacing w) 'invalidmove)
        (World (World-player1 w) (World-player2 w) newg
               (World-spacing w) (outcome newg)))))

(check-expect (apply-move w1 'black 3) w1m1)
(check-expect (apply-move w1m1 'white 3) w1m2) 
(check-expect (apply-move w3 'black 3) w3invalid)  ;; wrong player

(: add-one : Board Integer Player -> Board)
;; adds one of the given players chips to specified column of the board
;;
(define (add-one b c p)
  (local
    {(: newcolumn : (Listof Player))
     (define newcolumn (cons p (Stack-pieces (list-ref (Board-stacks b) c))))
     (: add-aux : Integer (Listof Stack) -> (Listof Stack))
     ;; helper funciton with accumulator
     (define (add-aux index ss)
       (match ss
         ['() '()]
         [(cons s sr) (if (= index c)
                          (cons (Stack (+ 1 (Stack-height s)) newcolumn) sr)
                          (cons s (add-aux (+ 1 index) sr)))]))}
    (Board (add-aux 0 (Board-stacks b)))))

(check-expect (add-one (Game-board new-game) 2 'black)
              (Board (list (Stack 0 '())
                           (Stack 0 '())
                           (Stack 1 (list 'black))
                           (Stack 0 '())
                           (Stack 0 '())
                           (Stack 0 '())
                           (Stack 0 '()))))
(check-expect (add-one b2 0 'white) b3)

(: next-player : Player -> Player)
;; determines the next player
;;
(define (next-player p)
  (if (symbol=? p 'white)
      'black
      'white))

(check-expect (next-player 'white) 'black)
(check-expect (next-player 'black) 'white)

(define-struct Direction
  ([dr : Integer]
   [dc : Integer]))

(define horizontal (Direction 0 1))
(define vertical (Direction -1 0))
(define d-up (Direction 1 1))
(define d-down (Direction -1 1))

(: next-pos : Pos -> Pos)
;; determines next position to check
;;
(define (next-pos p)
  (local
    {(define r (Pos-row p))
     (define c (Pos-col p))}    
    (cond
      [(and (= r 3) (= c 6)) (Pos 2 6)]
      [(and (= r 3) (< 3 c)) (Pos 5 (+ 1 c))]
      [(= r 0) (Pos 5 (+ 1 c))]
      [else (Pos (- r 1) c)])))
      
(check-expect (next-pos (Pos 3 6)) (Pos 2 6))
(check-expect (next-pos (Pos 3 5)) (Pos 5 6))
(check-expect (next-pos (Pos 0 2)) (Pos 5 3))
(check-expect (next-pos (Pos 3 1)) (Pos 2 1))

(: tie-or-ongoing : (U TempBoard Board) -> (U 'tie 'ongoing))
;; checks if the board is still playable
;;
(define (tie-or-ongoing b)
  (match b
    [(Board ss) (if (andmap (lambda ([x : Stack]) (= (Stack-height x) 6))
                            ss)
                    'tie
                    'ongoing)]
    [(TempBoard ss) 'ongoing]))

(check-expect (tie-or-ongoing b1) 'ongoing)
(check-expect (tie-or-ongoing
               (Board (make-list 7 (Stack 6 (make-list 6 'black))))) 'tie)

(: outcome : (U TempGame Game) -> (U Winning-Line 'tie 'ongoing))
;; determines the outcome of the game with the given game
;;
(define (outcome g)
  (local
    {(define b (match g
                 [(Game b n) b]
                 [(TempGame b n) b])) 
     (: o-aux : Pos -> (U Winning-Line 'tie 'ongoing))
     (define (o-aux p)
       (match* (p (next-pos p) (board-ref b p)) 
         [((Pos 2 6) _ _) (tie-or-ongoing b)]
         [((Pos r c) (Pos nr nc) (Some x))
          (cond
            [(and (<= c 3) (check-direction b p horizontal))
             (Winning-Line x p (Pos r (+ c 3)))]
            [(and (>= r 3) (check-direction b p vertical))
             (Winning-Line x p (Pos (- r 3) c))]
            [(and (< r 3) (<= c 3) (check-direction b p d-up))
             (Winning-Line x p (Pos (+ r 3) (+ c 3)))]
            [(and (>= r 3) (<= c 3) (check-direction b p d-down))
             (Winning-Line x p (Pos (- r 3) (+ c 3)))]
            [else (o-aux (Pos nr nc))])]
         [(_ (Pos nr nc) 'none) (o-aux (Pos nr nc))]))}
    (o-aux (Pos 5 0))))

(check-expect (outcome new-game) 'ongoing)
(check-expect (outcome (Game b1 'black))
              (Winning-Line 'black (Pos 3 0) (Pos 3 3)))
(check-expect (outcome (Game b2 'black))
              (Winning-Line 'white (Pos 0 1 ) (Pos 3 4)))
(check-expect (outcome (Game b5 'black))
              (Winning-Line 'white (Pos 4 1) (Pos 1 4)))
(check-expect (outcome (Game b6 'white))
              (Winning-Line 'white (Pos 3 0) (Pos 0 0)))
(check-expect (outcome (Game b4 'black)) 'tie)          
                       
(: check-direction : (U TempBoard Board) Pos Direction -> Boolean)
;; checks if there are 4 in a row with the given start position and direction
;;
(define (check-direction b startpos d)
  (local
    {(define startp (board-ref b startpos))
     (: check-aux : Integer Pos Player -> Boolean)
     (define (check-aux counter p currentw)
       (match* (counter (board-ref b p))
         [(_ 'none) #f]
         [(4 (Some x)) (symbol=? x currentw)]
         [(_ (Some x)) (and (symbol=? x currentw)
                            (check-aux (+ 1 counter)
                                       (Pos (+ (Direction-dr d) (Pos-row p))
                                            (+ (Direction-dc d) (Pos-col p)))
                                       currentw))]))}
    (and (not (symbol? startp))
         (check-aux 1 startpos (Some-value startp)))))
        
(check-expect (check-direction b1 (Pos 0 0) horizontal) #t)
(check-expect (check-direction b1 (Pos 3 0) d-down) #f)
(check-expect (check-direction b6 (Pos 5 0) vertical) #f)

(: int->byte : Integer -> Byte)
;; converts a valid integer into a byte
;;
(define (int->byte i)
  (cond
    [(byte? i) i]
    [(< i 0) 1]
    [else 255])) 

(check-expect (int->byte 34) 34)
(check-expect (int->byte -4) 1)
(check-expect (int->byte 300) 255)

(: get-chip : (Optional Player) Integer -> Image)
;; returns the color of the chip
;;
(define (get-chip p rad)
  (local
    {(define p-main-color (match p
                            [(Some 'black) 'goldenrod]
                            [(Some 'white) 'red]
                            ['none 'white]))
     (define p-second-color (match p
                              [(Some 'black) 'gold]
                              [(Some 'white) 'tomato]
                              ['none 'white]))
     (define letter (match p
                      [(Some 'black) "Y"]
                      [(Some 'white) "R"]
                      ['none ""]))}
    (overlay (text/font letter (int->byte (- (* 2 (quotient rad 3)) 3))
                        p-second-color "Courier" 'default 'normal 'bold #f)
             (circle (quotient rad 3) "solid" p-main-color)
             (circle (* 4 (quotient rad 5)) "solid" p-second-color)
             (circle rad "solid" p-main-color))))

(check-expect (image-height (get-chip (Some 'black) 60)) 120)
(check-expect (image-width (get-chip (Some 'white) 20)) 40)
(get-chip (Some 'black) 60)
(get-chip (Some 'white) 40)
(get-chip 'none 30)

(: board-image : Board Integer -> Image)
;; produces the image of the board with given integer padding (percentage)
;;
(define (board-image b space)
  (local
    {(define rad (* (quotient space 5) 2))
     (define p (- space (* 2 rad)))
     (define pad-h (+ (* 2 p) (* 2 rad)))
     (define pad-w (+ p (* 2 rad)))
     (: column-draw : Pos -> Image)
     ;; draws one column/stack of the board
     (define (column-draw p)
       (match* (p (board-ref b p))
         [((Pos 5 _) play)
          (overlay/align "right" "middle" (get-chip play rad)
                         (rectangle pad-w pad-h "solid" 'royalblue))]
         [((Pos r c) play)
          (above (column-draw (Pos (+ r 1) c))
                 (overlay/align "right" "top" (get-chip play rad)
                                (rectangle pad-w pad-w "solid" 'royalblue)))]))
     (: board-draw : Integer (Listof Stack) -> Image)
     ;; recursively builds the board
     (define (board-draw c ss)
       (match* (c ss)
         [(7 '()) (rectangle p (+ (* 5 pad-w) pad-h) "solid" 'royalblue)]
         [(col (cons s sr)) (beside (column-draw (Pos 0 col))
                                    (board-draw (+ 1 col) sr))]))}
    (overlay (rectangle (+ (* 8 p) (* 14 rad)) (+ (* 7 p) (* 12 rad))
                        "outline" 'mediumblue)
             (board-draw 0 (Board-stacks b)))))

(check-within (image-width (board-image b3 20)) (* 7.2 20) .1)
(check-within (image-height (board-image b2 15)) (* 6.2 15) .1)

(board-image b5 10)
(board-image b3 47)
(board-image b4 100)

(: c->text : Controller -> String)
;; converts the player symbol to string name
(define (c->text p)
  (match p
    [(Human name) (if (symbol? name)
                      (symbol->string name)
                      name)]
    [(Bot name strategy) (if (symbol? name)
                             (symbol->string name)
                             name)]))

(check-expect (c->text (Human 'a)) "a")
(check-expect (c->text (Human "a")) "a")
(check-expect (c->text (Bot 'b first-available)) "b")
(check-expect (c->text (Bot "b" first-available)) "b")

(: text-output : World -> String)
;; determines the text output
;;
(define (text-output w)
  (match w
    [(World _ _ _ _'invalidmove) "Invalid Move,\nTry Again"]
    [(World c1 c2 _ _ (Winning-Line p start end))
     (string-append (if (symbol=? p 'black)
                        (c->text c1)
                        (c->text c2)) " won!")]
    [(World c1 c2 (Game b p) _'ongoing)
     (string-append "Current Player:\n " (if (symbol=? p 'black)
                                             (c->text c1)
                                             (c->text c2)))]
    [(World _ _ _ _ 'tie) "Tie Game"]))
       
(check-expect (text-output w4) "a won!")
(check-expect (text-output wywin) "y won!")
(check-expect (text-output w1) "Current Player:\n b")
(check-expect (text-output w1m1) "Current Player:\n a")
(check-expect (text-output w3invalid) "Invalid Move,\nTry Again")
(check-expect (text-output wtie) "Tie Game")

(: game-image : Game Integer -> Image)
;; creates the game image
;;
(define (game-image g s)
  (local
    {(define rad (* (quotient s 5) 2))
     (define pad (- s (* 2 rad)))
     (define boxwidth (+ (* 8 pad) (* 14 rad)))
     (define status (outcome g))
     }
    (match status
      [(Winning-Line p (Pos sr sc) (Pos er ec))
       (add-line (board-image (Game-board g) s)
                 (* s (+ sc .6))
                 (* s (- 6 sr .4))
                 (* s (+ ec .6))
                 (* s (- 6 er .4))
                 'black)]
      [_ (board-image (Game-board g) s)])))

(game-image new-game 10)
(game-image (Game b1 'white) 100)
(game-image (Game (Board (list (Stack 0 '())
                               (Stack 0 '())
                               (Stack 1 (list 'black))
                               (Stack 0 '())
                               (Stack 0 '())
                               (Stack 0 '())
                               (Stack 0 '()))) 'white) 50)
(game-image (Game b2 'white) 30)



;;PART 2
(: always-choose : Integer -> Strategy)
(define (always-choose col)
  (local
    {(: cc : Strategy)
     (define (cc game) col)}
    cc))

(: always-choose-center : Strategy)
;; strategy that only chooses the center column
(define always-choose-center (always-choose 3))

(check-expect (always-choose-center new-game) 3)

(: first-available : Strategy)
;; strategy that  chooses the first available leftmost column 
;;
(define (first-available g)
  (local
    {(: first-aux : Integer (Listof Stack) -> Integer)
     (define (first-aux index ss)
       (match ss
         ['()  (- index 1)]
         [(cons s sr) (if (< (Stack-height s) 6)
                          index
                          (first-aux (+ 1 index) sr))]))}
    (first-aux 0 (Board-stacks (Game-board g)))))

(check-expect (first-available new-game) 0)
(check-expect (first-available (Game b4 'white)) 6)

;; world definitions for testing
(define w2 (World (Bot 'b always-choose-center)
                  (Bot 'a first-available) new-game 50 'ongoing))
(define w2m1 (World (Bot 'b always-choose-center) (Bot 'a first-available)
                    (Game (Board (list (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 1 (list 'black))
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())))
                          'white) 50 'ongoing))
(define w2m2 (World (Bot 'b always-choose-center) (Bot 'a first-available)
                    (Game (Board (list (Stack 1 (list 'white))
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 1 (list 'black))
                                       (Stack 0 '())
                                       (Stack 0 '())
                                       (Stack 0 '())))
                          'black) 50 'ongoing))

(: draw : World -> Image)
;; draws the game
;;
(define (draw w)
  (local
    {(define g (World-game w))
     (define s (World-spacing w))
     (define rad (* (quotient s 5) 2))
     (define pad (- s (* 2 rad)))
     (define boxwidth (+ (* 8 pad) (* 14 rad)))}  
    (above (game-image g s)
           (overlay (rectangle boxwidth s "outline" 'mediumblue)
                    (text/font (text-output w)
                               (int->byte  (* 2 (quotient s 5))) 'navy "Courier"
                               'default 'normal 'bold #f)
                    (rectangle boxwidth s "solid" 'royalblue)))))

(draw (World (Human 'Tim) (Human 'Arthur) new-game 50 'ongoing))
(draw wywin)
(draw wtie)
(draw (World (Human 'j) (Human 'a) new-game 25 'invalidmove))

(: react-to-mouse : World Integer Integer Mouse-Event -> World)
;; reacts to a user clicking
;;
(define (react-to-mouse w x y e)
  (match e
    ["button-down" (new-world x y w)]
    [_ w]))

(check-expect (react-to-mouse w1 180 27 "button-down") w1m1)
(check-expect (react-to-mouse w1 24 38 "drag") w1)

(: new-world : Integer Integer World -> World)
;; builds a new world responding to the click
;;
(define (new-world x y w)
  (match* ((get-column x y (World-spacing w)) w)
    [(c (World c1 c2 (Game b p) _ s)) (cond
                                        [(and (symbol=? 'black p) (Human? c1))
                                         (apply-move w p c)]
                                        [(and (symbol=? 'white p) (Human? c2))
                                         (apply-move w p c)]
                                        [else (World (World-player1 w)
                                                     (World-player2 w)
                                                     (World-game w)
                                                     (World-spacing w)
                                                     'invalidmove)])]))

(check-expect (new-world 180 27 w1) w1m1)
(check-expect (new-world 180 50 w1m1) w1m2)
(check-expect (World-game (new-world 180 27 (World (Bot 'tim first-available)
                                                   (Human 'jordan)
                                                   new-game 50 'ongoing)))
              new-game)
(check-expect (World-status (new-world 180 27 (World (Bot 'tim first-available)
                                                     (Human 'jordan)
                                                     new-game 50 'ongoing)))
              'invalidmove)
  
(: get-column : Integer Integer Integer -> Integer)
;; returns the column the x y coordinates are in, seven if
;; out of bounds (in text area)
(define (get-column x y s)
  (local
    {(define rad (* (quotient s 5) 2))
     (define pad (- s (* 2 rad)))
     (define boxwidth (+ (* 8 pad) (* 14 rad)))
     (define cwidth (/ boxwidth 7))}
    (if (< y (+ (* 12 rad) (* 7 pad)))
        (exact-floor (/ x cwidth))
        7)))

(check-expect (get-column 130 27 50) 2)
(check-expect (get-column 25 500 50) 7)

(: done? : World -> Boolean)
;; determines if the game is over
;;
(define (done? w)
  (match (World-status w)
    [(Winning-Line _ _ _) #t]
    ['tie #t]
    [_ #f]))

(check-expect (done? wywin) #t)
(check-expect (done? wtie) #t)
(check-expect (done? w1) #f)

(: bot-move : World -> World)
;; if it is a bot's turn, takes turn, else does nothing
;;
(define (bot-move w)
  (match w
    [(World (Bot name strategy) _ (Game board 'black) _ status)
     (apply-move w 'black (strategy (Game board 'black)))]
    [(World _ (Bot name strategy) (Game board 'white) _ status)
     (apply-move w 'white (strategy (Game board 'white)))]
    [_ w]))

(check-expect (bot-move w1) w1)
(check-expect (World-game (bot-move w2)) (World-game w2m1))
(check-expect (World-status (bot-move w2)) (World-status w2m1))
(check-expect (World-game (bot-move w2m1)) (World-game w2m2))
(check-expect (World-status (bot-move w2m1)) (World-status w2m2))

(: play : Controller Controller Integer -> World)
;; runs a new game
;;
(define (play c1 c2 s)
  (big-bang (World c1 c2 new-game s 'ongoing) : World
    [name "Connect Four"]
    [to-draw draw]
    [on-mouse react-to-mouse]
    [on-tick bot-move 2]
    [stop-when done? draw]))

(play (Human "Jordan")
      (Bot 'Sarah (make-minimax-strategy count-winning-positions 3))
       75)

(test)