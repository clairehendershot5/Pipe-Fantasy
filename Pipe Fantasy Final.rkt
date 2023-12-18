;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Task 7 : uncomment line 899
;; Note: my next pipe feeds from the bottom of my incoming pipe list so my pipe lists looks
;; opposite from the picture. 

(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.

(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-TL (make-pipe #true #false #true #false)) 
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-TBLR (make-pipe #true #true #true #true))

(define PIPE-ST (make-pipe #true #false #false #false))
(define PIPE-SB (make-pipe #false #true #false #false))
(define PIPE-SL (make-pipe #false #false #true #false))
(define PIPE-SR (make-pipe #false #false #false #true))

(define (pipe-temp p)
  (... (pipe-top p) ...
       (pipe-bot p) ...
       (pipe-left p) ...
       (pipe-right p) ...))

(define ALL-PIPES (list PIPE-TB PIPE-LR PIPE-TR PIPE-TL PIPE-BR PIPE-BL PIPE-TBLR PIPE-ST PIPE-SB PIPE-SL PIPE-SR))

;; pipe->image: Pipe Integer Integer Boolean String -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.
;; If filled? then draw the pipe with goo. The direction is used to determine
;; which parts of the TBLR pipes are filled with goo. 
(define (pipe->image pipe tile-side-length pipe-width filled? direction)
  (local [;; make-top : Number Number Boolean -> Image
          ;; Makes a top pipe.
          (define (make-top tile-side-length pipe-width filled?)
            (put-pinhole (* 0.5 pipe-width) (* 0.5 tile-side-length)
                         (if filled?
                             (rectangle pipe-width (* 0.5 tile-side-length) "solid" "lime")
                             (rectangle pipe-width (* 0.5 tile-side-length) "solid" "black"))))
          ;; make-bottom : Number Number Boolean -> Image
          ;; Makes a bottom pipe.
          (define (make-bottom tile-side-length pipe-width filled?)
            (put-pinhole (* 0.5 pipe-width) 0
                         (if filled?
                             (rectangle pipe-width (* 0.5 tile-side-length) "solid" "lime")
                             (rectangle pipe-width (* 0.5 tile-side-length) "solid" "black"))))
          ;; make-left : Number Number Boolean -> Image
          ;; Makes a left pipe.
          (define (make-left tile-side-length pipe-width filled?)
            (put-pinhole (* 0.5 tile-side-length) (* 0.5 pipe-width)
                         (if filled?
                             (rectangle (* 0.5 tile-side-length) pipe-width "solid" "lime")
                             (rectangle (* 0.5 tile-side-length) pipe-width "solid" "black"))))
          ;; make-right : Number Number Boolean -> Image
          ;; Makes a right pipe.
          (define (make-right tile-side-length pipe-width filled?)
            (put-pinhole 0 (* 0.5 pipe-width)
                         (if filled?
                             (rectangle (* 0.5 tile-side-length) pipe-width "solid" "lime")
                             (rectangle (* 0.5 tile-side-length) pipe-width "solid" "black"))))

          ;;tblr-pipe? : Pipe -> Boolean
          ;; Returns true if the given pipe is a tblr pipe.
          (define (tblr-pipe? pipe)
            (and (and (pipe-top pipe) (pipe-bot pipe))
                 (and (pipe-left pipe) (pipe-right pipe))))]     
  (clear-pinhole (overlay/pinhole
                  (if filled?
                      (square pipe-width "solid" "lime")
                      (square pipe-width "solid" "black"))
                  (if (and (tblr-pipe? pipe) filled?)
                      (cond [(string=? "BOTH" direction)
                             (overlay/pinhole (make-top tile-side-length pipe-width #true)
                                              (make-bottom tile-side-length pipe-width #true)
                                              (make-right tile-side-length pipe-width #true)
                                              (make-left tile-side-length pipe-width #true))]
                            [(or (string=? "UP" direction) (string=? "DOWN" direction))
                             (overlay/pinhole (make-top tile-side-length pipe-width #true)
                                              (make-bottom tile-side-length pipe-width #true)
                                              (make-right tile-side-length pipe-width #false)
                                              (make-left tile-side-length pipe-width #false))]
                            [(or (string=? "LEFT" direction) (string=? "RIGHT" direction)) 
                             (overlay/pinhole (make-top tile-side-length pipe-width #false)
                                              (make-bottom tile-side-length pipe-width #false)
                                              (make-right tile-side-length pipe-width #true)
                                              (make-left tile-side-length pipe-width #true))])
                      (overlay/pinhole (if (pipe-top pipe)
                                            (make-top tile-side-length pipe-width filled?)
                                            empty-image)
                                        (if (pipe-bot pipe)
                                            (make-bottom tile-side-length pipe-width filled?)
                                            empty-image)
                                        (if (pipe-left pipe)
                                            (make-left tile-side-length pipe-width filled?)
                                            empty-image)
                                        (if (pipe-right pipe)
                                            (make-right tile-side-length pipe-width filled?) 
                                            empty-image)))
                  (square tile-side-length "outline" "black")
                  (square tile-side-length "solid" "gray"))))) 

(define-struct pipe-cord [pipe x y direction])
;; A PipeCord is a (make-pipe-cord Pipe Integer Integer Direction)
;; Interpretation: a pipe with it's the x and y coordinates on the grid.
(define PIPE-CORD1 (make-pipe-cord PIPE-TB 3 2 "UP"))
(define PIPE-CORD2 (make-pipe-cord PIPE-LR 1 5 "UP"))
(define PIPE-CORD3 (make-pipe-cord PIPE-TL 1 3 "LEFT"))
(define PIPE-CORD4 (make-pipe-cord PIPE-TR 5 4 "UP"))
(define PIPE-CORD5 (make-pipe-cord PIPE-BL 3 3 "UP"))
(define PIPE-CORD6 (make-pipe-cord PIPE-BR 2 1 "UP"))
(define PIPE-CORD7 (make-pipe-cord PIPE-TBLR 1 4 "UP"))

(define PIPE-CORD8 (make-pipe-cord PIPE-ST 1 3 "UP"))
(define PIPE-CORD9 (make-pipe-cord PIPE-SB 1 5 "DOWN"))
(define PIPE-CORD10 (make-pipe-cord PIPE-SL 2 2 "LEFT"))
(define PIPE-CORD11 (make-pipe-cord PIPE-SR 1 1 "RIGHT"))

(define (pipe-cord-temp pc)
  (... (pipe-temp (pipe-cord-pipe pc)) ...
       (pipe-cord-x pc) ...
       (pipe-cord-y pc) ...))

(define PIPES-ON-GRID-L1 (cons PIPE-CORD8 '()))
(define PIPES-ON-GRID-L2 (cons PIPE-CORD3 (cons PIPE-CORD5 (cons PIPE-CORD7 (cons PIPE-CORD9 '())))))
(define PIPES-ON-GRID-L3 (cons PIPE-CORD1 (cons PIPE-CORD3 (cons PIPE-CORD10 '()))))

;; A Direction is either a:
;; - "UP"
;; - "DOWN"
;; - "LEFT"
;; - "RIGHT"
;; Interpretation: the possible direction's that the goo is flowing.

(define UP "UP")
(define DOWN "DOWN")
(define LEFT "LEFT")
(define RIGHT "RIGHT")

(define (direction-temp d)
  (... (cond [(string=? d UP) ...]
             [(string=? d DOWN) ...]
             [(string=? d LEFT) ...]
             [(string=? d RIGHT) ...])))

(define-struct gooflow [path direction]) 
;; A GooFlow is a (make-gooflow [Listof Pipe-Cord] Direction)
;; Interpretation: gooflow represents represents the path taken by the goo
;; by listing the pipes that have goo and the direction in which it is flowing.

(define (gooflow-temp gf)
  (... (list-temp (gooflow-path gf) ...)
       (direction-temp (gooflow-direction gf) ...)))

(define GF1 (make-gooflow (list PIPE-CORD8) "UP"))
(define GF2 (make-gooflow (list PIPE-CORD7 PIPE-CORD9) "DOWN"))
(define GF3 (make-gooflow (list PIPE-CORD10) "LEFT"))
  
(define-struct grid [width height x-counter pipe-list])
;; A Grid is a (make-grid Integer Integer Integer Integer [Listof PipeCord])
;; Interpretation: a grid with the given width and height and a list of
;; the placed pipes on the grid. The x-counter is used for abstraction
;; in the grid->image function. X-counter is always 1.
(define GRID1 (make-grid 3 3 1 PIPES-ON-GRID-L1))
(define GRID2 (make-grid 5 5 1 PIPES-ON-GRID-L2))
(define GRID3 (make-grid 2 5 1 PIPES-ON-GRID-L3))

(define (grid-temp g)
  (... (grid-width g) ...
       (grid-height g) ...
       (grid-x-counter g) ...
       (pipe-cord-temp (grid-pipe-list g)) ...))

(define STARTING-GRID (make-grid 7 7 1 '()))
       
;; place-pipe : Grid Pipe Integer Integer Direction -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(check-expect (place-pipe GRID1 PIPE-CORD7 1 3 "UP")
              (make-grid 3 3 1 (cons (make-pipe-cord PIPE-CORD7 1 3 "UP") PIPES-ON-GRID-L1)))
(check-expect (place-pipe GRID2 PIPE-CORD5 3 3 "UP")
              (make-grid 5 5 1 (cons (make-pipe-cord PIPE-CORD5 3 3 "UP") PIPES-ON-GRID-L2)))
(check-expect (place-pipe GRID3 PIPE-CORD4 1 4 "UP")
              (make-grid 2 5 1 (cons (make-pipe-cord PIPE-CORD4 1 4 "UP") PIPES-ON-GRID-L3)))

(define (place-pipe grid pipe row col dir)
  (make-grid (grid-width grid)
             (grid-height grid)
             (grid-x-counter grid)
             (cons (make-pipe-cord pipe row col dir) (grid-pipe-list grid))))

;; pipe-at : Grid Integer Integer -> [Optional Pipe-Cord]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(check-expect (pipe-at GRID1 2 2) #false)
(check-expect (pipe-at GRID2 1 3) PIPE-CORD3)
(check-expect (pipe-at GRID3 2 2) PIPE-CORD10)

(define (pipe-at grid row col)
  (cond
    [(empty? (grid-pipe-list grid)) #false]
    [(cons? (grid-pipe-list grid))
     (if (and (= row (pipe-cord-x (first (grid-pipe-list grid))))
              (= col (pipe-cord-y (first (grid-pipe-list grid)))))
         (first (grid-pipe-list grid))
         (pipe-at (make-grid (grid-width grid)
                             (grid-height grid)
                             (grid-x-counter grid)
                             (rest (grid-pipe-list grid)))
                  row col))]))

;; grid-goo-propagate : GooFlow Grid -> GooFlow
;; Moves the goo forward by one tile. If the goo is stuck, produce the same goo.
(check-expect (grid-goo-propagate GF1 GRID1) GF1) ;; boundary
(check-expect (grid-goo-propagate GF2 GRID2)
              (make-gooflow (list PIPE-CORD3 PIPE-CORD7 PIPE-CORD9) "LEFT")) ;; change direction and propogate goo
(check-expect (grid-goo-propagate GF3 GRID3) GF3) ;; empty cell
(check-expect (grid-goo-propagate GF3 (make-grid 5 5 1 (list (make-pipe-cord PIPE-TB 2 1 "LEFT") PIPE-CORD10))) GF3) ;; no opening
 
(define (grid-goo-propagate gf grid)
  (local [;; boundary-abstraction? : String Number (Pipe-Cord -> Number) GooFlow -> Boolean
          ;; Abstraction to check if the goo pointing towards a boundary.
          (define (boundary-abstraction? direction num func gf)
            (and (string=? direction (gooflow-direction gf)) (= num (func (first (gooflow-path gf))))))

          ;; empty-cell-abstraction? : String Number Number Grid GooFlow -> Boolean
          ;; Abstraction to check if the goo is at an empty cell.
          (define (empty-cell-abstraction? direction x y grid gf)
            (and (string=? direction (gooflow-direction gf)) (boolean? (pipe-at grid x y))))

          ;; no-opening-abstraction? : String Number Number Grid (Pipe -> Boolean) GooFlow -> Boolean
          ;; Abstraction to check if the goo is at a pipe without an opening.
          (define (no-opening-abstraction? direction x y grid func gf)
            (and (and (string=? direction (gooflow-direction gf)) (pipe-cord? (pipe-at grid x y)))
                 (boolean=? #false (func (pipe-cord-pipe (pipe-at grid x y))))))

          ;; get-gooflow-pipe : GooFlow Grid -> Pipe-Cord
          ;; Returns the correct pipe-cord to add to the gooflow path list. 
          (define (get-gooflow-pipe gf grid)
            (cond [(string=? "UP" (gooflow-direction gf))
                   (pipe-at grid (pipe-cord-x (first (gooflow-path gf))) (+ (pipe-cord-y (first (gooflow-path gf))) 1))]
                  [(string=? "DOWN" (gooflow-direction gf))
                   (pipe-at grid (pipe-cord-x (first (gooflow-path gf))) (- (pipe-cord-y (first (gooflow-path gf))) 1))]
                  [(string=? "LEFT" (gooflow-direction gf))
                   (pipe-at grid (- (pipe-cord-x (first (gooflow-path gf))) 1) (pipe-cord-y (first (gooflow-path gf))))]
                  [(string=? "RIGHT" (gooflow-direction gf))
                   (pipe-at grid (+ (pipe-cord-x (first (gooflow-path gf))) 1) (pipe-cord-y (first (gooflow-path gf))))])) 

          ;; return-type-pipe : Pipe -> String
          ;; Returns the name of the pipe.
          (define (return-type-pipe p)
            (cond [(and (and (boolean=? #true (pipe-top p)) (boolean=? #true (pipe-bot p)))
                        (and (boolean=? #true (pipe-right p)) (boolean=? #true (pipe-left p))))
                   "PIPE-TBLR"]
                  [(and (and (boolean=? #true (pipe-top p)) (boolean=? #true (pipe-bot p)))
                        (and (boolean=? #false (pipe-right p)) (boolean=? #false (pipe-left p))))
                   "PIPE-TB"]
                  [(and (and (boolean=? #false (pipe-top p)) (boolean=? #false (pipe-bot p)))
                        (and (boolean=? #true (pipe-right p)) (boolean=? #true (pipe-left p))))
                   "PIPE-LR"]
                  [(and (and (boolean=? #true (pipe-top p)) (boolean=? #false (pipe-bot p)))
                        (and (boolean=? #false (pipe-right p)) (boolean=? #true (pipe-left p))))
                   "PIPE-TL"]
                  [(and (and (boolean=? #false (pipe-top p)) (boolean=? #true (pipe-bot p)))
                        (and (boolean=? #false (pipe-right p)) (boolean=? #true (pipe-left p))))
                   "PIPE-BL"]
                  [(and (and (boolean=? #true (pipe-top p)) (boolean=? #false (pipe-bot p)))
                        (and (boolean=? #true (pipe-right p)) (boolean=? #false (pipe-left p))))
                   "PIPE-TR"]
                  [(and (and (boolean=? #false (pipe-top p)) (boolean=? #true (pipe-bot p)))
                        (and (boolean=? #true (pipe-right p)) (boolean=? #false (pipe-left p))))
                   "PIPE-BR"]))

          ;; change-direction : GooFlow Grid -> Direction
          ;; Returns the direction that the goo should flow in.
          (define (change-direction gf grid)
            (cond [(string=? "UP" (gooflow-direction gf))
                   (cond [(string=? "PIPE-BL" (return-type-pipe (pipe-cord-pipe (get-gooflow-pipe gf grid)))) "LEFT"]
                         [(string=? "PIPE-BR" (return-type-pipe (pipe-cord-pipe (get-gooflow-pipe gf grid)))) "RIGHT"]
                         [ "UP"])]
                  [(string=? "DOWN" (gooflow-direction gf))
                   (cond [(string=? "PIPE-TL" (return-type-pipe (pipe-cord-pipe (get-gooflow-pipe gf grid)))) "LEFT"]
                         [(string=? "PIPE-TR" (return-type-pipe (pipe-cord-pipe (get-gooflow-pipe gf grid)))) "RIGHT"]
                         [else "DOWN"])]
                  [(string=? "LEFT" (gooflow-direction gf))
                   (cond [(string=? "PIPE-TR" (return-type-pipe (pipe-cord-pipe (get-gooflow-pipe gf grid)))) "UP"]
                         [(string=? "PIPE-BR" (return-type-pipe (pipe-cord-pipe (get-gooflow-pipe gf grid)))) "DOWN"]
                         [else "LEFT"])]
                  [(string=? "RIGHT" (gooflow-direction gf))
                   (cond [(string=? "PIPE-TL" (return-type-pipe (pipe-cord-pipe (get-gooflow-pipe gf grid)))) "UP"]
                         [(string=? "PIPE-BL" (return-type-pipe (pipe-cord-pipe (get-gooflow-pipe gf grid)))) "DOWN"]
                         [else "RIGHT"])]))]
 (cond [(boundary-abstraction? "UP" (grid-height grid) pipe-cord-y gf) gf] 
       [(boundary-abstraction? "DOWN" 0 pipe-cord-y gf) gf]
       [(boundary-abstraction? "LEFT" 0 pipe-cord-x gf) gf]
       [(boundary-abstraction? "RIGHT" (grid-width grid) pipe-cord-x gf) gf]
       [(empty-cell-abstraction? "UP" (pipe-cord-x (first (gooflow-path gf))) (+ (pipe-cord-y (first (gooflow-path gf))) 1) grid gf) gf]
       [(empty-cell-abstraction? "DOWN" (pipe-cord-x (first (gooflow-path gf))) (- (pipe-cord-y (first (gooflow-path gf))) 1) grid gf) gf]
       [(empty-cell-abstraction? "LEFT" (- (pipe-cord-x (first (gooflow-path gf))) 1) (pipe-cord-y (first (gooflow-path gf))) grid gf) gf]
       [(empty-cell-abstraction? "RIGHT" (+ (pipe-cord-x (first (gooflow-path gf))) 1) (pipe-cord-y (first (gooflow-path gf))) grid gf) gf]
       [(no-opening-abstraction? "UP" (pipe-cord-x (first (gooflow-path gf))) (+ (pipe-cord-y (first (gooflow-path gf))) 1) grid pipe-bot gf) gf]
       [(no-opening-abstraction? "DOWN" (pipe-cord-x (first (gooflow-path gf))) (- (pipe-cord-y (first (gooflow-path gf))) 1) grid pipe-top gf) gf]
       [(no-opening-abstraction? "LEFT" (- (pipe-cord-x (first (gooflow-path gf))) 1) (pipe-cord-y (first (gooflow-path gf))) grid pipe-right gf) gf]
       [(no-opening-abstraction? "RIGHT" (+ (pipe-cord-x (first (gooflow-path gf))) 1) (pipe-cord-y (first (gooflow-path gf))) grid pipe-left gf) gf]
       [else (make-gooflow (cons (make-pipe-cord (pipe-cord-pipe (get-gooflow-pipe gf grid))
                                                 (pipe-cord-x (get-gooflow-pipe gf grid))
                                                 (pipe-cord-y (get-gooflow-pipe gf grid))
                                                 (change-direction gf grid))
                                 (gooflow-path gf)) (change-direction gf grid))]))) 

       
;; make-tile : Grid Integer Integer Integer Integer Gooflow -> Image
;; Makes a tile if a pipe does not exist at the given coordinate, otherwise
;; displays the pipe at that coordinate.
(check-expect (make-tile GRID1 2 2 100 25 GF1) (overlay
                                                (square 100 "outline" "black")
                                                (square 100 "solid" "light gray")))
(check-expect (make-tile GRID2 1 4 100 25 GF2) (pipe->image PIPE-TBLR 100 25 #true "DOWN"))
(check-expect (make-tile GRID3 3 2 100 25 GF3) (pipe->image PIPE-TB 100 25 #false "LEFT"))

(define (make-tile grid x y tile-side-length pipe-width gooflow)
  (local [;; pipe-equals? : Pipe-Cord Pipe-Cord -> Boolean
          ;; Returns true if the given pipe is the same pipe
          ;; as the pipe-cord.
          (define (pipe-equals? gf-pc pc)
            (and (and (and (boolean=? (pipe-top (pipe-cord-pipe gf-pc)) (pipe-top (pipe-cord-pipe pc)))
                           (boolean=? (pipe-bot (pipe-cord-pipe gf-pc)) (pipe-bot (pipe-cord-pipe pc))))
                      (boolean=? (pipe-left (pipe-cord-pipe gf-pc)) (pipe-left (pipe-cord-pipe pc))))
                 (boolean=? (pipe-right (pipe-cord-pipe gf-pc)) (pipe-right (pipe-cord-pipe pc)))))

          ;; pipe-in-gooflow? : [Listof Pipe-Cord] Pipe-Cord -> [Optional Pipe]
          ;; Returns the pipe if the given pipe is in the pipe-cord list, otherwise #false.
          (define (pipe-in-gooflow? gf-path pc)
            (cond [(empty? gf-path) #false]
                  [(cons? gf-path)
                   (if (and (= (pipe-cord-x pc) (pipe-cord-x (first gf-path)))
                            (= (pipe-cord-y pc) (pipe-cord-y (first gf-path)))
                            (pipe-equals? (first gf-path) pc))
                       (first gf-path) 
                       (pipe-in-gooflow? (rest gf-path) pc))]))

          ;; filter-to-tblr-pipes : Gooflow -> [Listof Pipe-Cord]
          ;; Filters the path of gooflow pipes down to the tblr pipes.
          (define (filter-to-tblr-pipes gf)
            (filter (lambda (pc) (and (and (pipe-top (pipe-cord-pipe pc)) (pipe-bot (pipe-cord-pipe pc)))
                                            (and (pipe-left (pipe-cord-pipe pc)) (pipe-right (pipe-cord-pipe pc)))))
                           (gooflow-path gf)))

          ;; same-tblr->num : (pipe-at grid x y) [Listof Pipe-Cord] -> [Listof Integer]
          ;; Changes the pipe-cord to 1 if its equal to the given pipe, otherwise 0.
          (define (same-tblr->num pc lopc)
            (map (lambda (pipe-cord) (if (and (= (pipe-cord-x pc) (pipe-cord-x pipe-cord))
                                              (= (pipe-cord-y pc) (pipe-cord-y pipe-cord)))
                                         1
                                         0))
                   lopc))

          ;; sum-list : [Listof Integer] -> Integer
          ;; Returns the sum of the given list.
          (define (sum-list lon)
            (foldr + 0 lon))

          ;; draw-direction : Grid Integer Integer Gooflow -> String
          ;; Returns the direction that the tblr pipe should be drawn in.
          (define (draw-direction grid x y gooflow)
            (if (= 2 (sum-list (same-tblr->num (pipe-at grid x y) (filter-to-tblr-pipes gooflow))))
              "BOTH"
              (pipe-cord-direction (pipe-in-gooflow? (gooflow-path gooflow) (pipe-at grid x y)))))] 
    (cond [(boolean? (pipe-at grid x y))
           (overlay
            (square tile-side-length "outline" "black")
            (square tile-side-length "solid" "light gray"))]
          [(pipe-cord? (pipe-at grid x y))
           (cond [(pipe-cord? (pipe-in-gooflow? (gooflow-path gooflow) (pipe-at grid x y)))
                  (pipe->image (pipe-cord-pipe (pipe-at grid x y)) tile-side-length pipe-width #true (draw-direction grid x y gooflow))]
                 [else (pipe->image (pipe-cord-pipe (pipe-at grid x y)) tile-side-length pipe-width #false (gooflow-direction gooflow))])])))

;; make-row : Grid Integer Integer GooFlow -> Image
;; Makes a row of tiles/pipes.
(check-expect (make-row GRID1 1 100 25 GF1) (beside (overlay
                                                     (square 100 "outline" "black")
                                                     (square 100 "solid" "light gray"))
                                                    (overlay
                                                     (square 100 "outline" "black")
                                                     (square 100 "solid" "light gray"))
                                                    (overlay
                                                     (square 100 "outline" "black")
                                                     (square 100 "solid" "light gray"))))
(check-expect (make-row GRID2 4 100 25 GF2) (beside (pipe->image PIPE-TBLR 100 25 #true "DOWN")
                                                    (overlay
                                                     (square 100 "outline" "black")
                                                     (square 100 "solid" "light gray"))
                                                    (overlay
                                                     (square 100 "outline" "black")
                                                     (square 100 "solid" "light gray"))
                                                    (overlay
                                                     (square 100 "outline" "black")
                                                     (square 100 "solid" "light gray"))
                                                    (overlay
                                                     (square 100 "outline" "black")
                                                     (square 100 "solid" "light gray"))))
(check-expect (make-row GRID3 3 100 25 GF3) (beside (pipe->image PIPE-TL 100 25 #false "LEFT")
                                                    (overlay
                                                     (square 100 "outline" "black")
                                                     (square 100 "solid" "light gray"))))

(define (make-row grid y tile-side-length pipe-width gooflow)
  (cond [(< (grid-width grid) (grid-x-counter grid)) empty-image]
        [(<= (grid-x-counter grid) (grid-width grid))
         (beside (make-tile grid (grid-x-counter grid) y tile-side-length pipe-width gooflow)
                 (make-row (make-grid (grid-width grid)
                                      (grid-height grid)
                                      (+ 1 (grid-x-counter grid))
                                      (grid-pipe-list grid))
                           y tile-side-length pipe-width gooflow))]))
         
;; grid->image : Grid Integer Integer GooFlow -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width gooflow)
  (cond [(= (grid-height grid) 0) empty-image]
        [(< 0 (grid-height grid))
         (above (make-row grid (grid-height grid) tile-side-length pipe-width gooflow)
                (grid->image (make-grid (grid-width grid)
                                        (- (grid-height grid) 1)
                                        (grid-x-counter grid)
                                        (grid-pipe-list grid))
                             tile-side-length pipe-width gooflow))]))

(define START-PIPES-L1 (list PIPE-TB PIPE-LR PIPE-TR PIPE-TL PIPE-BR PIPE-BL PIPE-TBLR PIPE-LR PIPE-TR PIPE-TL))
(define START-PIPES-L2 (list PIPE-TB PIPE-TBLR PIPE-LR PIPE-TR))
(define START-PIPES-L3 (list PIPE-TB PIPE-TR PIPE-TL PIPE-BR PIPE-BL PIPE-LR PIPE-TR PIPE-BR PIPE-BL PIPE-TBLR PIPE-TR PIPE-TL))

(define-struct gamestate [grid incoming-pipes tile-side-length pipe-width starting-pipe gooflow num-replaced timer])
;; A GameState is a (make-gamestate Grid [Listof Pipes] Integer Integer Pipe-Cord GooFlow Number Number)
;; Interpretation: a gamestate represents the current state of the game. The grid contains the size of
;; the grid and current pipes on the grid and the incoming pipes is a list of pipes for the player
;; to place. The tile side length and pipe width are dimensions for the tiles and pipes. Num-replaced
;; is the number of pipes the user has replaced. Time till goo represents the amount of ticks until the
;; goo propogates to the next pipe

(define GAMESTATE1 (make-gamestate STARTING-GRID START-PIPES-L1 100 25 (make-pipe-cord PIPE-ST 5 5 "UP") (make-gooflow (cons (make-pipe-cord PIPE-ST 5 5 "UP") '()) "UP") 0 140))
(define GAMESTATE2 (make-gamestate (make-grid 3 3 1 '()) START-PIPES-L2 100 25 (make-pipe-cord PIPE-SB 5 2 "DOWN") (make-gooflow (cons (make-pipe-cord PIPE-SB 5 2 "DOWN") '()) "DOWN") 0 140))
(define GAMESTATE3 (make-gamestate (make-grid 5 5 1 '()) START-PIPES-L3 100 25 (make-pipe-cord PIPE-SL 3 1 "LEFT") (make-gooflow (cons (make-pipe-cord PIPE-SL 3 1 "LEFT") '()) "LEFT") 0 140))
(define GAMESTATE4 (make-gamestate (make-grid 3 3 1 '()) START-PIPES-L3 100 25 (make-pipe-cord PIPE-SL 2 4 "LEFT") (make-gooflow (cons (make-pipe-cord PIPE-SL 2 4 "LEFT") '()) "LEFT") 0 140))

(define (gamestate-temp gs)
  (... (grid-temp (gamestate-grid gs)) ...
       (gamestate-incoming-pipes gs) ...
       (gamestate-tile-side-length gs) ...
       (gamestate-pipe-width gs)...
       (pipe-cord-temp (gamestate-starting-pipe gs)) ...
       (gamestate-gooflow gs) ...
       (gamestate-num-replaced gs) ...
       (gamestate-timer gs) ...))

;; gamestate-init : Integer Integer Integer Integer Direction [Listof Pipes] -> GameState
;; Creates a gamestate given the the grid dimension, the x and y grid
;; coordinates of the starting pipe, the direction of the starting pipe
;; and the incoming pipes list.
(check-expect (gamestate-init 7 7 5 5 UP START-PIPES-L1) GAMESTATE1)
(check-expect (gamestate-init 3 3 5 2 DOWN START-PIPES-L2) GAMESTATE2)
(check-expect (gamestate-init 3 3 2 4 LEFT START-PIPES-L3) GAMESTATE4)

(define (gamestate-init width height x y direction incoming-pipes)
  (local [;; make-starting-pipe : String Integer Integer -> Pipe-Cord
          ;; Creates a starting pipe given a direction.
          (define (make-starting-pipe d x y)
            (cond [(string=? d UP) (make-pipe-cord PIPE-ST x y d)]
                  [(string=? d DOWN) (make-pipe-cord PIPE-SB x y d)]
                  [(string=? d LEFT) (make-pipe-cord PIPE-SL x y d)]
                  [(string=? d RIGHT) (make-pipe-cord PIPE-SR x y d)]))]
            (make-gamestate (make-grid width height 1 '())
                  incoming-pipes
                  100
                  25
                  (make-starting-pipe direction x y)
                  (make-gooflow (cons (make-starting-pipe direction x y) '()) direction)
                  0
                  140)))
                  
;; draw-pipe-list : GameState -> Image
;; Draws the list of incoming pipes given a gamestate.
(check-expect (draw-pipe-list GAMESTATE1) (above (pipe->image PIPE-BR 100 25 #false "UP")
                                                 (pipe->image PIPE-TL 100 25 #false "UP")
                                                 (pipe->image PIPE-TR 100 25 #false "UP")
                                                 (pipe->image PIPE-LR 100 25 #false "UP")
                                                 (pipe->image PIPE-TB 100 25 #false "UP")))
(check-expect (draw-pipe-list GAMESTATE2) (above (pipe->image PIPE-TR 100 25 #false "DOWN")
                                                 (pipe->image PIPE-LR 100 25 #false "DOWN")
                                                 (pipe->image PIPE-TBLR 100 25 #false "DOWN")
                                                 (pipe->image PIPE-TB 100 25 #false "DOWN")))
(check-expect (draw-pipe-list GAMESTATE4) (above (pipe->image PIPE-BL 100 25 #false "LEFT")
                                                 (pipe->image PIPE-BR 100 25 #false "LEFT")
                                                 (pipe->image PIPE-TL 100 25 #false "LEFT")
                                                 (pipe->image PIPE-TR 100 25 #false "LEFT")
                                                 (pipe->image PIPE-TB 100 25 #false "LEFT"))) 

(define (draw-pipe-list gs)
  (cond [(<= 5 (length (gamestate-incoming-pipes gs)))
         (above (pipe->image (fifth (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (fourth (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (third (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (second (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (first (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs))))]
        [(= (length (gamestate-incoming-pipes gs)) 4)
         (above (pipe->image (fourth (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (third (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (second (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (first (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs))))]
        [(= (length (gamestate-incoming-pipes gs)) 3)
         (above (pipe->image (third (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (second (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (first (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs))))]
        [(= (length (gamestate-incoming-pipes gs)) 2)
         (above (pipe->image (second (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))
                (pipe->image (first (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs))))]
        [(= (length (gamestate-incoming-pipes gs)) 1)
         (pipe->image (first (gamestate-incoming-pipes gs)) (gamestate-tile-side-length gs) (gamestate-pipe-width gs) #false (gooflow-direction (gamestate-gooflow gs)))]
        [(= (length (gamestate-incoming-pipes gs)) 0)
         empty-image]))

;; calculate-grid-width : GameState -> Integer
;; Calculates the width of the grid in pixels.
(check-expect (calculate-grid-width GAMESTATE1) 700)
(check-expect (calculate-grid-width GAMESTATE2) 300)
(check-expect (calculate-grid-width GAMESTATE3) 500)

(define (calculate-grid-width gs)
  (* (grid-width (gamestate-grid gs)) (gamestate-tile-side-length gs)))

;; calculate-grid-height : GameState -> Integer
;; Calculates the height of the grid in pixels.
(check-expect (calculate-grid-height GAMESTATE1) 700)
(check-expect (calculate-grid-height GAMESTATE2) 300)
(check-expect (calculate-grid-height GAMESTATE3) 500)

(define (calculate-grid-height gs)
  (* (grid-height (gamestate-grid gs)) (gamestate-tile-side-length gs)))

;; make-grid-border : GameState -> Image
;; Makes a border for the grid.
(check-expect (make-grid-border GAMESTATE1) (overlay(rectangle 700 700  "outline" "red")
                                                    (rectangle 702 702  "outline" "red")
                                                    (rectangle 704 704  "outline" "white")
                                                    (rectangle 706 706  "outline" "red")
                                                    (rectangle 708 708  "outline" "red")))
(check-expect (make-grid-border GAMESTATE2)(overlay (rectangle 300 300  "outline" "red")
                                                    (rectangle 302 302  "outline" "red")
                                                    (rectangle 304 304  "outline" "white")
                                                    (rectangle 306 306  "outline" "red")
                                                    (rectangle 308 308  "outline" "red")))
(check-expect (make-grid-border GAMESTATE3)(overlay (rectangle 500 500  "outline" "red")
                                                    (rectangle 502 502  "outline" "red")
                                                    (rectangle 504 504  "outline" "white")
                                                    (rectangle 506 506  "outline" "red")
                                                    (rectangle 508 508  "outline" "red")))

(define (make-grid-border gs)
  (overlay 
   (rectangle (calculate-grid-width gs) (calculate-grid-height gs)  "outline" "red")
   (rectangle (+ 2 (calculate-grid-width gs)) (+ 2 (calculate-grid-height gs))  "outline" "red")
   (rectangle (+ 4 (calculate-grid-width gs)) (+ 4 (calculate-grid-height gs))  "outline" "white")
   (rectangle (+ 6 (calculate-grid-width gs)) (+ 6 (calculate-grid-height gs))  "outline" "red")
   (rectangle (+ 8 (calculate-grid-width gs)) (+ 8 (calculate-grid-height gs))  "outline" "red")))
              
;; draw-grid : GameState -> Image
;; Draws a grid of pipes and tiles with a border given a gamestate.
(check-expect (draw-grid GAMESTATE1) (overlay (make-grid-border GAMESTATE1)
                                              (grid->image STARTING-GRID 100 25 (make-gooflow (cons (make-pipe-cord PIPE-ST 5 5 "UP") '()) "UP"))))
(check-expect (draw-grid GAMESTATE2) (overlay (make-grid-border GAMESTATE2)
                                             (grid->image (make-grid 3 3 1 '()) 100 25 (make-gooflow (cons (make-pipe-cord PIPE-SB 5 2 "DOWN") '()) "DOWN"))))
(check-expect (draw-grid GAMESTATE3) (overlay (make-grid-border GAMESTATE3)
                                             (grid->image (make-grid 5 5 1 '()) 100 25 (make-gooflow (cons (make-pipe-cord PIPE-SL 3 1 "LEFT") '()) "LEFT"))))

(define (draw-grid gs)
  (overlay (make-grid-border gs)
           (grid->image (gamestate-grid gs)
                        (gamestate-tile-side-length gs)
                        (gamestate-pipe-width gs)
                        (gamestate-gooflow gs))))

;; calculate-game-width : GameState -> Integer
;; Calculates width of the game background in pixels considering
;; the dimensions of the grid.
(check-expect (calculate-game-width GAMESTATE1) 840)
(check-expect (calculate-game-width GAMESTATE2) 440)
(check-expect (calculate-game-width GAMESTATE3) 640)

(define (calculate-game-width gs)
  (+ (calculate-grid-width gs) (gamestate-tile-side-length gs) 40))

;; calculate-game-height : GameState -> Integer
;; Calculates the height of the game background in pixels considering
;; the dimensions of the grid.
(check-expect (calculate-game-height GAMESTATE1) 728)
(check-expect (calculate-game-height GAMESTATE2) 528)
(check-expect (calculate-game-height GAMESTATE3) 528)

(define (calculate-game-height gs)
  (if (<= (calculate-grid-height gs) 500)
      528
      (+ (calculate-grid-height gs) 28)))

;; draw-game-bg : GameState -> Image
;; Draws a game background with a border.
(check-expect (draw-game-bg GAMESTATE1)(overlay (rectangle 840 728 "outline" "black")
                                                (rectangle 842 730 "outline" "black")
                                                (rectangle 844 732 "outline" "white")
                                                (rectangle 846 734 "outline" "black")
                                                (rectangle 848 736 "outline" "black")))
(check-expect (draw-game-bg GAMESTATE2)(overlay (rectangle 440 528 "outline" "black")
                                                (rectangle 442 530 "outline" "black")
                                                (rectangle 444 532 "outline" "white")
                                                (rectangle 446 534 "outline" "black")
                                                (rectangle 448 536 "outline" "black")))
(check-expect (draw-game-bg GAMESTATE3)(overlay (rectangle 640 528 "outline" "black")
                                                (rectangle 642 530 "outline" "black")
                                                (rectangle 644 532 "outline" "white")
                                                (rectangle 646 534 "outline" "black")
                                                (rectangle 648 536 "outline" "black")))

(define (draw-game-bg gs)
  (overlay (rectangle (calculate-game-width gs) (calculate-game-height gs) "outline" "black")
           (rectangle (+ 2 (calculate-game-width gs)) (+ 2 (calculate-game-height gs)) "outline" "black")
           (rectangle (+ 4 (calculate-game-width gs)) (+ 4 (calculate-game-height gs)) "outline" "white")
           (rectangle (+ 6 (calculate-game-width gs)) (+ 6 (calculate-game-height gs)) "outline" "black")
           (rectangle (+ 8 (calculate-game-width gs)) (+ 8 (calculate-game-height gs)) "outline" "black")))

;; draw-goo-button : GameState ->Image
;; Draws a goo button with a border.
(check-expect (draw-goo-button GAMESTATE1)(overlay
                                           (text "start goo" 20 "black")
                                           (rectangle 100 30 "solid" "lime")
                                           (rectangle 101 31 "outline" "black")
                                           (rectangle 102 32 "outline" "black")))
(check-expect (draw-goo-button GAMESTATE2)(overlay
                                           (text "start goo" 20 "black")
                                           (rectangle 100 30 "solid" "lime")
                                           (rectangle 101 31 "outline" "black")
                                           (rectangle 102 32 "outline" "black")))
(check-expect (draw-goo-button GAMESTATE3)(overlay
                                           (text "start goo" 20 "black")
                                           (rectangle 100 30 "solid" "lime")
                                           (rectangle 101 31 "outline" "black")
                                           (rectangle 102 32 "outline" "black")))

(define (draw-goo-button gs)
  (overlay
   (text "start goo" 20 "black")
   (rectangle (gamestate-tile-side-length gs) 30 "solid" "lime")
   (rectangle (add1 (gamestate-tile-side-length gs)) 31 "outline" "black")
   (rectangle (+ 2 (gamestate-tile-side-length gs)) 32 "outline" "black")))

;; get-score: GameState -> Integer
;; Calculate the current score of the game given a gamestate.
(check-expect (get-score GAMESTATE1) 0)
(check-expect (get-score GAMESTATE2) 0)
(check-expect (get-score GAMESTATE3) 0)

(define (get-score gs)
  (local [;; length-path : Gamestate -> Integer
          ;; Calculates the length of the path of pipes with goo
          ;; given a gamestate.
          (define (length-path gs)
            (sub1 (length (gooflow-path (gamestate-gooflow gs)))))]
   (* 50 (- (length-path gs) (gamestate-num-replaced gs)))))

;; draw-score : GameState ->Image
;; Draws the current score of the game with a border.
(check-expect (draw-score GAMESTATE1)(overlay
                                      (text "Score: 0" 20 "black")
                                      (rectangle 110 35 "solid" "orange red")
                                      (rectangle 111 36 "outline" "black")
                                      (rectangle 112 37 "outline" "black")))
(check-expect (draw-score GAMESTATE2)(overlay
                                      (text "Score: 0" 20 "black")
                                      (rectangle 110 35 "solid" "orange red")
                                      (rectangle 111 36 "outline" "black")
                                      (rectangle 112 37 "outline" "black")))
(check-expect (draw-score GAMESTATE3)(overlay
                                      (text "Score: 0" 20 "black")
                                      (rectangle 110 35 "solid" "orange red")
                                      (rectangle 111 36 "outline" "black")
                                      (rectangle 112 37 "outline" "black"))) 

(define (draw-score gs)
  (overlay
   (text (string-append "Score: " (number->string (get-score gs))) 20 "black")
   (rectangle (+ 10 (gamestate-tile-side-length gs)) 35 "solid" "orange red")
   (rectangle (+ 11 (gamestate-tile-side-length gs)) 36 "outline" "black")
   (rectangle (+ 12 (gamestate-tile-side-length gs)) 37 "outline" "black")))

;; draw-game : GameState -> Image
;; Draws the gameboard with the incoming pipes and grid.
(check-expect (draw-game GAMESTATE1)
              (place-images (list (draw-grid (make-gamestate (make-grid 7 7 1 (cons (make-pipe-cord PIPE-ST 5 5 "UP") '()))
                                                             START-PIPES-L1 100 25 (make-pipe-cord PIPE-ST 5 5 "UP") (make-gooflow (cons (make-pipe-cord PIPE-ST 5 5 "UP") '()) "UP") 0 140))
                                  ;(draw-goo-button GAMESTATE1)
                                  (draw-pipe-list GAMESTATE1)
                                  (draw-score GAMESTATE1))
                            (list (make-posn 484.2 367.64)
                                  ;(make-posn 65 58.24)
                                  (make-posn 65 305.76)
                                  (make-posn 65 655.2))
                            (draw-game-bg GAMESTATE1)))
(check-expect (draw-game GAMESTATE2)
              (place-images (list (draw-grid GAMESTATE2)
                                  ;(draw-goo-button GAMESTATE2)
                                  (draw-pipe-list GAMESTATE2)
                                  (draw-score GAMESTATE2))
                            (list (make-posn 282.2 266.64)
                                  ;(make-posn 65 42.24)
                                  (make-posn 65 221.76)
                                  (make-posn 65 475.2))
                            (draw-game-bg GAMESTATE2)))
(check-expect (draw-game GAMESTATE3)
              (place-images (list (draw-grid (make-gamestate (make-grid 5 5 1 (cons (make-pipe-cord PIPE-SL 3 1 "LEFT") (grid-pipe-list (gamestate-grid GAMESTATE3))))
                                                             START-PIPES-L3 100 25 (make-pipe-cord PIPE-SL 3 1 "LEFT") (make-gooflow (cons (make-pipe-cord PIPE-SL 3 1 "LEFT") '()) "LEFT") 0 140))
                                  ;(draw-goo-button GAMESTATE3)
                                  (draw-pipe-list GAMESTATE3)
                                  (draw-score GAMESTATE3))
                            (list (make-posn 383.2 266.64)
                                  ;(make-posn 65 42.24)
                                  (make-posn 65 221.76)
                                  (make-posn 65 475.2))
                            (draw-game-bg GAMESTATE3)))

(define (draw-game gs)
  (local [(define GRID (gamestate-grid gs))
          ;; add-starting-pipe : GameState -> GameState
          ;; Adds the starting pipe to the pipe-list of the grid
          (define (add-starting-pipe gs)
            (make-gamestate (make-grid (grid-width GRID)
                                       (grid-height GRID)
                                       (grid-x-counter GRID)
                                       (cons (gamestate-starting-pipe gs) (grid-pipe-list GRID)))
                            (gamestate-incoming-pipes gs)
                            (gamestate-tile-side-length gs)
                            (gamestate-pipe-width gs)
                            (gamestate-starting-pipe gs) 
                            (gamestate-gooflow gs)
                            (gamestate-num-replaced gs)
                            (gamestate-timer gs)))]
  (place-images (list (draw-grid (add-starting-pipe gs))
                      ;(draw-goo-button gs)
                      (draw-pipe-list gs)
                      (draw-score gs))
                (list (make-posn (+ 10 (* (calculate-game-width gs) 0.505) (/ (gamestate-tile-side-length gs) 2)) (* (calculate-game-height gs) 0.505))
                      ;(make-posn (+ 15 (/ (gamestate-tile-side-length gs) 2)) (* (calculate-game-height gs) 0.08))
                      (make-posn (+ 15 (/ (gamestate-tile-side-length gs) 2)) (* (calculate-game-height gs) 0.42))
                      (make-posn (+ 15 (/ (gamestate-tile-side-length gs) 2)) (* (calculate-game-height gs) 0.9)))
                (draw-game-bg gs))))

;; place-pipe-or-goo-propagate-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(define (place-pipe-or-goo-propagate-on-click gs x y event)
  (local [;; mouse-x-cord->cord : Integer Integer -> Integer
          ;; Converts a given x mouse coordinate to an x coordinate on
          ;; the grid using given tile dimensions.
          (define (mouse-x-cord->cord x-cord tile-side-length)
            (ceiling (/ (- x-cord tile-side-length 32) tile-side-length)))

          ;; y-cord-checker : Integer
          ;; Checks if height of the gameboard is greater than 500. 
          (define (y-cord-checker gs y)
            (if (<= (calculate-grid-height gs) 500)
                (- y (/ (- 500 (calculate-grid-height gs)) 2) 19)
                (- y 20)))

          ;; mouse-y-cord->cord : Integer Integer Integer -> Integer
          ;; Converts a given y mouse coordinate to an y coordinate on
          ;; the grid using given tile dimensions.
          (define (mouse-y-cord->cord y-cord tile-side-length grid-height)
            (+ (- grid-height (ceiling (/ (y-cord-checker gs y) tile-side-length))) 1))

          ;; calculate-num-replaced : Gamestate -> Integer
          ;; Calculates the number of pipes replaced.
          (define (calculate-num-replaced gs)
            (if (pipe-cord? (pipe-at (gamestate-grid gs)
                                     (mouse-x-cord->cord x (gamestate-tile-side-length gs))
                                     (mouse-y-cord->cord y (gamestate-tile-side-length gs) (grid-height (gamestate-grid gs)))))
                (add1 (gamestate-num-replaced gs))
                (gamestate-num-replaced gs)))

          ;; pipe-with-goo? : Gamestate -> Boolean
          ;; Returns true if the pipe that the user wants to replace has goo in it. 
          (define (pipe-with-goo? x y gs)
            (andmap (lambda (p) (and (= (pipe-cord-x p) (mouse-x-cord->cord x (gamestate-tile-side-length gs)))
                                     (= (pipe-cord-y p) (mouse-y-cord->cord y (gamestate-tile-side-length gs) (grid-height (gamestate-grid gs))))))
                    (gooflow-path (gamestate-gooflow gs))))]
    
    (cond #;[(empty? (gamestate-incoming-pipes gs))
           (if (and (and (and (> x 15)
                              (< x (+ 15 (gamestate-tile-side-length gs))))
                         (and (> y (* (calculate-game-height gs) 0.06)) (< y (+ 30 (* (calculate-game-height gs) 0.06)))))
                    (mouse=? event "button-down")) 
               (make-gamestate (gamestate-grid gs)
                               (gamestate-incoming-pipes gs)
                               (gamestate-tile-side-length gs)
                               (gamestate-pipe-width gs)
                               (gamestate-starting-pipe gs)
                               (grid-goo-propagate (gamestate-gooflow gs) (gamestate-grid gs))
                               (gamestate-num-replaced gs)
                               (gamestate-timer gs))
               gs)] 
          [(cons? (gamestate-incoming-pipes gs))
           (if (and (mouse=? event "button-down") (not (pipe-with-goo? x y gs)))
               (make-gamestate (place-pipe (gamestate-grid gs)
                                           (first (gamestate-incoming-pipes gs))
                                           (mouse-x-cord->cord x (gamestate-tile-side-length gs))
                                           (mouse-y-cord->cord y (gamestate-tile-side-length gs) (grid-height (gamestate-grid gs)))
                                           "")
                               (rest (gamestate-incoming-pipes gs))
                               (gamestate-tile-side-length gs)
                               (gamestate-pipe-width gs)
                               (gamestate-starting-pipe gs)
                               (gamestate-gooflow gs)
                               (calculate-num-replaced gs)
                               (gamestate-timer gs)) 
               gs)])))

;; tick-goo : GameState -> GameState
;; If there is time left before the next goo propagation, then counts down the time by 1 tick.
;; If the countdown timer reaches 0, then propagates the goo by one cell and resets the countdown timer to 28 ticks (1 second).
;; If the goo cannot propagate anymore, then returns the same GameState.
(check-expect (tick-goo GAMESTATE1)
              (make-gamestate STARTING-GRID START-PIPES-L1 100 25 (make-pipe-cord PIPE-ST 5 5 "UP") (make-gooflow (cons (make-pipe-cord PIPE-ST 5 5 "UP") '()) "UP") 0 139))
(check-expect (tick-goo (make-gamestate GRID2 START-PIPES-L1 100 25 PIPE-CORD9 GF2 0 0))
              (make-gamestate GRID2 START-PIPES-L1 100 25 PIPE-CORD9 (make-gooflow (list PIPE-CORD3 PIPE-CORD7 PIPE-CORD9) "LEFT") 0 28))
(check-expect (tick-goo (make-gamestate GRID1 START-PIPES-L1 100 25 PIPE-CORD8 GF1 0 0))
              (make-gamestate GRID1 START-PIPES-L1 100 25 PIPE-CORD8 GF1 0 0))
                                        
(define (tick-goo gs)
  (local [;; gooflow-equals? : Gooflow GooFlow -> Boolean
          ;; Returns true if the given gooflows are equal.
          (define (gooflow-equals? gf1 gf2)
            (and (= (length (gooflow-path gf1)) (length (gooflow-path gf2)))
                 (string=? (gooflow-direction gf1) (gooflow-direction gf2))))]
  (cond [(> (gamestate-timer gs) 0)
         (make-gamestate (gamestate-grid gs)
                         (gamestate-incoming-pipes gs)
                         (gamestate-tile-side-length gs)
                         (gamestate-pipe-width gs)
                         (gamestate-starting-pipe gs)
                         (gamestate-gooflow gs)
                         (gamestate-num-replaced gs)
                         (sub1 (gamestate-timer gs)))]
        [(and (= (gamestate-timer gs) 0)
              (not (gooflow-equals? (grid-goo-propagate (gamestate-gooflow gs) (gamestate-grid gs)) (gamestate-gooflow gs))))
         (make-gamestate (gamestate-grid gs)
                         (gamestate-incoming-pipes gs)
                         (gamestate-tile-side-length gs)
                         (gamestate-pipe-width gs)
                         (gamestate-starting-pipe gs)
                         (grid-goo-propagate (gamestate-gooflow gs) (gamestate-grid gs))
                         (gamestate-num-replaced gs)
                         28)]
        [(and (= (gamestate-timer gs) 0)
              (gooflow-equals? (grid-goo-propagate (gamestate-gooflow gs) (gamestate-grid gs)) (gamestate-gooflow gs)))
         gs])))


;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-gs)
  (big-bang initial-gs
    [to-draw draw-game]
    [on-mouse place-pipe-or-goo-propagate-on-click]
    [on-tick tick-goo]
    ))

(define INCOMING-PIPES-EX (list PIPE-BR PIPE-BL PIPE-TBLR PIPE-LR PIPE-TR PIPE-TL PIPE-TB PIPE-LR PIPE-TR PIPE-TL PIPE-BR PIPE-BL PIPE-TBLR PIPE-LR PIPE-TR
                                PIPE-TL PIPE-TB PIPE-LR PIPE-TR PIPE-TL PIPE-BR PIPE-BL PIPE-TBLR PIPE-LR PIPE-TR PIPE-TL PIPE-TB PIPE-LR PIPE-TR PIPE-TL
                                PIPE-BR PIPE-BL PIPE-TBLR PIPE-LR PIPE-TR PIPE-TL PIPE-BR PIPE-BL PIPE-TBLR PIPE-LR PIPE-TR PIPE-TL PIPE-TB PIPE-LR PIPE-TR
                                PIPE-TL PIPE-BR PIPE-BL PIPE-TBLR PIPE-LR PIPE-TR PIPE-TL PIPE-TB PIPE-LR PIPE-TR PIPE-TL PIPE-BR PIPE-BL PIPE-TBLR PIPE-LR
                                PIPE-TR PIPE-TL PIPE-TB PIPE-LR PIPE-TR PIPE-TL PIPE-BR PIPE-BL PIPE-TBLR PIPE-LR PIPE-TR PIPE-TL))
(define GAMESTATE-EX1 (gamestate-init 7 7 3 2 "RIGHT" INCOMING-PIPES-EX))
;(pipe-fantasy GAMESTATE-EX1)

(define GAMESTATE-EX2 (gamestate-init 7 7 6 4 "LEFT" INCOMING-PIPES-EX))
;(pipe-fantasy GAMESTATE-EX2)

(define GAMESTATE-EX3 (make-gamestate (make-grid 6 6 1 (list (make-pipe-cord PIPE-BL 3 5 "") (make-pipe-cord PIPE-TBLR 3 4 "") (make-pipe-cord PIPE-TB 3 3 "")
                                                             (make-pipe-cord PIPE-TL 3 2 "") (make-pipe-cord PIPE-TR 2 2 "") (make-pipe-cord PIPE-TBLR 2 3 "")
                                                             (make-pipe-cord PIPE-BR 2 4 "") (make-pipe-cord PIPE-TBLR 3 4 "") (make-pipe-cord PIPE-LR 4 4 "") (make-pipe-cord PIPE-TL 5 4 "")))
                                      (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                                      100 25
                                      (make-pipe-cord PIPE-SR 2 5 "RIGHT")
                                      (make-gooflow (list (make-pipe-cord PIPE-SR 2 5 "RIGHT")) "RIGHT")
                                      0
                                      28))
;(pipe-fantasy GAMESTATE-EX3)