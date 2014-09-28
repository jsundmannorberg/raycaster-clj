(ns raycaster.core)

(import '(javax.swing JFrame JPanel))
;(import '(javax.imageio 
(import '(java.awt Dimension Graphics Color))
(import '(java.awt.event KeyListener KeyEvent))

(set! *warn-on-reflection* true)

(def map-grid-size 40)
(def map-square-color Color/GREEN)
(def map-player-color Color/RED)

(def panel-x-size 1024)
(def panel-y-size 768)

(def pixel-size 1)
(def square-size-2d 10)

(def angle-step 0.03)
(def player-step 10)

(def view-angle 0.45)

(def wall-height-factor 30)

(defn wall-height
  [distance]
  (* wall-height-factor (/ panel-y-size distance)))

(def test-map [[1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
               [1 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1]
               [1 0 1 0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 0 1]
               [1 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 1 0 1]
               [1 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1]
               [1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1]
               [1 0 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1]
               [1 1 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1]
               [1 0 0 0 1 1 1 1 1 1 0 0 1 0 0 0 0 0 0 1]
               [1 0 1 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1]
               [1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1]
               [1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1]
               [1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1]
               [1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1]
               [1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1]
               [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]])

(def test-game-data
  (atom {:game-map test-map,
         :player-x (* map-grid-size 5)
         :player-y (* map-grid-size 7)
         :player-direction 0;(/ Math/PI 2)
         :is-key-pressed false}))

(def game-data test-game-data)

;Raycasting functions

(def double-rem rem)

#_(defn double-rem 
  [x n]
  (- x (* (quot (int x) n) n)))

(defn map-value
  [game-map point]
  (let [[x y] point
        map-x (quot x map-grid-size)
        map-y (quot y map-grid-size)]
    ((game-map map-y) map-x)))
        
(defn has-adjacent-wall?
  [game-map point]
  (let [[x y] (map int point)
        coordinates (if-not ((meta point) :crosses-x)
                      [[x y] [(dec x) y]] 
                      [[x y] [x (dec y)]])] 
    (some #(= 1 (map-value game-map %)) coordinates)))

(defn distance-to-axis
  [coordinate delta]
  "Most of the calculations in this function really only have to be performed once, if I add fn in some places and return a function"
  (let [remainder (double-rem coordinate map-grid-size)]
    (cond
      (> 0 delta) (if (zero? remainder)
                    (- map-grid-size)
                    (- remainder))
      (< 0 delta) (if (zero? remainder)
                    map-grid-size
                    (- map-grid-size remainder))                
      :else 0)))

(defn crosses-x-axis?
  [delta-x delta-y x-distance y-distance]
  (if (zero? delta-x) false
    (let [slope (/ delta-y delta-x)
          slope2 (/ y-distance x-distance)]
      (if (> 0 slope)
        (< slope slope2)
        (> slope slope2)))))

(defn next-point 
  [current-point delta-x delta-y] ;note to self: this kind of arguments can introduce errors...
  (let [[x y] current-point
        x-distance (distance-to-axis x delta-x)
        y-distance (distance-to-axis y delta-y)]
    (if (crosses-x-axis? delta-x delta-y x-distance y-distance)
      (with-meta [(+ x (* y-distance (/ delta-x delta-y))) (+ y y-distance)] {:crosses-x true})
      (with-meta [(+ x x-distance) (+ y (* x-distance (/ delta-y delta-x)))] {:crosses-x false}))))

(defn cast-ray
   [game-map start-point direction]
   "While there is no wall adjacent to the current position
    update the x and y coordinates, then return the result"
   (loop [current-point (with-meta start-point {:crosses-x false})]
     (if (has-adjacent-wall? game-map current-point)
       current-point
       (recur (next-point current-point (Math/cos direction) (Math/sin direction))))))

(defn wall-direction
  [point player-coordinates]
  "Requires metadata :crosses-x"
  (let [[x y] point
        [player-x player-y] player-coordinates]
    (if ((meta point) :crosses-x)
      (if (< y player-y)
        :wall-down
        :wall-up)
      (if (< x player-x)
        :wall-right
        :wall-left))))        

;;Control functions

(defn update-angle!
  [game func]
  (let [current-game-state @game
        angle (:player-direction current-game-state)
        new-angle (func angle angle-step)]
    (swap! game assoc :player-direction new-angle)))

(defn move-player!
  [game func]
  "To move forward use (move-player! game +), backward (move-player! game -)"
  (let [current-game-state @game
        angle (:player-direction current-game-state)
        player-x (:player-x current-game-state)
        player-y (:player-y current-game-state)
        new-x (func player-x (* (Math/cos angle) player-step))
        new-y (func player-y (* (Math/sin angle) player-step))]
    (prn new-x new-y)
    (swap! game assoc :player-x new-x :player-y new-y)))

;;Drawing

(defn clear-screen
  [^Graphics graphics]
  (doto graphics
    (.setColor Color/BLACK)
    (.fillRect 0 0 panel-x-size panel-y-size)))

(defn map-2d-coordinate
  [coordinate]
  (/ (* square-size-2d coordinate) map-grid-size))

(defn draw-rectangle
  [^Graphics graphics x y width height color]
  (doto graphics
    (.setColor color)
    (.fillRect x y width height))) 

(defn draw-single-pixel
  [graphics x y color]
  (draw-rectangle graphics x y pixel-size pixel-size color))

(defn draw-map-square
  [graphics x y]
  (draw-rectangle graphics 
                  (* x square-size-2d) 
                  (* y square-size-2d) 
                  square-size-2d 
                  square-size-2d 
                  map-square-color))

(defn draw-map
  [graphics m]
  (doseq [y (range (count m))]
    (doseq [x (range (count (m y)))]
      (when (not (zero? ((m y) x)))
        (draw-map-square graphics x y)))))

(defn draw-player
  [graphics player-x player-y]
  (draw-single-pixel graphics 
                     (map-2d-coordinate player-x) 
                     (map-2d-coordinate player-y) 
                     map-player-color))

(defn draw-ray
  [^Graphics graphics game-map player-x player-y direction]
  (let [point (cast-ray game-map [player-x player-y] direction)
        x (first point)
        y (last point)]
    (.drawLine graphics (map-2d-coordinate player-x) (map-2d-coordinate player-y) (map-2d-coordinate x) (map-2d-coordinate y))))

(defn draw-rays
  [graphics game-map player-x player-y direction]
  (doseq [angle (range -10 11)]
    (draw-ray graphics game-map player-x player-y (+ direction (* angle 0.045)))))

(defn shaded-color-value
  [distance]
  (let [c-val (int (/ (* map-grid-size 200) distance))
        c ^Integer (if (<= c-val 255) c-val 255)]
    (Color. c c c)))

(defn draw-shaded-line
  [^Graphics graphics x distance height]
  (when (> distance 0) ;Distance should never be 0, we don't like division by 0
    (let [midpoint (/ panel-y-size 2)]
      (doto graphics
        (.setColor (shaded-color-value distance))
        (.drawLine x (- midpoint height) x (+ midpoint height))))))

(defn raycaster-angle 
  [direction n]
  (+ direction (/ (* 2 n view-angle) panel-x-size))) ;;(+ direction (Math/asin (* 2 n (/ (Math/sin view-angle) panel-x-size)))))

(defn point-distance
  [x1 y1 x2 y2]
  (let [x-diff (- x2 x1)
        y-diff (- y2 y1)]
    (Math/sqrt (+ (* x-diff x-diff) (* y-diff y-diff)))))

(defn fishbowl-corrected-distance
  [x1 y1 x2 y2 player-direction ray-direction]
  (* (point-distance x1 y1 x2 y2) (Math/cos (- player-direction ray-direction))))

(defn draw-shaded-3d-view
  [graphics game-map player-x player-y direction]
  (doseq [x (range panel-x-size)]
    (let [ray-direction (raycaster-angle direction (- x (/ panel-x-size 2)))
          [ray-x ray-y] (cast-ray game-map [player-x player-y] ray-direction)
          ray-distance (fishbowl-corrected-distance player-x player-y ray-x ray-y direction ray-direction)]
      (draw-shaded-line graphics x ray-distance (wall-height ray-distance)))))

(defn draw-game
  [graphics game]
  (let [game-map (:game-map @game)
        player-x (:player-x @game)
        player-y (:player-y @game)
        player-direction (:player-direction @game)]
    (draw-shaded-3d-view graphics game-map player-x player-y player-direction)
    (draw-map graphics game-map)
    (draw-player graphics player-x player-y)
    (draw-rays graphics game-map player-x player-y player-direction)))

;GUI

(defn make-panel
  [game]
  (proxy [JPanel KeyListener] []
    (paintComponent 
      [^Graphics g] 
      (doto g
        (clear-screen) ;This is used instead of a call to proxy-super paintComponent to avoid reflection
        (draw-game game)))
    (keyPressed 
      [^KeyEvent e]
      (do
        (cond
          (= (.getKeyCode e) 39 ) (update-angle! game-data +)
          (= (.getKeyCode e) 37 ) (update-angle! game-data -)
          (= (.getKeyCode e) 40 ) (move-player! game-data -)
          (= (.getKeyCode e) 38 ) (move-player! game-data +))
        (.repaint ^Graphics this)
        (println "A key has been pressed, keyCode == " (.getKeyCode e))))
    (keyReleased [e])
    (keyTyped [e]))) 

(defn setup-gui
  [frame panel]
  (doto panel
    (.setPreferredSize (Dimension. panel-x-size panel-y-size))
    (.setDoubleBuffered true)
    (.setFocusable true)
    (.addKeyListener panel))
  (doto frame
    ;Exit on close will shut down the REPL. Add it later. 
    ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.add panel)
    (.pack)
    (.setVisible true)))
    
(defn start-game
  [^String frame-title]
  (let [game game-data
        frame (JFrame. frame-title)
        panel (make-panel game)]
    (setup-gui frame panel)))

(start-game "Raycaster")

