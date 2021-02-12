(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [unicode-math.core :refer :all]))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14) ;; Was 14.1, then 14.25
(def keyswitch-width 14)
(def keyborder (- 17.4 keyswitch-width)) ; 17.4 is a safe value even us SA keycaps

(def sa-profile-key-height 12.7)

(def plate-thickness 5)
(def mount-width (+ keyswitch-width keyborder))
(def mount-height (+ keyswitch-height keyborder))

(def old-single-plate
  (->> 
    (difference
      (->>
        (cube (+ keyswitch-width keyborder) (+ keyswitch-width keyborder) plate-thickness)
        (translate [ 0 0 (/ plate-thickness 2) ])
      )
      (->>
        (cube keyswitch-width keyswitch-width plate-thickness)
        (translate [ 0 0 (/ plate-thickness 2) ])
      )
      (->>
        (cube (- (+ keyswitch-width keyborder) 2) (- (+ keyswitch-width keyborder) 2) (- plate-thickness 1.5))
        (translate [ 0 0 (/ (- plate-thickness 1.5) 2) ])
      )
    )
  )
)

(def alps-width 15.6)
(def alps-notch-width 15.5)
(def alps-notch-height 1)
(def alps-height 13)

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 2.2 plate-thickness)
                      (translate [0
                                  (+ (/ 2.2 2) (/ alps-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (union (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                              (translate [(+ (/ 1.5 2) (/ 15.6 2))
                                          0
                                          (/ plate-thickness 2)]))
                         (->> (cube 1.5 (+ keyswitch-height 3) 1.0)
                              (translate [(+ (/ 1.5 2) (/ alps-notch-width 2))
                                          0
                                          (- plate-thickness
                                             (/ alps-notch-height 2))]))
                         )
        plate-half (union top-wall left-wall)]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))


;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 (/ sa-double-length 2)
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 28 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range -1 6))
(def rows (range 0 5))

(def α (/ π 12))
(def β (/ π 36))
(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height 1/2) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width 2.0) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))

(defn key-place [column row shape]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* α (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-offset (cond
                        (= column -1) [0 -3 2.8]
                        (= column 2) [0 2.82 -3.0] ;;was moved -4.5
                        (>= column 4) [0 -13 6.6]
                        :else [0 0 0])
        column-angle (* β (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate column-offset))]
    (->> placed-shape
         (rotate (/ π 12) [0 1 0])
         (translate [0 0 13])
         (rotate (* 0.0528 π) [1 0 0])
         (translate [0 0 (- 0.5)])
         )))

(defn case-place [column row shape]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* α (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-offset [0 -4.35 5.64]
        column-angle (* β (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate column-offset))]
    (->> placed-shape
         (rotate (/ π 12) [0 1 0])
         (translate [0 0 13])
         (rotate (* 0.0528 π) [1 0 0])
         (translate [0 0 (- 0.5)])
         )))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (and (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (= row 4))) (not (and (>= column 4) (= row 4)))))
               ]
           (->> old-single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (= row 4))))
               ]
           (->> (sa-cap (if (= column 5) 1 1))
                (key-place column row)))))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 3.5)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (drop-last columns)
                row rows
               :when (and (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (= row 4))) (not (and (>= column 3) (>= row 4)))))
                ]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (drop-last rows)
                :when (and (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (= row 4))) (not (and (>= column 4) (>= row 3)))))
                ]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (drop-last columns)
                row (drop-last rows)
                :when (and (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (= row 4))) (not (and (>= column 3) (>= row 3)))))
                ]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl)))
           )))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(defn thumb-place [column row shape]
  (let [cap-top-height (+ plate-thickness sa-profile-key-height)
        α (/ π 12)
        row-radius (+ (/ (/ (+ mount-height 1) 2)
                         (Math/sin (/ α 2)))
                      cap-top-height)
        β (/ π 36)
        column-radius (+ (/ (/ (+ mount-width 2) 2)
                            (Math/sin (/ β 2)))
                         cap-top-height)
        #_(+ (/ (/ (+ pillar-width 5) 2)
                            (Math/sin (/ β 2)))
                         cap-top-height)]
    (->> shape
         (translate [0 0 (- row-radius)])
         (rotate (* α row) [1 0 0])
         (translate [0 0 row-radius])
         (translate [0 0 (- column-radius)])
         (rotate (* column β) [0 1 0])
         (translate [0 0 column-radius])
         (translate [mount-width 0 0])
         (rotate (* π (- 1/4 3/16)) [0 0 1])
         (rotate (/ π 12) [1 1 0])
         (translate [-52 -45 40])
         (rotate (* 0.0528 π) [1 0 0])
         (translate [0 0 (- 0.5)])
    )))

(defn thumb-2x-column [shape]
  (thumb-place 0 -1/2 shape))

(defn thumb-2x+1-column [shape]
  (union (thumb-place 1 -1/2 
           (
             translate [ 0 (* mount-height 0.375) 0 ] shape
           )
         )
         (thumb-place 1 1 shape)))

(defn thumb-2x-1-column [shape]
  (union (thumb-place 2 -1/2 
           (
             translate [ 0 (* mount-height -0.375) 0 ] shape
           )
         )
         (thumb-place 2 1 shape)))

(defn thumb-2x+1-bottom-column [shape]
  (thumb-place 1 -1/2 shape)
  )

(defn thumb-1x-column [shape]
  (union (thumb-place 2 -1 shape)
         (thumb-place 2 0 shape)
         (thumb-place 2 1 shape)))

(defn thumb-layout [shape]
  (union
   (thumb-2x-column shape)
   (thumb-2x+1-column shape)
   (thumb-2x-1-column shape)))

(defn thumb-bottom-layout [shape]
  (union
   (thumb-2x-column shape)
   (thumb-2x+1-bottom-column shape)
   (thumb-1x-column shape)))

(def double-plates
  (let [plate-height (/ (- sa-double-length mount-height) 2)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))
        top-plate (difference top-plate)]
    (union top-plate (mirror [0 1 0] top-plate))))

(def double-plates-upper
  (
    let [
      plate-height (/ (- sa-double-length mount-height) 2)
    ]
    (->> 
      (
        difference
        (cube mount-width (+ (* mount-height 2) 3) web-thickness)
        (->>
          (cube mount-width mount-height (+ web-thickness 1))
          (translate [ 0 (* mount-height 0.375) 0 ])
        )
      )
      (translate [ 0 0 (- plate-thickness (/ web-thickness 2)) ])
    )
  )
)

(def double-plates-lower
  (
    let [
      plate-height (/ (- sa-double-length mount-height) 2)
    ]
    (->> 
      (
        difference
        (cube mount-width (+ (* mount-height 2) 3) web-thickness)
        (->>
          (cube mount-width mount-height (+ web-thickness 1))
          (translate [ 0 (* mount-height -0.375) 0 ])
        )
      )
      (translate [ 0 0 (- plate-thickness (/ web-thickness 2)) ])
    )
  )
)

(def thumbcaps
  (union
   (thumb-2x-column (sa-cap 2))
   (thumb-place 1 -1/2 (sa-cap 2))
   (thumb-place 1 1 (sa-cap 1))
   (thumb-1x-column (sa-cap 1))))

(def thumb-connectors
  (union
   (apply union
          (concat
           (for [column [2] row [1]]
             (triangle-hulls (thumb-place column row web-post-br)
                             (thumb-place column row web-post-tr)
                             (thumb-place (dec column) row web-post-bl)
                             (thumb-place (dec column) row web-post-tl)))
           ))
   (let [plate-height (/ (- sa-double-length mount-height) 2)
         thumb-tl (->> web-post-tl
                       (translate [0 plate-height 0]))
         thumb-bl (->> web-post-bl
                       (translate [0 (- plate-height) 0]))
         thumb-tr (->> web-post-tr
                       (translate [0 plate-height 0]))
         thumb-br (->> web-post-br
                       (translate [0 (- plate-height) 0]))]
     (union

      ;;Connecting the two doubles
      (triangle-hulls (thumb-place 0 -1/2 thumb-tl)
                      (thumb-place 0 -1/2 thumb-bl)
                      (thumb-place 1 -1/2 thumb-tr)
                      (thumb-place 1 -1/2 thumb-br))

      (triangle-hulls (thumb-place 1 -1/2 thumb-tl)
                      (thumb-place 1 -1/2 thumb-bl)
                      (thumb-place 2 -1/2 thumb-tr)
                      (thumb-place 2 -1/2 thumb-br))

      ;;Connecting the double to the one above it
      (triangle-hulls (thumb-place 1 -1/2 thumb-tr)
                      (thumb-place 1 -1/2 thumb-tl)
                      (thumb-place 1 1 web-post-br)
                      (thumb-place 1 1 web-post-bl))
      (triangle-hulls (thumb-place 2 -1/2 thumb-tr)
                      (thumb-place 2 -1/2 thumb-tl)
                      (thumb-place 2 1 web-post-br)
                      (thumb-place 2 1 web-post-bl))

      ;;Connecting the 4 with the double in the bottom left
      (triangle-hulls (thumb-place 1 1 web-post-bl)
                      (thumb-place 1 -1/2 thumb-tl)
                      (thumb-place 2 1 web-post-br)
                      (thumb-place 2 -1/2 thumb-tr))

      ;;Connecting the thumb to everything
      (triangle-hulls (thumb-place 0 -1/2 thumb-br)
                      (key-place 1 4 web-post-bl)
                      (thumb-place 0 -1/2 thumb-tr)
                      (key-place 1 4 web-post-tl)
                      (key-place 1 3 web-post-bl)
                      (thumb-place 0 -1/2 thumb-tr)
                      (key-place 0 3 web-post-br)
                      (key-place 0 3 web-post-bl)
                      (thumb-place 0 -1/2 thumb-tr)
                      (thumb-place 0 -1/2 thumb-tl)
                      (key-place 0 3 web-post-bl)
                      (thumb-place 1 -1/2 thumb-tr)
                      (thumb-place 1 1 web-post-br)
                      (key-place 0 3 web-post-bl)
                      (key-place -1 2 web-post-br)
                      (thumb-place 1 1 web-post-br)
                      (thumb-place 1 1 web-post-tr))
      (hull (key-place 0 3 web-post-tl)
            (key-place -1 2 web-post-br)
            (key-place 0 3 web-post-bl)
            )
      (hull (thumb-place 0 -1/2 web-post-tr)
            (thumb-place 0 -1/2 thumb-tr)
            (key-place 1 4 web-post-bl)
            (key-place 1 4 web-post-tl))))))

(def thumb
  (union
   thumb-connectors
   (thumb-layout (rotate (/ π 2) [0 0 1] old-single-plate))
   (thumb-place 0 -1/2 double-plates)
   (thumb-place 1 -1/2 double-plates-upper)
   (thumb-place 2 -1/2 double-plates-lower)
   ))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

;; In column units
(def right-wall-column (+ (last columns) 0.55))
(def left-wall-column (- (first columns) 1/2))
(def thumb-back-y 0.93)
(def thumb-right-wall (- -1/2 0.05))
(def thumb-front-row (+ -1 0.07))
(def thumb-left-wall-column (+ 5/2 0.05))
(def back-y 0.02)

(defn range-inclusive [start end step]
  (concat (range start end step) [end]))

(def wall-step 0.2)
(def wall-sphere-n 20) ;;Sphere resolution, lower for faster renders

(defn wall-sphere-at [x y z size]
  (->> (sphere size)
       (translate [x y z])
       (with-fn wall-sphere-n)))

(defn scale-to-range [start end x]
  (+ start (* (- end start) x)))

(defn wall-sphere-bottom [front-to-back-scale size]
  (wall-sphere-at 0
                   (scale-to-range
                    (+ (/ mount-height -2) -3.5)
                    (+ (/ mount-height 2) 5.0)
                    front-to-back-scale)
                   -6
                   size))

(defn wall-sphere-top [front-to-back-scale size]
  (wall-sphere-at 0
                   (scale-to-range
                    (+ (/ mount-height -2) -3.5)
                    (+ (/ mount-height 2) 3.5)
                    front-to-back-scale)
                   10
                   size))

(def top-sphere-size 1)
(def wall-sphere-top-back (wall-sphere-top 1 1))
(def wall-sphere-top-back-top (translate [0 0 2] (wall-sphere-top 1 top-sphere-size)))
(def wall-sphere-bottom-back (wall-sphere-bottom 1 1))
(def wall-sphere-bottom-back-top (translate [0 0 2] (wall-sphere-bottom 1 top-sphere-size)))
(def wall-sphere-bottom-front (wall-sphere-bottom 0 1))
(def wall-sphere-bottom-front-top (translate [0 0 2] (wall-sphere-bottom 0 top-sphere-size)))
(def wall-sphere-top-front (wall-sphere-top 0 1))
(def wall-sphere-top-front-top (translate [0 0 2] (wall-sphere-top 0 top-sphere-size)))

(defn top-case-cover [place-fn sphere
                 x-start x-end
                 y-start y-end
                 step]
  (apply union
         (for [x (range-inclusive x-start (- x-end step) step)
               y (range-inclusive y-start (- y-end step) step)]
           (hull (place-fn x y sphere)
                 (place-fn (+ x step) y sphere)
                 (place-fn x (+ y step) sphere)
                 (place-fn (+ x step) (+ y step) sphere)))))

(def front-wall
  (let [step wall-step ;;0.1
        wall-step 0.05 ;;0.05
        place case-place
        top-cover (fn [x-start x-end y-start y-end]
                    (top-case-cover place wall-sphere-top-front
                                    x-start x-end y-start y-end
                                    wall-step))]
    (union
     (apply union
            (for [x (range 2 4)]
              (union
               (hull (place (- x 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
                     (place (+ x 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
                     (key-place x 4 web-post-bl)
                     (key-place x 4 web-post-br))
               (hull (place (- x 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
                     (key-place x 4 web-post-bl)
                     (key-place (- x 1) 4 web-post-br)))
              ))
     (hull (place (- 4 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
           (place (+ 4 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
           (key-place 4 3 web-post-bl)
           (key-place 4 3 web-post-br))
     (hull (key-place 4 3 web-post-bl)
           (key-place 3 4 web-post-tr)
           (key-place 3 3 web-post-br))
     (hull (key-place 4 3 web-post-bl)
           (key-place 3 4 web-post-tr)
           (key-place (- 4 1) 4 web-post-br))
     (hull (place (- 4 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
           (key-place 4 3 web-post-bl)
           (key-place (- 4 1) 4 web-post-br))
     (hull (place right-wall-column 4 (translate [-1.5 1.5 0.5] wall-sphere-bottom-front-top))
           (place (- right-wall-column 1) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
           (key-place 5 3 web-post-bl)
           (key-place 5 3 web-post-br))
     (hull (place (+ 4 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
           (place (- right-wall-column 1) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
           (key-place 4 3 web-post-br)
           (key-place 5 3 web-post-bl))
     (hull (place 0.7 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
           (place 1.5 4 (translate [0 1.5 0.5] wall-sphere-bottom-front-top))
           (key-place 1 4 web-post-bl)
           (key-place 1 4 web-post-br))
    )
  )
)

(def back-wall
  (let [step wall-step
        wall-sphere-top-backtep 0.05
        place case-place
        front-top-cover (fn [x-start x-end y-start y-end]
                          (apply union
                                 (for [x (range-inclusive x-start (- x-end wall-sphere-top-backtep) wall-sphere-top-backtep)
                                       y (range-inclusive y-start (- y-end wall-sphere-top-backtep) wall-sphere-top-backtep)]
                                   (hull (place x y wall-sphere-top-back)
                                         (place (+ x wall-sphere-top-backtep) y wall-sphere-top-back)
                                         (place x (+ y wall-sphere-top-backtep) wall-sphere-top-back)
                                         (place (+ x wall-sphere-top-backtep) (+ y wall-sphere-top-backtep) wall-sphere-top-back)))))]
    (union
     (hull (place left-wall-column 0 (translate [1 -1 0.5] wall-sphere-bottom-back-top))
           (place (+ left-wall-column 1) 0  (translate [0 -1 0.5] wall-sphere-bottom-back-top))
           (key-place -1 0 web-post-tl)
           (key-place -1 0 web-post-tr))

     (hull (place (- 5 1/2) 0 (translate [0 -1 0.5] wall-sphere-bottom-back-top))
           (place right-wall-column 0.02 (translate [-1.5 -1 3] (wall-sphere-bottom 1 1)))
           (key-place 5 0 web-post-tl)
           (key-place 5 0 web-post-tr))
     (hull (place (- 5 1/2) 0 (translate [0 -1 0.5] wall-sphere-bottom-back-top))
           (key-place 5 0 web-post-tl)
           (key-place (- 5 1) 0 web-post-tr))

     (apply union
            (for [x (range 0 5)]
              (union
               (hull (place (- x 1/2) 0 (translate [0 -1 0.5] wall-sphere-bottom-back-top))
                     (place (+ x 1/2) 0 (translate [0 -1 0.5] wall-sphere-bottom-back-top))
                     (key-place x 0 web-post-tl)
                     (key-place x 0 web-post-tr))
               (hull (place (- x 1/2) 0 (translate [0 -1 0.5] wall-sphere-bottom-back-top))
                     (key-place x 0 web-post-tl)
                     (key-place (- x 1) 0 web-post-tr)))))
     )))

(def right-wall
  (let [place case-place]
    (union
          (apply union
            (concat
             (for [x (range 0 4)]
               (union
                (hull (place right-wall-column x (translate [-1.5 0 3] (wall-sphere-bottom 1/2 top-sphere-size)))
                      (key-place 5 x web-post-br)
                      (key-place 5 x web-post-tr))))
             (for [x (range 0 3)]
               (union
                (hull (place right-wall-column x (translate [-1.5 0 3] (wall-sphere-bottom 1/2 top-sphere-size)))
                      (place right-wall-column (inc x) (translate [-1.5 0 3] (wall-sphere-bottom 1/2 top-sphere-size)))
                      (key-place 5 x web-post-br)
                      (key-place 5 (inc x) web-post-tr))))
             [(union
               (hull (place right-wall-column 0 (translate [-1.5 0 3] (wall-sphere-bottom 1/2 top-sphere-size)))
                     (place right-wall-column 0.02 (translate [-1.5 -1 3] (wall-sphere-bottom 1 1)))
                     (key-place 5 0 web-post-tr))
               (hull (place right-wall-column 4 (translate [-1.5 0 3] (wall-sphere-bottom 1/2 top-sphere-size)))
                     (place right-wall-column 3 (translate [-1.5 0 3] (wall-sphere-bottom 1/2 top-sphere-size)))
                     (key-place 5 3 web-post-br))
               (hull (place right-wall-column 4 (translate [-1.5 0 3] (wall-sphere-bottom 1/2 top-sphere-size)))
                     (place right-wall-column 4 (translate [-1.5 1.5 0.5] wall-sphere-bottom-front-top))
                     (key-place 5 3 web-post-br))
               )])))))

(def left-wall
  (let [place case-place]
    (union
     (hull (place left-wall-column 0 (translate [1 -1 0.5] wall-sphere-bottom-back-top))
           (place left-wall-column 1 (translate [1 0 0.5] wall-sphere-bottom-back-top))
           (key-place -1 0 web-post-tl)
           (key-place -1 0 web-post-bl))
     (hull (place left-wall-column 1 (translate [1 0 0.5] wall-sphere-bottom-back-top))
           (key-place -1 1 web-post-tl)
           (key-place -1 0 web-post-bl))
     (hull (place left-wall-column 1 (translate [1 0 0.5] wall-sphere-bottom-back-top))
           (place left-wall-column 2 (translate [1 0 0.5] wall-sphere-bottom-back-top))
           (key-place -1 1 web-post-tl)
           (key-place -1 1 web-post-bl))
     (hull (place left-wall-column 2 (translate [1 0 0.5] wall-sphere-bottom-back-top))
           (key-place -1 2 web-post-tl)
           (key-place -1 1 web-post-bl))
     (hull (place left-wall-column 2 (translate [1 0 0.5] wall-sphere-bottom-back-top))
           (place left-wall-column 3  (translate [0.9 3 1] wall-sphere-bottom-back-top))
           (key-place -1 2 web-post-tl)
           (key-place -1 2 web-post-bl))
     )))

(def thumb-back-wall
  (let [step wall-step
        top-step 0.05
        front-top-cover (fn [x-start x-end y-start y-end]
                          (apply union
                                 (for [x (range-inclusive x-start (- x-end top-step) top-step)
                                       y (range-inclusive y-start (- y-end top-step) top-step)]
                                   (hull (thumb-place x y wall-sphere-top-back)
                                         (thumb-place (+ x top-step) y wall-sphere-top-back-top)
                                         (thumb-place x (+ y top-step) wall-sphere-top-back-top)
                                         (thumb-place (+ x top-step) (+ y top-step) wall-sphere-top-back-top)))))
        back-y thumb-back-y]
    (union
     (hull (key-place -1 2 web-post-bl)
           (thumb-place 1 1 web-post-tr)
           (key-place -1 2 web-post-br))
     (hull
      (key-place -1 2 web-post-bl)
      (thumb-place 1 1 web-post-tr)
      (thumb-place 3/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back-top))
      (thumb-place 1 1 web-post-tl))
     (hull
      (case-place left-wall-column 3  (translate [0.9 3 1] wall-sphere-bottom-back-top))
      (key-place -1 2 web-post-bl)
      (thumb-place 3/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back-top))
      (thumb-place 1 1 web-post-tl))
     (hull
      (thumb-place (+ 5/2 0.05) thumb-back-y (translate [1 -1 1] wall-sphere-bottom-back-top))
      (thumb-place 3/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back-top))
      (thumb-place 1 1 web-post-tl)
      (thumb-place 2 1 web-post-tl)))))

(def thumb-left-wall
  (let [step wall-step
        place thumb-place]
    (union
     (comment
     (apply union
            (for [x (range-inclusive (+ -1 0.07) (- 1.95 step) step)]
              (hull (place thumb-left-wall-column x wall-sphere-top-front-top)
                    (place thumb-left-wall-column (+ x step) wall-sphere-top-front-top)
                    (place thumb-left-wall-column x wall-sphere-bottom-front-top)
                    (place thumb-left-wall-column (+ x step) wall-sphere-bottom-front-top))))
     (hull (place thumb-left-wall-column 1.95 wall-sphere-top-front-top)
           (place thumb-left-wall-column 1.95 wall-sphere-bottom-front-top)
           (place thumb-left-wall-column thumb-back-y wall-sphere-top-back-top)
      )
           (place thumb-left-wall-column thumb-back-y wall-sphere-bottom-back-top))

     (hull
      (thumb-place thumb-left-wall-column thumb-back-y (translate [1 -1 1] wall-sphere-bottom-back-top))
      (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back-top))
      (thumb-place 2 1 web-post-tl)
      (thumb-place 2 1 web-post-bl))
     (hull
      (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back-top))
      (thumb-place 2 0 web-post-tl)
      (thumb-place 2 1 web-post-bl))
     (hull
      (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back-top))
      (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back-top))
      (thumb-place 2 0 web-post-tl)
      (thumb-place 2 0 web-post-bl))
     (hull
      (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back-top))
      (thumb-place 2 -1 web-post-tl)
      (thumb-place 2 0 web-post-bl))
     (hull
      (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back-top))
      (thumb-place thumb-left-wall-column (+ -1 0.07) (translate [1 1 1] wall-sphere-bottom-front-top))
      (thumb-place 2 -1 web-post-tl)
      (thumb-place 2 -1 web-post-bl)))))

(def thumb-front-wall
  (let [step wall-step ;;0.1
        wall-sphere-top-fronttep 0.05 ;;0.05
        place thumb-place
        plate-height (/ (- sa-double-length mount-height) 2)
        thumb-tl (->> web-post-tl
                      (translate [0 plate-height 0]))
        thumb-bl (->> web-post-bl
                      (translate [0 (- plate-height) 0]))
        thumb-tr (->> web-post-tr
                      (translate [-0 plate-height 0]))
        thumb-br (->> web-post-br
                      (translate [-0 (- plate-height) 0]))]
    (union
     (hull (place thumb-right-wall thumb-front-row (translate [0 1 0.8] wall-sphere-bottom-front-top))
           (key-place 1 4 web-post-bl)
           (place 0 -1/2 thumb-br)
           (place 0 -1/2 web-post-br)
           (case-place 0.7 4 wall-sphere-bottom-front-top))

     (hull (place (+ 5/2 0.05) thumb-front-row (translate [1 1 1] wall-sphere-bottom-front-top))
           (place (+ 3/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front-top))
           (place 2 -1 web-post-bl)
           (place 2 -1 web-post-br))

     (hull (place thumb-right-wall thumb-front-row (translate [0 1 0.8] wall-sphere-bottom-front-top))
           (place (+ 1/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front-top))
           (place 0 -1/2 thumb-bl)
           (place 0 -1/2 thumb-br))
     (hull (place (+ 1/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front-top))
           (place (+ 3/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front-top))
           (place 0 -1/2 thumb-bl)
           (place 1 -1/2 thumb-bl)
           (place 1 -1/2 thumb-br)
           (place 2 -1 web-post-br)))))

(def new-case
  (union front-wall
         right-wall
         back-wall
         left-wall
         thumb-back-wall
         thumb-left-wall
         thumb-front-wall))

;;;;;;;;;;;;
;; Bottom ;;
;;;;;;;;;;;;


(def bottom-key-guard (->> (cube mount-width mount-height web-thickness)
                           (translate [0 0 (+ (- (/ web-thickness 2)) -3.7)])))
(def bottom-front-key-guard (->> (cube mount-width (/ mount-height 2) web-thickness)
                                 (translate [0 (/ mount-height 4) (+ (- (/ web-thickness 2)) -3.7)])))

(def bottom-plate
  (union
   (apply union
          (for [column columns
                row (drop-last rows) ;;
                :when (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (= row 4))))
                ]
            (->> bottom-key-guard
                 (key-place column row))))
   (thumb-bottom-layout (rotate (/ π 2) [0 0 1] bottom-key-guard))
   (apply union
          (for [column columns
                row [(last rows)] ;;
                :when (and (not (and (= column -1) (>= row 4))) (not (and (= column 0) (= row 4))))
                ]
            (->> bottom-front-key-guard
                 (key-place column row))))
   (let [shift #(translate [0 0 (+ (- web-thickness) -5.2)] %)
         web-post-tl (shift web-post-tl)
         web-post-tr (shift web-post-tr)
         web-post-br (shift web-post-br)
         web-post-bl (shift web-post-bl)
         half-shift-correction #(translate [0 (/ mount-height 2) 0] %)
         half-post-br (half-shift-correction web-post-br)
         half-post-bl (half-shift-correction web-post-bl)
         row-connections (concat
                          (for [column (drop-last columns)
                                row (drop-last rows)
                :when (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (= row 4))))
                                ]
                            (triangle-hulls
                             (key-place (inc column) row web-post-tl)
                             (key-place column row web-post-tr)
                             (key-place (inc column) row web-post-bl)
                             (key-place column row web-post-br)))
                          (for [column (drop-last columns)
                                row [(last rows)]
                                  :when (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (>= row 3))))]
                            (triangle-hulls
                             (key-place (inc column) row web-post-tl)
                             (key-place column row web-post-tr)
                             (key-place (inc column) row half-post-bl)
                             (key-place column row half-post-br))))
         column-connections (for [column columns
                                  row (drop-last rows)
                                  :when (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (= row 3))))]
                              (triangle-hulls
                               (key-place column row web-post-bl)
                               (key-place column row web-post-br)
                               (key-place column (inc row) web-post-tl)
                               (key-place column (inc row) web-post-tr)))
         diagonal-connections (for [column (drop-last columns)
                                    row (drop-last rows)
                                  :when (and (not (and (= column -1) (>= row 3))) (not (and (= column 0) (= row 3))))]
                                (triangle-hulls
                                 (key-place column row web-post-br)
                                 (key-place column (inc row) web-post-tr)
                                 (key-place (inc column) row web-post-bl)
                                 (key-place (inc column) (inc row) web-post-tl)))
         main-keys-bottom (concat row-connections
                                  column-connections
                                  diagonal-connections)
         front-wall (concat
                     (for [x (range 2 5)]
                       (union
                        (hull (case-place (- x 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front))
                              (case-place (+ x 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front))
                              (key-place x 4 half-post-bl)
                              (key-place x 4 half-post-br))
                        (hull (case-place (- x 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front))
                              (key-place x 4 half-post-bl)
                              (key-place (- x 1) 4 half-post-br))))
                     [
                      (hull 
                        (case-place right-wall-column 4 (translate [-1.5 2 0.6] wall-sphere-bottom-front))
                            (case-place (- right-wall-column 1) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front))
                            (key-place 5 4 half-post-bl)
                            (key-place 5 4 half-post-br))
                      (hull (case-place (+ 4 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front))
                            (case-place (- right-wall-column 1) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front))
                            (key-place 4 4 half-post-br)
                            (key-place 5 4 half-post-bl))])
         right-wall (concat
                     (for [x (range 0 4)]
                       (hull (case-place right-wall-column x (translate [-1.5 0 1] (wall-sphere-bottom 1/2 1)))
                             (key-place 5 x web-post-br)
                             (key-place 5 x web-post-tr)))
                     (for [x (range 0 4)]
                       (hull (case-place right-wall-column x (translate [-1.5 0 1] (wall-sphere-bottom 1/2 1)))
                             (case-place right-wall-column (inc x) (translate [-1.5 0 1] (wall-sphere-bottom 1/2 1)))
                             (key-place 5 x web-post-br)
                             (key-place 5 (inc x) web-post-tr)))
                     [(union
                       (hull (case-place right-wall-column 0 (translate [-1.5 0 1] (wall-sphere-bottom 1/2 1)))
                             (case-place right-wall-column 0.02 (translate [-1.5 -1 1] (wall-sphere-bottom 1 1)))
                             (key-place 5 0 web-post-tr)
                             )
                       (hull (case-place right-wall-column 4 (translate [-1.5 0 1] (wall-sphere-bottom 1/2 1)))
                             (case-place right-wall-column 4 (translate [-1.5 2 0.6] (wall-sphere-bottom 0 1)))
                             (key-place 5 4 half-post-br)
                             )
                       (hull (case-place right-wall-column 4 (translate [-1.5 0 1] (wall-sphere-bottom 1/2 1)))
                             (key-place 5 4 half-post-br)
                             (key-place 5 4 web-post-tr)))])
         back-wall (concat
                    (for [x (range 0 5)]
                      (union
                       (hull (case-place (- x 1/2) 0 (translate [0 -1 0.5] wall-sphere-bottom-back))
                             (case-place (+ x 1/2) 0 (translate [0 -1 0.5] wall-sphere-bottom-back))
                             (key-place x 0 web-post-tl)
                             (key-place x 0 web-post-tr))
                       (hull (case-place (- x 1/2) 0 (translate [0 -1 0.5] wall-sphere-bottom-back))
                             (key-place x 0 web-post-tl)
                             (key-place (- x 1) 0 web-post-tr))))
                    [
                     (union
                     (hull (case-place left-wall-column 0 (translate [1 -1 0.5] wall-sphere-bottom-back))
                           (case-place (+ left-wall-column 1) 0  (translate [0 -1 0.5] wall-sphere-bottom-back))
                           (key-place -1 0 web-post-tl)
                           (key-place -1 0 web-post-tr))
                     (hull (case-place right-wall-column 0 (translate [-1.5 -1 1] wall-sphere-bottom-back))
                           (case-place (- right-wall-column 1) 0  (translate [-1 -1 0.5] wall-sphere-bottom-back))
                           (key-place 5 0 web-post-tl)
                           (key-place 5 0 web-post-tr))
                     (hull (case-place (- 5 1/2) 0 (translate [0 -1 0.5] wall-sphere-bottom-back))
                             (key-place 5 0 web-post-tl)
                             (key-place (- 5 1) 0 web-post-tr))
                     )])
         left-wall (let [place case-place]
                     [(hull (place left-wall-column 0 (translate [1 -1 0.5] wall-sphere-bottom-back))
                            (place left-wall-column 1 (translate [1 0 0.5] wall-sphere-bottom-back))
                            (key-place -1 0 web-post-tl)
                            (key-place -1 0 web-post-bl))
                      (hull (place left-wall-column 1 (translate [1 0 0.5] wall-sphere-bottom-back))
                            (key-place -1 0 web-post-bl)
                            (key-place -1 1 web-post-tl))
                      (hull (place left-wall-column 1 (translate [1 0 0.5] wall-sphere-bottom-back))
                            (place left-wall-column 2 (translate [1 0 0.5] wall-sphere-bottom-back))
                            (key-place -1 1 web-post-tl)
                            (key-place -1 1 web-post-bl))
                      (hull (place left-wall-column 2 (translate [1 0 0.5] wall-sphere-bottom-back))
                            (key-place -1 1 web-post-bl)
                            (key-place -1 2 web-post-tl))
                      (hull (place left-wall-column 2 (translate [1 0 0.5] wall-sphere-bottom-back))
                            (place left-wall-column 1.6666  (translate [1 0 0.5] wall-sphere-bottom-front))
                            (key-place -1 2 web-post-tl)
                            (key-place -1 2 web-post-bl))
                      (hull (place left-wall-column 1.6666  (translate [1 0 0.5] wall-sphere-bottom-front))
                            (key-place -1 2 web-post-bl)
                            (key-place -1 3 web-post-tl))
                      (hull (place left-wall-column 3 (translate [1 0 0.5] wall-sphere-bottom-back))
                            (place left-wall-column 1.6666  (translate [1 0 0.5] wall-sphere-bottom-front))
                            (key-place -1 2 web-post-bl)
                            (key-place -1 3 web-post-tl))
                      ])
         thumbs [(hull (thumb-place 0 -1/2 web-post-bl)
                       (thumb-place 0 -1/2 web-post-tl)
                       (thumb-place 1 -1/2 web-post-tr)
                       (thumb-place 1 -1/2 web-post-br))
                 (hull (thumb-place 1 -1/2 web-post-tr)
                       (thumb-place 1 -1/2 web-post-tl)
                       (thumb-place 1 1 web-post-bl)
                       (thumb-place 1 1 web-post-br))
                 (hull (thumb-place 2 -1 web-post-tr)
                       (thumb-place 2 -1 web-post-tl)
                       (thumb-place 2 0 web-post-bl)
                       (thumb-place 2 0 web-post-br))
                 (hull (thumb-place 2 0 web-post-tr)
                       (thumb-place 2 0 web-post-tl)
                       (thumb-place 2 1 web-post-bl)
                       (thumb-place 2 1 web-post-br))
                 (triangle-hulls (thumb-place 2 1 web-post-tr)
                                 (thumb-place 1 1 web-post-tl)
                                 (thumb-place 2 1 web-post-br)
                                 (thumb-place 1 1 web-post-bl)
                                 (thumb-place 2 0 web-post-tr)
                                 (thumb-place 1 -1/2 web-post-tl)
                                 (thumb-place 2 0 web-post-br)
                                 (thumb-place 1 -1/2 web-post-bl)
                                 (thumb-place 2 -1 web-post-tr)
                                 (thumb-place 2 -1 web-post-br))
                 (hull (thumb-place 2 -1 web-post-br)
                       (thumb-place 1 -1/2 web-post-bl)
                       (thumb-place 1 -1 web-post-bl))
                 (hull (thumb-place 1 -1/2 web-post-bl)
                       (thumb-place 1 -1 web-post-bl)
                       (thumb-place 1 -1/2 web-post-br)
                       (thumb-place 1 -1 web-post-br))
                 (hull (thumb-place 0 -1/2 web-post-bl)
                       (thumb-place 0 -1 web-post-bl)
                       (thumb-place 0 -1/2 web-post-br)
                       (thumb-place 0 -1 web-post-br))
                 (hull (thumb-place 0 -1/2 web-post-bl)
                       (thumb-place 0 -1 web-post-bl)
                       (thumb-place 1 -1/2 web-post-br)
                       (thumb-place 1 -1 web-post-br))]
         thumb-back-wall [
                          (hull
                           (thumb-place (+ 5/2 0.05) thumb-back-y (translate [1 -1 1] wall-sphere-bottom-back))
                           (thumb-place 3/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))
                           (thumb-place 1 1 web-post-tl)
                           (thumb-place 2 1 web-post-tl))
                         (hull
                          (case-place left-wall-column 3  (translate [0.9 3 1] wall-sphere-bottom-back))
                          (thumb-place 3/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))
                          (key-place -1 2 web-post-bl)
                          (thumb-place 1 1 web-post-bl)
                          (key-place -1 3 web-post-tl)
                          (thumb-place 1 1 web-post-tl))
                          ]
         thumb-left-wall [(hull
                           (thumb-place thumb-left-wall-column thumb-back-y (translate [1 -1 1] wall-sphere-bottom-back))
                           (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place 2 1 web-post-tl)
                           (thumb-place 2 1 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place 2 0 web-post-tl)
                           (thumb-place 2 1 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place 2 0 web-post-tl)
                           (thumb-place 2 0 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place 2 -1 web-post-tl)
                           (thumb-place 2 0 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place thumb-left-wall-column (+ -1 0.07) (translate [1 1 1] wall-sphere-bottom-front))
                           (thumb-place 2 -1 web-post-tl)
                           (thumb-place 2 -1 web-post-bl))]
         thumb-front-wall [(hull (thumb-place (+ 5/2 0.05) thumb-front-row (translate [1 1 1] wall-sphere-bottom-front))
                                 (thumb-place (+ 3/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
                                 (thumb-place 2 -1 web-post-bl)
                                 (thumb-place 2 -1 web-post-br))
                           (hull (thumb-place (+ 1/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
                                 (thumb-place (+ 3/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
                                 (thumb-place 0 -1 web-post-bl)
                                 (thumb-place 1 -1 web-post-bl)
                                 (thumb-place 1 -1 web-post-br)
                                 (thumb-place 2 -1 web-post-br))
                           (hull (thumb-place thumb-right-wall thumb-front-row (translate [-1 1 1] wall-sphere-bottom-front))
                                 (thumb-place (+ 1/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
                                 (thumb-place 0 -1 web-post-bl)
                                 (thumb-place 0 -1 web-post-br))]
         thumb-inside [(triangle-hulls
                        (thumb-place 1 1 web-post-br)
                        (key-place -1 3 web-post-tr)
                        (key-place -1 3 web-post-tl)
                        (thumb-place 1 1 web-post-br)
                        (key-place -1 3 web-post-tr)
                        (key-place 0 3 web-post-tl)
                        (thumb-place 1 1 web-post-br)
                        (key-place 0 3 web-post-bl)
                        (thumb-place 1 -1/2 web-post-tr)
                        (thumb-place 0 -1/2 web-post-tl)
                        (key-place 0 3 web-post-bl)
                        (thumb-place 0 -1/2 web-post-tr)
                        (key-place 0 3 web-post-br)
                        (key-place 1 3 web-post-bl)
                        (thumb-place 0 -1/2 web-post-tr)
                        (key-place 1 4 web-post-tl)
                        (key-place 1 4 half-post-bl)
                        )
                       (hull
                        (key-place -1 3 web-post-tl)
                        (thumb-place 1 1 web-post-br)
                        (thumb-place 1 1 web-post-bl)
                       )

                       (hull
                         (thumb-place 1 1 web-post-tl)
                         (thumb-place 1 1 web-post-bl)
                         (key-place -1 3 web-post-tl)
                         (key-place -1 2 web-post-bl))

                       (hull
                        (thumb-place 0 -1/2 web-post-tr)
                        (thumb-place 0 -1/2 web-post-br)
                        (key-place 1 4 half-post-bl))

                       (hull
                        (key-place 1 4 half-post-bl)
                        (key-place 1 4 half-post-br)
                        (case-place (- 2 1/2) 4 (translate [0 1.5 0.5] wall-sphere-bottom-front))
                        (case-place 0.7 4 (translate [0 1.5 0.5] wall-sphere-bottom-front)))

                       (hull
                        (thumb-place 0 -1 web-post-br)
                        (thumb-place 0 -1/2 web-post-br)
                        (thumb-place thumb-right-wall thumb-front-row (translate [-1 1 1] wall-sphere-bottom-front))
                        (case-place 0.7 4 (translate [0 1 0] wall-sphere-bottom-front))
                        (key-place 1 4 half-post-bl))
                       ]
         stands (let [bumper-diameter 8.4
                      bumper-radius (/ bumper-diameter 2)
                      stand-diameter (+ bumper-diameter 2)
                      stand-radius (/ stand-diameter 2)
                      stand-top #(->>
                                  (sphere (+ stand-radius 2))
                                  (with-fn 100)
                                  (translate [0 0 (+ (/ stand-radius -2) -0.5)])
                                  %
                                  )
                      stand-cut #(union
                                   (->> (cube (+ 9 stand-diameter) (+ 9 stand-diameter) (+ 7 stand-radius))
                                        (translate [0 0 (+ 2.5 (/ stand-radius -2))])
                                        %)
                                   (->> (cube (+ 9 stand-diameter) (+ 9 stand-diameter) (+ 7 stand-radius))
                                        (translate [0 0 (+ 2.5 (/ stand-radius -2))])
                                       %)
                                 )
                      stand-bump-cut #(->> (sphere bumper-radius)
                                           (with-fn 100)
                                                 (translate [0 0 (+ (/ stand-radius -2) -0.5)])
                                                 %
                                                 (bottom 1)
                                        )
                      stand-at #(
                                 difference (->> (sphere stand-radius)
                                                 (with-fn 100)
                                                 (translate [0 0 (+ (/ stand-radius -2) -0.5)])
                                                 %
                                                 (bottom 0.1))
                                            )]
                  [
                   (difference
                     (hull
                       (stand-at #(key-place -2 -0.6 %))
                       (stand-top #(key-place -1 1 %))
                     )
                     (stand-cut #(key-place -1 1 %))
                     (stand-bump-cut #(key-place -2 -0.6 %))
                   )
                   (difference
                     (hull
                       (stand-at #(thumb-place 2 -1 %))
                       (stand-top #(thumb-place 1.5 -1/2 %))
                     )
                     (stand-cut #(thumb-place 1.5 -1/2 %))
                     (stand-bump-cut #(thumb-place 2 -1 %))
                   )
                   (difference
                     (hull
                       (stand-at #(key-place 5.1 -0.3 %))
                       (stand-top #(key-place 4.9 0.5 %))
                     )
                     (stand-cut #(key-place 4.9 0.5 %))
                     (stand-bump-cut #(key-place 5.1 -0.3 %))
                   )
                   (difference
                     (hull
                       (stand-at #(key-place 5.1 3.9 %))
                       (stand-top #(key-place 5 3.7 %))
                     )
                     (stand-cut #(key-place 5 3.7 %))
                     (stand-bump-cut #(key-place 5.1 3.9 %))
                   )
                   ])]
     (apply union
            (concat
             main-keys-bottom
             front-wall
             right-wall
             back-wall
             left-wall
             thumbs
             thumb-back-wall
             thumb-left-wall
             thumb-front-wall
             thumb-inside
             stands
             )))))

(def screw-hole (->> (union
                      (->>
                        (cylinder 1.5 60)
                        (translate [0 0 19])
                        (with-fn wall-sphere-n)
                      )
                     )))

(def screw-hole-holder (->> (hull (->> (cylinder 2.5 4.5) (translate [0 0 1.7]))
                                  (translate [0 0 -3] (cylinder 2.5 0.001))
                                  )
                     (translate [0 0 -3])
                     (with-fn wall-sphere-n)))

(def nut-cube (->> (cube 4.1 2.368 7)))

(def screw-nut-hole (->> (union
                           (rotate (* 2 (/ π 3)) [0 0 1] nut-cube)
                           (rotate (/ π 3) [0 0 1] nut-cube)
                           nut-cube)
                     (translate [0 0 -8])))

(def screw-holes
  (union
   (key-place (+ 4 1/2) 1/2 screw-hole)
   (key-place -1 1/2 screw-hole)
   (key-place (+ 4 1/2) (+ 3 1/2) screw-hole)
   (thumb-place 2 0 screw-hole)))

(def screw-hole-holders
  (union
   (key-place (+ 4 1/2) 1/2 screw-hole-holder)
   (key-place -1 1/2 screw-hole-holder)
   (key-place (+ 4 1/2) (+ 3 1/2) screw-hole-holder)
   (thumb-place 2 0 screw-hole-holder)))

(def screw-nut-holes
  (union
   (key-place (+ 4 1/2) 1/2 screw-nut-hole)
   (key-place -1 1/2 screw-nut-hole)
   (key-place (+ 4 1/2) (+ 3 1/2) screw-nut-hole)
   (thumb-place 2 0 screw-nut-hole)))

(defn circuit-cover [width length height]
  (let [cover-sphere-radius 1
        cover-sphere (->> (sphere cover-sphere-radius)
                          (with-fn 20))
        cover-sphere-z (+ (+ (- height) (- cover-sphere-radius)) -1.5)
        cover-sphere-x (- (+ (/ width 2) cover-sphere-radius) 0.35)
        cover-sphere-y (- (+ (/ length 2) (+ cover-sphere-radius)) 0.36)
        cover-sphere-tl (->> cover-sphere
                             (translate [(- cover-sphere-x) (- cover-sphere-y) cover-sphere-z])
                             (key-place 1/2 3/2))
        cover-sphere-tr (->> cover-sphere
                             (translate [cover-sphere-x (- cover-sphere-y) cover-sphere-z])
                             (key-place 1/2 3/2))
        cover-sphere-br (->> cover-sphere
                             (translate [cover-sphere-x cover-sphere-y cover-sphere-z])
                             (key-place 1/2 3/2))
        cover-sphere-bl (->> cover-sphere
                             (translate [(- cover-sphere-x) cover-sphere-y cover-sphere-z])
                             (key-place 1/2 3/2))

        lower-to-bottom #(translate [0 0 (+ (- cover-sphere-radius) -5.5)] %)
        bl (->> (translate [0 -5.5 1.2] cover-sphere) lower-to-bottom (key-place 0 1/2))
        br (->> (translate [0 -5.5 1.2] cover-sphere) lower-to-bottom (key-place 1 1/2))
        tl (->> (translate [0 5.5 1.2] cover-sphere) lower-to-bottom (key-place 0 5/2))
        tr (->> (translate [0 5.5 1.2] cover-sphere) lower-to-bottom (key-place 1 5/2))

        mlb (->> cover-sphere
                 (translate [(- cover-sphere-x) 0 (+ (- height) -2.5)])
                 (key-place 1/2 3/2))
        mrb (->> cover-sphere
                 (translate [cover-sphere-x 0 (+ (- height) -2.5)])
                 (key-place 1/2 3/2))

        mlt (->> cover-sphere
                 (translate [(+ (- cover-sphere-x) -4) 0 -5.2])
                 (key-place 1/2 3/2))
        mrt (->> cover-sphere
                 (translate [(+ cover-sphere-x 4) 0 -5.2])
                 (key-place 1/2 3/2))]
    (union
     (hull cover-sphere-bl cover-sphere-br cover-sphere-tl cover-sphere-tr)
     (hull cover-sphere-br cover-sphere-bl bl br)
     (hull cover-sphere-tr cover-sphere-tl tl tr)
     (hull cover-sphere-tl tl mlb mlt)
     (hull cover-sphere-bl bl mlb mlt)
     (hull cover-sphere-tr tr mrb mrt)
     (hull cover-sphere-br br mrb mrt))))

(def io-exp-width 10)
(def io-exp-height 8)
(def io-exp-length 36)

(def teensy-width 18.7)
(def teensy-height 12)
(def teensy-length 33.5)

(defn teensy-pos [shape]
  (->> (key-place 0.5 1.5 (translate [0 0 -5] shape)))
)

(def io-exp-cover (circuit-cover io-exp-width io-exp-length io-exp-height))
(def teensy-cover (translate [0 0 0.8] (circuit-cover teensy-width teensy-length teensy-height)))

(def trrs-diameter 8)
(def trrs-outer-diameter 10.2)
(def trrs-radius (/ trrs-diameter 2))
(def trrs-outer-radius (/ trrs-outer-diameter 2))
(def trrs-hole-depth 21)
(def trrs-offset -5.1)

(def trrs-cut
  (->> 
    (hull
      (->>
        (cube trrs-diameter trrs-diameter trrs-hole-depth)
        (translate [ 0 10 0 ])
      )
      (->>
        (cylinder trrs-radius trrs-hole-depth)
        (translate [ 0 trrs-offset 0 ])
      )
    )
      (rotate (/ π 2) [1 0 0])
      (translate [ 0 4 0 ])
      (with-fn 50)
  )
)

(def trrs-shell-ring
  (->>
    (difference
      (intersection
        (hull
          (cylinder (+ trrs-outer-radius 0.6) trrs-hole-depth)
          (translate [0 (- (+ trrs-outer-radius 0.6) -0.5) 1.5] (cube (* (+ trrs-outer-radius 0.6) 2) 1 (+ trrs-hole-depth 3)))
        )
        (hull
          (cylinder (+ trrs-outer-radius 0.6) trrs-hole-depth)
          (translate [0 (- (+ trrs-outer-radius 0.6) -0.5) 1.5] (cube (* (+ trrs-outer-radius 0.6) 2) 1 (+ trrs-hole-depth 3)))
        )
      )
      (translate [0 4 -4.5] (rotate (/ π 8) [ -1 0 0 ] (cube 15 10 6)))
      (translate [0 -6.3 13] (rotate (/ π 2.8) [ 1 0 0 ] (cube 15 10 6)))
      (translate [0 3 4] (cube 15 9 trrs-hole-depth))
    )
      (translate [ 0 trrs-offset 0 ])
      (rotate (/ π 2) [1 0 0])
      (translate [ 0 2.3 0 ])
      (with-fn 50)
  )
)

(def trrs-ring
  (->>
    (hull
      (->>
        (cube trrs-outer-diameter trrs-outer-diameter 1.6)
        (translate [ 0 10 0 ])
      )
      (cylinder trrs-outer-radius 1.6)
    )
      (translate [ 0 trrs-offset 0 ])
      (rotate (/ π 2) [1 0 0])
      (translate [ 0 9.5 0 ])
      (with-fn 50)
  )
)

(defn trrs-pos [obj]
  (->> obj (key-place 1/2 0))
)

(def trrs-cutout
  (->> (trrs-pos trrs-cut))
)

(def trrs-shell
  (->> (trrs-pos trrs-shell-ring))
)

(def trrs-ring-cutout
  (->> (trrs-pos trrs-ring))
)

(def teensy-pcb-thickness 1.6)
(def teensy-offset-height 3.8)

(def teensy-pcb (->> (cube 18 30.5 teensy-pcb-thickness)
                     (translate [0 0 (+ (/ teensy-pcb-thickness -2) (- teensy-offset-height))])
                     (key-place 1/2 3/2)
                     (color [1 0 0])))

(def teensy-support
  (->>
    (union
      (teensy-pos (translate [0 -15.5 -6.6] (cube 7.2 2.3 2.5)))
      (teensy-pos (translate [7.5 16 -6.6] (cube 6.5 2.3 2.5)))
      (teensy-pos (translate [-7.5 16 -6.6] (cube 6.5 2.3 2.5)))
    )
  )
)

(def teensy-clamp
  (->> 
    (difference
        (
          key-place 1/2 3/2
          (translate [0 0 -3.1] (cylinder 5 9.8))
        )
        (
          key-place 1/2 3/2
          (translate [0 0 -8.15] (cylinder 4.2 2.8))
        )
    )
  )
)

(def usb-cutout
  (let [hole-height 5
        side-radius (/ hole-height 2)
        hole-width 10.75
        side-cylinder (->> (cylinder side-radius teensy-length)
                           (with-fn 20)
                           (translate [(/ (- hole-width hole-height) 2) 0 0]))]
    (->> (hull side-cylinder
               (mirror [-1 0 0] side-cylinder))
         (rotate (/ π 2) [1 0 0])
         (translate [0 (/ teensy-length 2) (- side-radius)])
         (translate [0 0 (- 4.0)])
         (translate [0 0 (- teensy-offset-height)])
         (key-place 1/2 3/2))))

;;;;;;;;;;;;;;;;;;
;; Final Export ;;
;;;;;;;;;;;;;;;;;;

(def dactyl-bottom-base
  (difference
   (union
    teensy-cover
    teensy-support
    (difference
      (union
     bottom-plate
     trrs-shell
    screw-hole-holders
        )
     (hull teensy-cover)
     ;new-case
     teensy-cover
     screw-nut-holes
     trrs-cutout
     trrs-ring-cutout
     screw-holes))
     (->> (cube 1000 1000 10) (translate [0 0 -5.8]))
   )
)

(def dactyl-bottom-right
  (difference
   dactyl-bottom-base
   usb-cutout))

(def dactyl-bottom-left
  (mirror [-1 0 0]
   dactyl-bottom-base
          )
  )

(def dactyl-top-right
  (difference
   (union key-holes
          connectors
          thumb
          ;teensy-clamp
          new-case)
   screw-holes
   ))

(def dactyl-top-right-case
  new-case)

(def dactyl-top-left
  (mirror [-1 0 0]
          (difference
           (union key-holes
                  connectors
                  thumb
                  new-case)
           screw-holes)))

(def teensy-parts
  (intersection dactyl-bottom-right 
    (teensy-pos (cube (+ teensy-width 9) (+ teensy-length 17) (+ teensy-height 10)))
  )
)

(spit "things/sa-cap.scad"
      (write-scad (sa-cap 1)))
(spit "things/switch-hole.scad"
      (write-scad old-single-plate))
(spit "things/alps-holes.scad"
      (write-scad (union connectors key-holes)))
(comment
(spit "things/alps-caps.scad"
      (write-scad (union connectors caps)))
(spit "things/connectors.scad"
      (write-scad connectors))
)

(spit "things/dactyl-top-right.scad"
      (write-scad dactyl-top-right))

(spit "things/dactyl-top-right-case.scad"
      (write-scad dactyl-top-right-case))

(spit "things/dactyl-bottom-right.scad"
      (write-scad dactyl-bottom-right))

(spit "things/teensy-parts.scad"
      (write-scad teensy-parts))

(spit "things/dactyl-top-left.scad"
      (write-scad dactyl-top-left))

(spit "things/dactyl-bottom-left.scad"
      (write-scad dactyl-bottom-left))

(comment
(spit "things/dactyl-top-left-with-teensy.scad"
      (write-scad (mirror [-1 0 0] dactyl-top-right)))

(spit "things/dactyl-bottom-left-with-teensy.scad"
      (write-scad (mirror [-1 0 0] dactyl-bottom-right)))
        )
