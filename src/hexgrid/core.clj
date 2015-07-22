(ns hexgrid.core
  (:require [clojure.math.numeric-tower :as math]
            [schema.core :as s]))

(def CubeCell [(s/one s/Num "rx")
               (s/one s/Num "ry")
               (s/one s/Num "rz")])

(def AxialCell [(s/one s/Num "q")
                (s/one s/Num "r")])

(s/defn add :- [s/Num]
  [& cells :- [[s/Num]]]
  (apply (partial map +) cells))

(s/defn sub :- [s/Num]
  [& cells :- [[s/Num]]]
  (apply (partial map -) cells))

(s/defn multiply :- [s/Num]
  [& cells :- [[s/Num]]]
  (apply (partial map *) cells))

(s/defn div :- [s/Num]
  [& cells :- [[s/Num]]]
  (apply (partial map /) cells))

(s/defn cube-to-axial :- AxialCell
  [cube :- CubeCell]
  (let [[x _ z] cube]
    [x z]))

(s/defn axial-to-cube :- CubeCell
  [cell :- AxialCell]
  (let [[q r] cell]
    [q (- (- q) r) r]))

(s/defn cube-round :- CubeCell
  [cube-cell :- CubeCell]
  {:pre (= 3 (count cube-cell))}
  (let [[rx ry rz :as rounded-cube] (map math/round cube-cell)
        [x-diff y-diff z-diff] (map math/abs (map - rounded-cube cube-cell))]

    (cond (and (> x-diff y-diff) (> x-diff z-diff))
          [(- (- ry) rz) ry rz]
          (> y-diff z-diff)
          [rx (- (- rx) rz) rz]
          :else
          [rx ry (- (- rx) ry)])))

(s/defn axis-round :- AxialCell
  [cell :- AxialCell]
  {:pre (= (count cell) 2)}
  (cube-to-axial (cube-round (axial-to-cube cell))))

(def neighbours
  [[inc, identity], [inc, dec], [identity, dec],
   [dec, identity], [dec, inc], [identity, inc]])

(s/defn list-neighbours :- [AxialCell]
  [cell :- AxialCell]
  (let [[q r] cell]
    (map #(let [[fq fr] %]
            [(fq q) (fr r)]) neighbours)))

(s/defn cube-distance :- s/Num
  [cube1 :- CubeCell
   cube2 :- CubeCell]
  (/ (apply +
            (map math/abs
                 (map - cube1 cube2))) 2))

(s/defn distance :- s/Num
  [cell1 :- AxialCell
   cell2 :- AxialCell]
  (cube-distance (axial-to-cube cell1)
                 (axial-to-cube cell2)))

(s/defn line :- [AxialCell]
  [cellA :- AxialCell
   cellB :- AxialCell]
  (let [modifiedB (map (partial + 1e-6) cellB)              ;path goes to completion
        n (distance cellA modifiedB)]
    (map (fn [i]
           (axis-round
            (map (fn [^Number a ^Number b]
                   (+ (* a (- 1 (/ i n))) (* b (/ i n))))
                 cellA modifiedB)))
         (range n))))

(s/defn bound-range :- [CubeCell]
  [min-cube :- CubeCell
   max-cube :- CubeCell]
  (let [[^Number x-min ^Number y-min ^Number z-min] min-cube
        [^Number x-max ^Number y-max ^Number z-max] max-cube]
    (for [^Number x (range x-min (inc x-max))
          ^Number y (range (max y-min (- (- x) z-max))
                           (inc (min y-max (- (- x) z-min))))]
      (let [z (- (- x) y)]
        [x y z]))))

(s/defn cube-range :- [CubeCell]
  [cube :- CubeCell
   N :- s/Int]
  (bound-range (map #(- % N) cube) (map #(+ % N) cube)))

(s/defn axial-range :- [AxialCell]
  [cell :- AxialCell
   steps :- s/Int]
  (map cube-to-axial (cube-range (axial-to-cube cell) steps)))

(s/defn find-limit :- CubeCell
  [limitf :- (s/=> s/Bool)
   combinef :- (s/=> s/Int)
   & pairs :- [[(s/one CubeCell "cell")
                (s/one s/Int "N")]]]
  (map (partial apply limitf)
       (apply map vector
              (map (fn [p] (let [[cube N] p]
                             (map #(combinef % N) cube)))
                   pairs))))

(s/defn cube-overlap :- [CubeCell]
  [& pairs :- [[(s/one CubeCell "cell") (s/one s/Num "N")]]]
  (let [maxs (apply find-limit min + pairs)
        mins (apply find-limit max - pairs)]
    (bound-range mins maxs)))

(s/defn axial-overlap :- [AxialCell]
  [& pairs :- [[(s/one AxialCell "cell")
                (s/one s/Num "N")]]]
  (map cube-to-axial
       (apply cube-overlap
              (map (fn [[cube N]]
                     [(axial-to-cube cube) N])
                   pairs))))

(defn axial-hex-to-pixel
  [[q r] size-in-pixels]
  (map math/floor [(* size-in-pixels (math/sqrt 3) (+ q (/ r 2)))
                   (* size-in-pixels 3/2 r)]))

(defn axial-pixel-to-hex
  [x y size-in-pixels]
  (axis-round [(/ (- (* 1/3 (math/sqrt 3) x) (* 1/3 y)) size-in-pixels)
               (/ (* 2/3 y) size-in-pixels)]))
