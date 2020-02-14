(ns solver
  "Clojure port of the python sudoku solver demonstrated at
   https://www.youtube.com/watch?v=G_UYXzGuqvM
   Python Sudoku Solver - Computerphile")

(def sudoku-puzzle
  [5 3 0 , 0 7 0 , 0 0 0
   6 0 0 , 1 9 5 , 0 0 0
   0 9 8 , 0 0 0 , 0 6 0
  ;---------------------
   8 0 0 , 0 6 0 , 0 0 3
   4 0 0 , 8 0 3 , 0 0 1
   7 0 0 , 0 2 0 , 0 0 6
  ;---------------------
   0 6 0 , 0 0 0 , 2 8 0
   0 0 0 , 4 1 9 , 0 0 5
   0 0 0 , 0 8 0 , 0 7 9])

(defn index->coords [idx]
  [(rem idx 9) (quot idx 9)])

(defn coords->index [x y]
  (+ x (* y 9)))

(defn nav-grid [grid x y]
  (let [idx (get-index x y)]
    (get grid idx)))

(defn col [grid x]
  (->> grid (drop x) (take-nth 9)))

(defn row [grid y]
  (let [start (get-index 0 y)]
    (subvec grid start 9)))

(defn square [grid x y]
  (let [[x0 y0] (map #(-> % (quot 3) (* 3)) [x y])
        coords-list (for [i (range 3)]
                      (for [j (range 3)]
                        [(+ x0 i) (+ y0 j)]))
        coords (apply concat (vec coords-list))]
    (map (fn [[x y]] (nav-grid grid x y)) coords)))

(defn already-taken-numbers [coll]
  (->> coll (remove zero?) set))

(defn possible? [grid x y n]
  (let [taken-set (->> [(col grid x) (row grid y) (square grid x y)]
                       (map already-taken-numbers)
                       (apply clojure.set/union))]
    (-> n taken-set not)))

(defn solve [grid] ;;TODO get it actually working
  (loop [grid grid]
    (doseq [idx (range (* 9 9))]
      (let [curr-value (get grid idx)]
        (if (zero? curr-value)
          (for [n (range 10)]
            (let [[x y] (index->coords idx)]
              (if (possible? grid x y n)
                (recur (assoc grid idx n))))))))))
