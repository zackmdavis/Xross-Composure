(defn zip [& seqs]
  (apply map vector seqs))

(defn fit_predicate [constraints]
  (fn [candidate]
    (and (= (count candidate) (count constraints))
         (let [comparison (zip candidate constraints)]
           (every? #(or (= (second %) nil)
                        (= (first %) (second %)))
                   comparison)))))

(def b__ (fit_predicate [:B nil nil]))

(println (b__ [:B :A :T])) ;=> true
(println (b__ [:B :I :T])) ;=> true
(println (b__ [:C :A :T])) ;=> false
(println (b__ [:B :O :O :T])) ;=> false

(defn empty_grid [n]
  (vec (for [row (range n)]
    (vec (for [col (range n)] (atom nil))))))

(defn lookup [grid position]
  ((grid (first position)) (second position)))

(def demo_grid (empty_grid 4))

(defn word_to_seq [word]
  (map #(clojure.string/upper-case (keyword (str %))) word))

(println (word_to_seq "dogs")) ;=> (:D :O :G :S)

(def dictionary (map word_to_seq 
                     (clojure.string/split-lines 
                      (slurp "/usr/share/dict/words"))))

(println (nth dictionary 345)) ;=> (:A :L :E :X)
(println (nth dictionary 1345)) ;=> (:B :A :N :D :U :N :G :' :S)

(defn get_constraints [grid i orientation]
  (let [n (count grid)]
    (cond (= orientation :across) (for [j (range n)]
                                    (deref (lookup grid [i j])))
          (= orientation :down) (for [j (range n)]
                                  (deref (lookup grid [j i]))))))
(defn display_grid [grid]
  (let [n (count grid)]
    (doseq [i (range n)]
      (println (get_constraints grid i :across)))))

(defn write_word [grid word i orientation]
  (let [n (count grid)]
    (cond (= orientation :across) (doseq [j (range n)]
                                    (reset! (lookup grid [i j])
                                            (nth word j)))
          (= orientation :down) (doseq [j (range n)]
                                  (reset! (lookup grid [j i])
                                          (nth word j))))))

(write_word demo_grid [:G :R :I :N] 0 :across)
(println (get_constraints demo_grid 0 :across)) ;=> (:G :R :I :N)
(println (get_constraints demo_grid 0 :down)) ;=> (:G nil nil nil)
