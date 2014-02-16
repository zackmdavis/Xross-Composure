(defn zip [& seqs]
  (apply map vector seqs))

(defn fit_predicate [constraints]
  (fn [candidate]
    (and (= (count candidate) (count constraints))
         (let [comparison (zip candidate constraints)]
           (every? #(or (= (second %) nil)
                        (= (first %) (second %)))
                   comparison)))))

(defn empty_grid [n]
  (vec (for [row (range n)]
    (vec (for [col (range n)] (atom nil))))))

(defn lookup [grid position]
  ((grid (first position)) (second position)))

(def demo_grid (empty_grid 4))

(defn word_to_seq [word]
  (map #(keyword (clojure.string/upper-case (str %))) word))

(def dictionary 
  (map word_to_seq
       (filter (fn [word] (not (.contains word "'")))
               (clojure.string/split-lines 
                (slurp "/usr/share/dict/words")))))

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

(defn search [words constraints]
  (let [fit? (fit_predicate constraints)]
    (filter fit? words)))

(defn solve_word [words grid i orientation]
  (write_word grid
              (rand-nth (search words
                                (get_constraints grid i orientation)))
                        i
                        orientation))

(write_word demo_grid [:B :I :T :S] 0 :across)
(solve_word dictionary demo_grid 0 :down)
(solve_word dictionary demo_grid 1 :across)
(solve_word dictionary demo_grid 1 :down)

(display_grid demo_grid)
