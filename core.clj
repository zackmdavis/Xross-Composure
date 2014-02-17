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
      (println (get_constraints grid i :across))))
  (println))

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
  (let [results (search words
                        (get_constraints grid i orientation))]
    (if (seq results)
      (do
        (write_word grid (rand-nth results) i orientation)
        true)
      false)))

(defn solve [words grid prompts]
  (let [prompt (first prompts)]
    (if (solve_word words grid (first prompt) (second prompt))
      (do 
        (display_grid grid)
        (solve words grid (rest prompts)))
      (println "dead end or solved"))))

(def demo_grid (empty_grid 4))

(def demo_prompts
  (shuffle
   [[0 :across]
    [0 :down]
    [1 :across]
    [1 :down]
    [2 :across]
    [2 :down]
    [3 :down]]))

(solve dictionary demo_grid demo_prompts)