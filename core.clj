(require 'clojure.string)

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

(def four-dictionary (filter #(= (count %) 4) dictionary))

(defn positions [word]
  (let [n (count word)]
    (for [i&chr (map-indexed vector word)]
      (conj [(second i&chr) (first i&chr)] n))))

(defn global_positions [lexicon]
  (partition 3
             (flatten (for [word lexicon]
                        (positions word)))))

(defn position_scores [positions]
  (let [unique_positions (distinct positions)]
    (into {}
          (for [position unique_positions]
            [position (count (filter #(= position %)
                                     positions))]))))

(def four-scores (position_scores (global_positions four-dictionary)))

(defn score [scores word]
  (reduce +
          (map #(or (scores %) 0) 
               (for [i&chr (map-indexed vector word)]
                 (conj [(second i&chr) (first i&chr)] (count word))))))

(defn other [orientation]
  (cond :across :down
        :down :across))

(defn second_constraints [grid word i orientation]
  (let [n (count grid)
        extant_constraints (for [j (range n)]
                             (get_constraints grid j
                                              (other orientation)))]
    (map-indexed #(assoc (vec %2) i (nth word %1)) extant_constraints)))

(defn solve_word [words grid i orientation]
  (let [first_results (search words
                              (get_constraints grid i orientation))
        second_filter (fn [word]
                        (every? #(seq %) 
                                (for [constraint (second_constraints grid word i
                                                                     orientation)]
                                  (search words constraint))))
        second_results (filter second_filter first_results)
        sorted_results (sort #(> (score four-scores %1)
                                 (score four-scores %2))
                             second_results)]

    (if (seq sorted_results)
      (do
        (write_word grid (rand-nth (take 10 sorted_results)) i orientation)
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
   [[0 :down]
    [1 :across]
    [1 :down]
    [2 :across]
    [2 :down]
    [3 :down]]))

(write_word demo_grid [:S :A :K :E] 0 :across)
(solve four-dictionary demo_grid demo_prompts)