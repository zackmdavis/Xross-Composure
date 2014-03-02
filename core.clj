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

(defn string_to_sequence [word]
  (map #(keyword (clojure.string/upper-case (str %))) word))

(def dictionary 
  (map string_to_sequence
       (filter (fn [word] (not (.contains word "'")))
               (clojure.string/split-lines 
                (slurp "/usr/share/dict/words")))))

(defn rows [grid]
  (seq grid))

(def four-dictionary (filter #(= (count %) 4) dictionary))

(defn col [grid i]
  (for [row grid] (nth row i)))

(defn cols [grid]
  (for [i (range (count grid))] (col grid i)))

(defn other [oriented]
  (cond :across :down
        :down :across))

(def lines {:across rows :down cols})

(defn get_line [grid i oriented]
  (nth ((lines oriented) grid) i))

(defn get_text [grid i oriented]
  (map deref (get_line grid i oriented)))

(defn copy_line [grid i oriented]
  (map atom (get_text grid i oriented)))

(defn get_line_text [line]
  (map deref line))

(defn overwrite [line new]
  (doseq [i (range (count line))]
    (reset! (nth line i) (nth new i))))

(defn substitute [line i new]
  (reset! (nth line i) new))

(defn write_line [grid i oriented new]
  (overwrite (get_line grid i oriented) new))

(defn display_grid [grid]
  (let [n (count grid)]
    (doseq [i (range n)]
      (println (get_text grid i :across))))
  (println))

(defn search [words constraint]
  (let [fit? (fit_predicate constraint)]
    (filter fit? words)))

(defn crossed_lines [grid i oriented]
  (for [j (range (count grid))]
    (get_line grid j (other oriented))))

(defn crossed_line_copies [grid i oriented]
  (for [j (range (count grid))]
    (copy_line grid j (other oriented))))
