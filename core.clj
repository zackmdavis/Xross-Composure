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

(defn overwrite! [line new]
  (doseq [i (range (count line))]
    (reset! (nth line i) (nth new i)))
  line)

(defn substitute! [line i new]
  (reset! (nth line i) new)
  line)

(defn write_line [grid i oriented new]
  (overwrite! (get_line grid i oriented) new))

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

(defn second_constraints [grid i oriented candidate]
  (let [preconstraint_lines (crossed_line_copies grid i oriented)]
    (map get_line_text
         (for [j&line (map-indexed vector preconstraint_lines)]
           (substitute! (second j&line) i (nth candidate (first j&line)))))))

(defn promise [words grid i oriented candidate]
  (let [constraints (second_constraints grid i oriented candidate)]
    (apply min (for [constraint constraints]
                 (count (search words constraint))))))

(defn solve_word [words grid i oriented]
  (let [first_results (search words (get_text grid i oriented))]
    (write_line grid i oriented
                ; non-debugging version---
                ;; (apply max-key #(promise words grid i oriented %)
                ;;        first_results))))
                (do
                  (let [examination
                        (sort-by second >
                                 (for [candidate first_results]
                                   [candidate (promise words grid i
                                                       oriented
                                                       candidate)]))]
                    (println (take 12 examination))
                    (println)
                    (first (first examination)))))))

(def demo_grid (empty_grid 4))
(write_line demo_grid 0 :across (string_to_sequence "want"))
(display_grid demo_grid)
(solve_word four-dictionary demo_grid 0 :down)
(display_grid demo_grid)
(solve_word four-dictionary demo_grid 3 :down)
(display_grid demo_grid)
(solve_word four-dictionary demo_grid 2 :across)
(display_grid demo_grid)
(solve_word four-dictionary demo_grid 1 :down)
(display_grid demo_grid)
(solve_word four-dictionary demo_grid 1 :across)
(display_grid demo_grid)
(solve_word four-dictionary demo_grid 3 :across)
(display_grid demo_grid)
