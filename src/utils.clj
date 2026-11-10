(ns utils)

(defn ??? [] (throw (AssertionError. "Not implemented")))

(defn pairs [items]
  (loop [result    []
         remaining items]
    (if (empty? remaining)
      result
      (let [a  (first remaining)
            bs (rest remaining)]
        (recur (into result (map (fn [b] [a b]) bs))
               bs)))))

(defn consecutive-pairs [items]
  (partition 2 1 items))

(defn subsequence? [a b]
  (loop [a a, b b]
    (cond (empty? a) true
          (empty? b) false
          (= (first a) (first b)) (recur (rest a) (rest b))
          :else (recur a (rest b)))))

(defn all-distinct? [items]
  (or (empty? items)
      (apply distinct? items)))
