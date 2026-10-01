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

(defn subsequence? [sub seq]
  (cond
    (empty? sub) true
    (empty? seq) false
    (= (first sub) (first seq)) (recur (rest sub) (rest seq))
    :else (recur sub (rest seq))))

(defn all-distinct? [items]
  (or (empty? items)
      (apply distinct? items)))