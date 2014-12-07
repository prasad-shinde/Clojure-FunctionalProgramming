(ns assignment5.unique-id-generator
  (:gen-class))

(defn unique-id-file-name
  []
  (str "resources" (System/getProperty "file.separator") "unique-id.txt"))

(defn next-unique-id
  "returns the next unique id for the movie"
  []
  (let [next (read-string (slurp (unique-id-file-name)))]
    (spit "resources\\unique-id.txt" (str (inc next)))
    (inc next)))

