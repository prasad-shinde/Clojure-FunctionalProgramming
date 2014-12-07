(ns assignment5.file-handling
  (:gen-class))

(defn read-file
  [file-name]
  (let [contents (slurp file-name)]
    (if (empty? contents)
      nil
      (read-string contents))))

(defn write-file
  [file-name contents]
  (spit file-name contents))
