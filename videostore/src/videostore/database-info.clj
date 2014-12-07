(ns assignment5.database-info
  (:gen-class))

(defn- get-file-path
  "this function returns the relative path of a file"
  [file-name]
  (str "resources" (System/getProperty "file.separator") file-name))

(defn movie-list-file
  []
  (get-file-path "movies.txt"))

(defn rented-list-file
  []
  (get-file-path "rented.txt"))

(defn mappings-file
  []
  (get-file-path "mappings.txt"))

(defn rented-ids-file
  []
  (get-file-path "rented-ids.txt"))
