(ns assignment5.movies
  (:require [assignment5.unique-id-generator :refer :all]
            [assignment5.file-handling :refer :all]
            [assignment5.database-info :refer :all])
  (:gen-class))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dates - Get due dates returns the date 2 weeks from now ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; make-movie returns a map of movie  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '[clj-time.core :as t])
(require '[clj-time.local :as l])
(require '[clj-time.format :as f])

(def custom-formatter (f/formatter "yyyy-MM-dd"))


(defn get-due-date
  []
  (f/unparse custom-formatter (t/plus (l/local-now) (t/weeks 2))))


(defn make-movie
  "creates a map of movie"
  [name price quantity]
  {:name name :price price :quantity quantity})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; These functions either return list,sets of movies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn movie-list
  []
  (atom (read-file (movie-list-file))))


(defn rented-list
  []
  (atom (read-file (rented-list-file))))

(defn rented-ids
  "returns a list of rented movie ids"
  []
  (atom (read-file (rented-ids-file))))

(defn mapping-list
  []
  (atom (read-file (mappings-file))))

(defn movie-names-set
  [movie-list]
  (set (map :name (map val @movie-list))))

(declare movie-name-by-id)
(defn rented-movies
  []
  (map (fn [n] (movie-name-by-id n)) @(rented-ids)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; These functions store the id to name mapping ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; They also remove mapping when movies no longer exist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn store-mapping
  "map the unique id to a movie"
  [id movie-name]
  (let [mappings (mapping-list)]
    (reset! mappings (assoc-in @mappings [id] movie-name))
    (reset! mappings (assoc-in @mappings [movie-name] id))
    (write-file (mappings-file) @mappings)))

(defn remove-mapping
  "remove the id to movie mapping"
  [id movie-name]
  (let [mappings (mapping-list)]
    (reset! mappings (dissoc @mappings id))
    (reset! mappings (dissoc @mappings movie-name))
    (write-file (mappings-file) @mappings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Retrival Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Find name by ID or Renter name for move etc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Find movie price or Quantity ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn movie-id-by-name
  [movie-name]
  (let [mappings (mapping-list)
        [name id] (find @mappings movie-name)]
    id))

(defn movie-name-by-id
  [id]
  (let [mappings (mapping-list)
        [id name] (find @mappings id)]
    name))

(defn renter-name-by-id
  [id]
  (let [rented-list (rented-list)]
    (first (keys (@rented-list id)))))

(defn renter-name-by-movie
  [movie-name]
  (renter-name-by-id (movie-id-by-name movie-name)))

(declare due-date-by-id)
(defn due-date-by-movie
  [movie-name]
  (due-date-by-id (movie-id-by-name movie-name)))


(defn due-date-by-id
  [id]
  (let [rented-list (rented-list)]
    (first (vals (@rented-list id)))))

(declare get-attribute-value)
(defn find-price
  [identifier]
  (let [movie-list (movie-list)
        quant (get-attribute-value movie-list identifier :price)]
    (if (nil? quant)
      (get-attribute-value movie-list (movie-name-by-id identifier) :price)
      quant)))

(defn find-quantity
  [identifier]
  (let [movie-list (movie-list)
        quant (get-attribute-value movie-list identifier :quantity)]
    (if (nil? quant)
      (get-attribute-value movie-list (movie-name-by-id identifier) :quantity)
      quant)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Add movies to the db and store the mappings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- add-movie-db
  [movie & more]
  (let [movie-list (movie-list)
        next-id (if (nil? more)
                  (next-unique-id)
                  (first more))]
    (reset! movie-list (assoc-in @movie-list [next-id] movie))
    (write-file (movie-list-file) @movie-list)
    (store-mapping next-id (:name movie))))

(defn add-movie
  [name price quantity]
  (let [movie (make-movie name price quantity)]
    (add-movie-db movie)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Remove a movie from store and remove its dependencies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-movie
  [id]
  (let [movie-list (movie-list)
        rented-list (rented-list)
        rented-ids (rented-ids)]
    (reset! movie-list (dissoc @movie-list id))
    (reset! rented-list (dissoc @rented-list id))
    (reset! rented-ids (disj @rented-ids id))
    (write-file (movie-list-file) @movie-list)
    (write-file (rented-list-file) @rented-list)
    (write-file (rented-ids-file) @rented-ids)
    (remove-mapping id (movie-name-by-id id))))

(defn remove-movie-by-name
  [movie-name]
  (remove-movie (movie-id-by-name movie-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rent a available movie and add to rented list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rent-movie
  [id renter-name]
  (let [movie-list (movie-list)
        rented-list (rented-list)
        rented-ids (rented-ids)]
    (reset! movie-list (update-in @movie-list [id :quantity] dec))
    (reset! rented-ids (conj @rented-ids id))
    (reset! rented-list (assoc-in @rented-list [id] (assoc (@rented-list id) renter-name (get-due-date))))
    (write-file (movie-list-file) @movie-list)
    (write-file (rented-list-file) @rented-list)
    (write-file (rented-ids-file) @rented-ids)))

(defn rent-movie-by-name
  [movie-name  renter-name]
  (rent-movie (movie-id-by-name movie-name) renter-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Return rented movie to the store ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn return-rented-movie
  [id]
  (let [movie-list (movie-list)
        rented-list (rented-list)
        rented-ids (rented-ids)]
    (reset! movie-list (update-in @movie-list [id :quantity] inc))
    (reset! rented-list (dissoc @rented-list id))
    (reset! rented-ids (disj @rented-ids id))
    (write-file (movie-list-file) @movie-list)
    (write-file (rented-list-file) @rented-list)
    (write-file (rented-ids-file) @rented-ids)))

(defn return-rented-movie-by-name
  [movie-name]
  (return-rented-movie (movie-id-by-name movie-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions to retrive and update attributes of movies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-attribute-value
  [coll id attribute]
  (get-in @coll [id attribute]))

(defn- update-attribute
  [col id attribute new-value]
  (let [movie (get @col id)
        updated-movie (assoc-in movie [attribute] new-value)]
    (remove-movie id)
    (remove-mapping id (:name movie))
    (add-movie-db updated-movie id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Update operations on price and copies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn change-price
  [id new-price]
  (let [movie-list (movie-list)]
    (update-attribute movie-list id :price new-price)))

(defn add-copies
  [id extra-copies]
  (let [movie-list (movie-list)
        old-quant (find-quantity id)
        new-quant (+ old-quant extra-copies)]
    (update-attribute movie-list id :quantity new-quant)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




