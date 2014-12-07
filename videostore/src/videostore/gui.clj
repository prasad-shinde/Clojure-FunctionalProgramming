(ns gui.core
 (:gen-class)
 (:require [seesaw.core :as seesaw]
   [seesaw.dev :as dev]
   [assignment5.file-handling :refer :all]
   [assignment5.movies :refer :all]
   [assignment5.unique-id-generator :refer :all]))


(defn display
  "displays the contents in a window of specified size"
 [content width height]
 (let [window (seesaw/frame :title "Video Store"
 :content content
 :width width
 :height height)]
 (seesaw/show! window)))



(defn available-movies
  "The GUI for available movies, it provides ways to rent a particular movie and also remove it"
 []
 (let [ui-price-text (seesaw/text :columns 10 :editable? false)
       ui-quantity-text (seesaw/text :columns 10 :editable? false)
       ui-renter-name-text (seesaw/text
                       :editable? true
                       :columns 10)
       ui-movie-list (seesaw/listbox
                      :model (movie-names-set (movie-list))
                      :listen [:selection (fn [event] (let [m-name (seesaw/selection event)]
                        (seesaw/text! ui-price-text (find-price m-name))
                        (seesaw/text! ui-quantity-text (find-quantity m-name))))])
       ui-rent-button (seesaw/button
             :text "Rent"
             :listen [:action (fn [e] (let [m-name (seesaw/selection ui-movie-list)
                                            renter-name (seesaw/text ui-renter-name-text)]
                                        (rent-movie-by-name m-name renter-name)
                                        (seesaw/config! ui-movie-list :model (movie-names-set (movie-list)))))])
       ui-remove-movie-button (seesaw/button
                               :text "Remove"
                               :listen [:action (fn [e] (let [m-name (seesaw/selection ui-movie-list)]
                                                          (remove-movie-by-name m-name)
                                                          (seesaw/config! ui-movie-list :model (movie-names-set (movie-list)))))])]
 (seesaw/left-right-split (seesaw/scrollable ui-movie-list) (seesaw/flow-panel :items [(seesaw/top-bottom-split
                                         (seesaw/left-right-split (seesaw/label :text "Price") ui-price-text)
                                         (seesaw/left-right-split (seesaw/label :text "Quantity") ui-quantity-text))
                                                                   (seesaw/left-right-split (seesaw/label :text "Renter") ui-renter-name-text)
                                                                 ui-rent-button
                                                                   ui-remove-movie-button]))))


(defn ui-rented-movies
  "GUI for displaying rented movies and also returning the rented movies back to store"
 []
 (let [ui-due-date-text (seesaw/text :columns 10 :editable? false)
       ui-renter-name-text (seesaw/text :editable? false :columns 10)
       ui-movie-list (seesaw/listbox
                      :model (rented-movies)
                      :listen [:selection (fn [event] (let [m-name (seesaw/selection event)]
                        (seesaw/text! ui-renter-name-text (renter-name-by-movie m-name))
                        (seesaw/text! ui-due-date-text (due-date-by-movie m-name))))])
       ui-return-button (seesaw/button
             :text "Return"
             :listen [:action (fn [e] (let [m-name (seesaw/selection ui-movie-list)]
                                        (return-rented-movie-by-name m-name)
                                        (seesaw/config! ui-movie-list :model (rented-movies))))])]
 (seesaw/left-right-split (seesaw/scrollable ui-movie-list) (seesaw/flow-panel :items [(seesaw/top-bottom-split
                                         (seesaw/left-right-split (seesaw/label :text "Renter Name") ui-renter-name-text)
                                         (seesaw/left-right-split (seesaw/label :text "Due Date") ui-due-date-text))
                                                                 ui-return-button]))))

(defn ui-add-movie
  "small UI which provides inputs to add a movie"
  []
  (let [m-name (seesaw/input "movie name")
        quant (read-string (seesaw/input "quantity"))
        price (read-string (seesaw/input "Price"))]
    (add-movie m-name price quant)))


(defn video-store
  "Entry point to the application. Starts the video rental program"
  []
  (let [ui-add-movie-button (seesaw/button
                             :text "Add Movie"
                             :listen [:action (fn [e] (ui-add-movie))])
        ui-show-avail-movies (seesaw/button
                              :text "Available Movies"
                              :listen [:action (fn [e] (display (available-movies) 800 200))])
        ui-show-rented-movies (seesaw/button
                              :text "Rented Movies"
                              :listen [:action (fn [e] (display (ui-rented-movies) 700 200))])]
    (seesaw/flow-panel :items [ui-add-movie-button ui-show-avail-movies ui-show-rented-movies])))

(display (video-store) 500 100)



