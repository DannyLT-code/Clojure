(ns main
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

;;
(defn get-filenames-in-directory [directory-path]
  (->> (file-seq (io/file directory-path))
       (filter #(.isFile ^java.io.File %))
       (map #(.getName ^java.io.File %))))
;;
(defn measure-time [func]
  (let [start-time (System/currentTimeMillis)
        _ (func)
        end-time (System/currentTimeMillis)]
    (- end-time start-time)))
;;
(def current-pos (atom [0 0]))
;;
(defn read-carousel-data [filename]
  (with-open [rdr (io/reader filename)]
    (let [lines (line-seq rdr)
          carousel-data (mapv (fn [line]
           (let [line (subs line 1 (dec (count line))) 
              items (string/split line #"\)\s\(")]
             (mapv (fn [item]
                (let [[name quantity price] (string/split item #" ")]
                  {:name name
                   :quantity (Integer/parseInt quantity)
                   :price (Double/parseDouble price)})) items))) lines)]
      (if (or (< (count carousel-data) 20) (< (count (first carousel-data)) 5)) 
        (do
          (println (str "Error: Carousel data in " filename " does not meet minimum size requirements of 20 rows and 5 columns."))
          nil) 
        carousel-data)))) 
;;
(defn write-carousel-data [filename carousel-data]
  (with-open [wtr (io/writer filename)]
    (doseq [row carousel-data]
      (let [row-string (->> row
         (map (fn [item] (str "(" (:name item) " " (:quantity item) " " (:price item) ")")))
          (string/join " "))]
        (.write wtr (str row-string "\n"))))))
;;
(defn read-commands [filename]
  (with-open [rdr (io/reader filename)]
    (->> (line-seq rdr)
         (map #(string/trim %))
         (doall))))
;;
(defn find-carousel-position [carousel-data product]
  (let [rows (count carousel-data)
        cols (count (first carousel-data))]
    (loop [row 0 
           col 0]
      (cond
        (= product (get-in carousel-data [row col :name])) 
        (let [dist-row (min 
          (mod (- row (@current-pos 0)) rows) 
          (mod (- (@current-pos 0) row) rows)) 
         dist-col (min
           (mod (- col (@current-pos 1)) cols) 
           (mod (- (@current-pos 1) col) cols))] 
          [row col (+ dist-row dist-col)]) 
        :else 
        (let [next-row (if (= col (dec cols)) (mod (inc row) rows) row)
              next-col (mod (inc col) cols)] 
          (recur next-row next-col)))))) 
;;
(defn add-item [carousel-data product quantity]
  (let [[pos-row pos-col] @current-pos]
    (if product
      (let [[row col moves] (find-carousel-position carousel-data product)]
        (if (and row col)
          (do
            (println (str "Found " product " at [" row ", " col "]. Moves: " moves))
            (swap! current-pos assoc 0 row 1 col) 
            (assoc-in carousel-data [row col :quantity] (+ (get-in carousel-data [row col :quantity]) quantity)))
          (do
            (println (str "Product " product " not found. Adding to current position."))
            (assoc-in carousel-data [pos-row pos-col :quantity] (+ (get-in carousel-data [pos-row pos-col :quantity]) quantity)))))
      (do
        (println "Adding to current position.")
        (assoc-in carousel-data [pos-row pos-col :quantity] (+ (get-in carousel-data [pos-row pos-col :quantity]) quantity))))))
;;
(defn remove-item [carousel-data product quantity]
  (let [[pos-row pos-col] @current-pos]
    (if product
      (let [[row col moves] (find-carousel-position carousel-data product)]
        (if (and row col)
          (do
            (println (str "Found " product " at [" row ", " col "]. Moves: " moves))
            (swap! current-pos assoc 0 row 1 col) 
            (let [current-quantity (get-in carousel-data [row col :quantity])
                  new-quantity (max 0 (- current-quantity quantity))]
              (cond 
                (= current-quantity 0) 
                (do (println (str "Product " product " is out of stock"))
                    (assoc-in carousel-data [row col :quantity] new-quantity))
                
                (< current-quantity quantity) 
                (do (println (str "Took only " current-quantity " from " product ", now out of stock"))
                    (assoc-in carousel-data [row col :quantity] new-quantity))

                :else 
                (assoc-in carousel-data [row col :quantity] new-quantity))))
          (do
            (println (str "Product " product " not found. Removing from current position."))
            (let [current-quantity (get-in carousel-data [pos-row pos-col :quantity])
                  new-quantity (max 0 (- current-quantity quantity))]
              (cond 
                (= current-quantity 0) 
                (do (println "Current position is out of stock.")
                    (assoc-in carousel-data [pos-row pos-col :quantity] new-quantity))
                
                (< current-quantity quantity) 
                (do (println (str "Took only " current-quantity " from current position, now out of stock"))
                    (assoc-in carousel-data [pos-row pos-col :quantity] new-quantity))
                
                :else 
                (assoc-in carousel-data [pos-row pos-col :quantity] new-quantity))))))
      (do
        (println "Removing from current position.")
        (let [current-quantity (get-in carousel-data [pos-row pos-col :quantity])
              new-quantity (max 0 (- current-quantity quantity))]
          (cond 
            (= current-quantity 0) 
            (do (println "Current position is out of stock.")
                (assoc-in carousel-data [pos-row pos-col :quantity] new-quantity))
            
            (< current-quantity quantity) 
            (do (println (str "Took only " current-quantity " from current position, now out of stock"))
                (assoc-in carousel-data [pos-row pos-col :quantity] new-quantity))
            
            :else 
            (assoc-in carousel-data [pos-row pos-col :quantity] new-quantity)))))))
;;
(defn move-up [carousel-data]
  (let [[row col] @current-pos
        next-row (if (= row 0) (dec (count carousel-data)) (dec row))] 
    (swap! current-pos assoc 0 next-row)
    carousel-data)) 
;;
(defn move-down [carousel-data]
  (let [[row col] @current-pos
        next-row (mod (inc row) (count carousel-data))] 
    (swap! current-pos assoc 0 next-row)
    carousel-data)) 
;;
(defn move-left [carousel-data]
  (let [[row col] @current-pos
        next-col (if (= col 0) (dec (count (first carousel-data))) (dec col))] 
    (swap! current-pos assoc 1 next-col)
    carousel-data)) 
;;
(defn move-right [carousel-data]
  (let [[row col] @current-pos
        next-col (mod (inc col) (count (first carousel-data)))] 
    (swap! current-pos assoc 1 next-col)
    carousel-data))
;;
(defn process-command [carousel-data command]
  (let [command-parts (string/split command #" ")
        [action & args] command-parts]
    (cond
      (= action "Agregar") 
        (do
          (println "Adding item...")
          (let [[maybe-product maybe-quantity] args
                quantity (if (and maybe-quantity (re-matches #"\d+" maybe-quantity)) 
                  (Integer/parseInt maybe-quantity) 
                  (if (and maybe-product (re-matches #"\d+" maybe-product)) 
                  (Integer/parseInt maybe-product) 
                   1))
                product (if (= maybe-product (str quantity)) 
                  nil 
                  maybe-product)]
            (add-item carousel-data product quantity)))

      (= action "Remover") 
        (do
          (println "Removing item...")
          (let [[maybe-product maybe-quantity] args
                quantity (if (and maybe-quantity (re-matches #"\d+" maybe-quantity)) 
                  (Integer/parseInt maybe-quantity) 
                  (if (and maybe-product (re-matches #"\d+" maybe-product)) 
                  (Integer/parseInt maybe-product) 
                  1))
                product (if (= maybe-product (str quantity)) 
                  (get-in carousel-data [0 0 :name]) 
                  maybe-product)]
            (remove-item carousel-data product quantity)))

      (= action "Mover") 
        (do
          (println "Moving...")
          (let [direction (first args)]
            (cond
              (= direction "right") (move-right carousel-data)
              (= direction "left") (move-left carousel-data)
              (= direction "up") (move-up carousel-data)
              (= direction "down") (move-down carousel-data)
              :else carousel-data)))

      :else (do
              (println "Unknown command.")
              carousel-data))))
;;
(defn total-value [carousel-data]
  (let [total (->> carousel-data
                   (flatten)
                   (map :price)
                   (reduce +))]
    (println (str "Total value of the carousel: $" total))))
;;
(defn low-stock-items [carousel-data]
  (println "Items low in stock:")
  (doseq [row (range (count carousel-data))
          col (range (count (first carousel-data)))]
    (let [item (get-in carousel-data [row col])]
      (when (< (:quantity item) 5)
        (println (str (:name item) ", Quantity: " (:quantity item) ", Location: [" row ", " col "]"))))))
;;
(defn print-menu []
  (println "\nSelect an operation:")
  (println "1. Run base test")
  (println "2. Work on a single carousel")
  (println "3. Work on multiple carousels")
  (println "4. Exit"))
;;
(defn get-user-input [start end]
  (println (str "Enter your choice (" start "-" end "): "))
  (let [choice (read-line)]
    (let [choice-num (try 
      (Integer/parseInt choice)
      (catch NumberFormatException e
       nil))]
      (if (and (>= choice-num start) (<= choice-num end))
        choice-num
        (do
          (println (str "Invalid input. Please enter a number between " start " and " end "."))
          (recur start end))))))
;;
(defn base-test []
  (println "Choose the processing mode: ")
  (println "1. Run in parallel")
  (println "2. Run sequentially")
  (let [mode (get-user-input 1 2)
        filenames (get-filenames-in-directory "./carruseles")
        commands (read-commands "comandos.txt")]
    (cond
      (= mode 1)
      (let [start-time (System/currentTimeMillis)]
        (doall (pmap (fn [filename]
                       (let [id (subs filename 0 (- (count filename) 4)) 
                             initial-carousel-data (read-carousel-data (str "./carruseles/" filename))]
                         (when initial-carousel-data 
                           (let [final-carousel-data (reduce (fn [carousel-data command]
                              (process-command carousel-data command)) 
                              initial-carousel-data commands)]
                             (write-carousel-data (str "./carruseles/" filename) final-carousel-data)))))
                     filenames))
        (println "Parallel processing completed.")
        (println (str "Execution time: " (- (System/currentTimeMillis) start-time) " ms")))
      
      (= mode 2)
      (let [start-time (System/currentTimeMillis)]
        (doseq [filename filenames]
          (let [id (subs filename 0 (- (count filename) 4))
                initial-carousel-data (read-carousel-data (str "./carruseles/" filename))]
            (when initial-carousel-data 
              (let [final-carousel-data (reduce (fn [carousel-data command]
                (process-command carousel-data command)) 
                initial-carousel-data commands)]
                (write-carousel-data (str "./carruseles/" filename) final-carousel-data)))))
        (println "Sequential processing completed.")
        (println (str "Execution time: " (- (System/currentTimeMillis) start-time) " ms"))))))
;;
(defn work-on-single-carousel [id]
  (let [filename (str id ".txt")
        initial-carousel-data (read-carousel-data (str "./carruseles/" filename))]
    (if (and (>= (count initial-carousel-data) 20)
             (>= (count (first initial-carousel-data)) 5))
      (do
        (let [commands (read-commands "comandos.txt")
              final-carousel-data (reduce (fn [carousel-data command]
                (let [updated-carousel-data (process-command carousel-data command)]
                   (when (string/starts-with? command "Mover")
                     (let [[_ direction] (string/split command #" ")]
                        (println (str "Moved " direction ". Current position:" @current-pos))))
                     updated-carousel-data))
                  initial-carousel-data commands)]
          (println "Current position item:")
          (let [[row col] @current-pos
                item (get-in final-carousel-data [row col])]
            (println (str "Name: " (:name item) ", Quantity: " (:quantity item) ", Price: " (:price item))))
          (total-value final-carousel-data)
          (low-stock-items final-carousel-data)
          (write-carousel-data (str "./carruseles/" filename) final-carousel-data))
        true) 
      (do
        (println (str "Carousel " id " does not meet the size requirements. Please choose another one."))
        false))))
;;
(defn work-on-single-carousel-id []
  (println "Please input the ID of the carousel:")
  (let [id (read-line)]
    (if (work-on-single-carousel id)
      (println "Carousel processed.")
      (do
        (println "Carousel could not be processed due to incorrect ID or size mismatch.")
        (recur)))))
;;
(defn work-on-multiple-carousels []
  (loop []
    (println "Please input the IDs of the 5 carousels separated by commas:")
    (let [ids-string (read-line)
          ids (mapv string/trim (string/split ids-string #","))]
      (if (not= 5 (count ids))
        (do
          (println "Incorrect number of IDs. Please input exactly 5 IDs.")
          (recur))
        (do
          (println "Choose the processing mode: ")
          (println "1. Run in parallel")
          (println "2. Run sequentially")
          (let [mode (get-user-input 1 2)]
            (let [start-time (System/currentTimeMillis)
                  results (if (= mode 1)
                            (doall (pmap work-on-single-carousel ids))
                            (mapv work-on-single-carousel ids))
                  end-time (System/currentTimeMillis)]
              (println (str "Processing took " (- end-time start-time) " ms."))
              (if (every? identity results)
                (println "All carousels processed.")
                (do
                  (println "Some carousels could not be processed due to incorrect IDs or size mismatch.")
                  (recur))))))))))
;;
(defn calculate-total-value [carousel-data]
  (reduce + (map (fn [row] (reduce + (map #(* (:quantity %) (:price %)) row))) carousel-data)))

;;
(defn sum-total-value-all-carousels [filenames]
  (let [total-value (reduce + (map (fn [filename]
     (let [carousel-data (read-carousel-data (str "./carruseles/" filename))]
        (calculate-total-value carousel-data)))
       filenames))]
    (println "Total value of all carousels: " total-value)))
;;
(defn top-10-percent-carousels [filenames]
  (let [carousels-values (for [filename filenames
    :let [carousel-id (subs filename 0 (- (count filename) 4))
       carousel (read-carousel-data (str "./carruseles/" filename))]]
     (when carousel {:id carousel-id :value (calculate-total-value carousel)}))
        carousels-values (filter identity carousels-values)
        sorted-carousels (sort-by :value > carousels-values)
        top-ten-percent-count (max 1 (int (* 0.1 (count sorted-carousels))))]
    (println "Top 10% valued carousels:")
    (doseq [carousel (take top-ten-percent-count sorted-carousels)]
      (println (str "ID: " (:id carousel) ", Total value: " (:value carousel))))))
;;
(defn low-stock-items-in-carousel [carousel]
  (for [row (range (count carousel))
        col (range (count (first carousel)))
        :let [item (get-in carousel [row col])]
        :when (< (:quantity item) 5)]
    item))
;;
(defn low-stock-carousels [filenames]
  (doseq [filename filenames]
    (let [id (subs filename 0 (- (count filename) 4))
          carousel (read-carousel-data (str "./carruseles/" filename))
          low-stock-items (low-stock-items-in-carousel carousel)]
      (when (not-empty low-stock-items)
        (println (str "Carousel ID: " id " has low stock on the following items:"))
        (doseq [item low-stock-items]
          (println (:name item)))))))
;;
(defn -main []
  (print-menu)
  (let [user-choice (get-user-input 1 4)]
    (cond
      (= user-choice 1) (do (base-test) (-main))
      (= user-choice 2) (do (work-on-single-carousel-id) (-main))
      (= user-choice 3) (do (work-on-multiple-carousels) (-main))
      (= user-choice 4) 
        (do
          (let [filenames (get-filenames-in-directory "./carruseles")]
            (sum-total-value-all-carousels filenames)
            (top-10-percent-carousels filenames)
            (low-stock-carousels filenames))
          (println "Exiting the program.")))))
;;
(-main)
