(ns cast.core
  (:require [clojure.data.json :as json]
            [clojure.string :as string]))

(def green "\u001B[32m")
(def red "\u001B[31m")
(def reset "\u001B[0m")
(def cyan "\u001B[36m")
(def black-bold "\033[1;30m")
(def purple "\u001B[35m")

(def timestamp-matcher #".*shed at (\d\d:\d\d:\d\d.\d\d\d).*")

(defn write-json-line [line]
  (str (json/write-str line
                       :escape-unicode false
                       :escape-js-separators false
                       :escape-slash false) "\n"))

(defn insert [counter color row]
  (let [int-row (clojure.edn/read-string row)]
    (swap! counter update 0 conj {:color color
                                  :row   int-row})))

(defn color-passed-all-tests [counter s]
  (string/replace s
                  #"(\d+);(\d+)HPassed all tests"
                  (fn [[_ row column]]
                    (insert counter :green row)
                    (str row
                         ";"
                         column
                         "H"
                         green
                         "Passed all tests"
                         reset))))

(defn color-failed-assertions [counter s]
  (string/replace s
                  #"(\d+);(\d+)HFailed (\d) of (\d) assertions"
                  (fn [[_ row column $1 $2]]
                    (insert counter :red row)
                    (str row
                         ";"
                         column
                         "H"
                         red
                         "Failed "
                         $1
                         " of "
                         $2
                         " assertions"
                         reset))))

(defn color-error-reload-test [counter s]
  (string/replace s
                  #"(\d+);(\d+)H:error-while-loading diamond-kata.core-test"
                  (fn [[_ row column]]
                    (insert counter :red row)
                    (str row
                         ";"
                         column
                         "H"
                         red
                         ":error-while-loading diamond-kata.core-test"
                         reset))))

(defn count-cycles [counter s]
  (if-let [[_ timestamp-str] (re-matches timestamp-matcher s)]
    (let [timestamp (clojure.instant/read-instant-date (str "2019-12-20T"
                                                            timestamp-str
                                                            "-00:00"))
          millisecond (.getTime timestamp)]
      (comment prn timestamp millisecond)
      (swap! counter (fn [m]
                       (dissoc (if-let [best-candidate (last (sort-by :row (get m 0)))]
                                 (do (when (or true (= millisecond 1576868211666))
                                       (prn timestamp
                                            (get m millisecond)
                                            best-candidate
                                            (get m 0)
                                            millisecond
                                            :red (filter (comp (partial not= :green) :color) (vals m))
                                            :green (filter (comp (partial = :green) :color) (vals m))))
                                     (if-let [{:keys [row]} (get m millisecond)]
                                       (if (<= row (:row best-candidate))
                                         (assoc m millisecond best-candidate)
                                         m)
                                       (assoc m millisecond best-candidate)))
                                 m)
                               0)))
      s)
    s))

(defn move-to-statistics-place [row]
  (str "\u001b[" row ";92H"))

(defn print-avg-t [m]
  (if-let [$keys (seq (keys m))]
    (str (format "%.2f"
                 (double (/ (- (last $keys) (first $keys))
                            (count $keys)
                            1000)))
         " s      ")))

(defn print-cycle-count-statistics [row counter]
  (let [c (dissoc @counter 0)]
    (str (move-to-statistics-place row)
         reset
         "CYCLE-COUNT: "
         red
         "red: "
         (count (filter (comp (partial not= :green) :color) (vals c)))
         reset
         ", "
         green
         "green: "
         (count (filter (comp (partial = :green) :color) (vals c)))
         reset
         "    ")))

(defn print-cycle-time-statistics [row counter]
  (let [c (dissoc @counter 0)]
    (str (move-to-statistics-place row)
         purple
         "AVG-TDD-STATE-TIME: "
         black-bold
         (print-avg-t c)
         reset)))

(defn make-color-cast [counter]
  (comp (partial count-cycles counter)
        (partial color-failed-assertions counter)
        (partial color-error-reload-test counter)
        (partial color-passed-all-tests counter)))


(defn enhance [{:keys [slurp-file color-cast counter] :as context}]
  {:path    "/tmp/1-enhanced.cast"
   :content (mapcat (fn [line]
                      (let [json-data (json/read-str line)]
                        (if (vector? json-data)
                          (let [[$time flag s] json-data]
                            (if (re-matches timestamp-matcher s)
                              [[$time flag (color-cast s)]
                               [$time "o" (print-cycle-count-statistics 19 counter)]
                               [$time "o" (print-cycle-time-statistics 20 counter)]
                               [$time flag (color-cast s)]]
                              [[$time flag (color-cast s)]]))
                          [json-data])))
                    (string/split (slurp-file context) #"\n"))
   :type    :write-file})

(defn enhance! [context]
  (let [counter (atom (sorted-map))
        {:keys [path content]}
        (enhance (-> context
                     (assoc :slurp-file #(slurp (:path %)))
                     (assoc :counter counter)
                     (assoc :color-cast (make-color-cast counter))))]
    (spit path (write-json-line (first content)))
    (doseq [line (rest content)]
      (spit path (str (json/write-str line) "\n") :append true))
    counter))

(comment
  (def c (enhance! {:path "resources/diamond-kata-clojure.cast"})))