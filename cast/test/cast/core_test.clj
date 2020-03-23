(ns cast.core-test
  (:require [clojure.test :refer :all]
            [cast.core :refer :all]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(def header
  "{\"version\": 2, \"width\": 141, \"height\": 37, \"timestamp\": 1572717372, \"env\": {\"SHELL\": \"/bin/zsh\", \"TERM\": \"xterm-256color\"}}")

(def success-line
  [59.089491, "o", "\u001b[?25l\u001b[93m\u001b[1;92HPassed all tests                                  \u001b[2;92HFinished at 18:56:56.453 (run time: 0,027s)       \u001b[3;92H*********************************************     \u001b[4;92H*************** Running tests ***************     \u001b[5;92H:reloading (diamond-kata.core-test)               \u001b[6;92H                                                  \u001b[7;92HTesting diamond-kata.core-test                    \u001b[8;92H                                                  \u001b[9;92HRan 1 tests containing 1 assertions.              \u001b[10;92H0 failures, 0 errors.                             \u001b[11;92H                                                  \u001b[12;92HPassed all tests                                  \u001b[13;92HFinished at 18:57:00.644 (run time: 0,032s)       \u001b[14;92H*********************************************     \u001b[15;92H*************** Running tests ***************     \u001b[16;92H                                                  \u001b[m\u001b[93m\u001b[17;92H                                                  \u001b[93m\u001b[16;92H\u001b[93m:reloading"])

(def green "\u001b[33;92H\u001B[32mGREEN\u001B[0m")
(def green-line [60.0 "o" green])
(def red "\u001b[34;92H\u001B[31mRED\u001B[0m")
(def red-line [60.0 "o" red])

(defn sanitize [s]
  (string/replace s #"\W+" " "))

(defn enhance [{:keys [slurp-file] :as context}]
  {:path "/tmp/1-enhanced.cast"
   :content (mapcat (fn [line]
                      (let [json-data (json/read-str line)]
                        (if (vector? json-data)
                          (let [[$time flag s] json-data]
                            [(assoc green-line 0 $time)
                             (assoc red-line 0 $time)
                             [$time
                              flag
                              (clojure.string/replace s
                                                      #"Passed all tests"
                                                      (str green
                                                           red
                                                           "\u001B[32m"
                                                           "Passed all tests"
                                                           "\u001B[0m"))]])
                          [json-data]))) 
                    (string/split (slurp-file context) #"\n"))
   :type :write-file})

(deftest api-test
  (is (= {:path "/tmp/1-enhanced.cast"
          :content [[" version 2 "
                     " width 141 "
                     " height 37 "
                     " timestamp 1572717372 "
                     " env SHELL bin zsh TERM xterm 256color "]
                    ["59 089491"
                     "o"
                     " 25l 93m 1 92H 32mPassed all tests 0m 2 92HFinished at 18 56 56 453 run time 0 027s 3 92H 4 92H Running tests 5 92H reloading diamond kata core test 6 92H 7 92HTesting diamond kata core test 8 92H 9 92HRan 1 tests containing 1 assertions 10 92H0 failures 0 errors 11 92H 12 92H 32mPassed all tests 0m 13 92HFinished at 18 57 00 644 run time 0 032s 14 92H 15 92H Running tests 16 92H m 93m 17 92H 93m 16 92H 93m reloading"]]
          :type :write-file}
         (-> (enhance {:slurp-file (constantly (str header "\n" success-line))
                       :path "/tmp/1.cast"})
             (update :content (partial mapv
                                       (partial mapv
                                                sanitize)))))))



(defn enhance! [context]
  (let [{:keys [path content]}
        (enhance (assoc context :slurp-file #(slurp (:path %))))]
    (spit path (write-json-line (first content)))
    (doseq [line (rest content)]
      (spit path (str (json/write-str line) "\n") :append true))))

(deftest integration-test
  (is (= nil
         (enhance! {:path "resources/diamond-kata-clojure.cast"}))))
