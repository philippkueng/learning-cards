(ns generate-cards
  (:require [clj-pdf.core :as pdf]
            [clj-http.client :as client]
            [hickory.core :as hickory]
            [hickory.select :as s]
            [com.rpl.specter :as sp]))

(defn parse-url [url]
  (let [response (client/get url)
        html (:body response)
        parsed (hickory/parse html)
        hickory-tree (hickory/as-hickory parsed)]
    hickory-tree))

(defn parse-file [file-name]
  (let [html (slurp file-name)
        parsed (hickory/parse html)
        hickory-tree (hickory/as-hickory parsed)]
    hickory-tree))

(def words
  (->> (parse-url "https://www.gut1.de/grundwortschatz/grundwortschatz-200/")
    (s/select (s/tag :p))
    (sp/select [sp/ALL :content])
    (filter #(= 1 (-> % first count)))
    (map #(nth % 2))
    (map #(clojure.string/split % #",  "))
    flatten
    (map #(clojure.string/split % #"  "))
    flatten
    (remove #(= "–" %))))

(defn generate-cell [word]
  [:pdf-cell {:align :center :valign :middle :min-height 80}
   [:phrase {:size (condp > (count word)
                     5 24
                     10 20
                     15 18
                     20 15
                     40 10) :style :bold} word]])

(defn deal-with-odd-row [column-count words-of-row]
  (let [cells-to-pad (- column-count (count words-of-row))]
    (concat
      (into [] (map generate-cell words-of-row))
      (->> (range cells-to-pad)
        (map (fn [c] (generate-cell "")))
        (into [])))))

(comment
  (deal-with-odd-row 4 (list "hi"))

  (generate-cell "")

  )

(defn create-flashcards [words]
  (let [column-count 4]
    (->> words
      (partition column-count column-count nil)                                   ;; needed so we get the last few words too
      (map (fn [group]
             (if (= column-count (count group))
               (->> group
                 (map generate-cell)
                 (into []))

               (deal-with-odd-row column-count group))))
      (into [])
      (cons [10 10 10 10])
      (cons {:width-percent 100})
      (cons :pdf-table)
      (into []))))

(defn generate-flashcards-pdf [words output-file]
  (if (empty? words)
    (println "Error: Word list is empty. Cannot generate PDF.")
    (pdf/pdf
      [{:size :a4
        :orientation :portrait
        :pages true}
       (create-flashcards words)]
      output-file)))

(comment

  (->> (parse-url "https://www.gut1.de/grundwortschatz/grundwortschatz-200/")
    (s/select (s/tag :p)))

  ;; 200_words.html in this case is a subset of the page which I copied via the browser. It just so happens
  ;; that the page only used <p> tags for the data and hence `parse-file` and `parse-url` can be used interchangeably.
  (->>
    (parse-file "200_words.html")
    (s/select (s/tag :p)))


  )

(generate-flashcards-pdf words "flashcards.pdf")
