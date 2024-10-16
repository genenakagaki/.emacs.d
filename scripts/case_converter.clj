#!/usr/bin/env bb

(defn kebab? [text]
  (some? (re-find #"-" text)))

(defn camel? [text]
  (some? (re-find #"^[a-z].*[A-Z]" text)))

(defn pascal? [text]
  (some? (re-find #"^[A-Z].*[a-z]" text)))

(let [[case-type
       ;; "snake"
       ;; "camel"
       ;; "pascal"
       text
       ;; "this-is-a-test"
       ;; "ThisIsATest"
       ;; "thisIsATest"
       ]
      *command-line-args*
      ]
  (let [words (cond
                (kebab? text) (str/split text #"-")
                (pascal? text) (->> (re-seq #"[A-Z][a-z]*" text)
                                    (map str/lower-case))
                (camel? text) (cons (re-find #"^[a-z]*" text)
                                    (->> (re-seq #"[A-Z][a-z]*" text)
                                         (map str/lower-case))))
        result (-> (case case-type
                     "snake" (->> words
                                  (str/join "-"))
                     "camel" words
                     "pascal" words))]
    (println result)
    (shell/sh "pbcopy" :in result)
    )
  )

