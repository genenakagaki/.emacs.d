(ns emacs.preview.views
  (:require
   [cljs.pprint :refer [pprint]]
   [clojure.string :as s]
   [clojure.walk :as w]
   [emacs.preview.data :refer [image-data org-data]]
   [emacs.preview.subs :as subs]
   [re-frame.core :as re-frame]))

(defn replace-p-with-span [element-list]
  (->> element-list
       (map #(if (= (first %) :p)
               (into [] (cons :span (rest %)))
               %))))

(defn org-element->html [element]
  (let [contents (:contents element)
        props (:props element)]
    (case (:type element)
      "org-data" (into [] (cons :main.prose.mx-auto contents)) 

      "property-drawer" nil
      "node-property" nil
      "keyword" nil

      "section" (into [] (cons :div.section contents))

      "headline"
      (let [tag (keyword (str "h" (:level props)))]
        [:<>
         (into [] (cons tag (replace-p-with-span (:title props))))
         (into [] (cons :div contents))])

      "plain-list"
      (let [tag (if (= (:type props) "ordered") 
                  :ol
                  :ul)]
        (into [] (cons tag contents)))
      "item" (into [] (cons :li (replace-p-with-span contents)))
      "src-block" [:pre [:code (:value props)]]

      "link"
      (let [link (into [] (concat [:a {:src (:raw-link props)}]
                                  (replace-p-with-span contents)))
            post-blank (s/join (repeat (:post-blank props) " "))]
        (if (empty? post-blank)
          link
          [:<>
           link
           [:span post-blank]]))
      "paragraph" (into [] (cons :p contents))
      "bold" (into [] (cons :span.font-bold (replace-p-with-span contents)))
      "code" [:code (:value props)]

      (print (str "not handled: " (:type element))))))

(defn org->html [org-data]
  (w/postwalk
   (fn [form]
     (if (and (map? form)
              (contains? form :props))
       (org-element->html form)
       form))
   org-data))

(defn main-panel []
  (cond
    (some? org-data) (org->html org-data)
    (some? image-data) [:div.absolute.h-full.w-full.flex.bg-gray-700
                        [:img.m-auto {:src image-data}]]
    :else [:div "no data"]))
