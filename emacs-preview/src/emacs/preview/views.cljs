(ns emacs.preview.views
  (:require
   [re-frame.core :as re-frame]
   [emacs.preview.subs :as subs]
   [emacs.preview.data :refer [org-data]]))

(defn render-org-element [element]
  (case (:type element)
    "org-data" (:contents element)

    "section"
    [:div.section
     (for [el (:contents element)]
       ^{:key (str (random-uuid))}
       [render-org-element el])]))

(defn main-panel []
  [:main.prose.mx-auto
   ()
   ])
