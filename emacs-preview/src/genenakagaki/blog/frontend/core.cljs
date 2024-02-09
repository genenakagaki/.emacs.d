(ns genenakagaki.blog.frontend.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [genenakagaki.blog.frontend.events :as events]
   [genenakagaki.blog.frontend.views :as views]
   [genenakagaki.blog.frontend.preview :as preview]
   [genenakagaki.blog.frontend.config :as config]
   ))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [preview/main-panel] root-el)))

(defn init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))
