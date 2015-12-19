(ns om-tut.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.events :as events]
            [goog.dom :as g-dom]
            [clojure.string :as s]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(def app-config { :current-level 0
                  :levels [{:name "Level 1"
                            :hero-pos {:x 6 :y 3}
                            :structure [[2 2 2 2 1 1 1 1]
                                        [1 0 0 0 0 0 0 1]
                                        [1 0 2 0 0 3 0 1]
                                        [1 0 3 0 0 2 4 1]
                                        [1 0 0 0 0 0 0 1]
                                        [1 1 1 1 1 1 1 1]]}
                           {:name "Level 2"
                            :hero-pos {:x 6 :y 3}
                            :structure [[1 1 1 1 0 0 0 0] 
                                        [1 0 0 1 1 1 1 1] 
                                        [1 0 2 0 0 3 0 1] 
                                        [1 0 3 0 0 2 4 1] 
                                        [1 1 1 0 0 1 1 1] 
                                        [0 0 1 1 1 1 0 0]]}] })


(defn proc-rows [row]
  (map (fn [tile] { :elements (if (= tile 0) [tile] [0 tile]) }) row))

(defn proc-structure [structure] 
  (map proc-rows structure))

(defn init-state [config level]
  (let [lev (get (:levels config) level)]
    (assoc lev 
           :structure
           (proc-structure (:structure lev)))))

(defonce app-state (atom (init-state app-config 0)))

(defn move-hero [x y]
  (let [hero-pos (:hero-pos @app-state)
        _ (println "1111111" @app-state)
        a (update-in @app-state [:structure (:x hero-pos) (:y hero-pos) :elements] drop-last)
        b (update-in a [:structure (+ (:x hero-pos) x) (+ (:y hero-pos) y) :elements] #(conj %1 4))]
      (reset! app-state b)
      (swap! app-state assoc :hero-pos {:x (+ (:x hero-pos) x) :y (+ (:y hero-pos) y)})
      ))

(defn on-keydown [key-code]
  (cond 
    (= key-code 37) (move-hero -1 0)
    (= key-code 38) (move-hero 0 -1)
    (= key-code 39) (move-hero 1 0)
    (= key-code 40) (move-hero 0 1)))


(defonce setup-listeners 
  (events/listen (g-dom/getWindow) "keydown" #(on-keydown (.-keyCode %1))))






(defn current-level [data] 
  (get (:levels data) (:current-level data)))
     
(defn render-row [row] (s/join " " 
                               (map (comp last :elements) row)))
                          
(om/root
  (fn [data owner]
    (om/component
      (dom/div #js {:onClick (fn [e] (println "om click handler"))}
        (dom/h2 nil (:name data))
        (dom/pre nil (s/join "\n"
                       (map render-row
                            (:structure data)))))
      ))
  app-state
  {:target (. js/document (getElementById "app"))})


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
