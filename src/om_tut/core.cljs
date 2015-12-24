(ns om-tut.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.events :as events]
            [goog.dom :as g-dom]
            [clojure.string :as s]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(def app-config {:current-level 0
                 :levels [{:name "Level 1"
                           :hero-pos {:x 6 :y 3}
                           :structure [[1 1 1 1 1 1 1 1]
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
                                       [1 1 1 0 0 0 1 1] 
                                       [0 0 1 1 1 1 0 0]]}
                          {:name "Level 3"
                           :hero-pos {:x 2 :y 2}
                           :structure [[1 1 1 1 1 1 1 1] 
                                       [1 1 1 0 0 0 1 1] 
                                       [1 2 4 3 0 0 1 1] 
                                       [1 1 1 0 3 2 1 1] 
                                       [1 2 1 1 3 0 1 1] 
                                       [1 0 0 0 2 0 1 1]
                                       [1 3 0 3 3 3 2 1]
                                       [1 0 0 0 2 0 0 1]
                                       [1 1 1 1 1 1 1 1]]}]})

(defn proc-rows [row]
  (mapv (fn [tile] {:elements (if (= tile 0) [tile] [0 tile]) }) row))

(defn proc-structure [structure] 
  (mapv proc-rows structure))

(defn init-state [config]
  (let [lev (get (:levels config) (:current-level config))]
    (assoc lev 
           :structure
           (proc-structure (:structure lev))
           :current-level (:current-level config))))

(defonce app-state (atom (init-state app-config)))

(defn tile [x y]
  (get-in @app-state [:structure y x :elements]))

(defn move-is-valid? [move]
  (let [last-val (last (tile (:x move) (:y move)))]
    (if (or (= 0 last-val) (= 2 last-val)) true false)))

(defn sobokan-cords [new-pos x y]
  {:x (+ (:x new-pos) x) :y (+ (:y new-pos) y)})

(defn move-is-sobokan? [old-pos new-pos x y]
  (if-let [sobokan (= 3 (last (tile (:x new-pos) (:y new-pos))))]
    (if (move-is-valid? (sobokan-cords new-pos x y)) true false)))

(defn move-entity [old-pos new-pos type]
  (let [new-map (update-in @app-state [:structure (:y old-pos) (:x old-pos) :elements] (comp vec drop-last))
        new-map (update-in new-map [:structure (:y new-pos) (:x new-pos) :elements] #(conj %1 type))]
    (reset! app-state new-map)))

(defn move-hero [old-hero-pos new-hero-pos]
  (move-entity old-hero-pos new-hero-pos 4)
  (swap! app-state assoc :hero-pos {:x (:x new-hero-pos) :y (+ (:y new-hero-pos))}))

(defn winning? [state]
  (empty? (filter (fn [a] (seq (filter #(and (= 2 (second (:elements %))) 
                                             (not= 3 (last (:elements %)))) 
                                       a))) 
                  (:structure state))))

(defn update-state [x y]
  (let [hero-pos (:hero-pos @app-state)
        new-hero-pos {:x (+ (:x hero-pos) x) :y (+ (:y hero-pos) y)}]
    (cond (move-is-valid? new-hero-pos) (move-hero hero-pos new-hero-pos)
          (move-is-sobokan? hero-pos new-hero-pos x y) (do (move-entity new-hero-pos (sobokan-cords new-hero-pos x y) 3)
                                                           (move-hero hero-pos new-hero-pos))
          :else (println "invalid move"))
    (when (winning? @app-state)
      (swap! app-state #(init-state (assoc app-config :current-level (inc (:current-level @app-state))))))))

(defn on-keydown [key-code]
  (cond 
    (= key-code 37) (update-state -1 0)
    (= key-code 38) (update-state 0 -1)
    (= key-code 39) (update-state 1 0)
    (= key-code 40) (update-state 0 1)))

(defonce setup-listeners 
  (events/listen (g-dom/getWindow) "keydown" #(on-keydown (.-keyCode %1))))

(defn current-level [data] 
  (get (:levels data) (:current-level data)))

(defn render-row [row] 
  (s/join " " 
          (map (comp last :elements) row)))

(om/root
 (fn [data owner]
   (om/component
    (dom/div (dom/h2 nil (:name data))
             (dom/pre nil (s/join "\n"
                                  (map render-row
                                       (:structure data)))))))
 app-state
 {:target (. js/document (getElementById "app"))})


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
