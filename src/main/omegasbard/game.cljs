(ns omegasbard.game
  (:require [clojure.string :as string]))


(defn add-defaults [game]
  (assoc-in game [:places :holding :xywh] [80 80 20 20])
  )

(def default-visibility {:show "as-is"
                         :counted "no"
                         :arrange "stack"})

(defn get-visibility [{:keys [visibility]} player-id]
  (merge default-visibility
         (or (first (for [[ids vis] (dissoc visibility :default)
                          :when (ids player-id)]
                      vis))
             (:default visibility))))

(defn render-side [stack-offset offset-x offset-y {:keys [image size] :as side}]
  ;(println "render-side")
  (let [raw-size (or size 10)
        raw-radius (/ raw-size 2)]
    (if image
      [:image {:key                 stack-offset
               :href                (if (= image "url...")
                                      "https://mdn.mozillademos.org/files/6457/mdn_logo_only_color.png"
                                      image)
               :x                   (- offset-x raw-radius)
               :y                   (- offset-y raw-radius stack-offset)
               :height              raw-size
               :width               raw-size
               :preserveAspectRatio "xMidYMid meet"}]
      [:circle {:key          stack-offset
                :cx           offset-x
                :cy           (- offset-y stack-offset)
                :r            raw-radius
                :stroke       "black"
                :stroke-width 0.5
                :fill         "green"}])))

(defn render-piece [show [offset-x offset-y] {:keys [sides rotation] :as piece} stack-offset]
  ;(println "render-piece" show (type show))
  (render-side
    stack-offset
    offset-x offset-y
    (case show
      "as-is" (first (drop (or rotation 0) (cycle sides)))
      "face-up" (first (filter :face sides)))))

(defn estimate [pieces]
  ;(println pieces)
  (condp <= (count pieces)
    6 "☰"
    3 "="
    1 "-"
    ""))

(defn render-stack [pieces show counted [offset-x offset-y] path pick-up-fn put-down]
  ;(println "render-stack")
  [:g {:on-click #(pick-up-fn (concat path [(:id (first pieces))]) put-down)}
   (map (partial render-piece show [offset-x offset-y]) (take 5 pieces) (range))
   [:text {:x         (+ offset-x 7)
           :y         (+ offset-y 2.5)
           :font-size 5}
    (case counted
      "yes" (count pieces)
      "estimate" (estimate pieces)
      "no" ""
      nil)]])

(defn render-pieces [{:keys [pieces] :as place} {:keys [show counted arrange] :as visibility} [offset-x offset-y]
                     path pick-up-fn put-down]
  (println "render-pieces" arrange show counted)
  (case arrange
    "stack" (render-stack pieces show counted [offset-x offset-y] path pick-up-fn put-down)
    "spread" nil
    (do (println "no arrange")
        #_(render-stack pieces show counted [offset-x offset-y] path pick-up-fn))
    ))

(declare render-place)

(defn render-place-as-is [{:keys [xy image size] :as place} player-id [x-offset y-offset scale]]
  ; if image
  ;   render image with fixed aspect ratio, longest side scaled to size, centered on xy
  ; else
  ;   render a circle centered on xy with radius = size/2
  )

#_(defn render-place-face-up [{:keys [xy xywh] :as place} player-id context]
  (let [[x y w h] (or xywh xy)]
    [:rect {:x x :y y :width (or w 20) :height (or h 20) :rx 2 :fill "pink"}]))

(defn scale [re-scale]
  (str "scale(" re-scale ")"))

(defn translate [x-offset y-offset]
  (str "translate(" x-offset " " y-offset ")"))

(defn transform [& xfs]
  (string/join " " xfs))

(defn context-transform [[context-x context-y context-scale]]
  (transform (translate context-x context-y) (scale context-scale)))

(defn render-place [k {:keys [label xy image size set-rotation insert-at] :as place} player-id [offset-x offset-y scale]
                    path pick-up-fn put-down-fn]
  ;(println "render-place" k path)
  (let [{:keys [show] :as visibility} (get-visibility place player-id)]
    ;(println (:label place) visibility)
    (when (not= show "hidden")
      (let [x (+ offset-x (* scale (first xy)))
            y (+ offset-y (* scale (second xy)))
            raw-size (or size 10)
            re-scaled-size (* raw-size scale)
            raw-radius (/ raw-size 2)
            re-scaled-radius (* raw-radius scale)
            pieces-path (concat path [:pieces])
            put-down #(put-down-fn pieces-path set-rotation insert-at)] ; TODO: make path always a vector, then conj instead of concat
        [:g {:key (last path)}
         [:text {:x x
                 :y (+ y re-scaled-radius 7)
                 :font-size 5
                 :text-anchor "middle"} label]
         (if image
           [:image {:href                (if (= image "url...")
                                           "https://mdn.mozillademos.org/files/6457/mdn_logo_only_color.png"
                                           image)
                    :x                   (- x re-scaled-radius)
                    :y                   (- y re-scaled-radius)
                    :height              re-scaled-size
                    :width               re-scaled-size
                    :preserveAspectRatio "xMidYMid meet"
                    :on-click            put-down}]
           [:circle {:cx           x
                     :cy           y
                     :r            raw-radius
                     :stroke       "rgb(0,255,0)"
                     :stroke-width 0.5
                     :fill         "rgba(0,255,0,0.1)"
                     :on-click     put-down}])
         (for [[k child-place] (:places place)]
           (render-place k child-place player-id [x y (* scale (/ size 200))] (concat path [:places k]) pick-up-fn put-down-fn))
         (render-pieces place visibility [x y] pieces-path pick-up-fn put-down)]))))

(defn render-holding [piece]
  (if piece
    [:g
     [:rect {:x 80 :y 80 :width 22 :height 22 :rx 1 :stroke "pink" :fill "white"}]
     (render-piece "as-is" [90 90] piece 0)
     [:text {:x 98
             :y 77
             :font-size 5
             :text-anchor "end"}
      (:label piece)]]
    [:rect {:x 80 :y 80 :width 22 :height 22 :rx 1 :fill "pink"}]))

(defn render-game [game user-id pick-up-fn put-down-fn]
  (let [game (add-defaults game)
        player-id (-indexOf (:players game) user-id)
        path []]
    [:svg {:view-box "-100 -100 200 200"
           ;:width "100%"
           :style    {:border     "1px solid"
                      :background "white"
                      :width      "100%"
                      ;:height     "150px"
                      }
           }

     (println (:players game) player-id)
     (render-holding (first (get-in game [:places :holding :places player-id :pieces])))
     (for [[k place] (dissoc (:places game) :holding)]
       [:g {:key k} (render-place
                      k place player-id [0 0 1]
                      [:places k]
                      (partial pick-up-fn player-id)
                      (partial put-down-fn player-id))])

     #_[
        [:polygon {:fill   "red" :stroke-width 0
                   :points "0,10 20,10 10,0"}]

        [:circle {:r 50, :cx 75, :cy 75, :fill "green"}]
        [:circle {:r 25, :cx 75, :cy 75, :fill "blue"}]
        [:path {:stroke-width 12
                :stroke       "white"
                :fill         "none"
                :d            "M 30,40 C 100,40 50,110 120,110"}]]]))
