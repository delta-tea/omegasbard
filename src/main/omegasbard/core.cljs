(ns omegasbard.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [reagent.core :as reagent]
    [reagent.dom :as dom]
    [clojure.string :as string]
    [cljs-http.client :as http]
    [cljs.core.async :as async :refer [<!]]
    [no.en.core :refer [url-encode url-decode]]
    [clojure.edn :as edn]
    [omegasbard.game :as game]))


(def development-secrets {:username   nil
                          :password   nil
                          :room-alias nil})

(defn- ns'd [& bits]
  (string/join "." (cons "io.github.rkwest" bits)))

(def event-type-game (ns'd "game"))

(def event-type-game-create (ns'd "game" "create"))
(def event-type-game-end (ns'd "game" "end"))

(def event-type-game-join (ns'd "game" "join"))
(def event-type-game-leave (ns'd "game" "leave"))

(def event-type-game-setup (ns'd "game" "setup"))
(def event-type-game-play (ns'd "game" "play"))

(defn new-id []
  (subs (str (random-uuid)) 0 8))

(defn clj->json
  [ds]
  (.stringify js/JSON (clj->js ds)))

(defonce !state (reagent/atom {:overviews {:in-progress false
                                           :game-id->events {}}}))

(defonce !overviews-in-progress (reagent/cursor !state [:overviews :in-progress]))
(defonce !overview-events (reagent/cursor !state [:overviews :game-id->events]))

(defonce !game (reagent/cursor !state [:game]))

(let [transaction-id (atom 0)]
  (defn txn-id []
    (str (swap! transaction-id inc))))

(defn busy! [business]
  (println "Busy: " business)
  (swap! !state assoc :busy business))

(defn done! []
  (println "Done!")
  (swap! !state dissoc :busy))

(defn page [title breadcrumb backstep & content]
  (into [:div
         [:div (string/join " / " breadcrumb) [:span " - "] [:input {:type :button
                                                       :value "x"
                                                       :on-click #(when backstep
                                                                    (swap! !state dissoc backstep))}]]
         [:h1 title]]
        content))

(defn get-input-value [id]
  (.-value (.getElementById js/document id)))

(defn login []
  (let [host (get-input-value "loginHost")
        username (get-input-value "loginUsername")
        password (get-input-value "loginPassword")
        login-url (str "https://" host "/_matrix/client/r0/login")]
    (busy! "Logging in")
    (go (let [{:keys [status body]} (<! (http/post login-url
                                                   {:with-credentials? false
                                                    :json-params {:type       "m.login.password"
                                                                  :identifier {:type "m.id.user"
                                                                               :user username}
                                                                  :password   password}}))]
          (println "login:" status body)
          (swap! !state assoc :creds {:host     host
                                      :username username
                                      :user-id  (:user_id body)
                                      :access-token (:access_token body)})
          (done!)))))

(defn get-room-creation [{:keys [host access-token]} {:keys [room-host room-id] :as room}]
  (let [sync-url (str "https://" host "/_matrix/client/r0/sync")]
    (go (let [{:keys [status body]} (<! (http/get sync-url {:with-credentials? false
                                                            :query-params      {"access_token" access-token
                                                                                "filter"       (clj->json {:room {:rooms    [(str room-id ":" room-host)]
                                                                                                                  :timeline {:types ["m.room.create"]
                                                                                                                             :limit 1}
                                                                                                                  :state    {:types ["m.room.create"]
                                                                                                                             :limit 1}}})}}))
              ;_ (println body)
              state-create-event-id (-> body :rooms :join first val :state :events first :event_id)
              timeline-create-event-id (-> body :rooms :join first val :timeline :events first :event_id)
              room' (assoc room :create-event-id (or state-create-event-id timeline-create-event-id))]
          (println "get-room-creation:" status)
          (swap! !state assoc :room room')
          (done!)))))

(defn lookup-room [{:keys [host access-token] :as creds}]
  (busy! "Finding your room")
  (let [room-alias (get-input-value "roomAlias")
        room-url (str "https://" host "/_matrix/client/r0/directory/room/" (url-encode room-alias))]
    (go (let [{:keys [status body]} (<! (http/get room-url {:with-credentials? false
                                                            :query-params      {"access_token" access-token}}))
              [room-id room-host] (string/split (:room_id body) #":")
              room {:room-alias room-alias
                    :room-id    room-id
                    :room-host  room-host}]
          (println "lookup-room:" status room)
          ;(swap! !state assoc :room room)
          (get-room-creation creds room)))))

(defn fetch-more-events [{:keys [host access-token]} {:keys [room-id room-host]} event-id]
  (go (let [room-url (str "https://" host "/_matrix/client/r0/rooms/" (url-encode (str room-id ":" room-host)) "/context/" (url-encode event-id))
            {:keys [status body]} (<! (http/get room-url {:with-credentials? false
                                                          :query-params      {"access_token" access-token}}))
            {:keys [events_before event events_after]} body

            ]
        (println "fetch-more-events" status)
        (-> body println)

        ;(async/onto-chan channel )
        )))

(defn fetch-all-events
  ([creds room channel]
   (fetch-all-events creds room channel (:create-event-id room)))
  ([{:keys [host access-token]} {:keys [room-id room-host]} channel first-event-id]
   (go
     (loop [event-id first-event-id]
       (let [room-url (str "https://" host "/_matrix/client/r0/rooms/" (url-encode (str room-id ":" room-host)) "/context/" (url-encode event-id))
             {:keys [status body]} (<! (http/get room-url {:with-credentials? false
                                                           :query-params      {"access_token" access-token}}))
             {:keys [events_before event events_after]} body
             events (cons event events_after)]
         (println "fetch-more-events: " event-id status (count events) events)
         (if (seq events_after)
           (do (async/onto-chan! channel (butlast events) false)
               (recur (:event_id (last events))))
           (do (async/onto-chan! channel events false)
               (async/close! channel))))))
   channel))

(def conjv (comp vec conj))

(defrecord GameOverview [description player-count current-players create-event-id])

(defn overview-event-handler [{:keys [description player-count current-players] :as game-overview} event]
  ;(println "overview-event-handler" event)
  (cond
    (and (not game-overview) (= (:type event) event-type-game-create))
    (->GameOverview (-> event :content :description)
                    (-> event :content :player-count)
                    []
                    (:event_id event))

    (and game-overview (= (:type event) event-type-game-join) (< (count current-players) player-count))
    (update game-overview :current-players conj (:user_id event))

    ;TODO: leave + end

    :else
    game-overview
    ))

(defn- overview-reducer [overviews event]
  (let [game-id (-> event :content :game-id)
        event-type (-> event :type)
        event-id (:event_id event)]
    ;(println event-type event-type-game-join)
    ;(println (= event-type event-type-game-join))
    ;(println (type event-type) (type event-type-game-join))
    (println event)
    (cond
      (or (= event-type event-type-game-create)
          (= event-type event-type-game-end)) (assoc-in overviews [game-id event-type] (-> event
                                                                                           :content
                                                                                           (dissoc :game-id)
                                                                                           (assoc :event-id event-id)))
      (or (= event-type event-type-game-join)
          (= event-type event-type-game-leave)) (assoc-in overviews [game-id event-type event-id] (:sender event))
      :else (do
              (println event-type)
              overviews)
      )))

(defn discover-games [{:keys [host access-token]} {:keys [room-host room-id] :as room} !projection order-agnostic-reducer !continue?]

  ; sync - no since parameter, no event limit
  ;   https://matrix.org/docs/spec/client_server/latest#get-matrix-client-r0-sync
  ; loop call to /messages with prev_batch for room:
  ;   https://matrix.org/docs/spec/client_server/latest#get-matrix-client-r0-rooms-roomid-messages
  ; also loop call to /sync with next_batch and timeout

  ; all new events get added to the projection
  ;   projection stores game-id, creation-event-id, count of join/leave + create/destroy events

  ; =======================
  (go
    ; initial sync
    (let [sync-url (str "https://" host "/_matrix/client/r0/sync")
          messages-url (str "https://" host "/_matrix/client/r0/rooms/" (url-encode (str room-id ":" room-host)) "/messages")
          overview-event-filter (clj->json {:room {:rooms    [(str room-id ":" room-host)]
                                                   :timeline {:types [event-type-game-create
                                                                      event-type-game-end
                                                                      event-type-game-join
                                                                      event-type-game-leave]
                                                              :limit 10}
                                                   :state    {:limit 0}}})]
      (go (let [{:keys [status body]} (async/<!
                                        (http/get sync-url
                                                  {:with-credentials? false
                                                   :query-params      {"access_token" access-token
                                                                       "filter"       overview-event-filter}}))
                ;_ (println body)
                ;state-events (-> body :rooms :join first val :state :events)
                timeline-events (-> body :rooms :join first val :timeline :events)
                next-batch (-> body :next_batch)
                prev-batch (-> body :rooms :join first val :timeline :prev_batch)]

            ;(println (count state-events))
            ;(println (count timeline-events))
            ;(println (count (concat state-events timeline-events)))
            (println "initial /sync: " status (count timeline-events))

            (swap! !projection (partial reduce order-agnostic-reducer) timeline-events)

            ; loop call to /messages
            (loop [continuation-token prev-batch
                   limit 150] ; Todo: manually make these calls and see what happens when we get to the beginning, we should stop then as well
              ;Todo: filter this call for only the events we are interested in?
              (let [{:keys [status body]} (async/<! (http/get messages-url {:with-credentials? false
                                                                            :query-params      {"access_token" access-token
                                                                                                "from"         continuation-token
                                                                                                "dir"          "b"
                                                                                                "limit"        10
                                                                                                "filter"       overview-event-filter}}))
                    events (-> body :chunk)
                    end-token (-> body :end)]

                (println "/messages: " status (count events) end-token)

                (swap! !projection (partial reduce order-agnostic-reducer) events)
                (when (and (pos? limit) (pos? (count events)))
                  (recur end-token (- limit (count events))))))

            ; loop call to /sync
            (loop [continuation-token next-batch]
              (let [{:keys [status body]} (async/<! (http/get sync-url {:with-credentials? false
                                                                        :query-params      {"access_token" access-token
                                                                                            "since"        continuation-token
                                                                                            "timeout"      10000
                                                                                            "filter"       overview-event-filter}}))
                    timeline-events (some-> body :rooms :join first val :timeline :events)
                    next-batch (-> body :next_batch)]

                (println "subsequent /sync: " status (count timeline-events) next-batch)

                (swap! !projection (partial reduce order-agnostic-reducer) timeline-events)
                (when @!continue?
                  (when (empty? timeline-events)
                    (async/<! (async/timeout 15000)))
                  (recur next-batch))))

            )))))

(defn fetch-game-overviews [creds room]
  (println "fetch-game-overviews" @!overviews-in-progress)
  (when-not @!overviews-in-progress
    (println "triggering discover-games")
    (reset! !overviews-in-progress true)
    (discover-games creds room !overview-events overview-reducer !overviews-in-progress))


  ; if there are no known games then
  ;  fetch all the events from the channel, filter for new game events, add the games to a list
  #_(do
    (swap! !state assoc :game-overviews {})
    ; TODO: FIX 1: call sync, then context backwards to find game events
    ;              stop walking backwards after <X> events, or after the start of the channel is reached
    (let [channel (fetch-all-events creds room (async/chan))]
      (go
        (loop []
          (when-let [event (async/<! channel)]
            (println "fetch-game-overviews: " event)
            (when (string/starts-with? (:type event) event-type-game)
              (swap! !state update-in [:game-overviews (-> event :content :game-id)] overview-event-handler event))
            (recur)))
        ;(swap! !state assoc-in [:games :found-all?] true)
        (done!)))))

(defn send-event [{:keys [host access-token] :as creds} {:keys [room-id room-host] :as room} event-type event]
  ; PUT /_matrix/client/r0/rooms/{roomId}/send/{eventType}/{txnId}
  ;TODO: 429 / {"errcode":"M_LIMIT_EXCEEDED","error":"Too Many Requests","retry_after_ms":3580}
  (go
    (println event)
    (let [put-url (str "https://" host "/_matrix/client/r0/rooms/" (url-encode (str room-id ":" room-host))
                       "/send/" event-type "/" (txn-id))
          {:keys [status body]} (<! (http/put put-url {:with-credentials? false
                                                       :query-params      {"access_token" access-token}
                                                       :body              (clj->json event)}))]
      (println status body (or (:retry_after_ms body) 5000))
      (if (= status 429)
        (do (<! (async/timeout (or (:retry_after_ms body) 5000)))
            (<! (send-event creds room event-type event)))
        (<! (async/timeout 200))))))

#_(defn set-game-status [creds room game-id status & kvs]
  (send-event creds room event-type-game-status (merge
                                                  (into {} kvs)
                                                  {:game-id game-id
                                                   :status  status})))

(defn create-game [creds room game-id description player-count]
  (send-event creds room event-type-game-create {:game-id      game-id
                                                 :description  description
                                                 :player-count player-count}))

(defn join-game [creds room game-id]
  (send-event creds room event-type-game-join {:game-id game-id}))

(defn leave-game [creds room game-id]
  (send-event creds room event-type-game-leave {:game-id game-id}))

(defn setup-game [creds room game-id template]
  (go
    (let [template-minus-places (dissoc template :places)
          places (:places template)]

;TODO: 429 / {"errcode":"M_LIMIT_EXCEEDED","error":"Too Many Requests","retry_after_ms":3580}

      (doseq [[k v] template-minus-places]
        (<! (send-event creds room event-type-game-setup {:game-id game-id
                                                          :path    [k]
                                                          :data    v})))
      (doseq [[k place] places]
        (<! (send-event creds room event-type-game-setup {:game-id game-id
                                                          :path    [:places k]
                                                          :data    place}))))))

(defn new-game-from-template [creds room]
  (let [desc (get-input-value "createGameDescription")
        template-url (get-input-value "createGameTemplateUrl")]
    (go
      (let [{:keys [status body]} (<! (http/get template-url {}))
            player-count (count (filter #{"required"} (:players body)))
            game-id (new-id)]
        (println status (keys body))
        (println "player-count:" player-count)
        (<! (create-game creds room game-id desc player-count))
        (<! (setup-game creds room game-id body))
        (<! (join-game creds room game-id))))))

(defn insert [coll x insert-at]
  (case insert-at
    :top (vec (concat [x] coll))
    :bottom (vec (concat coll [x]))
    :random (let [n (rand-int (inc (count coll)))
                  [fore aft] (split-at n coll)]
              (vec (concat fore [x] aft)))))

(def open-slot? #{"required" "optional"})
(defn fix-edn [x]
  (cond
    (map? x) (into {} (map fix-edn x))
    (map-entry? x) [(fix-edn (key x)) (fix-edn (val x))]
    (vector? x) (mapv fix-edn x)
    (and (keyword? x) (string/starts-with? (name x) "#{")) (edn/read-string (name x))
    :else x))

(defmulti specific-game-event-handler (fn [_game event] (:type event)))
(defmethod specific-game-event-handler :default [_game {event-type :type}]
  (println (str "No specific-game-event-handler for event type '" event-type "'")))

(defmethod specific-game-event-handler event-type-game-create [_game event]
  (println "Game creation event: " event)
  {:instance-description (:description (:content event))})
(defmethod specific-game-event-handler event-type-game-join [game {:keys [user_id]}]
  (update game :players #(let [[players placeholders] (split-with (complement open-slot?) %)]
                           (vec (concat players [user_id] (rest placeholders))))))

(defmethod specific-game-event-handler event-type-game-setup [game {{:keys [path data]} :content}]
  (println "setup: " path)
  (assoc-in game (map keyword path) (fix-edn data)))
(defmethod specific-game-event-handler event-type-game-play [game {{:keys [id from-path to-path rotation]} :content user :user_id}]
  ; pick-up / put-down both modelled as a move (with new rotation?)
  (let [from-place (get-in game from-path)
        {:keys [insert-at set-rotation] :as to-place} (get-in game to-path)
        new-rotation (or set-rotation rotation 0)

        piece (first (filter #(-> % :id (= id)) (:pieces from-place)))
        new-from-place (update from-place :pieces #(vec (remove (partial = piece) %)))

        new-piece (assoc piece :rotation new-rotation)
        new-to-place (update to-place :pieces insert new-piece insert-at)
        ]
    ; filter from-pieces for id, add that piece to to-pieces
    ; set rotation
    ; assoc new from-pieces, to-pieces
    (-> game
        (assoc-in from-path new-from-place)
        (assoc-in to-path new-to-place))))

(defn game-event-handler [game event]
  (println "game-event-handler: " (:type event))
  (-> (specific-game-event-handler game event)
      (assoc :latest-event-id (:event_id event))))

(defn load-game [creds room game-id create-event-id]
  ;(swap! !state assoc :game {})
  (let [channel (fetch-all-events creds room
                                  (async/chan 3 (filter #(-> % :content :game-id (= game-id))))
                                  create-event-id)]
    (go
      (loop []
        (when-let [event (async/<! channel)]
          (println event)
          (when (string/starts-with? (:type event) event-type-game)
            (swap! !state update :game game-event-handler event))
          (recur)))
      (done!)))

  ; read all events from the room for this game id
  ; project game state

  ; sync call to keep up to date?
  ; or just poll on the context call?
  )

(defn transform-overview [[game-id events]]
  ;(println game-id events)
  (let [players (set (vals (mapcat events [event-type-game-join event-type-game-leave])))]
    {:game-id         game-id
     :create-event-id (get-in events [event-type-game-create :event-id])
     :description     (get-in events [event-type-game-create :description])
     :player-count    (get-in events [event-type-game-create :player-count])
     :ended           (some? (get events event-type-game-end))
     :current-players players})

  )

(defn render-game-overviews [creds room overviews]
  ; games {
  ;   "game-id" {:players [matrix-id, ...]}
  ;
  ; }
  [:div
   [:hr]
   [:div "Create new game:"
    [:div [:input {:id "createGameDescription" :type :text :size 25 :placeholder "Description..."}]]
    [:div [:input {:id "createGameTemplateUrl" :type :text :size 25 :value "./templates/tictactoe/game.edn"}]]
    ; TODO: add template URL input...
    [:div [:input {:type     :button
                   :value    "Create new game"
                   :on-click #(new-game-from-template creds room)}]]]
   [:hr]
   [:div "Existing games:" (str (count (:game-id->events overviews)))
    (if (zero? (count (:game-id->events overviews)))
      (do
        (fetch-game-overviews creds room)
        [:div "Looking for games..."])
      [:table
       [:tbody
        [:tr
         [:th "Id"]
         [:th "Players"]
         [:th "Description"]]
        (for [{:keys [game-id description current-players player-count create-event-id]}
              (map transform-overview (:game-id->events overviews))]
          (let [joined? ((set current-players) (:user-id creds))
                complete? (>= (count current-players) player-count)]
            [:tr {:key (or game-id "None")}
             [:td game-id]
             [:td (count current-players) \/ player-count]
             [:td description]
             [:td
              (when (and joined? complete?)
                [:input {:type     :button
                         :value    "Play"
                         :on-click #(load-game creds room game-id create-event-id)}])]
             [:td
              (when (and (not joined?) (not complete?))
                [:input {:type     :button
                         :value    "Join"
                         :on-click #(join-game creds room game-id)}])]
             [:td
              (when joined?
                [:input {:type     :button
                         :value    "Leave"
                         :on-click #(leave-game creds room game-id)}])]
             ]))]])]])

(defn pick-up [game player-id path]
  (if (empty? (get-in game [:places :holding :places player-id :pieces]))
    (let [pieces-path (butlast path)
          piece-id (last path)
          _ (println pieces-path (map type pieces-path) piece-id)
          pieces (get-in game pieces-path)
          piece (first (filter #(= piece-id (:id %)) pieces))
          other-pieces (remove #(= piece-id (:id %)) pieces)]
      (println "pick up!" piece)
      (-> game
          (assoc-in pieces-path other-pieces)
          (assoc-in [:places :holding :places player-id :pieces] [piece])
          (assoc-in [:ui :pick-up] true)))
    (assoc-in game [:ui :pick-up] false)))

(defn add-piece [pieces piece]
  (conj pieces piece))

(defn put-down [game player-id path set-rotation insert-at]
  (let [piece (first (get-in game [:places :holding :places player-id :pieces]))
        pieces (get-in game path)]
    (if (and piece pieces)
      (let [new-pieces (add-piece pieces piece)]
        (-> game
            (assoc-in path new-pieces)
            (assoc-in [:places :holding :places player-id :pieces] [])))
      game)))

(defn pick-up-fn [player-id path or-else]
  ; TODO: send event to other players, and read other players' events - and our own events???!!!! - yes!
  (let [picked-up (get-in (swap! !state update :game pick-up player-id path)
                      [:game :ui :pick-up])]
    (when (and or-else (not picked-up))
      (or-else))))

(defn put-down-fn [player-id path set-rotation insert-at]
  (println "put down!" path)
  ; TODO: send event to other players, and read other players' events
  (swap! !state update :game put-down player-id path set-rotation insert-at))

(defn render-game [game user-id]

  [:div "GAME!!!!"

   [:h2 (:description game) " / " (:instance-description game)]

   #_[:div

    (for [[k place] (:places game)]
      [:div k (or (:label place) (name k))])

    ]

   (game/render-game game user-id pick-up-fn put-down-fn)
   ]
  )

(defn render-app []
  (let [{:keys [busy creds room game-overviews overviews game]} @!state]
    ;(println game-overviews)
    [:div

     (cond
       busy [:div
             [:h2 "One moment please"]
             [:div (str busy "...")]]
       (not creds) (page "Omega's Bard" [] nil
                     [:div
                      [:h2 "Login"]
                      [:div [:input {:id "loginHost" :type :text :size 25 :placeholder "Host e.g. matrix-client.matrix.org" :value "matrix-client.matrix.org"}]]
                      [:div [:input {:id "loginUsername" :type :text :size 25 :placeholder "Username" :value (:username development-secrets)}]]
                      [:div [:input {:id "loginPassword" :type :text :size 25 :placeholder "Password" :value (:password development-secrets)}]]
                      [:div [:input {:type     :button
                                     :value    "Login to the Matrix host"
                                     :on-click login}]]])
       (not room) (page "Omega's Bard" [(-> creds :host) (-> creds :user-id)] :creds
                    [:div
                     [:h2 "Room selection"
                      [:div [:input {:id "roomAlias" :type :text :size 25 :placeholder "Matrix Room Alias" :value (:room-alias development-secrets)}]]
                      [:div [:input {:type     :button
                                     :value    "Enter the room"
                                     :on-click #(lookup-room creds)}]]
                      ]])

       ;true (do (sync-call creds room)
       ;         [:div "stuff"])

       (not game) (page "Omega's Bard" [(-> creds :host) (-> creds :user-id) (:room-alias room)] :game-overviews
                     [:div
                      [:h2 "Game Selection"]
                      (render-game-overviews creds room overviews)
                      ])

       :else (page "Omega's Bard" [(-> creds :host) (-> creds :user-id) (:room-alias room)] :game
                   (render-game game (:user-id creds))
                   #_[:div
                    #_[:textarea {:value
                                (with-out-str (cljs.pprint/pprint (:game @!state)))}]
                    (render-game game (:user-id creds))

                    ])

       ; read room for game events
       ;   get first event with a sync call?
       ;   get subsequent events with a context call?

       )
     ]))

(when-let [element (. js/document (getElementById "root"))]
  (dom/render [render-app] element))

(defn init []
  (println "Hello!!!!!!2!231231  !!!!"))


