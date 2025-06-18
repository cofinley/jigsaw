(ns jigsaw.events
  (:require
   ["soundfont-player" :as soundfont]
   [jigsaw.algo :as algo]
   [jigsaw.db :as db]
   [jigsaw.spec :as specs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(re-frame/reg-event-fx
 ::initialize-db
 (fn [_ _]
   {:db db/default-db
    :fx [[:dispatch [::add-node {:id "a" :type :input-piano :position {:x 0 :y 0} :data {:notes #{:Gb4 :A4 :C5 :E5}}}]]
         [:dispatch [::add-node {:id "b" :type :input-piano :position {:x 0 :y 400} :data {:notes #{:Gb4 :A4 :B4 :Eb5}}}]]
         [:dispatch [::add-node {:id "c" :type :input-piano :position {:x 0 :y 800} :data {:notes #{:E4 :G4 :B4}}}]]
         [:dispatch [::add-node {:id "d" :type :function-connect-shapes :position {:x 900 :y 200} :data {:view-type :output-piano}}]]
         [:dispatch [::add-edge {:id "a->d" :source "a" :target "d"}]]
         [:dispatch [::add-edge {:id "b->d" :source "b" :target "d"}]]
         [:dispatch [::add-edge {:id "c->d" :source "c" :target "d"}]]
         ;[:dispatch [::update-edge-props "c->d" {:data {:highlighted? true}}]]
         ]}))

(re-frame/reg-event-db
 ::set-nodes
 (fn [db [_ nodes]]
   (assoc db :nodes nodes)))

(re-frame/reg-event-db
 ::set-edges
 (fn [db [_ edges]]
   (assoc db :edges edges)))

(defn add-edge [db edge]
  (assoc db :edges (.concat (:edges db) (clj->js (assoc edge :type :custom-edge)))))

(re-frame/reg-event-db
 ::add-edge
 (fn [db [_ edge]]
   (add-edge db edge)))

(re-frame/reg-event-db
 ::update-edge-props
 (fn [db [_ id props]]
   (let [edges (:edges db)
         edge (first (.filter edges #(= id (.-id %))))
         new-edge (clj->js (merge (js->clj edge :keywordize-keys true) props))]
     (-> db
         (assoc :edges (.map (:edges db)
                             (fn [js-edge]
                               (if (= id (.-id js-edge))
                                 new-edge
                                 js-edge))))))))

(re-frame/reg-event-db
 ::clear-edge-highlighting
 (fn [db [_]]
   (-> db
       (assoc :edges (.map (:edges db)
                           (fn [js-edge]
                             (let [clj-edge (js->clj js-edge :keywordize-keys true)
                                   data (:data clj-edge)]
                               (clj->js (assoc clj-edge :data (dissoc data :highlighted?))))))))))

(defn create-node [db _node-props & [parent-id]]
  (let [node-type (keyword (:type _node-props))
        ;; Convert screen coordinates to flow coordinates if available
        flow-position (when (and (:mouse-x _node-props) (:mouse-y _node-props) (:flow-instance _node-props))
                        (let [flow-instance (:flow-instance _node-props)
                              screen-to-flow-pos (.-screenToFlowPosition flow-instance)]
                          (when screen-to-flow-pos
                            (let [flow-pos (screen-to-flow-pos #js {:x (:mouse-x _node-props) :y (:mouse-y _node-props)})]
                              {:x (.-x flow-pos) :y (.-y flow-pos)}))))
        node-props (cond-> _node-props
                     true (assoc :type node-type)
                     flow-position (assoc :position flow-position))
        parent-node (when parent-id
                      (assoc (js->clj (first (filter #(= parent-id (.-id %)) (:nodes db))) :keywordize-keys true)
                             :data (get-in db [:node-data parent-id])))
        node (db/->node (dissoc node-props :data :mouse-x :mouse-y :flow-instance) parent-node)
        node-data (assoc (:data node-props) :type node-type)
        id (:id node)]
    (cond-> db
      true (assoc :nodes (.concat (:nodes db) (clj->js node)))
      true (assoc-in [:node-data id] node-data)
      (some? parent-id) (add-edge {:id (str parent-id "->" id) :source parent-id :target id}))))

(re-frame/reg-event-db
 ::add-node
 (fn [db [_ node-props & [parent-id]]]
   (create-node db node-props parent-id)))

(defn delete-node [db id]
  (-> db
      (assoc :nodes (.filter (:nodes db) #(not= id (.-id %))))
      (update :node-data dissoc id)))

(re-frame/reg-event-db
 ::delete-node
 (fn [db [_ id]]
   (delete-node db id)))

(re-frame/reg-event-db
 ::toggle-note
 (fn [db [_ id midi]]
   (let [notes-path [:node-data id :notes]
         notes (set (or (get-in db notes-path) #{}))
         note (algo/midi->note midi nil)]
     (assoc-in db notes-path ((if (some? (some #{note} notes)) disj conj) notes note)))))

;; TODO: do this in output piano node (reactive), not on shape node change (stale on piano re-render)
(defn calculate-shape [node]
  (let [shape-type (if (utils/in? [:input-chord :function-scale-chords] (:type node)) :chord :scale)
        {:keys [pitch name]} node]
    (when (and (some? pitch) (some? name))
      (algo/->shape (algo/pitch->note pitch) (keyword name)))))

(re-frame/reg-event-db
 ::calculate-shape
 (fn [db [_ id]]
   (let [node (get-in db [:node-data id])
         shape (calculate-shape node)]
     (cond-> db
       (some? shape) (update-in [:node-data id] merge shape)))))

(re-frame/reg-event-db
 ::update-node-data
 (fn [db [_ id data]]
   (update-in db [:node-data id] merge data)))

(re-frame/reg-event-db
 ::set-selected-shape
 (fn [db [_ id shape-type selected-shape]]
   (let [node (get-in db [:node-data id])
         ;; TODO: create clear-shape fn to remove any scale/chord keys, like :degrees, before setting new shape
         new-node (merge (update node :data dissoc :degrees) (merge selected-shape {:selected-shape-type shape-type}))]
     (assoc-in db [:node-data id] new-node))))

;; Audio state management
(defonce audio-state (atom {:instruments {} :audio-context nil}))

(defn init-audio-context! []
  (when-not (:audio-context @audio-state)
    (let [ctx (js/AudioContext.)]
      (swap! audio-state assoc :audio-context ctx)
      ctx)))

(defn load-instrument! [instrument-name]
  (let [ctx (init-audio-context!)]
    (when-not (get-in @audio-state [:instruments instrument-name])
      (-> (soundfont/instrument ctx instrument-name)
          (.then (fn [instrument]
                   (swap! audio-state assoc-in [:instruments instrument-name] instrument)))))))

(re-frame/reg-event-fx
 ::play-shape
 (fn [{:keys [_]} [_ shape]]
   (let [instrument-name "acoustic_grand_piano"
         note-offset-ms (if (specs/chord? shape) 30 300)]
     (load-instrument! instrument-name)
     (js/setTimeout
      (fn []
        (when-let [instrument (get-in @audio-state [:instruments instrument-name])]
          (doseq [[i note] (map-indexed vector (:notes shape))]
            (js/setTimeout
             (fn []
               (.play instrument (algo/note->midi note)))
             (* i note-offset-ms)))))
      100) ; Small delay to ensure instrument is loaded
     {})))
