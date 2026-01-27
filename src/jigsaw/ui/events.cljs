(ns jigsaw.ui.events
  (:require
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.db :as db]
   [re-frame.core :as re-frame]
   ["soundfont-player" :as soundfont]))

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

; Computation methods
(defmulti should-compute? (fn [parent-data data] (:type data)))
(defmulti compute-node (fn [parent-data data] (:type data)))

(re-frame/reg-event-db
 ::set-nodes
 (fn [db [_ nodes]]
   (assoc db :nodes nodes)))

(re-frame/reg-event-db
 ::set-edges
 (fn [db [_ edges]]
   (assoc db :edges edges)))

(defn get-parent-data-for-node [db node-id]
  (let [node-type (get-in db [:node-data node-id :type])]
    (case node-type
    ; Multiple parents
      (:function-connect-shapes :function-fit-shape)
      (let [sources (filter #(= (.-target %) node-id) (:edges db))
            source-ids (map #(.-source %) sources)]
        (map #(get-in db [:node-data %]) source-ids))
    ;; Single parent for other function nodes
      (let [sources (filter #(= (.-target %) node-id) (:edges db))]
        (when (seq sources)
          (get-in db [:node-data (.-source (first sources))]))))))

(defn add-edge [db edge]
  (assoc db :edges (clj->js (conj (js->clj (:edges db))
                                  (clj->js (assoc edge :type :custom-edge))))))

(re-frame/reg-event-fx
 ::add-edge
 (fn [{:keys [db]} [_ edge]]
   (let [new-db (add-edge db edge)
         target-id (:target edge)
         target-data (get-in new-db [:node-data target-id])
         parent-data (get-parent-data-for-node new-db target-id)]
     {:db new-db
      :fx (when (should-compute? parent-data target-data)
            [[:dispatch ^:flush-dom [::compute-function-result target-id parent-data target-data]]])})))

(re-frame/reg-event-fx
 ::recompute
 (fn [{:keys [db]} [_ id]]
   (let [data (get-in db [:node-data id])
         parent-data (get-parent-data-for-node db id)]
     (cond-> {:db db}
       (and parent-data (should-compute? parent-data data))
       (assoc :fx [[:dispatch [::compute-function-result id parent-data data]]])))))

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
    [id (cond-> db
          true (assoc :nodes (clj->js (conj (js->clj (:nodes db))
                                            (clj->js node))))
          true (assoc-in [:node-data id] node-data)
          (some? parent-id) (add-edge {:id (str parent-id "->" id) :source parent-id :target id}))]))

(re-frame/reg-event-fx
 ::add-node
 (fn [{:keys [db]} [_ node-props & [parent-id]]]
   (let [[node-id new-db] (create-node db node-props parent-id)
         parent-data (when (some? parent-id) (get-parent-data-for-node new-db node-id))
         node-data (get-in new-db [:node-data node-id])]
     (cond-> {:db new-db}
       (and (some? parent-data) (should-compute? parent-data node-data))
       (assoc :fx [[:dispatch ^:flush-dom [::compute-function-result node-id parent-data node-data]]])))))

(defn delete-node [db id]
  (-> db
      (assoc :nodes (clj->js (remove #(= id (get % "id"))
                                     (js->clj (:nodes db)))))
      (update :node-data dissoc id)))

(re-frame/reg-event-db
 ::delete-node
 (fn [db [_ id]]
   (delete-node db id)))

;; TODO: do this in output piano node (reactive), not on shape node change (stale on piano re-render)
(defn calculate-shape [node]
  (let [{:keys [pitch name]} node]
    (when (and (some? pitch) (some? name))
      (jigsaw/->shape (theory/pitch->note pitch) (keyword name)))))

(re-frame/reg-event-fx
 ::calculate-shape
 (fn [{:keys [db]} [_ id]]
   (let [node (get-in db [:node-data id])
         shape (calculate-shape node)]
     {:fx [[:dispatch ^:flush-dom [::update-node-data id shape]]]})))

(defn get-child-nodes [db parent-id]
  (let [edges (:edges db)
        child-edges (filter #(= (.-source %) parent-id) edges)
        child-ids (map #(.-target %) child-edges)]
    child-ids))

(defmethod should-compute? :function-scale-chords [parent-data data]
  (and parent-data (contains? parent-data :degrees)))
(defmethod compute-node :function-scale-chords [parent-data data]
  (let [shape-refs (jigsaw/scale->chords parent-data)]
    (map #(merge % (jigsaw/->shape (assoc % :note (theory/pitch->note (:pitch %))))) shape-refs)))

(defmethod should-compute? :function-chord-scales [parent-data data]
  (and parent-data (contains? parent-data :intervals)))
(defmethod compute-node :function-chord-scales [parent-data data]
  (let [selected-degree (:selected-degree data)
        shape-refs (jigsaw/chord->scales (jigsaw/->shape parent-data) :degree selected-degree)]
    (map #(merge % (jigsaw/->shape (assoc % :note (theory/pitch->note (:pitch %))))) shape-refs)))

(defmethod should-compute? :function-find-shape [parent-data data]
  (and parent-data (seq (:notes parent-data))))
(defmethod compute-node :function-find-shape [parent-data data]
  (let [notes (:notes parent-data)
        incoming-shape-type (cond
                              (contains? parent-data :degrees) :scale
                              (contains? parent-data :intervals) :chord
                              :else :notes)
        selected-shape-type (or (:selected-shape-type data) (if (= :chord incoming-shape-type) :scale :chord))
        selected-pitch (or (:selected-pitch data) "")
        heuristic (or (:heuristic data) :overlap)
        max-shapes (or (:max-shapes data) 10)
        shapes (jigsaw/notes->shapes-memo notes
                                          selected-shape-type
                                          :max-shapes max-shapes
                                          :heuristic (keyword heuristic)
                                          :selected-pitch (if (= selected-pitch :all) nil selected-pitch))
        resolved-shapes (map #(merge % (jigsaw/->shape (theory/pitch->note (:pitch %)) (:name %))) shapes)]
    resolved-shapes))

(defmethod should-compute? :function-connect-shapes [parent-data data]
  (> (count parent-data) 1))
(defmethod compute-node :function-connect-shapes [parent-data data]
  (let [max-shapes (or (:max-shapes data) 1)]
    (if (every? #(contains? % :name) parent-data)
      (jigsaw/connect-shapes-memo parent-data :chord)
      (jigsaw/connect-memo (map :notes parent-data) :chord :max-shapes max-shapes))))

(defmethod should-compute? :function-fit-shape [parent-data data]
  (= (count parent-data) 2))
(defmethod compute-node :function-fit-shape [parent-data data]
  (let [target-shape (first (filter #(contains? % :name) parent-data))
        candidate-input (first (filter #(not= % target-shape) parent-data))
        max-shapes (or (:max-shapes data) 1)
        shapes (jigsaw/fit target-shape (:notes candidate-input) :max-shapes max-shapes)
        resolved-shapes (map #(merge % (jigsaw/->shape (theory/pitch->note (:pitch %)) (:name %))) shapes)]
    resolved-shapes))

(defmethod should-compute? :function-transpose [parent-data data]
  (some? parent-data))
(defmethod compute-node :function-transpose [parent-data data]
  (let [interval (keyword (or (:interval data) "P1"))
        multiplier (or (:multiplier data) 1)]
    (theory/transpose (dissoc parent-data :type :view-type) interval multiplier)))

(defmethod should-compute? :function-chords-by-degrees [parent-data data]
  (and parent-data (theory/scale? parent-data)))
(defmethod compute-node :function-chords-by-degrees [parent-data data]
  (let [scale parent-data
        chord-degrees (or (:chord-degrees data) [])]
    (jigsaw/->progression scale chord-degrees)))

(re-frame/reg-event-fx
 ::update-node-data
 (fn [{:keys [db]} [_ id data]]
   (let [old-data (get-in db [:node-data id])
         new-data (merge old-data data)
         new-db (assoc-in db [:node-data id] new-data)
         parent-data (get-parent-data-for-node new-db id)
         opt-changed? (not-any? #(contains? data %) [:pitch :note :name])

         ;; Only trigger computation for this node, not children yet
         this-node-fx (when (and opt-changed?
                                 (contains? (methods compute-node) (:type new-data))
                                 (should-compute? parent-data new-data))
                        [[:dispatch ^:flush-dom [::compute-function-result id parent-data new-data]]])

         child-fx [[:dispatch ^:flush-dom [::cascade-to-children id]]]]

     {:db new-db
      :fx (concat this-node-fx child-fx)})))

(re-frame/reg-event-fx
 ::compute-function-result
 (fn [{:keys [db]} [_ id parent-data data]]
   {:db (assoc-in db [:node-loading id] true)
    :fx [[:dispatch ^:flush-dom [::execute-function-computation id parent-data data]]]}))

(re-frame/reg-event-fx
 ::execute-function-computation
 (fn [{:keys [db]} [_ id parent-data data]]
   (try
     (let [result (compute-node parent-data data)]
       {:db (-> db
                (assoc-in [:function-results id] result)
                (assoc-in [:node-loading id] false))})
     (catch js/Error e
       (js/console.error "Function computation error:" e)
       {:db (assoc-in db [:node-loading id] false)}))))

(re-frame/reg-event-fx
 ::cascade-to-children
 (fn [{:keys [db]} [_ parent-id]]
   (let [child-ids (get-child-nodes db parent-id)
         child-fx (for [child-id child-ids
                        :let [child-data (get-in db [:node-data child-id])
                              parent-data (get-parent-data-for-node db child-id)]
                        :when (and (contains? (methods should-compute?) (:type child-data))
                                   (should-compute? parent-data child-data))]
                    ; Don't recursively cascade; many of the nodes are nondeterministic and require user actions to proceed
                    [:dispatch [::compute-function-result child-id parent-data child-data]])]
     {:fx child-fx})))

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

(defn play-notes [notes note-offset-ms]
  (let [instrument-name "acoustic_grand_piano"]
    (load-instrument! instrument-name)
    (js/setTimeout
     (fn []
       (when-let [instrument (get-in @audio-state [:instruments instrument-name])]
         (doseq [[i note] (map-indexed vector notes)]
           (js/setTimeout
            (fn []
              (.play instrument (theory/note->midi note)))
            (* i note-offset-ms)))))
     100) ; Small delay to ensure instrument is loaded
    {}))

(re-frame/reg-event-fx
 ::play-shape
 (fn [{:keys [_]} [_ shape]]
   (let [note-offset-ms (if (theory/chord? shape) 30 300)]
     (play-notes (:notes shape) note-offset-ms))))

(re-frame/reg-event-fx
 ::play-notes
 (fn [{:keys [_]} [_ notes]]
   (let [note-offset-ms 30]
     (play-notes notes note-offset-ms))))

;; Drag and drop functionality
(re-frame/reg-event-db
 ::create-node-from-drag
 (fn [db [_ shape-data position]]
   (when (theory/shape-ref? shape-data)
     (let [node-type (cond
                       (contains? theory/chords (:name shape-data)) :input-chord
                       (contains? theory/scales (:name shape-data)) :input-scale
                       :else nil)
           shape (jigsaw/->shape (theory/pitch->note (:pitch shape-data)) (:name shape-data))]
       (if node-type
         (second (create-node db {:type node-type
                                  :position position
                                  :data (assoc shape :view-type :output-piano)}))
         db)))))
