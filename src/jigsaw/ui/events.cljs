(ns jigsaw.ui.events
  (:require
   [clojure.string :as str]
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

;; Computation declarations
(defmulti should-compute? (fn [parent-data data] (:type data)))
(defmulti compute-node (fn [parent-data data] (:type data)))

;; Graph functions

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
            [[:dispatch ^:flush-dom [::recompute target-id]]])})))

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
       (assoc :fx [[:dispatch ^:flush-dom [::recompute node-id]]])))))

(defn delete-node [db id]
  (-> db
      (assoc :nodes (clj->js (remove #(= id (get % "id"))
                                     (js->clj (:nodes db)))))
      (assoc :edges (remove #(or (= id (.-source %))
                                 (= id (.-target %)))
                            (:edges db)))
      (update :node-data dissoc id)
      (update :function-results dissoc id)
      (update :node-loading dissoc id)))

(re-frame/reg-event-db
 ::delete-node
 (fn [db [_ id]]
   (delete-node db id)))

(defn get-child-nodes [db parent-id]
  (let [edges (:edges db)
        child-edges (filter #(= (.-source %) parent-id) edges)
        child-ids (map #(.-target %) child-edges)]
    child-ids))

(re-frame/reg-event-fx
 ::update-node-data
 (fn [{:keys [db]} [_ id data]]
   (let [old-data (get-in db [:node-data id])
         new-data (merge old-data data)
         new-db (assoc-in db [:node-data id] new-data)]
     {:db new-db
      :fx [[:dispatch ^:flush-dom [::recompute id]]]})))

;; Computation

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
        multiplier (or (:multiplier data) 1)
        result (theory/transpose (select-keys parent-data [:pitch :pitches :intervals :degrees :name :notes]) interval multiplier)]
    result))

(defmethod should-compute? :function-chords-by-degrees [parent-data data]
  (and parent-data (theory/scale? parent-data)))
(defmethod compute-node :function-chords-by-degrees [parent-data data]
  (let [scale parent-data
        chord-degrees (map #(keyword "chord-degree" %) (str/split (or (:chord-degrees-str data) "") #"\s+"))]
    (map #(merge % (jigsaw/->shape (theory/pitch->note (:pitch %)) (:name %))) (jigsaw/->progression scale chord-degrees))))

; Recompute current node, kick off recomputation for children
(re-frame/reg-event-fx
 ::recompute
 (fn [{:keys [db]} [_ id]]
   (let [data (get-in db [:node-data id])
         parent-data (get-parent-data-for-node db id)
         #_#_opt-changed? (not-any? #(contains? data %) [:pitch :note :name])
         should-update? (and #_opt-changed?
                         (contains? (methods compute-node) (:type data))
                             (should-compute? parent-data data))
         this-node-fx (when should-update?
                        [[:dispatch ^:flush-dom [::toggle-loading id true]]
                         [:dispatch ^:flush-dom [::execute-function-computation id]]
                         [:dispatch ^:flush-dom [::toggle-loading id false]]])
         child-fx (for [child-id (get-child-nodes db id)]
                    [:dispatch [::recompute child-id]])]
     {:fx (concat this-node-fx child-fx)})))

(re-frame/reg-event-db
 ::toggle-loading
 (fn [db [_ id loading?]]
   (assoc-in db [:node-loading id] loading?)))

(re-frame/reg-event-db
 ::execute-function-computation
 (fn [db [_ id]]
   (let [data (get-in db [:node-data id])
         parent-data (get-parent-data-for-node db id)]
     (try
       (let [result (compute-node parent-data data)]
         (cond-> db
           true (assoc-in [:function-results id] result)
           ; If scalar result, use it automatically
           (not (sequential? result)) (update-in [:node-data id] merge result)))
       (catch js/Error e
         (js/console.error "Function computation error:" e)
         db)))))

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
