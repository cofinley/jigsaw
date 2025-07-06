(ns jigsaw.events
  (:require
   ["soundfont-player" :as soundfont]
   [jigsaw.algo :as algo]
   [jigsaw.db :as db]
   [jigsaw.search :as search]
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

(defn should-trigger-computation? [node-type parent-data]
  (case node-type
    :function-scale-chords (and parent-data (contains? parent-data :degrees))
    :function-chord-scales (and parent-data (contains? parent-data :intervals))
    :function-find-shape (and parent-data (seq (:notes parent-data)))
    :function-connect-shapes (> (count parent-data) 1)
    :function-fit-shape (= (count parent-data) 2)
    false))

(defn add-edge [db edge]
  (assoc db :edges (clj->js (conj (js->clj (:edges db))
                                  (clj->js (assoc edge :type :custom-edge))))))

(re-frame/reg-event-fx
 ::add-edge
 (fn [{:keys [db]} [_ edge]]
   (let [new-db (add-edge db edge)
         target-id (:target edge)
         target-data (get-in new-db [:node-data target-id])
         target-type (:type target-data)
         parent-data (get-parent-data-for-node new-db target-id)]
     {:db new-db
      :fx (when (should-trigger-computation? target-type parent-data)
            [[:dispatch ^:flush-dom [::compute-function-result target-id target-type parent-data target-data]]])})))

(re-frame/reg-event-fx
 ::recompute
 (fn [{:keys [db]} [_ id]]
   (let [data (get-in db [:node-data id])
         node-type (:type data)
         parent-data (get-parent-data-for-node db id)]
     (cond-> {:db db}
       (and parent-data (should-trigger-computation? node-type parent-data))
       (assoc :fx [[:dispatch [::compute-function-result id node-type parent-data data]]])))))

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
         node-type (keyword (:type node-props))
         parent-data (when (some? parent-id) (get-parent-data-for-node new-db node-id))
         node-data (get-in new-db [:node-data node-id])]
     (cond-> {:db new-db}
       (and (some? parent-data) (should-trigger-computation? node-type parent-data))
       (assoc :fx [[:dispatch ^:flush-dom [::compute-function-result node-id node-type parent-data node-data]]])))))

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
      (algo/->shape (algo/pitch->note pitch) (keyword name)))))

(re-frame/reg-event-fx
 ::calculate-shape
 (fn [{:keys [db]} [_ id]]
   (let [node (get-in db [:node-data id])
         shape (calculate-shape node)]
     {:fx [[:dispatch ^:flush-dom [::update-node-data id shape]]]})))

;; Function computation helpers
(defn compute-scale-chords [parent-data data]
  (when (and parent-data (contains? parent-data :degrees))
    (let [num-thirds (or (:num-thirds data) 3)
          shape-refs (search/scale->chords parent-data :num-thirds num-thirds)]
      (map #(merge % (algo/->shape (assoc % :note (algo/pitch->note (:pitch %))))) shape-refs))))

(defn compute-chord-scales [parent-data data]
  (when (and parent-data (contains? parent-data :intervals))
    (let [selected-degree (:selected-degree data)
          shape-refs (search/chord->scales parent-data :degree selected-degree)]
      (map #(merge % (algo/->shape (assoc % :note (algo/pitch->note (:pitch %))))) shape-refs))))

(defn compute-closest-shapes [parent-data data]
  (when-let [notes (seq (get-in parent-data [:notes]))]
    (let [incoming-shape-type (cond
                                (contains? parent-data :degrees) :scale
                                (contains? parent-data :intervals) :chord
                                :else :notes)
          selected-shape-type (or (:selected-shape-type data) (if (= :chord incoming-shape-type) :scale :chord))
          selected-pitch (or (:selected-pitch data) "")
          heuristic (or (:heuristic data) :overlap)
          max-shapes (or (:max-shapes data) 10)
          shapes (search/notes->shapes-memo notes
                                            selected-shape-type
                                            :max-shapes max-shapes
                                            :heuristic (keyword heuristic)
                                            :selected-pitch (if (= selected-pitch :all) nil selected-pitch))
          resolved-shapes (map #(merge % (algo/->shape (algo/pitch->note (:pitch %)) (:name %))) shapes)]
      resolved-shapes)))

(defn compute-shape-connections [parent-data data]
  (when (> (count parent-data) 1)
    (let [max-shapes (or (:max-shapes data) 1)]
      (if (every? #(contains? % :name) parent-data)
        (search/memoize-connect-shapes parent-data :chord)
        (search/memoize-connect (map :notes parent-data) :chord :max-shapes max-shapes)))))

(defn compute-fitted-shapes [parent-data data]
  (when (= (count parent-data) 2)
    (let [target-shape (first (filter #(contains? % :name) parent-data))
          candidate-input (first (filter #(not= % target-shape) parent-data))
          max-shapes (or (:max-shapes data) 1)
          shapes (search/fit target-shape (:notes candidate-input) :max-shapes max-shapes)
          resolved-shapes (map #(merge % (algo/->shape (algo/pitch->note (:pitch %)) (:name %))) shapes)]
      resolved-shapes)))

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
         new-db (assoc-in db [:node-data id] new-data)
         node-type (:type new-data)
         parent-data (get-parent-data-for-node new-db id)
         opt-changed? (not-any? #(contains? data %) [:pitch :note :name])

         ;; Only trigger computation for this node, not children yet
         this-node-fx (when (and opt-changed?
                                 (should-trigger-computation? node-type parent-data))
                        [[:dispatch ^:flush-dom [::compute-function-result id node-type parent-data new-data]]])

         child-fx [[:dispatch ^:flush-dom [::cascade-to-children id]]]]

     {:db new-db
      :fx (concat this-node-fx child-fx)})))

(re-frame/reg-event-fx
 ::compute-function-result
 (fn [{:keys [db]} [_ id node-type parent-data opts]]
   {:db (assoc-in db [:node-loading id] true)
    :fx [[:dispatch ^:flush-dom [::execute-function-computation id node-type parent-data opts]]]}))

(re-frame/reg-event-fx
 ::execute-function-computation
 (fn [{:keys [db]} [_ id node-type parent-data opts]]
   (try
     (let [result (case node-type
                    :function-scale-chords (compute-scale-chords parent-data opts)
                    :function-chord-scales (compute-chord-scales parent-data opts)
                    :function-find-shape (compute-closest-shapes parent-data opts)
                    :function-connect-shapes (compute-shape-connections parent-data opts)
                    :function-fit-shape (compute-fitted-shapes parent-data opts)
                    nil)]
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
                              parent-data (get-parent-data-for-node db child-id)
                              child-type (:type child-data)]
                        :when (should-trigger-computation? child-type parent-data)]
                    [:dispatch [::compute-function-result child-id child-type parent-data child-data]])]
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

;; Drag and drop functionality
(re-frame/reg-event-db
 ::create-node-from-drag
 (fn [db [_ shape-data position]]
   (when (specs/shape-ref? shape-data)
     (let [node-type (cond
                       (contains? specs/chords (:name shape-data)) :input-chord
                       (contains? specs/scales (:name shape-data)) :input-scale
                       :else nil)
           shape (algo/->shape (algo/pitch->note (:pitch shape-data)) (:name shape-data))]
       (if node-type
         (second (create-node db {:type node-type
                                  :position position
                                  :data (assoc shape :view-type :output-piano)}))
         db)))))
