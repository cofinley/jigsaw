(ns jigsaw.ui.components.settings-panel
  (:require
   [re-frame.core :as re-frame]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.subs :as subs]
   [jigsaw.ui.components.button :refer [button]]
   [clojure.string :as str]))

(defn settings-panel []
  (let [access @(re-frame/subscribe [::subs/midi-access])
        inputs (some->> access .-inputs .values)
        outputs (some->> access .-outputs .values)
        current-input @(re-frame/subscribe [::subs/midi-input])
        current-output @(re-frame/subscribe [::subs/midi-output])
        midi-triggers @(re-frame/subscribe [::subs/midi-triggers])
        recording-id @(re-frame/subscribe [::subs/recording-id])
        play-chords-broken? @(re-frame/subscribe [::subs/play-chords-broken?])]
    [:div.flex.flex-col.gap-y-4
     [:label.font-bold "MIDI Input"
      [:select.flex.p-2.rounded.dark:bg-neutral-700.dark:text-neutral-100.border.dark:border-neutral-500
       {:on-change #(re-frame/dispatch [::events/on-midi-select-input (-> % .-target .-value)])
        :value (if (nil? current-input) "" current-input)}
       [:option "Select"]
       (for [input inputs]
         ^{:key (.-id input)}
         [:option (.-name input)])]]
     [:label.font-bold "MIDI Output"
      [:select.flex.p-2.rounded.dark:bg-neutral-700.dark:text-neutral-100.border.dark:border-neutral-500
       {:on-change #(re-frame/dispatch [::events/on-midi-select-output (-> % .-target .-value)])
        :value (if (nil? current-output) "" current-output)}
       [:option "Select"]
       (for [output outputs]
         ^{:key (.-id output)}
         [:option (.-name output)])]]
     [:div.flex.flex-col.gap-2
      [:label.font-bold.flex.gap-x-2 "MIDI triggers"]
      (for [[midi-trigger note] midi-triggers]
        ^{:key midi-trigger}
        [:label.flex.gap-x-2.capitalize (str/replace (name midi-trigger) #"-" " ")
         [button {:on-click (fn [e]
                              (.stopPropagation e)
                              (if (and (not (= recording-id midi-trigger))
                                       (or (not current-input) (= current-input "Select")))
                                (js/alert "Must select MIDI input from settings first")
                                (re-frame/dispatch [::events/toggle-recording midi-trigger])))}
          (if (= recording-id midi-trigger) "Recording..." "Record")]
         [:span
          {:class "w-5 h-5 p-1 rounded"} note]])]
     [:label.font-bold.flex.gap-x-2 "Play chords broken?"
      [:input
       {:class "w-5 h-5 p-1 rounded"
        :type "checkbox" :checked play-chords-broken? :on-change #(re-frame/dispatch [::events/on-play-chords-broken-change (-> % .-target .-checked)])}]]
     [button
      {:class "self-start mt-5"
       :on-click #(re-frame/dispatch [::events/reset-settings])}
      "Reset to Default Settings"]]))
