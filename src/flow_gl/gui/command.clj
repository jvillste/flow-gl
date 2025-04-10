(ns flow-gl.gui.command
  (:require
   [clojure.string :as string]
   [clojure.test :refer :all]
   [flow-gl.gui.keyboard :as keyboard]
   [flow-gl.gui.scene-graph :as scene-graph]
   [flow-gl.gui.visuals :as visuals]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.derivation :as derivation]
   [fungl.layouts :as layouts]
   [medley.core :as medley]))

(derivation/def-derivation focused-subtrees-with-command-sets
  (if-let [focused-node-id @keyboard/focused-node-id-derivation]
    ;;    TODO: this path-to call must be cached because otherwise this derivation invalidates the view compilation. There are anonyme closueres in text editor command set which make the consecutive values unequal even though they are in practice the same
    (->> (scene-graph/path-to #_scene-graph/current-scene-graph ;; @scene-graph/current-scene-graph-atom is not dependable so this derivation does not get invalidated when the scene graph changes
                              ;;(:scene-graph @keyboard/state-atom)
                              @scene-graph/current-scene-graph-atom
                              focused-node-id)
         (filter :command-set)
         (reverse))
    []))

;; (defn command-help-container-keyboard-event-handler [show-help? state-atom _scene-graph event]
;;   (when (not (:handled? event))
;;     (let [focused-subtrees-with-command-sets @focused-subtrees-with-command-sets]
;;       (if (empty? focused-subtrees-with-command-sets)
;;         event
;;         (if (and show-help?
;;                  (= :descent (:phase event))
;;                  (= :key-pressed (:type event))
;;                  (not (:alt? event))
;;                  (not (:shift? event))
;;                  (not (:control? event))
;;                  (not (:meta? event))
;;                  (:character event)
;;                  (not (= :enter (:key event)))
;;                  (not (= :escape (:key event))))
;;           (if (= :back-space (:key event))
;;             (swap! state-atom update :text (fn [old-text]
;;                                              (subs old-text 0 (max 0
;;                                                                    (dec (count old-text))))))
;;             (swap! state-atom update :text str (:character event)))
;;           event)))))


;; (defn command-help [triggered-key-patterns query-text command-sets]
;;   (ver 20
;;        (when (not (empty? query-text))
;;          (text (string/replace query-text
;;                                #"[^a-z]"
;;                                "")))
;;        (when (not (empty? triggered-key-patterns))
;;          (text triggered-key-patterns))
;;        (for [command-set command-sets]
;;          (ver 0
;;               (text (:name command-set)
;;                     {:font bold-font})
;;               (for [command (filter (fn [command]
;;                                       (and (string/includes? (:name command)
;;                                                              query-text)
;;                                            (keyboard/key-patterns-prefix-match? triggered-key-patterns
;;                                                                                 (:key-patterns command))))
;;                                     (:commands command-set))]
;;                 (text (str (pr-str (:key-patterns command))
;;                            " "
;;                            (:name command))
;;                       {:color (if (:available? command)
;;                                 (:text-color theme)
;;                                 (add-color (:text-color theme)
;;                                            [0 0 0 -100]))}))))))

;; (defn remove-runs [command-set]
;;   (update command-set :commands (fn [commands]
;;                                   (map #(dissoc % :run!)
;;                                        commands))))

;; (defn command-help-container [_show-help? _child]
;;   (let [state-atom (dependable-atom/atom "command-help-container-state"
;;                                          {:text ""})]
;;     (fn [show-help? child]
;;       (let [focused-subtrees-with-command-sets @focused-subtrees-with-command-sets]
;;         (-> (layouts/vertical-split child
;;                                     (when show-help?
;;                                       (ver 10
;;                                            (assoc (visuals/rectangle-2 :fill-color (:text-color theme))
;;                                                   :height 10)
;;                                            {:node [command-help
;;                                                    (:triggered-key-patterns @state-atom)
;;                                                    (:text @state-atom)
;;                                                    (map (comp remove-runs :command-set)
;;                                                         focused-subtrees-with-command-sets)]
;;                                             :local-id :command-help})))
;;             (assoc :keyboard-event-handler [command-help-container-keyboard-event-handler show-help? state-atom]))))))






(defn key-pattern-sequences [key-patterns]
  (if (every? (comp set? first)
              key-patterns)
    [key-patterns]
    key-patterns))

(deftest test-key-pattern-sequences
  (let [single-key-pattern-sequence [[#{:control} :e]
                                     [#{:control} :a]]]
    (is (= [single-key-pattern-sequence]
           (key-pattern-sequences single-key-pattern-sequence)))

    (is (= [single-key-pattern-sequence
            single-key-pattern-sequence]
           (key-pattern-sequences [single-key-pattern-sequence
                                   single-key-pattern-sequence])))))


(defn command-handler-keyboard-event-handler [state-atom _scene-graph event]
  (when (not (:handled? event))
    (let [focused-subtrees-with-command-sets @focused-subtrees-with-command-sets]
      (if (empty? focused-subtrees-with-command-sets)
        event
        (if (and (= :ascent (:phase event))
                 (= :key-pressed (:type event)))
          (let [triggered-key-patterns (conj (:triggered-key-patterns @state-atom)
                                             (keyboard/event-to-key-pattern event))
                possible-commands-and-subtrees (->> focused-subtrees-with-command-sets
                                                    (mapcat (fn [subtree]
                                                              (for [command (:commands (:command-set subtree))]
                                                                {:subtree subtree
                                                                 :command command})))
                                                    (filter (fn [command-and-subtree]
                                                              (and (:available? (:command command-and-subtree))
                                                                   (some (partial keyboard/key-patterns-prefix-match? triggered-key-patterns)
                                                                         (key-pattern-sequences (:key-patterns (:command command-and-subtree))))))))]
            (if (empty? possible-commands-and-subtrees)
              (do (swap! state-atom assoc :triggered-key-patterns [])
                  event)
              (if-let [matched-command-and-subtree (medley/find-first (fn [command-and-subtree]
                                                                        (keyboard/key-patterns-match? triggered-key-patterns
                                                                                                      (:key-patterns (:command command-and-subtree))))
                                                                      possible-commands-and-subtrees)]
                (do ((:run! (:command matched-command-and-subtree))
                     (:subtree matched-command-and-subtree))
                    (swap! state-atom assoc :text "")
                    nil)
                (do (swap! state-atom assoc :triggered-key-patterns triggered-key-patterns)
                    event))))
          event)))))

(defn command-handler [_child]
  (let [state-atom (dependable-atom/atom "command-handler-state"
                                         {:triggered-key-patterns []})]
    (fn [child]
      {:node {:children [child]}
       :keyboard-event-handler [command-handler-keyboard-event-handler state-atom]})))
