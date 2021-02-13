(ns firestone.core
  (:require
    [clojure.test :refer [function?]]
    [ysera.test :refer [is is-not is= error?]]
    [ysera.error :refer [error]]
    [ysera.collections :refer [seq-contains?]]
    [firestone.definitions :refer [get-definition]]
    [clojure.pprint :refer [pprint]]
    [firestone.spec :as fspec]
    [firestone.utils :refer [remove-first]]
    [firestone.damage-entity :refer [damage-entity]]
    [clojure.spec.alpha :as spec]
    [firestone.info :refer
     [get-health get-owner get-hero sleepy? is-hero? frozen? stealth?
      get-opponent-player-id valid-attack? is-minion? get-entity get-max-health
      get-attack get-mana get-max-mana get-all-targetable-ids]]
    [firestone.events :refer [fire-start-of-turn-event fire-end-of-turn-event]]
    [firestone.construct :refer
     [create-card get-hand create-game add-minion-to-board create-hero
      create-minion get-heroes remove-minion get-minion update-minion
      add-minion-to-board get-minions is-player? get-secrets remove-secret]]
    [firestone.mana :refer [refill-mana increase-max-mana]]))


(declare freeze-entity)

(defn remove-stealth
  {:test (fn []
           (is-not (-> (create-game
                         [{:minions [(create-minion "Blood Imp" :id "b1")]}])
                       (remove-stealth "b1")
                       (stealth? "b1")))
           (is-not (-> (create-game
                         [{:minions [(create-minion "War Golem" :id "wg1")]}])
                       (remove-stealth "wg1")
                       (stealth? "wg1"))))}
  [state entity-id]
  (update-minion state
                 entity-id
                 :states
                 (fn [states] (dissoc states :stealth))))

(defn update-seed
  "Update the random seed."
  [state new-seed]
  {:pre [(int? new-seed)]}
  (assoc state :seed new-seed))


(defn switch-side-minion
  "Make minion switch side. Used for sylvanas and similar effect."
  {:test (fn []
           (as-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}
                               {:minions
                                  [(create-minion "War Golem" :id "wg1")]}]) $
             (switch-side-minion $ "wg1")
             (do (is= (count (get-minions $ "p1")) 2)
                 (is= (count (get-minions $ "p2")) 0)
                 (is= (get-owner $ "wg1") "p1")
                 (is= (get-owner $ "i1") "p1")
                 (as-> (switch-side-minion $ "wg1") $
                   (switch-side-minion $ "i1")
                   (do (is= (count (get-minions $ "p2")) 2)
                       (is= (count (get-minions $ "p1")) 0)
                       (is= (get-owner $ "wg1") "p2")
                       (is= (get-owner $ "i1") "p2"))))))}
  [state minion-id]
  (let [minion (get-entity state minion-id)
        new-owner-id (get-opponent-player-id state (:owner-id minion))]
    (-> (remove-minion state minion-id)
        (add-minion-to-board {:player-id new-owner-id,
                              :minion (assoc minion :owner-id new-owner-id),
                              :position 0}))))




(defn heal-entity
  "Heal an entity, hero or minion"
  {:test
     (fn []
       (is=
         (-> (create-game
               [{:hero (create-hero "Jaina Proudmoore" :id "h1" :health 25)}])
             (heal-entity "h1" 4)
             (get-health "h1"))
         29)
       (is=
         (-> (create-game
               [{:hero (create-hero "Jaina Proudmoore" :id "h1" :health 25)}])
             (heal-entity "h1" 6)
             (get-health "h1"))
         30)
       (is= (-> (create-game
                  [{:minions [(create-minion "Ogre Magi" :id "i" :health 1)]}])
                (heal-entity "i" 3)
                (get-health "i"))
            4)
       (is= (-> (create-game
                  [{:minions [(create-minion "Ogre Magi" :id "i" :health 1)]}])
                (heal-entity "i" 4)
                (get-health "i"))
            4))}
  [state entity-id amount]
  ; TODO throw exception on negative amount?
  ; TODO could be unified such that the cond is unnecessary? using something
  ; like update-entity[state entity-id]
  ; TODO how do we test the exception?
  (cond
    (is-hero? state entity-id) ; heal hero
      (let [owner (get-owner state entity-id)
            hero (get-hero state owner)]
        (assoc-in state
          [:players owner :hero :health]
          (min (:max-health hero) (+ (:health hero) amount))))
    (is-minion? state entity-id) ; heal minion
      (let [owner (get-owner state entity-id)
            minion (get-minion state entity-id)]
        (update-minion state
                       entity-id
                       :health
                       (min (:max-health minion)
                            (+ (:health minion) amount))))
    :else (throw (Exception.
                   (str "Entity id \"" entity-id "\" does not exist.")))))

(defn add-buff
  {:test
     (fn []
       (as-> (create-game
               [{:minions [(create-minion "War Golem" :id "wg1" :health 4)]}]) $
         (add-buff $ "wg1" {:name "Test", :attack-buff 1, :max-health-buff 1})
         (do ;(pprint $)
             (is= (get-health $ "wg1") 5)
             (is= (get-max-health $ "wg1") 8)
             (is= (get-attack $ "wg1") 8))))}
  [state minion-id buff]
  (as-> (update-minion
          state
          minion-id
          :states
          (fn [states]
            (update
              states
              :buffs
              (fn [old-buffs]
                (-> (if (or (vector? old-buffs) (list? old-buffs)) old-buffs [])
                    (conj buff))))))
    $
    ; If the health was changed, must update the health of the minion as well
    ; This does not count as a heal but functions similarly to it
    (update-minion $
                   minion-id
                   :health
                   (+ (:health (get-entity $ minion-id))
                      (get buff :max-health-buff 0)))))

(defn change-stats-minion
  "Change the stats of a minion with the given health and damage. Adds a new If dealing damage to a minion, use damage-minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
                    (change-stats-minion "i" "Test buff" 1 0)
                    (get-health "i"))
                2)
           (is= (-> (create-game
                      [{:minions [(create-minion "Imp" :id "i" :health 2)]}])
                    (change-stats-minion "i" "Test buff2" -1 0)
                    (get-health "i"))
                1))}
  [state minion-id buff-name max-health-buff attack-buff]
  {:pre [(map? state) (string? minion-id) (string? buff-name)
         (int? max-health-buff) (int? attack-buff)
         (or (not= 0 max-health-buff) (not= 0 attack-buff))]}
  (let [buff {:name buff-name}]
    ; If attack is not given or zero, don't add an attack buff
    (as-> (if (not= attack-buff 0) (assoc buff :attack-buff attack-buff) buff) $
      ; If health is not given or zero, don't add a health buff
      (if (not= max-health-buff 0) (assoc $ :max-health-buff max-health-buff) $)
      (add-buff state minion-id $))))

(defn buff-health
  [state minion-id buff-name health-buff]
  (change-stats-minion state minion-id buff-name health-buff 0))

(defn buff-attack
  [state minion-id buff-name attack-buff]
  (change-stats-minion state minion-id buff-name 0 attack-buff))

(comment (-> (update-minion state
                            minion-id
                            :attack
                            (fn [previous-attack] (+ previous-attack attack)))
             (update-minion minion-id
                            :max-health
                            (fn [previous-health] (+ previous-health health)))
             (update-minion minion-id
                            :health
                            (fn [previous-health] (+ previous-health health)))))

(defn overdraw-card
  "Overdraw one card. Only works if the player has no cards in the deck."
  {:test (fn []
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (overdraw-card "p1")
                    (get-health "h1"))
                29)
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (overdraw-card "p1")
                    (overdraw-card "p1")
                    (get-health "h1"))
                27))}
  [state player-id]
  {:pre [(= 0 (count (get-in state [:players player-id :deck])))
         (string? player-id) (map? state)]}
  (let [keys-to-overdraw [:players player-id :cards-overdrawn]
        hero-id (:id (get-hero state player-id))]
    (do (as-> (update-in state keys-to-overdraw inc) $
          (damage-entity $ hero-id (get-in $ keys-to-overdraw))))))

(defn pop-top-card-from-deck
  "Returns the top card from the deck and the state of the game after the card is removed"
  [state player-id]
  [(update-in state [:players player-id :deck] rest)
   (first (get-in state [:players player-id :deck]))])


(defn freeze-hero
  "Update the :frozen status (true or false) of a hero."
  {:test (fn []
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (freeze-hero "h1" true)
                    (get-entity "h1")
                    (:frozen))
                true)
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (freeze-hero "h1" true)
                    (freeze-hero "h1" false)
                    (get-entity "h1")
                    (:frozen))
                false))}
  [state hero-id status]
  ; TODO: are there event triggers to fire in freeze-hero?
  (assoc-in state [:players (get-owner state hero-id) :hero :frozen] status))


(defn freeze-minion
  "Update the :frozen status (true or false) of a minion."
  {:test
     (fn []
       (is= (-> (create-game
                  [{:minions [(create-minion (create-card "Imp") :id "c1")]}])
                (freeze-minion "c1" true)
                (get-entity "c1")
                (:frozen))
            true)
       (is= (-> (create-game
                  [{:minions [(create-minion (create-card "Imp") :id "c1")]}])
                (freeze-minion "c1" true)
                (freeze-minion "c1" false)
                (get-entity "c1")
                (:frozen))
            false))}
  [state minion-id status]
  ; TODO: are there event triggers to fire in freeze-minion?
  (update-minion state minion-id :frozen status))

(defn freeze-entity
  "Freeze an entity."
  {:test (fn []
           (is
             (-> (create-game
                   [{:minions [(create-minion (create-card "Imp") :id "c1")]}])
                 (freeze-entity "h2")
                 (get-entity "h2")
                 (:frozen))))}
  ; TODO: tests
  [state entity-id]
  ; TODO: are there event triggers to fire in freeze-entity?
  (cond (is-minion? state entity-id) (freeze-minion state entity-id true)
        (is-hero? state entity-id) (freeze-hero state entity-id true)
        :else (throw (Exception.
                       (str "Entity id \"" entity-id "\" does not exist.")))))


(defn map-minions
  "Given a state and a function, apply the function to every minion in the state.
     Example: (map-minions state f) will apply (f state minion-id) for every minion in the state,
              (map-minions state f args) will apply (f state minion-id args) for every minion in the state."
  {:test
     (fn []
       (as-> (create-game
               [{:minions [(create-minion (create-card "Blood Imp") :id "1")
                           (create-minion (create-card "Imp") :id "2")]}
                {:minions
                   [(create-minion (create-card "Blood Imp") :id "3")
                    (create-minion (create-card "Shrinkmeister") :id "4")]}]) $
         (do (is= (get-in (get-minion $ "1") [:states :stealth]) true)
             (is= (get-in (get-minion $ "2") [:states :stealth]) nil)
             (is= (get-in (get-minion $ "3") [:states :stealth]) true)
             (is= (get-in (get-minion $ "4") [:states :stealth]) nil)
             $)
         (map-minions $ remove-stealth)
         (do (is= (get-in (get-minion $ "1") [:states :stealth]) nil)
             (is= (get-in (get-minion $ "2") [:states :stealth]) nil)
             (is= (get-in (get-minion $ "4") [:states :stealth]) nil)
             (is= (get-in (get-minion $ "3") [:states :stealth]) nil)
             $)
         (do (is= (get-in (get-minion $ "1") [:buffs 0 :name]) nil)
             (is= (get-in (get-minion $ "2") [:buffs 0 :name]) nil)
             (is= (get-in (get-minion $ "3") [:buffs 0 :name]) nil)
             (is= (get-in (get-minion $ "4") [:buffs 0 :name]) nil)
             $)
         (map-minions $
                      add-buff
                      {:name "Test", :attack-buff 1, :max-health-buff 1})
         (do (is= (get-in (get-minion $ "1") [:states :buffs 0 :name]) "Test")
             (is= (get-in (get-minion $ "2") [:states :buffs 0 :name]) "Test")
             (is= (get-in (get-minion $ "3") [:states :buffs 0 :name]) "Test")
             (is= (get-in (get-minion $ "4") [:states :buffs 0 :name]) "Test")
             $)
         (map-minions $
                      add-buff
                      (fn [minion] (= (get-owner minion) "p1"))
                      {:name "OnlyP1", :attack-buff 1, :max-health-buff 1})
         (do (is= (count (get-in (get-minion $ "1") [:states :buffs])) 2)
             (is= (count (get-in (get-minion $ "2") [:states :buffs])) 2)
             (is= (count (get-in (get-minion $ "3") [:states :buffs])) 1)
             (is= (count (get-in (get-minion $ "4") [:states :buffs])) 1)
             $)))}
  ([state f]
   {:pre [(map? state) (function? f)]}
   (reduce (fn [state entity-id] (f state entity-id))
     state
     (map :id (get-minions state))))
  ([state f args-or-filter-fn]
   {:pre [(map? state) (function? f)]}
   (if (function? args-or-filter-fn)
     (map-minions state f args-or-filter-fn nil)
     (map-minions state f identity args-or-filter-fn)))
  ([state f filter-fn args]
   {:pre [(map? state) (function? f) (function? filter-fn)]}
   (if (nil? args)
     (reduce (fn [state entity-id] (f state entity-id))
       state
       (map :id (filter filter-fn (get-minions state))))
     (reduce (fn [state entity-id] (f state entity-id args))
       state
       (map :id (filter filter-fn (get-minions state)))))))


(defn remove-secrets
  "Given only a state, removes ALL secrets. Given a state and a player id, removes all secrets
     belonging to the given player id."
  {:test (fn []
           (as-> (create-game [{:secrets [(create-card "Snake Trap")]}
                               {:secrets [(create-card "Snake Trap")]}]) $
             (do (is= (count (get-secrets $)) 2)
                 (is= (count (get-secrets (remove-secrets $ "p1"))) 1)
                 (is= (count (get-secrets (remove-secrets $ "p2"))) 1)
                 (is= (count (get-secrets (remove-secrets $))) 0)
                 $)))}
  ([state]
   {:pre [(map? state)]}
   (reduce (fn [state secret] (remove-secret state (:id secret)))
     state
     (get-secrets state)))
  ([state player-id]
   {:pre [(map? state) (is-player? state player-id)]}
   (reduce (fn [state secret] (remove-secret state (:id secret)))
     state
     (get-secrets state player-id))))

(defn give-effect
  [state minion-id effect-type effect]
  (update-minion state
                 minion-id
                 :states
                 (fn [states]
                   (let [old-effects-of-right-type
                           (get-in states [:effect effect-type] [])]
                     (as-> (conj old-effects-of-right-type effect) $
                       (assoc-in states [:effect effect-type] $))))))

(defn remove-one-buff
  [state minion-id buff-name]
  (update-minion
    state
    minion-id
    :states
    (fn [states]
      (as-> (update states
                    :buffs
                    (fn [buffs]
                      (remove-first buffs
                                    (fn [buff] (= (:name buff) buff-name))))) $
        (if (empty? (get $ :buffs)) (dissoc $ :buffs) $)))))


(defn remove-one-effect
  "Remove one effect of type <effect-type> if it matches the <effect>. Tests can be found in minion_tests under abusive sergeant"
  [state minion-id effect-type effect]
  (update-minion
    state
    minion-id
    :states
    (fn [states]
      (as-> (update-in states
                       [:effect effect-type]
                       (fn [effects] (remove-first effects effect))) $
        (update $
                :effect
                (fn [effects]
                  (as-> (if (empty? (get effects effect-type))
                    (dissoc effects effect-type)
                    effects) $
                  (if (empty? $)
                    nil
                    $))))))))


(defn unfreeze-entity
  [state id]
  (cond
    (is-hero? state id) (freeze-hero state id false)
    (is-minion? state id) (freeze-minion state id false)
    :else
      (error
        "Not supporting any entities other than heroes and minions at the moment.")))

(defn unfreeze-entities
  "Unfreeze all entities for a player."
  {:test (fn []
           ; Test if minions are unfrozen
           (is-not
             (-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}])
                 (freeze-entity "i1")
                 (unfreeze-entities "p1")
                 (frozen? "i1"))))}
  [state player-id]
  (loop [state state
         entities (conj (map :id (get-minions state player-id))
                        (:id (get-hero state player-id)))]
    (if (empty? entities)
      state
      (recur (unfreeze-entity state (first entities)) (rest entities)))))
