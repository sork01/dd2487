(ns
  ^{:doc
      "Functions that do not return the state, only information about the state."}
  firestone.info
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.collections :refer [seq-contains?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [difference]]
            [firestone.construct :refer
             [create-game create-secret is-player? get-player create-card
              create-hero create-minion update-minion add-minion-to-board]]))

(declare valid-attack?)
(declare is-hero?)
(declare get-mana)
(declare is-minion?)
(declare get-hero)
(declare get-owner)
(declare get-stats-with-buffs)
(declare get-minions)
(declare get-owner)
(declare get-opponent-player-id)
(declare stealth?)
(declare get-attack)
(declare get-function-from-effect-description)
(declare has-enough-mana?)

; TODO maybe move these to .info

(def get-minions firestone.construct/get-minions)
(def get-heroes firestone.construct/get-heroes)

(def get-secrets firestone.construct/get-secrets)

(def get-entity firestone.construct/get-entity)

(def get-minion firestone.construct/get-minion)
(def get-hand firestone.construct/get-hand)
(def get-deck firestone.construct/get-deck)

(defn is-players-turn?
  [state player-id]
  (= (:player-id-in-turn state) player-id))

(defn get-spell-effect
  "Returns the spell effect accordning to its definition."
  ; TODO is there a case were the spell effect is rewritten?
  [card-name-or-entity]
  (let [card-name (if (map? card-name-or-entity)
                    (:name card-name-or-entity)
                    card-name-or-entity)
        card-def (get-definition card-name)]
    (:spell-effect card-def)))

(defn has-card?
  " See if player has card "
  {:test (fn []
           (is (-> (create-game [{:hand [(create-card "Imp" :id "c1")]}])
                   (has-card? "p1" "c1")))
           (is (-> (create-game [{} {:hand [(create-card "Imp" :id "c1")]}])
                   (has-card? "p2" "c1")))
           (is-not (-> (create-game [{:hand [(create-card "Imp" :id "c1")]}])
                       (has-card? "p1" "c99"))))}
  [state player-id card-id]
  (not (empty? (filter (fn [card] (= (get card :id) card-id))
                 (get-in state [:players player-id :hand])))))

(defn can-use-hero-power?
  "Given a player-id or hero-id, check whether the hero is currently able to use its hero power."
  {:test (fn []
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    ; test using hero id
                    (can-use-hero-power? "h1"))
                true)
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    ; test using player id
                    (can-use-hero-power? "p1"))
                true)
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (assoc-in [:players "p1" :hero :times-hero-power-used] 1)
                    (can-use-hero-power? "h1"))
                false)
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (assoc-in [:players "p1" :hero :times-hero-power-used] 1)
                    (can-use-hero-power? "p1"))
                false)
           ; can't use hero power because it is not their turn
           (is-not (-> (create-game
                         [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                       (assoc-in [:players "p1" :hero :times-hero-power-used] 1)
                       (can-use-hero-power? "p2"))))}
  [state player-or-hero-id]
  (as-> (cond (is-player? state player-or-hero-id) (get-hero state
                                                             player-or-hero-id)
              :else (get-entity state player-or-hero-id)) $
    (and (is-players-turn? state (get-owner state (:id $)))
         ; TODO change this if hero power mana cost can be changed
         (has-enough-mana? state (get-owner state (:id $)) 2)
         (= (:times-hero-power-used $) 0))))

(defn get-total-spell-damage
  "Get the sum of all spelldamage on one side of the board."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Imp")
                                             (create-minion "Ogre Magi")
                                             (create-minion "Dalaran Mage")]}
                                  {:minions [(create-minion "Ogre Magi")]}])
                    (get-total-spell-damage "p1"))
                2)
           ; test a minion that has two buffs
           (is= (-> (create-game [{:minions [{:auras [{:spell-damage 1}
                                                      {:spell-damage 1}],
                                              :id "fakeminion1"}]}])
                    (get-total-spell-damage "p1"))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Malygos")]}
                                  {:minions [(create-minion "Ogre Magi")]}])
                    (get-total-spell-damage "p1"))
                5))}
  [state player-id]
  (let [minions (get-minions state player-id)]
    (reduce (fn [previous-spell-damage minion]
              (+
                (reduce
                  #(+ %1 (if (contains? %2 :spell-damage) (:spell-damage %2) 0))
                  0
                  (get minion :auras []))       ;(get (get minion :auras [])
                ;:spell-damage 0)
                previous-spell-damage))
      0
      minions)))


(defn get-all-targetable-ids
  "Given a state, returns a vector of all targetable entity ids"
  {:test (fn []
           (as-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}
                               {:minions [(create-minion "Imp" :id "i2")]}]) $
             (do (is= (get-all-targetable-ids $) ["i1" "i2" "h1" "h2"]))))}
  [state]
  {:pre [(map? state)]}
  (into [] (map #(:id %) (concat (get-minions state) (get-heroes state)))))


(defn get-default-minion-targets
  "Given a state and a minion id, returns a vector of all non-friendly entity ids"
  {:test (fn []
           (as-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}
                               {:minions [(create-minion "Imp" :id "i2")]}]) $
             (do (is= (get-default-minion-targets $ "i1") ["i2" "h2"]))))}
  [state minion-id]
  {:pre [(map? state) (is-minion? state minion-id)]}
  (let [owner (:owner-id (get-minion state minion-id))]
    (filter #(not= owner (get-owner state %)) (get-all-targetable-ids state))))


(defn get-default-hero-power-targets
  "Given a state and a player id, returns a vector of all entities except for stealthed non-friendly minions."
  {:test (fn []
           (as-> (create-game [{:minions [(create-minion "Imp" :id "1")
                                          (create-minion "Moroes" :id "2")]},
                               {:minions [(create-minion "Imp" :id "3")
                                          (create-minion "Moroes" :id "4")]}]) $
                 (do (is= (count (get-default-hero-power-targets $ "p1")) 5)
                     (is= (difference (set (get-default-hero-power-targets $ "p1")) #{"h1", "h2", "1", "2", "3"}) #{}))))}
  [state player-id]
  {:pre [(map? state) (is-player? state player-id)]}
  (filter (fn [entity-id] (not (and (= (get-owner state entity-id)
                                       (get-opponent-player-id state player-id))
                                    (stealth? state entity-id))))
          (get-all-targetable-ids state)))


(defn get-valid-targets
  "Given a state and a minion id, returns a vector of entity ids that the minion is able to attack."
  ; TODO: get-valid-targets needs more tests, especially interesting ones where
  ; not everything is targetable
  {:test (fn []
           (as-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}
                               {:minions [(create-minion "Imp" :id "i2")]}]) $
             (do (is= (get-valid-targets $ "i1") ["i2" "h2"]))))}
  [state minion-id]
  {:pre [(map? state) (is-minion? state minion-id)]}
  (let [minion (get-minion state minion-id)
        card-def (get-definition (:name minion))]
    (filter #(valid-attack? state minion-id %)
      (if (contains? card-def :valid-targets)
        ((:valid-targets card-def) state minion)
        (get-default-minion-targets state minion-id)))))

(defn can-attack?
  [state entity-id]
  (cond (is-minion? state entity-id)
          (and (is-players-turn? state (get-owner state entity-id))
               (< 0 (count (get-valid-targets state entity-id))))
        ; TODO change this if weapons are implemented
        (is-hero? state entity-id) false
        :else (error "Could not find owner of id.")))

(defn get-valid-card-targets
  "Return a map of valid targets for cards that needs targets"
  {:test (fn []
           (as-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}]) $
             (do (is= (get-valid-card-targets $ "p1" "Frostbolt")
                      '("i1" "h1" "h2"))
                 (is= (get-valid-card-targets $ "p2" "Mind Control") ["i1"])
                 (is= (get-valid-card-targets $ "p1" "Big Game Hunter") nil))))}
  [state player-id card-name-or-entity]
  {:pre [(map? state)
         (or (map? card-name-or-entity) (string? card-name-or-entity))],
   ; Checks that all targets found are IDs
   :post [(empty? (filter (fn [e] (not (string? e))) %))]}; :post
  (let [card-name (if (map? card-name-or-entity)
                    (:name card-name-or-entity)
                    card-name-or-entity)
        card-def (get-definition card-name)
        targets-func (:valid-targets-when-card-played card-def)]
    (if (nil? targets-func) nil (targets-func state player-id))))


(defn valid-card-target?
  "See if the target is valid when the card is played. Used mostly for spells and battlecries."
  {:test (fn []
            (is= (as-> (create-game [{:hand [(create-card "Fireball" :id "fireball")]},
                                     {:minions [(create-minion "Moroes" :id "moroes")]}]) $
                       (valid-card-target? $ "p1" "Fireball" "moroes"))
                 false)
            (is= (as-> (create-game [{:hand [(create-card "Fireball" :id "fireball")]},
                                     {:minions [(create-minion "Moroes" :id "moroes")]}]) $
                       (assoc-in $ [:players "p2" :minions 0 :states :stealth] false)
                       (valid-card-target? $ "p1" "Fireball" "moroes"))
                 true))}
  [state player-id card-name-or-entity target-id]
  {:pre [(map? state)
         (or (map? card-name-or-entity) (string? card-name-or-entity))
         (or (nil? target-id) (string? target-id)) (string? player-id)]}
  (let [card-name (if (map? card-name-or-entity)
                    (:name card-name-or-entity)
                    card-name-or-entity)
        targets (get-valid-card-targets state player-id card-name)]
    (cond ; No target is needed, no targets are given
          (and (nil? targets) (nil? target-id)) true
          (or (nil? targets) (nil? target-id)) false
          ; If target is within the list of valid targets
          (and (not (and (= (get-owner state target-id)
                            (get-opponent-player-id state player-id))
                         (stealth? state target-id)))
               (seq-contains? targets target-id)) true
          :else false)))

(defn sleepy?
  "Checks if the minion with given id is sleepy."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}]
                                :minion-ids-summoned-this-turn
                                ["i"])
                   (sleepy? "i")))
           (is (-> (create-game)
                   (add-minion-to-board {:player-id "p1",
                                         :minion (create-minion "Imp" :id "i1"),
                                         :position 0})
                   (sleepy? "i1")))
           (is-not (-> (create-game [{:minions
                                        [(create-minion "Imp" :id "i")]}])
                       (sleepy? "i"))))}
  [state id]
  (seq-contains? (:minion-ids-summoned-this-turn state) id))

(defn has-enough-mana?
  "Checks if the player has enough mana to play a card."
  {:test (fn []
           (is= (-> (create-game [{:mana 1}])
                    (has-enough-mana? "p1" 1))
                true)
           (is= (-> (create-game [{:mana 1}])
                    (has-enough-mana? "p1" 2))
                false))}
  [state player-id amount]
  {:pre [(string? player-id)]}
  (let [mana (get-mana state player-id)] (<= amount mana)))

(defn frozen?
  "Check frozen state of a hero or minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion (create-card "Imp")
                                                            :id "c1"
                                                            :frozen true)]}])
                    (frozen? "c1"))
                true)
           (is= (-> (create-game [{:minions [(create-minion (create-card "Imp")
                                                            :id "c1"
                                                            :frozen false)]}])
                    (frozen? "c1"))
                false))}
  [state entity-id]
  (:frozen (get-entity state entity-id)))

(defn get-owner
  "Returns the owner of a minion/hero/card(in-hand)"
  {:test
     (fn []
       (is= (-> (create-game [{:hero
                                 (create-hero "Jaina Proudmoore" :id "h1")}])
                (get-owner "h1"))
            "p1")
       (is= (-> (create-game [{:hand [(create-card "War Golem" :id "wg1")]}])
                (get-owner "wg1"))
            "p1")
       (is= (-> (create-game [{} {:minions [(create-minion "Imp" :id "m2")]}])
                (get-owner "m2"))
            "p2"))}
  ([entity]
   {:pre [(map? entity) (contains? entity :owner-id)]}
   (:owner-id entity))
  ([state id]
   {:pre [(map? state) (string? id)]}
   (if (contains? (get-entity state id) :owner-id)
     (:owner-id (get-entity state id))
     ; Check the heroes and card
     (loop [owners (keys (:players state))]
       (cond (empty? owners)
               (throw (Exception. (str "Entity id \"" id "\" does not exist.")))
             (= (get-in state [:players (first owners) :hero :id]) id) (first
                                                                         owners)
             (some #(= (:id %) id) (get-hand state (first owners))) (first
                                                                      owners)
             :else (recur (rest owners)))))))

(defn get-max-health
  "Returns the max health of the character."
  {:test (fn [] (is= (get-max-health (create-minion "War Golem" :health 5)) 7))}
  ([entity] {:pre [(map? entity)]} (:max-health (get-stats-with-buffs entity)))
  ([state id] (get-max-health (get-entity state id))))

(defn cant-attack-buff?
  [state minion-id]
  (let [minion (get-entity state minion-id)]
    (if (contains? (get minion :states) :can-attack?)
      (contains? (set (map #((get-function-from-effect-description %)
                              state
                              {:entity minion})
                        (get-in minion [:states :can-attack?])))
                 false)
      false)))


(defn valid-attack?
  "Checks if the attack is valid"
  {:test
     (fn []
       ; Should be able to attack an enemy minion
       (is (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}
                             {:minions [(create-minion "War Golem" :id "wg")]}])
               (valid-attack? "i" "wg")))
       ; Should be able to attack an enemy hero
       (is (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
               (valid-attack? "i" "h2")))
       ; Should not be able to attack your own minions
       (is-not
         (-> (create-game [{:minions [(create-minion "Imp" :id "i")
                                      (create-minion "War Golem" :id "wg")]}])
             (valid-attack? "i" "wg")))
       ; Should able to attack if it is not your turn (this is a special
       ; interaction and should be guarded agains with a high level api function
       ; that does not exist yet TODO)
       (is (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}
                             {:minions [(create-minion "War Golem" :id "wg")]}]
                            :player-id-in-turn
                            "p2")
               (valid-attack? "i" "wg")))
       ; Should not be able to attack if you are sleepy
       (is-not
         (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}
                           {:minions [(create-minion "War Golem" :id "wg")]}]
                          :minion-ids-summoned-this-turn
                          ["i"])
             (valid-attack? "i" "wg")))
       ; Should not be able to attack a minion if the target has stealth
       (is-not
         (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}
                           {:minions [(create-minion "Blood Imp" :id "bi1")]}])
             (assoc :minion-ids-summoned-this-turn [])
             (valid-attack? "i" "bi1")))
       ; Should not be able to attack if you already attacked this turn
       (is-not
         (->
           (create-game
             [{:minions
                 [(create-minion "Imp" :id "i" :attacks-performed-this-turn 1)]}
              {:minions [(create-minion "War Golem" :id "wg")]}])
           (valid-attack? "i" "wg"))))}
  [state attacker-id target-id]
  (let [attacker (get-minion state attacker-id)
        target (get-entity state target-id)]
    ; This is not needed as there are cards that can attack even though it is
    ; not their turn
    ;(= (:player-id-in-turn state) player-id)
    (and (< (:attacks-performed-this-turn attacker) 1)
         (not (sleepy? state attacker-id))
         (not (frozen? state attacker-id))
         (not (stealth? state target-id))
         ; Can't attack if it has not attack value
         (< 0 (get-attack state attacker-id))
         ; Check that it is allowed to attack by checking if the :cant-attack
         ; state is present
         (not (cant-attack-buff? state attacker-id))
         (not (some (fn [aura] (= aura :cant-attack)) (get attacker :auras [])))
         (not= (:owner-id attacker) (:owner-id target)))))

(defn get-buff
  {:test (fn []
           (is= 1
                (get-buff
                  {:states {:buffs [{:name "Test", :attack 1, :max-health 1}]},
                   :max-health 7,
                   :health 5}
                  :max-health)))}
  [character buff-type]
  {:pre [(map? character)], :post [(int? %)]}
  (->> (get-in character [:states :buffs] [])
       (reduce (fn [sum next-buff] (+ sum (get next-buff buff-type 0))) 0)))


(defn get-stats-with-buffs
  [character]
  {:pre [(map? character)]}
  (let [attack (max 0
                    (+ (get character :attack 0)
                       (get-buff character :attack-buff)))
        health (:health character)
        max-health (+ (:max-health character)
                      (get-buff character :max-health-buff))]
    {:attack attack, :health health, :max-health max-health}))

(defn get-health
  "Returns the health of the character."
  {:test (fn []
           ; The health of minions
           (is= (get-health (create-minion "War Golem")) 7)
           (is= (get-health (create-minion "War Golem" :health 5)) 5)
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
                    (get-health "i"))
                1)
           ; The health of heroes
           (is= (get-health (create-hero "Jaina Proudmoore")) 30)
           (is= (get-health (create-hero "Jaina Proudmoore" :health 28)) 28)
           (is= (-> (create-game
                      [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (get-health "h1"))
                30))}
  ([character]
   {:pre [(map? character) (contains? character :health)]}
   (:health (get-stats-with-buffs character)))
  ([state id] (get-health (get-entity state id))))



(defn get-attack
  "Returns the attack of the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
                    (get-attack "i"))
                1)
           (is= (-> (create-game
                      [{:minions [(create-minion "Imp" :id "i" :health 3)]}])
                    (get-health "i"))
                3))}
  [state id]
  (let [minion (get-minion state id)] (:attack (get-stats-with-buffs minion))))



(defn get-hero [state player-id] (get-in state [:players player-id :hero]))
"Return the id of a hero"

(defn get-opponent-player-id
  "Return the id of the opponent to the player-id supplied"
  {:test (fn []
           (is= (-> (create-game)
                    (get-opponent-player-id "p1"))
                "p2"))}
  [state player-id]
  (first (filter (fn [p-id] (not= p-id player-id)) (keys (:players state)))))


(comment ; should not be used, use get-health
         (defn get-hero-health
           "Get the current health of the player"
           {:test (fn []
                    (is= (-> (create-game)
                             (get-hero-health "p1"))
                         30))}
           [state player-id]
           (get-health state (get-hero-id state player-id))))

(defn is-minion?
  "Check whether an entity is a minion"
  {:test (fn []
           (is (-> (create-game
                     [{:hero (create-hero "Rexxar" :health 20),
                       :minions [(create-minion "Deranged Doctor" :id "m1")]}])
                   (is-minion? "m1")))
           (is-not (-> (create-game)
                       (is-minion? "i3")))
           (is (-> (create-game [{:minions [(create-minion "Imp" :id "i1")]}])
                   (is-minion? "i1"))))}
  [state entity-id]
  (= (:entity-type (get-entity state entity-id)) "minion"))

(defn is-secret?
  "Check if an entity is a secret"
  {:test (fn []
           (is-not
             (-> (create-game
                   [{:hero (create-hero "Rexxar" :health 20),
                     :minions [(create-minion "Deranged Doctor" :id "m1")]}])
                 (is-secret? "m1")))
           (is (-> (create-game [{:secrets
                                    [(create-secret "Snake Trap" :id "s1")]}])
                   (is-secret? "s1"))))}
  [state entity-id]
  (= (:entity-type (get-entity state entity-id)) "secret"))


(defn is-hero?
  "Check if an entity is an hero"
  [state entity-id]
  ; TODO fix when :entity-type should be "hero" not :hero
  (= (:entity-type (get-entity state entity-id)) :hero))


(defn get-hero-power-def
  "Get the full definition of the hero power where lookup-id is a player id, an entity id (belonging to a hero) or a string of the full name of a hero."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore")}])
                    (get-hero-power-def "p1"))
                (get-definition "Fireblast"))
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn")}])
                    (get-hero-power-def "h1"))
                (get-definition "Lesser Heal"))
           (is= (-> (get-hero-power-def "Rexxar"))
                (get-definition "Steady Shot")))}
  ([state lookup-id]
   (cond (is-player? state lookup-id)
           (get-definition (:hero-power (get-hero state lookup-id)))
         (is-hero? state lookup-id)
           (get-definition (:hero-power (get-entity state lookup-id)))
         ; TODO: should get-hero-power-def throw an exception for invalid
         ; lookup-id?
     ))
  ([hero-name] (get-definition (:hero-power (get-definition hero-name)))))


(defn get-max-mana
  "Get the current amount of max mana"
  [state player-id]
  (get-in state [:players player-id :max-mana]))

(defn get-mana
  "Get the current spendable amount of mana that a player has."
  {:test (fn []
           (is= (-> (create-game [{:mana 2}])
                    (get-mana "p1"))
                2))}
  [state player-id]
  (get-in state [:players player-id :mana]))


(defn get-seed "Get the random seed from state" [state] (:seed state))

(defn get-number-of-cards-in-deck
  {:test (fn []
           (is= (get-number-of-cards-in-deck (create-game) "p1") 0)
           (is= (get-number-of-cards-in-deck (create-game [{:deck [(create-card
                                                                     "Imp")]}])
                                             "p1")
                1))}
  ([player] (count (:deck player)))
  ([state player-id] (count (get-deck state player-id))))

(defn is-injured?
  [state entity-id]
  (let [entity (get-entity state entity-id)]
    (< (get-health entity) (get-max-health state entity-id))))

(defn has-battlecry?
  {:test (fn []
           (is (-> (create-game
                     [{:hand [(create-card "Big Game Hunter" :id "bgh1")]}])
                   (has-battlecry? "bgh1"))))}
  [state entity-or-id]
  (let [name (if (map? entity-or-id)
               (:name entity-or-id)
               (:name (get-entity state entity-or-id)))]
    (contains? (get-definition name) :battlecry)))

(defn is-alive? [entity] (> (get-health entity) 0))

(defn stealth?
  "Checks if a entity has stealth.
  Guaranteed works for:
    Minions"
  {:test (fn []
           (is (-> (create-game [{:minions
                                    [(create-minion "Blood Imp" :id "bi1")]}])
                   (stealth? "bi1")))
           (is-not (-> (create-game [{:minions
                                        [(create-minion "Imp" :id "i1")]}])
                       (stealth? "i1"))))}
  [state entity-id]
  (let [entity (get-entity state entity-id)]
    (get-in entity [:states :stealth] false)))

(defn has-used-hero-power?
  [state player-id]
  (< 0 (get-in state [:players player-id :hero :times-hero-power-used])))

(defn get-function-from-effect-description
  [effect-description]
  {:post [(not (nil? %))]}
  (get (get-definition (:effect-source effect-description))
       (:effect-key effect-description)))

(defn playable?
  "Checks if a card is playable. Checks that the player has enough mana. Checks that it is not a secret that the player already has equipped. Checks that it is not a minion that is trying to be played on a full board."
  ; TODO might be cards that have requirement before they are played.
  [state player-id card-id]
  (let [card (get-entity state card-id)
        card-def (get-definition (:name card))]
    (and (has-enough-mana? state player-id (:mana-cost card))
         (empty? (filter #(= (:name %) (:name card))
                   (get-secrets state player-id)))
         (if (= :minion (:type card-def))
           (> 7 (count (get-minions state player-id)))
           true))))
