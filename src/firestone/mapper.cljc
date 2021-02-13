(ns firestone.mapper
  (:require [firestone.spec :as fspec]
            [firestone.info :refer
             [sleepy? get-number-of-cards-in-deck get-max-mana get-owner
              get-attack get-health get-max-health get-mana can-use-hero-power?
              stealth? get-hero-power-def has-used-hero-power? get-valid-targets
              frozen? get-secrets playable? get-minions get-valid-card-targets
              has-enough-mana? can-attack?]]
            [clojure.pprint :refer [pprint]]
            [ysera.test :refer [is is-not is= error?]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer
             [create-game create-card create-minion create-secret]]
            [clojure.spec.alpha :as s]))

(defn core-secret->client-secret
  [original-state secret]
  (let [card-def (get-definition (:name secret))]
    {:name (:name secret),
     :owner-id (get-owner secret),
     :class (name (:class card-def)),
     :id (:id secret),
     :entity-type "secret",
     :rarity (name (get card-def :rarity "none")),
     :original-mana-cost (:mana-cost card-def),
     :description (:description card-def)}))

(defn core-hero-power->client-hero-power
  [original-state owner hero-power]
  (let [target-function (:valid-targets (get-hero-power-def original-state
                                                            owner))]
    {:can-use (can-use-hero-power? original-state owner),
     :owner-id owner,
     :entity-type "hero-power",
     :has-used-your-turn (has-used-hero-power? original-state owner),
     :name (:name (get-hero-power-def original-state owner)),
     :description (:description (get-hero-power-def original-state owner)),
     ; Optional
     ; TODO might need to be altered in case cards that change hero power mana
     ; cost is implemented
     :mana-cost 2,
     :original-mana-cost 2,
     :valid-target-ids
       (if (nil? target-function) [] (target-function original-state owner))}))


(defn core-hero->client-hero
  {:test (fn []
           (let [state (create-game [{:deck [(create-card "Imp")]}])
                 h (core-hero->client-hero state
                                           (get-in state
                                                   [:players "p1" :hero]))]
             ;(s/explain ::fspec/hero h)
             (is (s/valid? ::fspec/hero h))))}
  [original-state hero]
  {:pre [(map? original-state) (map? hero)]}
  (let [owner (get-owner original-state (:id hero))]
    (as-> {; TODO, change this when armor is implemented
           :armor 0,
           :owner-id owner,
           :entity-type "hero",
           ; TODO implement this if weapons are implemented
           :attack 0,
           ; TODO change this if weapons are implemented
           :can-attack (can-attack? original-state (:id hero)),
           :health (get-health original-state (:id hero)),
           :max-health (get-max-health original-state (:id hero)),
           :mana (get-mana original-state owner),
           :max-mana (get-max-mana original-state owner),
           :id (:id hero),
           :name (:name hero),
           ; TODO make sure these are the actual states (i believe frozen is
           :states [],
           ; TODO Add these when weapons are implemented
           :valid-attack-ids [],
           ; Optionals
           :hero-power (core-hero-power->client-hero-power original-state
                                                           owner
                                                           (:hero-power hero)),
           :class (name (:class (get-definition (:name hero)))),
           ; TODO implement this someday maybe
           ; :weapon
           } $
      (if (frozen? original-state (:id hero))
        (update $ :states (fn [st] (conj st "FROZEN")))
        $))))

(defn core-minion->client-board-entity
  [original-state minion]
  {:pre [(map? minion)]}
  (let [card-definition (get-definition minion)]
    (as-> {:name (:name minion),
           :set (name (:set card-definition)),
           ; Move our states to his states
           :states [],
           :sleepy (sleepy? original-state (:id minion)),
           :mana-cost (:mana-cost card-definition),
           :can-attack (can-attack? original-state (:id minion)),
           :attack (get-attack original-state (:id minion)),
           :original-attack (:attack card-definition),
           :health (get-health original-state (:id minion)),
           :max-health (get-max-health original-state (:id minion)),
           :original-health (:health card-definition),
           :description (get card-definition :description ""),
           :id (:id minion),
           :position (:position minion),
           :owner-id (:owner-id minion),
           :entity-type (:entity-type minion),
           :rarity (name (get card-definition :rarity "none")),
           :valid-attack-ids (get-valid-targets original-state (:id minion))} $
      ; Optional arguments
      (if (stealth? original-state (:id minion))
        (update $ :states (fn [st] (conj st "STEALTH")))
        $)
      (if (frozen? original-state (:id minion))
        (update $ :states (fn [st] (conj st "FROZEN")))
        $)
      (if (contains? (:states minion) :deathrattle)
        (update $ :states (fn [st] (conj st "DEATHRATTLE")))
        $)
      (if (contains? (:states minion) :effect)
        (update $ :states (fn [st] (conj st "EFFECT")))
        $)
      (if (contains? card-definition :class)
        (assoc $ :class (name (:class card-definition)))
        $))))

(defn core-card->client-card
  [original-state card]
  (let [card-def (get-definition (:name card))
        owner (get-owner original-state (:id card))]
    (as-> {:entity-type "card",
           :name (:name card),
           :mana-cost (:mana-cost card),
           :original-mana-cost (:mana-cost card-def),
           :type (name (:type card-def)),
           ; Optional
           :owner-id owner,
           :id (:id card),
           :playable (playable? original-state owner (:id card)),
           :rarity (name (get card-def :rarity "none")),
           :description (get card-def :description "")} $
      ; Add attack and health if it is a minion
      (if (= (:type $) "minion")
        (-> (assoc $ :attack (:attack card))
            (assoc :original-attack (:attack card-def))
            (assoc :health (:health card))
            (assoc :original-health (:health card-def)))
        $)
      (let [targets (get-valid-card-targets original-state owner (:name card))]
        (if (nil? targets) $ (assoc $ :valid-target-ids targets)))
      (if (contains? card-def :class)
        (assoc $ :class (name (:class card-def)))
        $))))


(defn core-player->client-player
  {:test (fn []
           (let [state (create-game
                         [{:deck [(create-card "Imp")],
                           :minions [(create-minion "Imp")
                                     (create-minion "Sylvanas Windrunner")
                                     (create-minion "Shrinkmeister")
                                     (create-minion "Big Game Hunter")]}])
                 p (core-player->client-player state
                                               (get-in state [:players "p1"]))]
             ;(s/explain ::fspec/player p)
             ;(pprint p)
             (is (s/valid? ::fspec/player p))))}
  [original-state player]
  {:pre [(map? original-state) (map? player)]}
  {:deck-size (get-number-of-cards-in-deck player),
   :hero (core-hero->client-hero original-state (:hero player)),
   :board-entities (map #(core-minion->client-board-entity original-state %)
                     (sort-by :position (:minions player))),
   :active-secrets (map #(core-secret->client-secret original-state %)
                     (:secrets player)),
   :id (:id player),
   :hand (map #(core-card->client-card original-state %) (:hand player)),
   ; Optionals
   ; :active-quests
   })


(defn core-players->client-players
  {:test (fn []
           (let [state (create-game)
                 p (core-players->client-players state (:players state))]
             ;(pprint p)
             ;(s/explain ::fspec/players p)
             (is (s/valid? ::fspec/players p))))}
  [original-state players]
  {:pre [(map? original-state) (map? players)]}
  (->> (map #(core-player->client-player original-state %) (vals players))
       (into [])))

(defn core-game->client-game
  {:test (fn []
           (is (let [state
                       (create-game [{:hand [(create-card "Lorewalker Cho")
                                             (create-card "Fireball")
                                             (create-card "The Coin")
                                             (create-card "War Golem")],
                                      :secrets [(create-secret "Snake Trap")]}])
                     state (core-game->client-game state)]
                 ;(s/explain ::fspec/game-states state)
                 (s/valid? ::fspec/game-states state)))
           (let [state (create-game)
                 s (core-game->client-game state)]
             ;(s/explain ::fspec/game-states s)
             (is (s/valid? ::fspec/game-states s))))}
  [original-state]
  {:pre [(map? original-state)], :post [(not (nil? %))]}
  [{:player-in-turn (:player-id-in-turn original-state),
    :id "1",
    :players (core-players->client-players original-state
                                           (:players original-state))}])
