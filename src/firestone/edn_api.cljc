(ns firestone.edn-api
  (:require [firestone.construct :as construct :refer
             [create-game create-card create-hero create-minion]]
            [clojure.pprint :refer [pprint]]
            [firestone.api :refer
             [end-turn attack-entity play-spell-card play-minion-card
              play-hero-power]]
            [firestone.mapper :refer [core-game->client-game]]))

(defonce state-atom (atom nil))

(defn get-player-id-in-turn
  "This function is NOT pure!"
  [game-id]
  (-> (deref state-atom)
      (construct/get-player-id-in-turn)))

(def default-game
  (create-game
    [{:hand [(create-card "Lorewalker Cho") (create-card "Fireball")
             (create-card "Moroes") ],
      :deck [(create-card "Sylvanas Windrunner") (create-card "Deranged Doctor")
             (create-card "Big Game Hunter") (create-card "Acolyte of Pain")
             (create-card "Ancient Watcher") (create-card "Mind Control")
             (create-card "Sneed's Old Shredder") (create-card "Alarm-o-Bot")
             (create-card "King Mukla") (create-card "Doomsayer")
             (create-card "Arcane Golem") (create-card "Eater of Secrets")
             (create-card "Ogre Magi") (create-card "Silver Hand Recruit")
             (create-card "Defender") (create-card "Dalaran Mage")
             (create-card "Loot Hoarder") (create-card "Shrinkmeister")
             (create-card "Archmage Antonidas") (create-card "Cabal Shadow Priest")
             (create-card "Trade Prince Gallywix") (create-card "Malygos")
             (create-card "Flare") (create-card "Frothing Berserker")
             (create-card "Steward") (create-card "Unpowered Mauler")
             (create-card "Competitive Spirit") (create-card "Snake")
             (create-card "Blood Imp") (create-card "Imp") ]}
     {:hero (create-hero "Rexxar"),
      :hand [(create-card "Fireball") (create-card "Snake Trap")
             (create-card "The Coin") (create-card "War Golem")],
      :deck [(create-card "Sylvanas Windrunner") (create-card "Deranged Doctor")
             (create-card "Big Game Hunter") (create-card "Acolyte of Pain")
             (create-card "Ancient Watcher") (create-card "Mind Control")
             (create-card "Sneed's Old Shredder") (create-card "Alarm-o-Bot")
             (create-card "King Mukla") (create-card "Doomsayer")
             (create-card "Arcane Golem") (create-card "Eater of Secrets")
             (create-card "Ogre Magi") (create-card "Silver Hand Recruit")
             (create-card "Defender") (create-card "Dalaran Mage")
             (create-card "Loot Hoarder") (create-card "Shrinkmeister")
             (create-card "Archmage Antonidas") (create-card "Cabal Shadow Priest")
             (create-card "Trade Prince Gallywix") (create-card "Malygos")
             (create-card "Flare") (create-card "Frothing Berserker")
             (create-card "Steward") (create-card "Unpowered Mauler")
             (create-card "Archmage Antonidas") (create-card "Abusive Sergeant")
             (create-card "Rampage") (create-card "Imp") ]}]))

(defn create-game!
  [game-id]
  (core-game->client-game (reset! state-atom default-game)))


(defn attack!
  [game-id player-id attacker-id target-id]
  (core-game->client-game
    (swap! state-atom #(attack-entity % attacker-id target-id))))

(defn end-turn!
  [game-id player-id]
  (core-game->client-game (swap! state-atom #(end-turn %))))

(defn play-minion-card!
  [game-id player-id card-id position target-id]
  (core-game->client-game
    (swap! state-atom
      #(play-minion-card % player-id card-id position target-id))))

(defn play-spell-card!
  [game-id player-id card-id target-id]
  (core-game->client-game (swap! state-atom
                            #(play-spell-card % player-id card-id target-id))))

(defn use-hero-power!
  [game-id player-id target-id]
  (core-game->client-game
    (swap! state-atom #(play-hero-power % player-id target-id))))
