(ns ^{:doc "Manipulate mana."} firestone.mana
  (:require [ysera.test :refer [is is-not is= error?]]
            [clojure.pprint :refer [pprint]]
            [firestone.construct :refer [create-game]]
            [firestone.info :refer [get-mana get-max-mana]]))

(defn spend-mana
  "Spend mana"
  {:test (fn []
           (is= (-> (create-game [{:max-mana 1, :mana 1}])
                    (spend-mana "p1" 1)
                    (get-mana "p1"))
                0)
           (error? (-> (create-game)
                       (spend-mana "p1" 100))))}
  [state player-id amount]
  (if (< (get-mana state player-id) amount)
    (throw
      (Exception. (str "Can't spend more mana than you have. You have only "
                         (get-mana state player-id)
                       " mana and tried to spend " amount)))
    (update-in state
               [:players player-id :mana]
               (fn [current-mana] (- current-mana amount)))))

(defn refill-mana
  "Set the amount of mana equal to the max mana."
  {:test (fn []
           (is= (-> (create-game [{:max-mana 1, :mana 1}])
                    (spend-mana "p1" 1)
                    (refill-mana "p1")
                    (get-mana "p1"))
                1))}
  [state player-id]
  (assoc-in state [:players player-id :mana] (get-max-mana state player-id)))

(defn increase-mana
  "Increase the temporary mana of the player. Not that this does not increase mana over 10."
  {:test (fn []
           ; Mana should not be increased if player already has mana
           (is= (-> (create-game [{:mana 10}])
                    (increase-mana "p1")
                    (get-mana "p1"))
                10)
           (is= (-> (create-game [{:mana 1}])
                    (increase-mana "p1" 2)
                    (get-mana "p1"))
                3))}
  ([state player-id amount]
   (update-in state
              [:players player-id :mana]
              (fn [current-max-mana] (min 10 (+ amount current-max-mana)))))
  ([state player-id] (increase-mana state player-id 1)))

(defn increase-max-mana
  "Increase the max mana of the player. Increases by one by default. Can be called with extra argument amount.
  Note that these are empty mana crystals. If more spendable mana is needed, use increase-mana. Max mana can't exceed 10."
  {:test (fn []
           ; If the player already has 10 as max mana, it should not be
           ; increased.
           (is= (-> (create-game [{:max-mana 10}])
                    (increase-max-mana "p1" 3)
                    (get-max-mana "p1"))
                10)
           (is= (-> (create-game [{:max-mana 1}])
                    (increase-max-mana "p1" 3)
                    (get-max-mana "p1"))
                4))}
  ([state player-id amount]
   (update-in state
              [:players player-id :max-mana]
              (fn [current-max-mana] (min 10 (+ amount current-max-mana)))))
  ([state player-id] (increase-max-mana state player-id 1)))
