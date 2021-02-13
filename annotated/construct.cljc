(ns firestone.construct
  (:require [clojure.test :refer [function?]]
            [ysera.test :refer [is is-not is= error?]]
            [firestone.definitions :refer [get-definition]]))

; notes:
; any code snippets in annotations are to be read in clojure syntax unless
; they contain a leading asterisk, in which case the snippet is to be read
; in python syntax

(defn create-hero
  "Creates a hero from its definition by the given hero name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-hero "Jaina Proudmoore")
                {:name         "Jaina Proudmoore"
                 :entity-type  :hero
                 :damage-taken 0})
           (is= (create-hero "Jaina Proudmoore" :damage-taken 10)
                {:name         "Jaina Proudmoore"
                 :entity-type  :hero
                 :damage-taken 10}))}
  [name & kvs]
  (let [hero {:name         name
              :entity-type  :hero
              :damage-taken 0}]
    (if (empty? kvs)
      hero
      (apply assoc hero kvs)))) ; (annotation-1)
                                ; (apply f x args) Applies fn f to the argument list formed by
                                ; prepending intervening arguments to args.
                                ; 
                                ; (assoc mapobj key val) => mapobj.put(key, val) (returns new map)
                                ; 
                                ; kvs being some list of stuff, e.g '(:a 1 :b 2 ... :z 30)
                                ; (apply assoc hero kvs) is equivalent to
                                ; (assoc hero :a 1 :b 2 ... :z 30)
                                ; 
                                ; the apply is necessary because there is no assoc/2 e.g (assoc <struct> <list>)
                                ; 
                                ; maybe it is idiomatic not to include such a variant and
                                ; instead leave it to apply

(defn create-card
  "Creates a card from its definition by the given card name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-card "Imp" :id "i")
                {:id          "i"
                 :entity-type :card
                 :name        "Imp"}))}
  [name & kvs]
  (let [card {:name        name
              :entity-type :card}]
    (if (empty? kvs)
      card
      (apply assoc card kvs)))) ; see (annotation-1)

(defn create-minion
  "Creates a minion from its definition by the given minion name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-minion "Imp" :id "i" :attacks-performed-this-turn 1)
                {:attacks-performed-this-turn 1
                 :damage-taken                0
                 :entity-type                 :minion
                 :name                        "Imp"
                 :id                          "i"}))}
  [name & kvs]
  (let [definition (get-definition name)                    ; Will be used later
        minion {:damage-taken                0
                :entity-type                 :minion
                :name                        name
                :attacks-performed-this-turn 0}]
    (if (empty? kvs)
      minion
      (apply assoc minion kvs)))) ; see (annotation-1)


(defn create-empty-state
  "Creates an empty state with the given heroes."
  {:test (fn []
           (is= (create-empty-state [(create-hero "Jaina Proudmoore")
                                     (create-hero "Jaina Proudmoore")])
                (create-empty-state))

           (is= (create-empty-state [(create-hero "Jaina Proudmoore")
                                     (create-hero "Jaina Proudmoore")])
                {:player-id-in-turn             "p1"
                 :players                       {"p1" {:id      "p1"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :hero    {:name         "Jaina Proudmoore"
                                                                 :id           "h1"
                                                                 :damage-taken 0
                                                                 :entity-type  :hero}}
                                                 "p2" {:id      "p2"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :hero    {:name         "Jaina Proudmoore"
                                                                 :id           "h2"
                                                                 :damage-taken 0
                                                                 :entity-type  :hero}}}
                 :counter                       1
                 :minion-ids-summoned-this-turn []}))}
  ([heroes]
    ; Creates Jaina Proudmoore heroes if heroes are missing.
    ;
    ; (annotation-2)
    ; actually always creates two instances of Jaina Proudmoore and re-binds "heroes"
    ; (previously bound to the input argument) to the list comprised of the first two elements of
    ; the union of itself and a list containing the two Jaina instances
   (let [heroes (->> (concat heroes [(create-hero "Jaina Proudmoore")
                                     (create-hero "Jaina Proudmoore")])
                     (take 2))]
     {:player-id-in-turn             "p1"
      :players                       (->> heroes
                                          ; (annotation-3)
                                          ; map-indexed takes a function f taking two arguments and a collection,
                                          ; and returns a lazy
                                          ; list where each element is computed by applying the function
                                          ; such as *f(n, nth element of the input collection)
                                          ; e.g (map-indexed heroes f) => *[f(0, heroes[0]), f(1, heroes[1]), ...]
                                          (map-indexed (fn [index hero]
                                                         {:id      (str "p" (inc index))
                                                          :deck    []
                                                          :hand    []
                                                          :minions []
                                                          :hero    (assoc hero :id (str "h" (inc index)))}))
                                          ; (annotation-4)
                                          ; the reduce here does the equivalent of
                                          ;   *result = dict()
                                          ;   *for (element in inputlist): result[element.id] = element
                                          ;   *return result
                                          ;
                                          ; where inputlist is the result of the map-indexed call above
                                          (reduce (fn [a v]
                                                    (assoc a (:id v) v))
                                                  {}))
      :counter                       1
      :minion-ids-summoned-this-turn []}))
  ([]
   (create-empty-state []))) ; (annotation-5)
                             ; an overload that takes no arguments and calls the other variant
                             ; with an empty list as the argument

(defn get-player
  "Returns the player with the given id."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player "p1")
                    (:id))
                "p1"))}
  [state player-id]
  ; (annotation-6)
  ; (get-in assocobj [k1 k2 ... kn]) is roughly equivalent to (get (get (get assocobj k1) k2) k3)
  ; meaning its essentially *assocobj[k1][k2][k3]
  (get-in state [:players player-id]))

(defn get-minions
  "Returns the minions on the board for the given player-id or for both players."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-minions "p1"))
                [])
           (is= (-> (create-empty-state)
                    (get-minions))
                []))}
  ([state player-id]
   (:minions (get-player state player-id)))
  ([state]
   (->> (:players state)
        (vals)
        (map :minions)
        (apply concat))))

(defn- generate-id
  "Generates an id and returns a tuple with the new state and the generated id."
  {:test (fn []
           (is= (generate-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  [(update state :counter inc) (:counter state)])

(defn add-minion-to-board
  "Adds a minion with a given position to a player's minions and updates the other minions' positions."
  {:test (fn []
           ; Adding a minion to an empty board
           (is= (as-> (create-empty-state) $
                      (add-minion-to-board $ {:player-id "p1" :minion (create-minion "Imp" :id "i") :position 0})
                      (get-minions $ "p1")
                      (map (fn [m] {:id (:id m) :name (:name m)}) $))
                [{:id "i" :name "Imp"}])
           ; Adding a minion and update positions
           (let [state (-> (create-empty-state)
                           (add-minion-to-board {:player-id "p1" :minion (create-minion "Imp" :id "i1") :position 0})
                           (add-minion-to-board {:player-id "p1" :minion (create-minion "Imp" :id "i2") :position 0})
                           (add-minion-to-board {:player-id "p1" :minion (create-minion "Imp" :id "i3") :position 1})
                           (get-minions "p1"))]
             (is= (map :id state) ["i1" "i2" "i3"])
             (is= (map :position state) [2 0 1]))
           ; Generating an id for the new minion
           (let [state (-> (create-empty-state)
                           (add-minion-to-board {:player-id "p1" :minion (create-minion "Imp") :position 0}))]
             (is= (-> (get-minions state "p1")
                      (first)
                      (:id))
                  "m1")
             (is= (:counter state) 2)))}
  [state {player-id :player-id minion :minion position :position}]
  {:pre [(map? state) (string? player-id) (map? minion) (number? position)]}
  (let [[state id] (if (contains? minion :id)
                     [state (:id minion)]
                     (let [[state value] (generate-id state)]
                       [state (str "m" value)]))]
    (update-in state
               [:players player-id :minions]
               (fn [minions]
                 (conj (->> minions
                            (mapv (fn [m]
                                    (if (< (:position m) position)
                                      m
                                      (update m :position inc)))))
                       (assoc minion :position position
                                     :owner-id player-id
                                     :id id))))))

(defn create-game
  "Creates a game with the given deck, hand, minions (placed on the board), and heroes."
  {:test (fn []
           (is= (create-game) (create-empty-state))
           (is= (create-game [{:hero (create-hero "Anduin Wrynn")}])
                (create-game [{:hero "Anduin Wrynn"}]))
           (is= (create-game [{:minions [(create-minion "Imp")]}
                              {:hero (create-hero "Anduin Wrynn")}]
                             :player-id-in-turn "p2")
                {:player-id-in-turn             "p2"
                 :players                       {"p1" {:id      "p1"
                                                       :deck    []
                                                       :hand    []
                                                       :minions [{:damage-taken                0
                                                                  :attacks-performed-this-turn 0
                                                                  :entity-type                 :minion
                                                                  :name                        "Imp"
                                                                  :id                          "m1"
                                                                  :position                    0
                                                                  :owner-id                    "p1"}]
                                                       :hero    {:name         "Jaina Proudmoore"
                                                                 :id           "h1"
                                                                 :entity-type  :hero
                                                                 :damage-taken 0}}
                                                 "p2" {:id      "p2"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :hero    {:name         "Anduin Wrynn"
                                                                 :id           "h2"
                                                                 :entity-type  :hero
                                                                 :damage-taken 0}}}
                 :counter                       2
                 :minion-ids-summoned-this-turn []}))}
  ; 
  ([data & kvs]
   (let [state (as-> (create-empty-state (map (fn [player-data]
                                                (cond (nil? (:hero player-data))
                                                      (create-hero "Jaina Proudmoore")

                                                      (string? (:hero player-data))
                                                      (create-hero (:hero player-data))

                                                      :else
                                                      (:hero player-data)))
                                              data)) $
                     ; Add minions to the state
                     (reduce (fn [state {player-id :player-id minions :minions}]
                               (reduce (fn [state [index minion]]
                                         (add-minion-to-board state {:player-id player-id
                                                                     :minion    minion
                                                                     :position  index}))
                                       state
                                       (map-indexed (fn [index minion] [index minion])
                                                    minions)))
                             $
                             (map-indexed (fn [index player-data]
                                            {:player-id (str "p" (inc index))
                                             :minions   (:minions player-data)})
                                          data)))]
     (if (empty? kvs)
       state
       (apply assoc state kvs))))
  ([]
   (create-game [])))

(defn get-minion
  "Returns the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
                    (get-minion "i")
                    (:name))
                "Imp"))}
  [state id]
  (->> (get-minions state)
       (filter (fn [m] (= (:id m) id)))
       (first)))

(defn get-heroes
  {:test (fn []
           (is= (->> (create-game)
                     (get-heroes)
                     (map :name))
                ["Jaina Proudmoore" "Jaina Proudmoore"]))}
  [state]
  (->> (:players state)
       (vals)
       (map :hero)))

(defn replace-minion
  "Replaces a minion with the same id as the given new-minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "minion")]}])
                    (replace-minion (create-minion "War Golem" :id "minion"))
                    (get-minion "minion")
                    (:name))
                "War Golem"))}
  [state new-minion]
  (let [owner-id (or (:owner-id new-minion)
                     (:owner-id (get-minion state (:id new-minion))))]
    (update-in state
               [:players owner-id :minions]
               (fn [minions]
                 (map (fn [m]
                        (if (= (:id m) (:id new-minion))
                          new-minion
                          m))
                      minions)))))

(defn update-minion
  "Updates the value of the given key for the minion with the given id. If function-or-value is a value it will be the
   new value, else if it is a function it will be applied on the existing value to produce the new value."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
                    (update-minion "i" :damage-taken inc)
                    (get-minion "i")
                    (:damage-taken))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
                    (update-minion "i" :damage-taken 2)
                    (get-minion "i")
                    (:damage-taken))
                2))}
  [state id key function-or-value]
  (let [minion (get-minion state id)]
    (replace-minion state (if (function? function-or-value)
                            (update minion key function-or-value)
                            (assoc minion key function-or-value)))))

(defn remove-minion
  "Removes a minion with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Imp" :id "i")]}])
                    (remove-minion "i")
                    (get-minions))
                []))}
  [state id]
  (let [owner-id (:owner-id (get-minion state id))]
    (update-in state
               [:players owner-id :minions]
               (fn [minions]
                 (remove (fn [m] (= (:id m) id)) minions)))))

(defn remove-minions
  "Removes the minions with the given ids from the state."
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Imp" :id "i1")
                                               (create-minion "Imp" :id "i2")]}
                                    {:minions [(create-minion "Imp" :id "i3")
                                               (create-minion "Imp" :id "i4")]}]) $
                      (remove-minions $ "i1" "i4")
                      (get-minions $)
                      (map :id $))
                ["i2" "i3"]))}
  [state & ids]
  (reduce remove-minion state ids))