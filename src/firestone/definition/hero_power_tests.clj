(ns firestone.definition.hero-power-tests
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [firestone.api :refer [play-hero-power]]
            [firestone.info :refer [get-health get-entity get-minions]]
            [firestone.construct :refer [create-hero create-game]]))

(clojure.test/deftest fireblast
  (as-> (create-game [{:hero (create-hero "Jaina Proudmoore")}]) $
    (play-hero-power $ "p1" "h2")
    (do (is= (get-health $ "h2")
             (- (:health (get-definition (:name (get-entity $ "h2")))) 1)))))

(clojure.test/deftest lesserheal
  (as-> (create-game [{:hero (create-hero "Anduin Wrynn")}
                      {:hero (create-hero "Anduin Wrynn" :health 1)}]) $
    (play-hero-power $ "p1" "h2")
    (do (is= (get-health $ "h2") 3))))

(clojure.test/deftest reinforce
  (as-> (create-game [{:hero (create-hero "Uther Lightbringer"), :minions []}])
    $
    (do (is= (count (filter #(= (:name %) "Silver Hand Recruit")
                      (get-minions $ "p1")))
             0)
        $)
    (play-hero-power $ "p1")
    (do (is= (count (filter #(= (:name %) "Silver Hand Recruit")
                      (get-minions $ "p1")))
             1))))

(clojure.test/deftest steadyshot
  (as-> (create-game [{:hero (create-hero "Rexxar")}]) $
    (play-hero-power $ "p1" "h2")
    (do (is= (get-health $ "h2")
             (- (:health (get-definition (:name (get-entity $ "h2")))) 2)))))
