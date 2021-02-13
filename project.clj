(defproject firestone "firestone"
  :description "dd2487 2018 firestone lab"
  :license {}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [ysera "1.2.0"]
                 [http-kit "2.2.0"]
                 [org.clojure/data.json "0.2.6"]]
  :main ^:skip-aot firestone.api
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-exec "0.3.7"]]
  :aliases {"doc" ["exec" "-p" "autodoc.clj"]
            "server" ["exec" "-ep" "(require 'firestone.definitions-loader 'firestone.server 'firestone.spec 'firestone.instrumentation 'firestone.server) (firestone.server/start!)"]}
  :compile-path ".")
