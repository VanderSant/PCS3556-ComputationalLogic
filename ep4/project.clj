(defproject PCS3556/ep4 "0.1.0-SNAPSHOT"
  :main ep4.core
  :dependencies [
    [org.clojure/clojure "1.5.1"]
    [PCS3556/ep2 "0.1.0-SNAPSHOT"  ]
  ]

  :plugins [[lein-cljfmt "0.9.2"]]
  
  :test-paths ["test/"]
)