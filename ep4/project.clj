(defproject PCS3556/ep4 "0.1.0-SNAPSHOT"
  :main ep4.core
  :dependencies [
    [org.clojure/clojure "1.5.1"]
    [PCS3556/ep2 "0.1.0-SNAPSHOT"]
  ]

  :plugins [[lein-cljfmt "0.9.2"] 
            [lein-auto-install "0.1.1"]]
  
  :test-paths ["test/"]
  ;; :pedantic? false
  ;; :warn-on-reflection false
  ;; :lein-auto-install {:pedantic? false :warn-on-reflection false}
)