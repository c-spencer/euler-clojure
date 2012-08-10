(defproject euler "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [org.clojure/math.combinatorics "0.0.3"]]
  :profiles {
    :dev {
      :plugins [[lein-midje "2.0.0-SNAPSHOT"]]
      :dependencies [[midje "1.4.0"]]}})
