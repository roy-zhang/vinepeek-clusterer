(defproject vinepeek-clusterer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"project" "file:repo"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.6.5"]
                 [org.clojure/data.json "0.2.1"]
                 [prismatic/plumbing "0.1.0"]
                 [clj-time "0.5.0"]
                 [local/mp4Decoder "0.4"]
                 [local/musicg "1.4.2.0"]
                 ]
  :main vinepeek-clusterer.core)
