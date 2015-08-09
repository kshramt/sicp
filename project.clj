(defproject sicp/sicp "0.1.0-SNAPSHOT"
  :license {:name "GNU General Public License Version 3",
            :url "http://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.trace "0.7.8"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/core.typed "0.3.10"]]
  :min-lein-version "2.0.0"
  :description "SICP in Clojure"
  :global-vars {*warn-on-reflection* true}
  :core.typed {:check [sicp.core]}
  :profiles {:uberjar {:aot :all}}
  :main sicp.core)
