(defproject psswrd-solver "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clj-webdriver "0.7.2"]
                 [org.seleniumhq.selenium/selenium-server "2.49.0"]]
  :main ^:skip-aot psswrd-solver.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
