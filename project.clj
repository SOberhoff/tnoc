(defproject tnoc "0.1.0"
  :description "Clojure code inspired by The Nature of Computation by Cristopher Moore & Stephan Mertens"
  :url "https://github.com/SOberhoff/tnoc"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha12"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/test.check "0.9.0"]
                 [com.rpl/specter "1.0.1"]]
  :jvm-opts ["-Xss64m"])

