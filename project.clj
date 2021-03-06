(defproject tnoc "0.1.2"
  :description "Clojure code inspired by The Nature of Computation by Cristopher Moore & Stephan Mertens"
  :url "https://github.com/SOberhoff/tnoc"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "0.3.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/test.check "0.10.0"]
                 [com.rpl/specter "1.1.2"]]
  :jvm-opts ["-Xss64m"])

