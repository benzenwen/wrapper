(defproject wrapper "0.1.0"
  :description "A demo wrapper for HACKER, the stacking block solver./nHACKER was originally written by Gerald Sussman for his PhD.  It was re-written in Clojure by Dylan Holmes.\nWRAPPER was written by Ben Wen to run the code with Leiningen tooling."
  :url ["https://github.com/benzenwen/wrapper"
        "http://logical.ai/auai/doc/doc-hacker.html"
        "http://logical.ai/auai/dl/hacker.tar.bz2"
        "http://logical.ai/auai/#code"
        "http://dspace.mit.edu/handle/1721.1/6894#files-area"]
  :license {:name "Gnu Public License 2.0+"
            :url "http://gnu.org/licenses/old-licenses/gpl-2.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot wrapper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
