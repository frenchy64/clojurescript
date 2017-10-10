(defn find-file [d f]
  (.trim (:out (clojure.java.shell/sh
                "bash" "-c" (format "find %s -name %s" d f)))))

(defn yjp-file [f]
  (find-file "/Applications/YourKit*" f))

(defn libyjp-jar-path []
  (yjp-file "yjp-controller-api-redist.jar"))

(defn libyjp-agent-path []
  (yjp-file "libyjpagent.jnilib"))

(defproject org.clojure/clojurescript "0.0-SNAPSHOT"
  :description "ClojureScript compiler and core runtime library"
  :parent [org.clojure/pom.contrib "0.1.2"]
  :url "https://github.com/clojure/clojurescript"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true" "-Xmx512m" "-server"
											 ~(str "-agentpath:" (libyjp-agent-path))
											 "-Dclojure.server.repl={:port 53888 :accept clojure.core.server/repl}"]
  :source-paths ["src/main/clojure" "src/main/cljs"]
  :resource-paths [~(libyjp-jar-path)
									 "src/main/cljs"]
  :test-paths ["src/test/clojure" "src/test/cljs" "src/test/self" "src/test/cljs_cp"]
  :dependencies [[org.clojure/clojure "1.9.0-beta1"]
                 [org.clojure/core.typed "0.4.2-SNAPSHOT"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.reader "1.0.2"]
                 [org.clojure/test.check "0.9.0" :scope "test"]
                 [com.cognitect/transit-clj "0.8.300"]
                 [org.clojure/google-closure-library "0.0-20170519-fa0499ef"]
                 [com.google.javascript/closure-compiler-unshaded "v20170626"]
                 [org.mozilla/rhino "1.7R5"]]
  :test-selectors {:compiler :compiler}
  :profiles {:1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :uberjar {:aot :all :main clojure.main}}
  :aliases {"test-all" ["with-profile" "test,1.5:test,1.6" "test"]
            "check-all" ["with-profile" "1.5:1.6" "check"]}
  :min-lein-version "2.0.0")
