;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer-tests
  (:require [clojure.java.io :as io]
            [cljs.analyzer :as a]
            [cljs.env :as e]
            [cljs.env :as env]
            [cljs.analyzer.api :as ana-api]
            [cljs.util :as util]
            [cljs.externs :as externs])
  (:use clojure.test))

(defn collecting-warning-handler [state]
  (fn [warning-type env extra]
    (when (warning-type a/*cljs-warnings*)
      (when-let [s (a/error-message warning-type extra)]
        (swap! state conj s)))))

;;******************************************************************************
;;  cljs-warnings tests
;;******************************************************************************

(def warning-forms
  {:undeclared-var (let [v (gensym)] `(~v 1 2 3))
   :fn-arity '(do (defn x [a b] (+ a b))
                  (x 1 2 3 4))
   :keyword-arity '(do (:argumentless-keyword-invocation))})

(defn warn-count [form]
  (let [counter (atom 0)
        tracker (fn [warning-type env & [extra]]
                  (when (warning-type a/*cljs-warnings*)
                    (swap! counter inc)))]
    (a/with-warning-handlers [tracker]
      (a/analyze (a/empty-env) form))
    @counter))

(deftest no-warn
  (is (every? zero? (map (fn [[name form]] (a/no-warn (warn-count form))) warning-forms))))

(deftest all-warn
  (is (every? #(= 1 %) (map (fn [[name form]] (a/all-warn (warn-count form))) warning-forms))))

;; =============================================================================
;; NS parsing

(def ns-env (assoc-in (a/empty-env) [:ns :name] 'cljs.user))

(deftest spec-validation
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require {:foo :bar})))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns & options] and lib.ns specs supported in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [:foo :bar])))
          (catch Exception e
            (.getMessage e)))
        "Library name must be specified as a symbol in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :plop])))
          (catch Exception e
            (.getMessage e)))
        "Only :as alias, :refer (names) and :rename {from to} options supported in :require"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :plop true])))
          (catch Exception e
            (.getMessage e)))
        "Only :as, :refer and :rename options supported in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :as boz :refer []])))
          (catch Exception e
            (.getMessage e)))
        "Each of :as and :refer options may only be specified once in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:refer-clojure :refer [])))
          (catch Exception e
            (.getMessage e)))
        "Only [:refer-clojure :exclude (names)] and optionally `:rename {from to}` specs supported"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:refer-clojure :rename [1 2])))
          (catch Exception e
            (.getMessage e)))
        "Only [:refer-clojure :exclude (names)] and optionally `:rename {from to}` specs supported"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :exclude []])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :only])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :only [1 2 3]])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :rename [1 2]])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [foo.bar :rename {baz qux}])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :only [foo] :only [bar]])))
          (catch Exception e
            (.getMessage e)))
        "Each of :only and :rename options may only be specified once in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as []])))
          (catch Exception e
            (.getMessage e)))
        ":as must be followed by a symbol in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as woz] [noz.goz :as woz])))
          (catch Exception e
            (.getMessage e)))
        ":as alias must be unique"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [foo.bar :rename {baz qux}])))
          (catch Exception e
            (.getMessage e)))
        "Renamed symbol baz not referred"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:unless [])))
          (catch Exception e
            (.getMessage e)))
        "Only :refer-clojure, :require, :require-macros, :use, :use-macros, and :import libspecs supported. Got (:unless []) instead."))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require baz.woz) (:require noz.goz)))
          (catch Exception e
            (.getMessage e)))
        "Only one ")))

;; =============================================================================
;; Inference tests

(def test-cenv (atom {}))
(def test-env (assoc-in (a/empty-env) [:ns :name] 'cljs.core))

(a/no-warn
  (e/with-compiler-env test-cenv
    (binding [a/*analyze-deps* false]
      (a/analyze-file (io/file "src/main/cljs/cljs/core.cljs")))))

(deftest basic-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '1)))
         'number))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '"foo")))
         'string))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(make-array 10))))
         'array))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(js-obj))))
         'object))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '[])))
         'cljs.core/IVector))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '{})))
         'cljs.core/IMap))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '#{})))
         'cljs.core/ISet))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env ())))
         'cljs.core/IList))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(fn [x] x))))
         'function)))

(deftest if-inference
  (is (= (a/no-warn
           (e/with-compiler-env test-cenv
             (:tag (a/analyze test-env '(if x "foo" 1)))))
         '#{number string})))

(deftest method-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(.foo js/bar))))
         'js)))

(deftest fn-inference
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env
  ;                 '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
  ;                    (x :one)))))
  ;      'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env
  ;                 '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
  ;                    (x :one :two)))))
  ;      'string))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env
  ;                 '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
  ;                    (x :one :two :three)))))
  ;      'cljs.core/IList))
  )

(deftest lib-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(+ 1 2))))
         'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(alength (array)))))
  ;       'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(aclone (array)))))
  ;       'array))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(-count [1 2 3]))))
  ;      'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(count [1 2 3]))))
  ;       'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(into-array [1 2 3]))))
  ;       'array))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(js-obj))))
  ;       'object))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(-conj [] 1))))
  ;       'clj))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(conj [] 1))))
  ;       'clj))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(assoc nil :foo :bar))))
  ;       'clj))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(dissoc {:foo :bar} :foo))))
  ;       '#{clj clj-nil}))
  )

(deftest test-always-true-if
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(if 1 2 "foo"))))
         'number)))

;; will only work if the previous test works
(deftest test-count
  ;(is (= (cljs.env/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(count []))))
  ;       'number))
  )

(deftest test-numeric
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(dec x)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(int x)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(unchecked-int x)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(mod x y)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(quot x y)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(rem x y)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(bit-count n)))))
  ;       'number))
  )

;; =============================================================================
;; Catching errors during macroexpansion

(deftest test-defn-error
  (is (.startsWith
        (try
          (a/analyze test-env '(defn foo 123))
          (catch Exception e
            (.getMessage e)))
        "Parameter declaration \"123\" should be a vector")))

;; =============================================================================
;; ns desugaring

(deftest test-cljs-975
  (let [spec '((:require [bar :refer [baz] :refer-macros [quux]] :reload))]
    (is (= (set (a/desugar-ns-specs spec))
           (set '((:require-macros (bar :refer [quux]) :reload)
                  (:require (bar :refer [baz]) :reload)))))))

(deftest test-rewrite-cljs-aliases
  (is (= (a/rewrite-cljs-aliases
           '((:require-macros (bar :refer [quux]) :reload)
             (:require (clojure.spec :as s :refer [fdef]) :reload)))
         '((:require-macros (bar :refer [quux]) :reload)
           (:require (cljs.spec :as s :refer [fdef])
                     (cljs.spec :as clojure.spec) :reload))))
  (is (= (a/rewrite-cljs-aliases
           '((:refer-clojure :exclude [first])
              (:require-macros (bar :refer [quux]) :reload)
              (:require (clojure.spec :as s) :reload)))
         '((:refer-clojure :exclude [first])
           (:require-macros (bar :refer [quux]) :reload)
           (:require (cljs.spec :as s) (cljs.spec :as clojure.spec) :reload))))
  (is (= (a/rewrite-cljs-aliases
           '((:require-macros (bar :refer [quux]) :reload)
             (:require clojure.spec :reload)))
         '((:require-macros (bar :refer [quux]) :reload)
           (:require (cljs.spec :as clojure.spec) :reload)))))

;; =============================================================================
;; Namespace metadata

(deftest test-namespace-metadata
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (is (= (do (a/analyze ns-env '(ns weeble.ns {:foo bar}))
               (meta a/*cljs-ns*))
           {:foo 'bar}))

    (is (= (do (a/analyze ns-env '(ns ^{:foo bar} weeble.ns))
               (meta a/*cljs-ns*))
           {:foo 'bar}))

    (is (= (do (a/analyze ns-env '(ns ^{:foo bar} weeble.ns {:baz quux}))
               (meta a/*cljs-ns*))
           {:foo 'bar :baz 'quux}))

    (is (= (do (a/analyze ns-env '(ns ^{:foo bar} weeble.ns {:foo baz}))
               (meta a/*cljs-ns*))
           {:foo 'baz}))

    (is (= (meta (:name (a/analyze ns-env '(ns weeble.ns {:foo bar}))))
           {:foo 'bar}))

    (is (= (meta (:name (a/analyze ns-env '(ns ^{:foo bar} weeble.ns))))
           {:foo 'bar}))

    (is (= (meta (:name (a/analyze ns-env '(ns ^{:foo bar} weeble.ns {:baz quux}))))
           {:foo 'bar :baz 'quux}))

    (is (= (meta (:name (a/analyze ns-env '(ns ^{:foo bar} weeble.ns {:foo baz}))))
           {:foo 'baz}))))

(deftest test-cljs-1105
  ;; munge turns - into _, must preserve the dash first
  (is (not= (a/gen-constant-id :test-kw)
            (a/gen-constant-id :test_kw))))

(deftest test-symbols-munge-cljs-1432
  (is (not= (a/gen-constant-id :$)
            (a/gen-constant-id :.)))
  (is (not= (a/gen-constant-id '$)
            (a/gen-constant-id '.))))

(deftest test-unicode-munging-cljs-1457
  (is (= (a/gen-constant-id :C♯) 'cst$kw$C_u266f_)
      (= (a/gen-constant-id 'C♯) 'cst$sym$C_u266f_)))

;; Constants

(deftest test-constants
 (is (.startsWith
        (try
          (a/analyze test-env '(do (def ^:const foo 123)  (def foo 246)))
          (catch Exception e
            (.getMessage e)))
        "Can't redefine a constant"))
  (is (.startsWith
        (try
          (a/analyze test-env '(do (def ^:const foo 123)  (set! foo 246)))
          (catch Exception e
            (.getMessage e)))
        "Can't set! a constant")))

(deftest test-cljs-1508-rename
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (let [parsed-ns (e/with-compiler-env test-cenv
                      (a/analyze test-env
                        '(ns foo.core
                           (:require [clojure.set :as set :refer [intersection] :rename {intersection foo}]))))]
      (is (nil? (-> parsed-ns :uses (get 'foo))))
      (is (nil? (-> parsed-ns :uses (get 'intersection))))
      (is (some? (-> parsed-ns :renames (get 'foo))))
      (is (= (-> parsed-ns :renames (get 'foo))
             'clojure.set/intersection)))
    (is (e/with-compiler-env test-cenv
          (a/analyze test-env
            '(ns foo.core
               (:use [clojure.set :only [intersection] :rename {intersection foo}])))))
    (is (= (e/with-compiler-env (atom {::a/namespaces
                                       {'foo.core {:renames '{foo clojure.set/intersection}}}})
             (a/resolve-var {:ns {:name 'foo.core}} 'foo))
            '{:name clojure.set/intersection
              :ns   clojure.set}))
    (let [rwhen (e/with-compiler-env (atom (update-in @test-cenv [::a/namespaces]
                                             merge {'foo.core {:rename-macros '{always cljs.core/when}}}))
                  (a/resolve-macro-var {:ns {:name 'foo.core}} 'always))]
      (is (= (-> rwhen :name)
             'cljs.core/when)))
    (let [parsed-ns (e/with-compiler-env test-cenv
                      (a/analyze test-env
                        '(ns foo.core
                           (:refer-clojure :rename {when always
                                                    map  core-map}))))]
      (is (= (-> parsed-ns :excludes) #{}))
      (is (= (-> parsed-ns :rename-macros) '{always cljs.core/when}))
      (is (= (-> parsed-ns :renames) '{core-map cljs.core/map})))
    (is (thrown? Exception (e/with-compiler-env test-cenv
                             (a/analyze test-env
                               '(ns foo.core
                                  (:require [clojure.set :rename {intersection foo}]))))))))

(deftest test-cljs-1274
  (let [test-env (assoc-in (a/empty-env) [:ns :name] 'cljs.user)]
    (binding [a/*cljs-ns* a/*cljs-ns*]
      (is (thrown-with-msg? Exception #"Can't def ns-qualified name in namespace foo.core"
            (a/analyze test-env '(def foo.core/foo 43))))
      (is (a/analyze test-env '(def cljs.user/foo 43))))))

(deftest test-cljs-1763
  (let [parsed (a/parse-ns-excludes {} '())]
    (is (= parsed
           {:excludes #{}
            :renames {}}))
    (is (set? (:excludes parsed)))))

(deftest test-cljs-1785-js-shadowed-by-local
  (let [ws (atom [])]
    (a/with-warning-handlers [(collecting-warning-handler ws)]
      (a/analyze ns-env
        '(fn [foo]
           (let [x js/foo]
             (println x)))))
    (is (.startsWith (first @ws) "js/foo is shadowed by a local"))))

(deftest test-canonicalize-specs
  (is (= (a/canonicalize-specs '((quote [clojure.set :as set])))
         '([clojure.set :as set])))
  (is (= (a/canonicalize-specs '(:exclude (quote [map mapv])))
         '(:exclude [map mapv])))
  (is (= (a/canonicalize-specs '(:require (quote [clojure.set :as set])))
         '(:require [clojure.set :as set])))
  (is (= (a/canonicalize-specs '(:require (quote clojure.set)))
         '(:require [clojure.set]))))

(deftest test-canonicalize-import-specs
  (is (= (a/canonicalize-import-specs '(:import (quote [goog Uri])))
         '(:import [goog Uri])))
  (is (= (a/canonicalize-import-specs '(:import (quote (goog Uri))))
         '(:import (goog Uri))))
  (is (= (a/canonicalize-import-specs '(:import (quote goog.Uri)))
         '(:import goog.Uri))))

(deftest test-cljs-1346
  (testing "`ns*` special form conformance"
    (let [test-env (a/empty-env)]
      (is (= (-> (a/parse-ns '((require '[clojure.set :as set]))) :requires)
            '#{cljs.core clojure.set})))
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*cljs-warnings* nil]
      (let [test-env (a/empty-env)]
        (is (= (-> (a/analyze test-env '(require '[clojure.set :as set])) :requires vals set)
              '#{clojure.set})))
      (let [test-env (a/empty-env)]
        (is (= (-> (a/analyze test-env '(require '[clojure.set :as set :refer [union intersection]])) :uses keys set)
              '#{union intersection})))
      (let [test-env (a/empty-env)]
        (is (= (-> (a/analyze test-env '(require '[clojure.set :as set]
                                          '[clojure.string :as str]))
                 :requires vals set)
              '#{clojure.set clojure.string})))
      (let [test-env (a/empty-env)]
        (is (= (-> (a/analyze test-env '(require-macros '[cljs.test :as test])) :require-macros vals set)
              '#{cljs.test})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(require-macros '[cljs.test :as test  :refer [deftest is]]))]
        (is (= (-> parsed :require-macros vals set)
              '#{cljs.test}))
        (is (= (-> parsed :use-macros keys set)
              '#{is deftest})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(require '[cljs.test :as test :refer-macros [deftest is]]))]
        (is (= (-> parsed :requires vals set)
              '#{cljs.test}))
        (is (= (-> parsed :require-macros vals set)
              '#{cljs.test}))
        (is (= (-> parsed :use-macros keys set)
              '#{is deftest})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(use '[clojure.set :only [intersection]]))]
        (is (= (-> parsed :uses keys set)
              '#{intersection}))
        (is (= (-> parsed :requires)
              '{clojure.set clojure.set})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(use-macros '[cljs.test :only [deftest is]]))]
        (is (= (-> parsed :use-macros keys set)
              '#{deftest is}))
        (is (= (-> parsed :require-macros)
              '{cljs.test cljs.test}))
        (is (nil? (-> parsed :requires))))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(import '[goog.math Long Integer]))]
        (is (= (-> parsed :imports)
              (-> parsed :requires)
              '{Long goog.math.Long
                Integer goog.math.Integer})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(refer-clojure :exclude '[map mapv]))]
        (is (= (-> parsed :excludes)
              '#{map mapv})))))
  (testing "arguments to require should be quoted"
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*cljs-warnings* nil]
      (is (thrown-with-msg? Exception #"Arguments to require must be quoted"
            (a/analyze test-env
              '(require [clojure.set :as set]))))
      (is (thrown-with-msg? Exception #"Arguments to require must be quoted"
            (a/analyze test-env
              '(require clojure.set))))))
  (testing "`:ns` and `:ns*` should throw if not `:top-level`"
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*cljs-warnings* nil]
      (are [analyzed] (thrown-with-msg? Exception
                        #"Namespace declarations must appear at the top-level."
                        analyzed)
          (a/analyze test-env
          '(def foo
             (ns foo.core
               (:require [clojure.set :as set]))))
        (a/analyze test-env
          '(fn []
             (ns foo.core
               (:require [clojure.set :as set]))))
        (a/analyze test-env
          '(map #(ns foo.core
                   (:require [clojure.set :as set])) [1 2])))
      (are [analyzed] (thrown-with-msg? Exception
                        #"Calls to `require` must appear at the top-level."
                        analyzed)
        (a/analyze test-env
          '(def foo
             (require '[clojure.set :as set])))
        (a/analyze test-env
          '(fn [] (require '[clojure.set :as set])))
        (a/analyze test-env
          '(map #(require '[clojure.set :as set]) [1 2]))))))

(deftest test-gen-user-ns
  ;; note: can't use `with-redefs` because direct-linking is enabled
  (let [s   "src/cljs/foo.cljs"
        sha (util/content-sha s)]
    (is (= (a/gen-user-ns s) (symbol (str "cljs.user.foo" (apply str (take 7 sha)))))))
  (let [a   "src/cljs/foo.cljs"
        b   "src/cljs/foo.cljc"]
    ;; namespaces should have different names because the filename hash will be different
    (is (not= (a/gen-user-ns a) (a/gen-user-ns b)))
    ;; specifically, only the hashes should differ
    (let [nsa (str (a/gen-user-ns a))
          nsb (str (a/gen-user-ns b))]
      (is (not= (.substring nsa (- (count nsa) 7)) (.substring nsb (- (count nsb) 7))))
      (is (= (.substring nsa 0 (- (count nsa) 7)) (.substring nsb 0 (- (count nsb) 7)))))))

(deftest test-cljs-1536
  (let [parsed (e/with-compiler-env test-cenv
                 (a/analyze (assoc test-env :def-emits-var true)
                   '(def x 1)))]
    (is (some? (:var-ast parsed))))
  (let [parsed (e/with-compiler-env test-cenv
                 (a/analyze (assoc test-env :def-emits-var true)
                   '(let [y 1] (def y 2))))]
    (is (some? (-> parsed :expr :ret :var-ast)))))

(defn ana' [form]
  (e/with-compiler-env test-cenv
    (a/analyze test-env form)))

(defmacro ana [form]
  `(ana' '~form))

(defn prs-ana [fstr]
  (e/with-compiler-env test-cenv
    (let [[form] (a/forms-seq*
                   (java.io.StringReader. fstr))]
      (ana' form))))

(def juxt-op-val (juxt :op :val))

(deftest analyze-ops
  ;constants
  (is (= (-> (ana 1) juxt-op-val) [:const 1]))
  (is (= (:op (ana '(1 2 3))) :quote))
  ;variables
  (is (= (:op (ana inc)) :var))
  ;do
  (is (= (-> (ana (do 1 2)) :op) :do))
  (is (= (-> (ana (do 1 2)) :children) [:statements :ret]))
  ;   :statements
  (is (vector? (-> (ana (do)) :statements)))
  (is (vector? (-> (ana (do 1)) :statements)))
  (is (vector? (-> (ana (do 1 2)) :statements)))
  (is (= (-> (ana (do 1 2)) :statements first :op) :const))
  ;   :ret
  (is (= (-> (ana (do)) :ret juxt-op-val) [:const nil]))
  (is (= (-> (ana (do nil)) :ret juxt-op-val) [:const nil]))
  (is (= (-> (ana (do 1)) :ret juxt-op-val) [:const 1]))
  (is (= (-> (ana (do 1 2)) :ret juxt-op-val) [:const 2]))
  ;let
  (is (= (-> (ana (let [])) :op) :let))
  (is (= (-> (ana (let [a 1] a)) :children) [:bindings :body]))
  ;  :bindings
  (is ((every-pred vector? empty?) (-> (ana (let [])) :bindings)))
  (is (vector? (-> (ana (let [a 1] a)) :bindings)))
  (is (vector? (-> (ana (let [a 1 b 2] a)) :bindings)))
  (is (= (-> (ana (let [a 1] a)) :bindings first :op) :binding))
  (is (= (-> (ana (let [a 1] a)) :bindings first :init :op) :const))
  ;  :body
  (is (= (-> (ana (let [a 1] a)) :body :op) :do))
  ;local
  (is (empty? (-> (ana (let [a 1] a)) :body :ret :children)))
  (is (= (-> (ana (let [a 1] a)) :body :ret :op) :local))
  ;local shadow
  (is (= (a/no-warn (-> (ana (let [alert 1] js/alert)) :body :ret :op))
         :local))
  (comment
    (-> (ana (let [a 1] a)) :body :ret :env :locals clojure.pprint/pprint)
    (-> (ana (let [alert js/alert] alert)))
    )
  ;loop
  (is (= (-> (ana (loop [])) :op) :loop))
  (is (= (-> (ana (loop [a 1])) :bindings first :op) :binding))
  (is (= (-> (ana (loop [a 1] a)) :bindings first :init :op) :const))
  (is (= (-> (ana (loop [a 1] (recur 1))) :children) [:bindings :body]))
  ;recur
  (is (= (-> (ana (loop [a 1] (recur 1))) :body :ret :op) :recur))
  (is (= (-> (ana (loop [a 1] (recur 1))) :body :ret :children) [:exprs]))
  ;    :exprs
  (is ((every-pred vector? empty?) (-> (ana (loop [] (recur))) :body :ret :exprs)))
  (is (vector? (-> (ana (loop [a 1] (recur 1))) :body :ret :exprs)))
  (is (vector? (-> (ana (loop [a 1 b 2] (recur 1 2))) :body :ret :exprs)))
  (is (= (-> (ana (loop [a 1] (recur 1))) :body :ret :exprs first :op) :const))
  (is (= (-> (ana (loop [a 1 b 2] (recur 1 2))) :body :ret :exprs second :op) :const))
  ;try
  (is (= (-> (ana (try)) :op) :try))
  (is (= (-> (ana (try)) :children) [:body]))
  (is (= (-> (ana (try (catch :default e))) :children) [:body :catch]))
  (is (= (-> (ana (try (catch :default e) (finally))) :children) [:body :catch :finally]))
  (is (= (-> (ana (try (finally))) :children) [:body :finally]))
  ;   :name
  (is (symbol? (-> (ana (try (catch :default a))) :name)))
  (is (nil? (-> (ana (try)) :name)))
  ;   :catch
  (is (keyword? (-> (ana (try (catch :default a))) :catch :op)))
  ;   :finally
  (is (= (-> (ana (try (finally 1))) :finally :op) :do))
  (is (= (-> (ana (try (finally 1))) :finally :ret :op) :const))
  ;case
  (is (= (-> (ana (case 1)) :op) :let))
  (is (= (-> (ana (case 1)) :body :ret :op) :case))
  (is (= (-> (ana (case 1)) :body :ret :children) [:test :nodes :default]))
  ;   :test
  (is (= (-> (ana (case 1)) :body :ret :test :op) :var))
  ;   :nodes
  (is (vector? (-> (ana (case 1)) :body :ret :nodes)))
  (is (vector? (-> (ana (case 1 :a 1)) :body :ret :nodes)))
  (is (vector? (-> (ana (case 1 (:a :b) 1)) :body :ret :nodes)))
  ;       :tests
  (is (vector?
        (-> (ana (case 1 :a 1)) :body :ret :nodes first :tests)))
  (is (vector?
        (-> (ana (case 1 :a 1 :b 2)) :body :ret :nodes first :tests)))
  (is (= (-> (ana (case 1 :a 1)) :body :ret :nodes first :tests first :op)
         :case-test))
  (is (= (-> (ana (case 1 :a 1)) :body :ret :nodes first :tests first :test juxt-op-val)
         [:const "a"]))
  (is (= (-> (ana (case 1 :a 1 :b 2)) :body :ret :nodes second :tests first :test juxt-op-val)
         [:const "b"]))
  (is (= (-> (ana (case 1 :a 1 (:b :faz) 2)) :body :ret :nodes (nth 2) :tests first :test juxt-op-val)
         [:const "faz"]))
  ;       :thens
  (is (= (-> (ana (case 1 :a 3)) :body :ret :nodes first :then :op)
         :case-then))
  (is (= (-> (ana (case 1 :a 3)) :body :ret :nodes first :then :then juxt-op-val)
         [:const 3]))
  (is (= (-> (ana (case 1 :a 3 :b 4)) :body :ret :nodes second :then :then juxt-op-val)
         [:const 4]))
  (is (= (-> (ana (case 1 :a 3 (:b :c) 4)) :body :ret :nodes (nth 2) :then :then juxt-op-val)
         [:const 4]))
  ;   :default
  (is (= :throw (-> (ana (case 1)) :body :ret :default :op)))
  (is (= [:const 2] (-> (ana (case 1 2)) :body :ret :default juxt-op-val)))
  ;def
  (is (= :def (-> (ana (def a)) :op)))
  (is (= nil (-> (ana (def a)) :children)))
  (is (= [:init] (-> (ana (def a 1)) :children)))
  ;   :name
  (is (= "a" (-> (ana (def a 1)) :name name)))
  ;   :init
  (is (= nil (-> (ana (def a)) :init)))
  (is (= [:const 1] (-> (ana (def a 1)) :init juxt-op-val)))
  ;deftype
  (is (= :deftype (-> (ana (deftype A [])) :statements first :op)))
  (is (= [:body] (-> (ana (deftype A [])) :statements first :children)))
  ;   :body
  (is (= :do (-> (ana (deftype A [a] Object (toString [this] a))) :statements first :body :op)))
        ; field reference
  (is (= [:local true]
         (-> (ana (deftype A [a] Object (toString [this] a))) 
             :statements first :body :ret :val :methods
             first :body :ret :body :ret 
             ((juxt :op (comp :field :info))))))
  ;defrecord
  (is (= :defrecord (-> (ana (defrecord Ab [])) :body :statements first :ret :op)))
  (is (= [:body] (-> (ana (defrecord Ab [])) :body :statements first :ret :children)))
  ;   :body
  (is (= :do (-> (ana (defrecord Ab [] Object (toString [this] "a"))) :body :statements first :ret :body :op)))
  ;fn
  (is (= :fn (-> (ana (fn [])) :op)))
  (is (= [:methods] (-> (ana (fn [])) :children)))
  ;   :variadic
  (is (true? (-> (ana (fn [& a])) :variadic)))
  (is (false? (-> (ana (fn [])) :variadic)))
  ;   :methods
  (is (vector? (-> (ana (fn [])) :methods)))
  (is (vector? (-> (ana (fn ([]) ([a]))) :methods)))
  ;fn-method
  (is (= :fn-method (-> (ana (fn [])) :methods first :op)))
  (is (= [:body] (-> (ana (fn [])) :methods first :children)))
  ;   :max-fixed-arity
  (is (= 0 (-> (ana (fn [])) :methods first :max-fixed-arity)))
  (is (= 1 (-> (ana (fn [a])) :methods first :max-fixed-arity)))
  (is (= 2 (-> (ana (fn [a b & c])) :methods first :max-fixed-arity)))
  ;   :variadic
  (is (true? (-> (ana (fn [a b & c])) :variadic)))
  (is (false? (-> (ana (fn [a b])) :variadic)))
  ;   :body
  (is (= [:const 1] (-> (ana (fn [] 1)) :methods first :body :ret juxt-op-val)))
    ;FIXME add tests for :fn-method :params
  ;if
  (is (= :if (-> (ana (if 1 2)) :op)))
  (is (= :if (-> (ana (if 1 2 3)) :op)))
  (is (= [:test :then :else] (-> (ana (if 1 2 3)) :children)))
  (is (= [:test :then :else] (-> (ana (if 1 2)) :children)))
  ;   :test
  (is (= [:const 1] (-> (ana (if 1 2)) :test juxt-op-val)))
  (is (= [:const 1] (-> (ana (if 1 2 3)) :test juxt-op-val)))
  ;   :then
  (is (= [:const 2] (-> (ana (if 1 2)) :then juxt-op-val)))
  (is (= [:const 2] (-> (ana (if 1 2 3)) :then juxt-op-val)))
  ;   :else
  (is (= [:const nil] (-> (ana (if 1 2)) :else juxt-op-val)))
  (is (= [:const 3]   (-> (ana (if 1 2 3)) :else juxt-op-val)))
  ;invoke
  (is (= :invoke (-> (ana (:a 1)) :op)))
  (is (= [:fn :args] (-> (ana (:a 1)) :children)))
  (is ((every-pred vector? empty?) (-> (ana (#'str)) :args)))
  (is (vector? (-> (ana (:a 1)) :args)))
  (is (vector? (-> (ana (:a 1 2)) :args)))
  ;   :fn
  (is (= :the-var (-> (ana (#'str)) :fn :op)))
  ;   :invoke
  (is (= [:const 1] (-> (ana (:a 1)) :args first juxt-op-val)))
  (is (= [:const 2] (-> (ana (:a 1 2)) :args second juxt-op-val)))
  ;js-array
  (is (= :js-array (-> (prs-ana "#js ['a]") :op)))
  (is (= [:items] (-> (prs-ana "#js ['a]") :children)))
  (is (vector? (-> (prs-ana "#js ['a]") :items)))
  (is (= 'array (-> (prs-ana "#js ['a]") :tag)))
  (is (= [:const :a] (-> (prs-ana "#js [:a]") :items first juxt-op-val)))
  ;js-object
  (is (= :js-object (-> (prs-ana "#js {:a 1}]") :op)))
;; FIXME :keys should be an expression too
  (is (= [:vals] (-> (prs-ana "#js {:a 1}") :children)))
  (is (vector? (-> (prs-ana "#js {:a 1}") :vals)))
  (is (= :a (-> (prs-ana "#js {:a 1}") :keys first)))
  (is (vector? (-> (prs-ana "#js {:a 1}") :keys)))
  (is (= [:const 1] (-> (prs-ana "#js {:a 1}") :vals first juxt-op-val)))
  ;js*
  (is (= :js (-> (ana (js* "~{}" 'food)) :op)))
  (is (= [:args] (-> (ana (js* "~{}" 'food)) :children)))
  (is (vector? (-> (ana (js* "~{}" 'food)) :args)))
  (is (= [:const 'food] (-> (ana (js* "~{}" 'food)) :args first juxt-op-val)))
;; FIXME why not a vector?
  ;(is (vector? (-> (ana (js* "~{} / ~{}" 1 2)) :segs)))
  (is (= ["" " / " ""] (-> (ana (js* "~{} / ~{}" 1 2)) :segs)))
  ;letfn
  (is (= :letfn
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :op)))
  (is (= [:bindings :body]
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :children)))
  ;   :bindings
  (is (vector?
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :bindings)))
  (is (vector?
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :bindings)))
  (is (= :binding
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :bindings
             first
             :op)))
  (is (= :fn
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :bindings
             first
             :init
             :op)))
  ;   :body
  (is (= :invoke
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :body :ret :op)))
  (is (= :var
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :body :ret :fn :op)))
  ;map
  (is (= :map (-> (ana {:a 1}) :op)))
  (is (= [:keys :vals] (-> (ana {:a 1}) :children)))
  ;   :keys
  (is ((every-pred vector? empty?) (-> (ana {}) :keys)))
  (is (vector? (-> (ana {:a 1}) :keys)))
  (is (= [:const :a] (-> (ana {:a 1}) :keys first juxt-op-val)))
  ;   :vals
  (is ((every-pred vector? empty?) (-> (ana {}) :vals)))
  (is (vector? (-> (ana {:a 1}) :vals)))
  (is (= [:const 1] (-> (ana {:a 1}) :vals first juxt-op-val)))
  ;new
  (is (= :new
         (-> (ana (do (deftype Person [a]) (Person. 1)))
             :ret
             :op)))
  (is (= [:ctor :args]
         (-> (ana (do (deftype Person [a]) (Person. 1)))
             :ret
             :children)))
  ;   :ctor
  (is (= [:var 'cljs.core/Person]
         (-> (ana (do (deftype Person [a]) (Person. 1)))
             :ret
             :ctor
             ((juxt :op (comp :name :info))))))
  ;   :args
  (is ((every-pred vector? empty?)
         (-> (ana (do (deftype Noarg []) (Noarg.)))
             :ret
             :args)))
  (is (= [:const 1]
         (-> (ana (do (deftype Person [a]) (Person. 1)))
             :ret
             :args
             first
             juxt-op-val)))
  ;set
  (is (= :set (-> (ana #{:a :b}) :op)))
  (is (= [:items] (-> (ana #{:a :b}) :children)))
  ;   :items
  (is ((every-pred vector? empty?)  (-> (ana #{}) :items)))
  (is (vector? (-> (ana #{:a}) :items)))
  (is (vector? (-> (ana #{:a :c :b}) :items)))
  (is (= [:const :a] (-> (ana #{:a}) :items first juxt-op-val)))
  ;set!
  (is (= :set!
         (-> (ana (do (def a 1) (set! a "Hi!")))
             :ret :op)))
  (is (= [:target :val]
         (-> (ana (do (def a 1) (set! a "Hi!")))
             :ret :children)))
  ;   :target
  (is (= [:var 'cljs.core/a]
         (-> (ana (do (def a 1) (set! a "Hi!")))
             :ret :target ((juxt :op (comp :name :info))))))
  ;   :val
  (is (= [:const "Hi!"]
         (-> (ana (do (def a 1) (set! a "Hi!")))
             :ret :val juxt-op-val)))
  ;the-var
  (is (= :the-var (-> (ana #'+) :op)))
  (is (nil? (-> (ana #'+) :children)))
  ;   :var
  (is (= 'cljs.core/+ (-> (ana #'+) :var :info :name)))
  ;throw
  (is (= :throw (-> (ana (throw (js/Error. "bad"))) :op)))
  (is (= [:exception] (-> (ana (throw (js/Error. "bad"))) :children)))
  ;   :exception
  (is (= [:var 'js/Error] (-> (ana (throw (js/Error. "bad"))) :exception 
                              :ctor
                              ((juxt :op (comp :name :info))))))
  ;vector
  (is (= :vector (-> (ana [1]) :op)))
  (is (= [:items] (-> (ana [1]) :children)))
  ;   :items
  (is ((every-pred vector? empty?) (-> (ana []) :items)))
  (is (vector? (-> (ana [1]) :items)))
  (is (= [:const 1] (-> (ana [1]) :items first juxt-op-val)))
  ;with-meta
  (is (= :with-meta (-> (ana ^:blah (fn [])) :op)))
  (is (= [:meta :expr] (-> (ana ^:blah (fn [])) :children)))
  ;   :meta
  (is (= :map (-> (ana ^:blah (fn [])) :meta :op)))
  (is (= [:const :blah] (-> (ana ^:blah (fn [])) :meta :keys first juxt-op-val)))
  (is (= [:const true] (-> (ana ^:blah (fn [])) :meta :vals first juxt-op-val)))
  ;   :expr
  (is (= :fn (-> (ana ^:blah (fn [])) :expr :op)))
  ;host-field
  (is (= :host-field (-> (ana (.-field 'a)) :op)))
  (is (= [:target] (-> (ana (.-field 'a)) :children)))
  (is (= 'field (-> (ana (.-field 'a)) :field)))
  ;   :target
  (is (= [:const 'a] (-> (ana (.-field 'a)) :target juxt-op-val)))
  ;host-call
  (is (= :host-call (-> (ana (.call 'a)) :op)))
  (is (= [:target :args] (-> (ana (.call 'a)) :children)))
  (is (= 'call (-> (ana (.call 'a)) :method)))
  ;   :target
  (is (= [:const 'a] (-> (ana (.call 'a)) :target juxt-op-val)))
  ;   :args
  (is ((every-pred vector? empty?) (-> (ana (.call 'a)) :args)))
  (is (= [:const 1] (-> (ana (.call 'a 1)) :args first juxt-op-val)))
  ;ns
  (is (binding [a/*cljs-ns* 'cljs.user]
        (= :ns (-> (ana (ns fazz.foo)) :op))))
  ;ns*
  (is (binding [a/*cljs-ns* 'cljs.user]
        (= :ns* (-> (ana (refer-clojure :exclude '[locking])) :op))))
  ;quote
  (is (= :quote (-> (ana (quote a)) :op)))
  (is (= [:expr] (-> (ana (quote a)) :children)))
  (is (map? (-> (ana (quote a)) :env)))
  (is (= 'quote (-> (ana (quote a)) :form first)))
  ;   :expr
  (is (= [:const 'a] (-> (ana (quote a)) :expr juxt-op-val)))
)

(deftest quote-args-error-test
  (is (.startsWith
        (try
          (ana (quote))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to quote"))
  (is (.startsWith
        (try
          (ana (quote a b))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to quote"))
  (is (.startsWith
        (try
          (ana (quote a b c d))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to quote")))

(deftest var-args-error-test
  (is (.startsWith
        (try
          (ana (var))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to var"))
  (is (.startsWith
        (try
          (ana (var a b))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to var"))
  (is (.startsWith
        (try
          (ana (var nil))
          (catch Exception e
            (.getMessage e)))
        "Argument to var must be symbol")))

(comment
  (require '[cljs.compiler :as cc])

  ;; TODO: need to handle the method/fn case
  (let [test-cenv (atom {::a/externs (externs/default-externs)})]
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*cljs-warnings* (assoc a/*cljs-warnings* :infer-warning true)]
      (e/with-compiler-env test-cenv
        (a/analyze-form-seq
          '[(ns foo.core)
            (defn bar [a] (js/parseInt a))
            (def c js/React.Component)
            (js/console.log "Hello world!")
            (fn [& args]
              (.apply (.-log js/console) js/console (into-array args)))
            (js/console.log js/Number.MAX_VALUE)
            (js/console.log js/Symbol.iterator)]))
      (cc/emit-externs
        (reduce util/map-merge {}
          (map (comp :externs second)
            (get @test-cenv ::a/namespaces))))))
  )
