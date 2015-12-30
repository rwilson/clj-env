(ns clj-env.core-test
  (:require [clojure.test :refer :all]
            [clojure.edn :as edn]
            [clj-env.core :refer :all]
            [taoensso.timbre :as log]))

(defn log-level-fixture [f]
  (log/with-level :error (f)))

(use-fixtures :once log-level-fixture)

(def ^:dynamic *test-int* 0)
(def ^:dynamic *test-kw* :na)
(def ^:dynamic *test-map* {:a "b" :c "d"})

(def good-env "test-data/good.edn")
(def bad-env "test-data/bad.edn")

(defn assert-initial-bindings
  "Util for checking the bindings from the initial load."
  []
  (is (= 0 *test-int*))
  (is (= :na *test-kw*))
  (is (= {:a "b" :c "d"} *test-map*))  )

(defn assert-good-bindings
  "Util for checking the bindings from the good env file."
  []
  (is (= 1 *test-int*))
  (is (= :ya *test-kw*))
  (is (= {:e "f" :g "h"} *test-map*)))

(deftest test-bindings-map-valid-env
  (let [env (edn/read-string (slurp good-env))]
    (is (map? (bindings-map env)))))

(deftest test-bindings-map-invalid-env
  (let [env (edn/read-string (slurp bad-env))]
    (is (thrown-with-msg? IllegalArgumentException #"Invalid env override.*"
                          (bindings-map env)))))

(deftest test-with-env
  (let [env (edn/read-string (slurp good-env))
        bindings (bindings-map env)]
    (with-env bindings
      (assert-good-bindings))))

(deftest test-validate
  (let [good (edn/read-string (slurp good-env))
        bad (edn/read-string (slurp bad-env))]
    (is (true? (validate good)))
    (is (seq? (validate bad)))))

(deftest test-backup-vars
  (let [backup (backup-vars (list #'clj-env.core-test/*test-int*
                                  #'clj-env.core-test/*test-kw*
                                  #'clj-env.core-test/*test-map*))]
    (is (= 0
           (get backup #'clj-env.core-test/*test-int*)))
    (is (= :na
           (get backup #'clj-env.core-test/*test-kw*)))
    (is (= {:a "b" :c "d"}
           (get backup #'clj-env.core-test/*test-map*)))))

(deftest test-load!
  (let [unloader (load! good-env)]
    (assert-good-bindings)
    @unloader)
  (assert-initial-bindings))
