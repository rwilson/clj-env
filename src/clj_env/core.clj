(ns clj-env.core
  "A library for defining environments by setting ^:dynamic identities using
   file defined envs. There are a few ways to use this:

  First, via the `with-env` macro, like so:

        (with-env (do-something))

  Second, by loading an env to apply globally:

        (do
          (load-env! env)
          (do-something))

  The former is useful for narrow scopes, the latter to ensure that all threads
  use the same bindings until changed."
  (:require [taoensso.timbre :as log]
            [clojure.edn :as edn]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as clj-str]
            [clj-lib.util :as util :refer (safe mapmerge)]
            [clj-lib.types :as types :refer (to-string)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The core of the core...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ^{:doc "Implementation detail. Holds the env bindings as a map of
                 namespace qualified vars to values. This tracks the calls to
                 `set-var!` which can be useful to inspect what has been
                 modified."}
  overrides (atom {}))

(defn set-var!
  "Implementation detail.

  Sets a single dynamic var to the specified value, regardless of its
  present value.

  Note: this implementation could result in inconsistencies if it is called
        concurrently to modify the same var."
  ([var value]
   (swap! overrides assoc var value)
   (alter-var-root var (constantly value)))
  ([[var value]] (set-var! var value)))

(defn set-vars!
  "Implementation detail.

  Sets multiple dynamic vars specified in the input map. The input map must be
  a map of var => value."
  [var-map] (doall (map set-var! var-map)))

(defn backup-vars
  "Implementation detail.

  Backs up multiple vars current values, returns a map of var=>value."
  [vars]
  (reduce (fn [c v] (assoc c v (var-get v)))
          {}
          vars))

(defn invalid-override-exception
  "Implementation detail.

  Creates an IllegalArgumentException with details of the bad override"
  ([ns name]
   (IllegalArgumentException.
    (str "Invalid env override, could not resolve var #'" ns "/" name)))
  ([var]
   (let [{:keys [ns name]} (meta var)]
     (invalid-override-exception ns name))))

(defn validate
  "Returns true if the specified env is valid, otherwise a list of errors"
  [env]
  (let [errors (mapcat (fn [{:keys [ns bindings]}]
                         (reduce-kv (fn [errors k v]
                                      (when-not (find-ns ns)
                                        (safe (require ns)))
                                      (if-let [var (safe (ns-resolve ns k))]
                                        errors
                                        (conj errors (str "#'" ns "/" k))))
                                    nil
                                    bindings))
                       env)]
    (if (empty? errors)
      true
      errors)))

(defn resolve-bindings
  "Resolves the fully-qualified bindings. Transforms this:

        {:ns some.namespace
         :bindings {:some-var 1
                    :a-value :foo}}

  Into a `with-bindings` compatible bindings map, like so:

        {#'some.namespace/some-var 1
         #'some.namespace/a-value :foo}"
  [{:keys [ns bindings]}]
  (reduce-kv (fn [m name value]
               (if-let [var (safe (ns-resolve ns name))]
                 (assoc m var value)
                 (throw (invalid-override-exception ns name))))
             {}
             bindings))

(defn bindings-map
  "Converts the env file format into a map that can be passed to with-bindings.
   Throws an Exception if there is an invalid config."
  [env]
  (mapmerge resolve-bindings env))


(defmacro with-env
  "Evaluates body with env bindings."
  [env & body] `(with-bindings ~env ~@body))

(defn to-string-keys
  "Exaclty as named. This is a hack to fix Var printing in clojure.pprint until
   [CLJ-1565](http://dev.clojure.org/jira/browse/CLJ-1565) is resolved."
  [map]
  (persistent! (reduce-kv
                (fn [m k v] (assoc! m (to-string k) v))
                (transient {})
                map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load!
  "Attempts to load a config. `src` can be anything `slurp` can handle, such as
  a string representing a file or resource path, a java.net URI/URL, or
  java.io.File.

  Loads `env`, backing up any vars to be rebound and then binding vars globally
  to their new values.

  Returns a delay that, when forced, restores all vars to their original values."
  [src]
  (if-let [str-data (slurp src)]
    (if-let [env (edn/read-string str-data)]
      (let [validation (validate env)]
        (if (true? validation)
          (let [env-map (bindings-map env)
                backup-map (backup-vars (keys env-map))]
            (set-vars! env-map)
            (log/info (str "Loaded env:" \newline
                           (with-out-str (pprint (to-string-keys @overrides)))))
            (delay
             (do
               (set-vars! backup-map)
               (log/info (str "Unloaded env:" \newline
                              (with-out-str (pprint (to-string-keys backup-map))))))))
          (log/warn (str "Failed to load env." \newline \newline
                         "The following vars do not exist:" \newline
                         "\t" (clj-str/join (str \newline "\t") validation)))))
      (log/error (str "Failed to read env, invalid edn: " str-data)))
    (log/error (str "Invalid config, src nil: " src))))
