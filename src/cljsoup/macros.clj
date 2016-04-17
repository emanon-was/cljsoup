(ns cljsoup.macros
  (:gen-class))

;;
;; defstrict
;;

(defn types [& _]
  (vec (map class _)))

(def common-types
  {:Nil      nil
   :Function clojure.lang.Fn
   :String   java.lang.String
   :Number   java.lang.Number
   :List     clojure.lang.PersistentList
   :Vector   clojure.lang.PersistentVector
   :HashMap  clojure.lang.PersistentArrayMap
   :HashSet  clojure.lang.PersistentHashSet
   :Symbol   clojure.lang.Symbol
   :Sequence clojure.lang.Sequential})

(defn- match-types
  ([types-map] #(match-types types-map %))
  ([types-map key]
   (if (contains? types-map key)
     (types-map key)
     (symbol (name key)))))

(defn- typed-args [_]
  (let [args (partition 2 _)]
    {:dispatch-val (vec (map first args))
     :params       (vec (map second args))}))

(defn- method-signature [_]
  (let [args (typed-args (first _))]
    `(~(vec (map (match-types common-types) (:dispatch-val args)))
      ~(:params args)
      ~@(rest _))))

(defmacro defstrict* [name & sigs]
  (let [method-sigs (map method-signature sigs)]
    `(do
       (def ~name nil)
       (defmulti ~name types)
       ~@(map (fn [s] `(defmethod ~name ~@s)) method-sigs))))

(defmacro defstrict [name & sigs]
  (if (vector? (first sigs))
    `(defstrict* ~name ~sigs)
    `(defstrict* ~name ~@sigs)))
