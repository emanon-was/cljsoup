(ns cljsoup.macros
  (:gen-class))

;;
;; defstrict
;;

(defrecord TypedArg [type arg])

(let [common-type
      {:Nil nil
       :Function clojure.lang.Fn
       :String java.lang.String
       :Number java.lang.Number
       :List clojure.lang.PersistentList
       :Vector clojure.lang.PersistentVector
       :HashMap clojure.lang.PersistentArrayMap
       :HashSet clojure.lang.PersistentHashSet
       :Symbol clojure.lang.Symbol
       :Sequence clojure.lang.Sequential}]

  (defn- typed-args [args]
    (loop [acc nil type nil args args]
      (let [args? (seq args) type? type
            arg (first args)]
        (cond
         (not args?) (reverse acc)
         (= arg '&)  (reverse (into acc args))
         (not type?) (if (keyword? arg)
                       (recur acc
                              (if (contains? common-type arg)
                                (common-type arg)
                                (symbol (name arg)))
                              (rest args))
                       (recur (cons arg acc) nil (rest args)))
         type? (if (symbol? arg)
                 (recur (cons (->TypedArg type arg) acc) nil (rest args))
                 (recur (cons arg acc) nil (rest args))))))))

(defn- typed-arg? [arg]
  (instance? TypedArg arg))

(defn- signature [sig]
  (let [base-args (typed-args (first sig))
        target-args (filter typed-arg? base-args)
        dispatch-args
        (vec (map (fn [_] (if (typed-arg? _) (:arg _) _)) base-args))]
    {:multi-args  dispatch-args
     :multi-body  (vec (map (fn [target] `(class ~(:arg target))) target-args))
     :method-vals (vec (map #(:type %) target-args))
     :method-args dispatch-args
     :method-body (rest sig)}))

(defn- multi-signature [sig]
  `(~(:multi-args sig)
    ~(:multi-body sig)))

(defn- method-signature [sig]
  `(~(:method-vals sig)
    ~(:method-args sig)
    ~@(:method-body sig)))


(defn- signatures [sigs]
  (map signature sigs))

(defn- multi-signatures [sigs]
  (map (comp multi-signature first)
       (vals (group-by #(count (:multi-args %)) sigs))))

(defn- method-signatures [sigs]
  (map method-signature sigs))

(defmacro defstrict* [name & sigs]
  (let [sigs (signatures sigs)
        multi-sigs (multi-signatures sigs)
        method-sigs (method-signatures sigs)]
    `(do
       (def ~name nil)
       (defmulti ~name (fn ~@multi-sigs))
       ~@(map (fn [s] `(defmethod ~name ~@s)) method-sigs))))

(defmacro defstrict [name & sigs]
  (if (vector? (first sigs))
    `(defstrict* ~name ~sigs)
    `(defstrict* ~name ~@sigs)))

;;
;; ns-inheritance, ns-uninheritance
;;

(defmacro ns-inheritance [ns]
  (let [publics (ns-publics ns)
        syms (keys publics)]
    `(do ~@(map (fn [sym] `(def ~sym ~(publics sym))) syms))))

(defmacro ns-uninheritance [ns]
  (let [syms (keys (ns-publics ns))]
    `(do ~@(map (fn [s] `(ns-unmap '~(ns-name *ns*) '~s))
                (filter #(= (ns-resolve (find-ns ns) %)
                            (let [r (ns-resolve *ns* %)]
                              (if r (var-get r) nil)))
                        syms)))))

