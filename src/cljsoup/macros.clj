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

  (defn- build-typedarg [key sym]
    (->TypedArg
     (if (contains? common-type key)
       (common-type key)
       (symbol (name key)))
     sym)))


(defn- typed-args [args]
  (loop [arg  (first  args)
         more (rest args)
         acc  nil]
    (cond
     (not arg) (reverse acc)
     (empty? more) (reverse (cons arg acc))
     :else (let [next (first more)
                 key  (if (keyword? arg) arg nil)
                 sym  (if (symbol? next) next nil)]
             (cond
              (not key) (recur next (rest more) (cons arg acc))
              (= next '&) (recur next (rest more) acc)
              sym (let [rest-more (rest more)]
                    (recur (first rest-more) (rest rest-more)
                           (cons (build-typedarg key sym) acc))))))))

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

