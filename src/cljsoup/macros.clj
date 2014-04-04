(ns cljsoup.macros
  (:gen-class))

;;
;; defstrict
;;

(let [common-type
      {'& clojure.lang.ArraySeq
       :Function clojure.lang.Fn
       :String java.lang.String
       :Number java.lang.Number
       :List clojure.lang.PersistentList
       :Vector clojure.lang.PersistentVector
       :HashMap clojure.lang.PersistentArrayMap
       :HashSet clojure.lang.PersistentHashSet
       :Symbol clojure.lang.Symbol
       :Sequence clojure.lang.ArraySeq
       :Nil nil}]

  (defn- dispatch-fn-arg [args]
    (let [fn #(if (= '& (first %)) % (second %))]
      (vec (flatten (map fn args)))))

  (defn- dispatch-val [args]
    (let [f #(cond (contains? common-type %) (common-type %)
                   :else (symbol (name %)))]
      (vec (map (comp f first) args))))

  (defn- dispatch-sym [args]
    (vec (map second args))))


(defn- signature [sig]
  (let [args (partition 2 (first sig))]
    {:dispatch-fn-arg (dispatch-fn-arg args)
     :dispatch-val (dispatch-val args)
     :dispatch-sym (dispatch-sym args)
     :fn-tail-body (rest sig)}))

(defn- signatures [sigs]
  (map signature sigs))

(defn- multi-signature [sig]
  (let [args (sig :dispatch-fn-arg)
        body (vec (map (fn [sym] `(class ~sym))
                       (-> sig :dispatch-sym)))]
    `(~args ~body)))

(defn- multi-signatures [sigs]
  (map (comp multi-signature first)
       (vals (group-by #(count (% :dispatch-fn-arg)) sigs))))
  
(defn- method-signature [sig]
  (let [types (sig :dispatch-val)
        syms  (sig :dispatch-fn-arg)
        body  (sig :fn-tail-body)]
    `(~types ~syms ~@body)))

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

