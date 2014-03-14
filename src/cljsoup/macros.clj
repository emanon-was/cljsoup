(ns cljsoup.macros
  (:gen-class))

;;
;; defstrict
;;

(defn classes? [sym]
  (cond (= sym 'Nil) true
        (= sym 'Function) true
        (= sym 'List) true
        (= sym 'Vector) true
        (= sym 'HashMap) true
        (= sym 'HashSet) true
        (= sym 'Symbol) true
        (= sym '&) true
        :else (class? (resolve sym))))

(defn classes [val]
  (cond (nil? val) 'Nil
        (fn?  val) 'Function
        (list? val) 'List
        (vector? val) 'Vector
        (map? val) 'HashMap
        (set? val) 'HashSet
        (symbol? val) 'Symbol
        (seq? val) 'Sequence
        :else (class val)))

;;
;; 
;;
(defn- default-args [args]
  (let [fn #(if (and (classes? (first %))
                     (not= '& (first %)))
              (second %) %)]
    (flatten (map fn args))))

(defn typed-args [args]
  (let [typed-args (filter (comp classes? first) args)]
    {:types (map #(let [f (first %)] (if (= '& f) 'Sequence f))
                 typed-args),
     :syms (map second typed-args)}))

(defn- signature [sig]
  (let [args (partition 2 (first sig))]
    {:default-args (default-args args),
     :typed-args (typed-args args),
     :body (rest sig)}))

(defn- signatures [sigs]
  (map signature sigs))

(defn- multi-signature [sig]
  (let [args (vec (sig :default-args))
        body (vec (map (fn [sym] `(classes ~sym))
                       (-> sig :typed-args :syms)))]
    `(~args ~body)))

(defn- multi-signatures [sigs]
  (map (comp multi-signature first)
       (vals (group-by #(count (% :default-args)) sigs))))
  
(defn- method-signature [sig]
  (let [types (vec (-> sig :typed-args :types))
        syms  (vec (sig :default-args))
        body  (sig :body)]
    `(~types ~syms ~@body)))

(defn- method-signatures [sigs]
  (map method-signature sigs))

(defmacro defstrict* [name & sigs]
  (let [sigs (signatures sigs)
        multi-sigs (multi-signatures sigs)
        method-sigs (method-signatures sigs)
        imitate (filter #(not (resolve %))
                        '[Nil Function List Vector HashMap HashSet Symbol Sequence])]
    `(do
       ~@(map (fn [s] `(def ~(with-meta s {:private true}) '~s)) imitate)
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

