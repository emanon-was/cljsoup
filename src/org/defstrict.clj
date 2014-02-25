(ns org.defstrict)

(defn- resolve-class? [sym]
  (class? (resolve sym)))

(defn- typed-args [args]
  (let [s (group-by resolve-class? args)]
    {:types (s true), :syms (s false)}))

(defn- fn-definition [sig]
  {:args (first sig), :body (rest sig)})

(defn- multi-definition [fn-def]
  (let [args (-> fn-def :args typed-args :syms)
        body (vec (map (fn [sym] `(class ~sym)) args))]
    `(~args ~body)))

(defn- method-definition [fn-def]
  (let [args  (-> fn-def :args typed-args)
        types (args :types)
        syms  (args :syms)
        body  (-> fn-def :body)]
    `(~types ~syms ~@body)))

(defmacro defstrict* [name & sigs]
  (let [sigs    (map fn-definition sigs)
        multis  (map multi-definition sigs)
        methods (map method-definition sigs)]
    `(do
       (remove-all-methods ~name)
       (defmulti ~name (fn ~@multis))
       ~@(map (fn [m] `(defmethod ~name ~@m)) methods))))

