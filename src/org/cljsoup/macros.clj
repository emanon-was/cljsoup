(ns org.cljsoup.macros
  (:gen-class))

(defn- resolve-class? [sym]
  (if (= 'nil sym)
    true
    (class? (resolve sym))))

(defn- typed-args [args]
  (let [s (group-by resolve-class? args)]
    {:types (s true), :syms (s false)}))

(defn- signature [sig]
  {:args (first sig), :body (rest sig)})

(defn- signatures [sigs]
  (map signature sigs))

(defn- multi-signature [sig]
  (let [args (or (-> sig :args typed-args :syms) [])
        body (vec (map (fn [sym] `(class ~sym)) args))]
    `(~args ~body)))

(defn- multi-signatures [sigs]
  (map (comp multi-signature first)
       (vals (group-by #(count (-> % :args typed-args :syms)) sigs))))
  
(defn- method-signature [sig]
  (let [args  (-> sig :args typed-args)
        types (or (args :types) [])
        syms  (or (args :syms) [])
        body  (-> sig :body)]
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
       (remove-all-methods ~name)
       ~@(map (fn [s] `(defmethod ~name ~@s)) method-sigs))))

(defmacro defstrict [name & sigs]
  (if (vector? (first sigs))
    `(defstrict* ~name ~sigs)
    `(defstrict* ~name ~@sigs)))
