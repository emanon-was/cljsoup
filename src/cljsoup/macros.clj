(ns cljsoup.macros
  (:gen-class))

(defn classes? [sym]
  (cond (= sym 'Nil) true
        (= sym 'Function) true
        (= sym 'List) true
        (= sym 'Vector) true
        (= sym 'Map) true
        (= sym '&) false
        :else (class? (resolve sym))))

(defn classes [val]
  (cond (nil? val) 'Nil
        (fn?  val) 'Function
        (list? val) 'List
        (vector? val) 'Vector
        (map? val) 'Map
        :else (class val)))

(defn- default-args [args]
  (flatten (map #(if (classes? (first %)) (second %) %) args)))

(defn typed-args [args]
  (let [typed-args (filter (comp classes? first) args)]
    {:types (map first typed-args),
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
                        '[Nil Function List Vector Map])]
    `(do
       ~@(map (fn [s] `(def ~s '~s)) imitate)
       (def ~name nil)
       (defmulti ~name (fn ~@multi-sigs))
       ~@(map (fn [s] `(defmethod ~name ~@s)) method-sigs))))

(defmacro defstrict [name & sigs]
  (if (vector? (first sigs))
    `(defstrict* ~name ~sigs)
    `(defstrict* ~name ~@sigs)))
