(ns org.cljsoup.macros
  (:gen-class))

(defn classes? [sym]
  (cond (= sym 'Nil) true
        (= sym 'Function) true
        ;; (= sym 'List) true
        ;; (= sym 'Vector) true
        ;; (= sym 'Map) true
        (= sym '&) false
        :else (class? (resolve sym))))

(defn classes [val]
  (cond (nil? val) 'Nil
        (fn?  val) 'Function
        ;; (list? val) 'List
        ;; (vector? val) 'Vector
        ;; (map? val) 'Map
        :else (class val)))

(defn- typed-args [args]
  {:types (filter classes? (map first args)),
   :syms  (map #(if (= '& (first %))
                  second)
               args)})

(defn- signature [sig]
  {:args (partition 2 (first sig)),
   :body (rest sig)})

(defn- signatures [sigs]
  (map signature sigs))

(defn- multi-signature [sig]
  (let [args (or (-> sig :args typed-args :syms) [])
        body (vec (map (comp (fn [sym] `(classes ~sym)) second)
                       (filter (comp classes? first)
                               (partition 2 (sig :args)))))]
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
        method-sigs (method-signatures sigs)
        Nil 'Nil Function 'Function
        ;;List 'List Vector 'Vector Map 'Map
        ]
    `(do
       (def ~Nil '~Nil)
       (def ~Function '~Function)
       ;; (def ~List '~List)
       ;; (def ~Vector '~Vector)
       ;; (def ~Map '~Map)
       (def ~name nil)
       (defmulti ~name (fn ~@multi-sigs))
       ~@(map (fn [s] `(defmethod ~name ~@s)) method-sigs))))

(defmacro defstrict [name & sigs]
  (if (vector? (first sigs))
    `(defstrict* ~name ~sigs)
    `(defstrict* ~name ~@sigs)))
