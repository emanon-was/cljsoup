(ns org.cljsoup.inherit
  (:gen-class))

(defn inherit-var [var]
  (eval
   (let [meta-data (meta var)
         name      (meta-data :name)
         macro     (meta-data :macro)
         dynamic   (meta-data :dynamic)]
     `(def ~(with-meta name
              (assoc (meta name)
                :macro   macro
                :dynamic dynamic)) ~var))))

(defn inherit-ns [ns]
  (let [vars (vals (ns-publics ns))]
    (doseq [v vars] (inherit-var v))))
