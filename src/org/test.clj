(ns org.test)

(defprotocol Monad
  (monad-context [this m]))
 
(defrecord MonadUnit [a])
(defrecord MonadBind [m k])
 
(extend-protocol Monad
  MonadUnit
    (monad-context [this m] m)
  MonadBind
    (monad-context [this k]
      (let [{:keys [m]} this]
        (monad-context m k))))
 
(defn return [a] (MonadUnit. a))
(defn >>= [m k] (monad-context m (MonadBind. m k)))
(defn >> [m k] (>>= m (fn [_] k)))
 
 
(defrecord Just [a])
(defrecord Nothing [])
 
(defn maybe-context [m]
  (cond
    (instance? Just m) m
    (instance? Nothing m) m
    (instance? MonadUnit m)
      (let [{:keys [a]} m] (Just. a))
    (instance? MonadBind m)
      (let [{:keys [m k]} m
            m (maybe-context m)]
        (cond
          (instance? Nothing m) m
          (instance? Just m)
            (let [{:keys [a]} m]
              (maybe-context (k a)))))))
 
(extend-protocol Monad
  Just
    (monad-context [this m]
      (maybe-context m))
  Nothing
    (monad-context [this m]
      (maybe-context m)))
 
 
(return 42)
; #user.MonadUnit{:a 42}
(maybe-context (return 42))
; #user.Just{:a 42}
(>>= (return 42) return)
; #user.MonadBind{:m #user.MonadUnit{:a 42}, :k #<user$return user$return@47e9d9b1>}
(maybe-context (>>= (return 42) return))
; #user.Just{:a 42}
(>>= (Just. 42) return)
; #user.Just{:a 42}
(>>= (Nothing.) return)
; #user.Nothing{}
(maybe-context (>>= (>>= (return 42) #(return (inc %))) (fn [x] (return (inc x)))))
; #user.Just{:a 44}
(maybe-context (>>= (return 42) (fn [x] (>>= (return (inc x)) (fn [x] (return (inc x)))))))
; #user.Just{:a 44}
(>>= (>>= (Nothing.) (fn [x] (return (inc x)))) (fn [x] (return (inc x))))
; #user.Nothing{}
(>>= (Nothing.) (fn [x] (>>= (return (inc x)) (fn [x] (return (inc x))))))
; #user.Nothing{}
(>>= (>>= (Just. 42) (fn [x] (Nothing.))) (fn [x] (return (inc x))))
; #user.Nothing{}
(>>= (Just. 42) (fn [x] (>>= (Nothing.) (fn [x] (return (inc x))))))
; #user.Nothing{}
