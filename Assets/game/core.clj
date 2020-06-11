(ns game.core
  (:require
   [arcadia.core :refer :all]
   [arcadia.linear :as l]
   [arcadia.introspection :as i])
  (:import
   (UnityEngine.Random)
   [UnityEngine MeshRenderer Time FixedJoint2D SpriteRenderer BoxCollider2D Transform Animator Input Rigidbody2D UI.Text]))

(set! *warn-on-reflection* true)

(def skateboard-height (float -2.1))

(def col-height-range [(float 2.8) (float -2.0)])
(def skateboard-hold-time (float 20))
(def  empty-up-force (l/v2 0 120))
(def  with-skateboard-up-force (l/v2 0 300))

(defn bird []
  (object-named "Bird"))

(defn controller []
  (object-named "GameController"))

(defn ground []
  (object-named "Ground"))

(defn revive []
  (update-state (bird) :flap (fn [s] (assoc s :dead? false))))

(def dead? (atom false))

(declare init)

(defn get-status-cont []
  (get-in (state (controller)) [:init :status-cont]))

(defn get-score-cont []
  (get-in (state (controller)) [:init :score-cont]))

(defn display-score []
  (let [c (cmpt (get-score-cont) Text)]
    (set! (.. c text) (str "Score: " (get-in (state (controller)) [:init :score])))))

(defn score-point []
  (update-state (controller) :init (fn [s] (update s :score inc)))
  (display-score))

(defn restart-scene []
  (SceneManager/LoadScene (.buildIndex (SceneManager/GetActiveScene)))
  (update-state (object-named "Bird") :flap (fn [s] (assoc s :dead? false))))

(def rs restart-scene)

(defn kill-bird [bird k]
  (update-state bird k (fn [s] (assoc s :dead? true)))
  (.SetActive (get-status-cont) true)
  (reset! dead? true)
  (let [anim (:animator (state bird k))]
    (.SetTrigger anim "Die")))

(defn on-bird-update [obj k]
  (let [{:keys [skateboard-drop-time dead? ^Rigidbody2D rigid-body ^Animator animator up-force]} (state obj k)]
    (if-not dead?
      (do

        (if (< (.. obj transform position x) -6)
          (kill-bird obj k)
          (when (Input/GetButtonDown "Jump")
            (set! (.. rigid-body velocity) (l/v2 0 0))
            (.AddForce rigid-body up-force)
            (let [anim (:animator (state obj k))]
              (.SetTrigger animator "Flap"))))

        (when (> (Time/time) skateboard-drop-time)
          (log "Dropping skateboard..........")
          (when-let [fj (cmpt obj FixedJoint2D)]
            (log "Got fixed-joint")
            (let [sb (gobj (.. fj connectedBody))]
              (cmpt- obj FixedJoint2D)
              (retire sb)))

          (update-state obj k (fn [s] (-> s
                                          (assoc  :skateboard-drop-time (float 999999))
                                          (assoc :up-force empty-up-force))))
          (log "Dropped skateboard")))

      ;; If dead then flap restarts
      (when (Input/GetButtonDown "Jump")
        (SceneManager/LoadScene (.buildIndex (SceneManager/GetActiveScene)))
        #_(restart-scene)))))

(defn on-bird-collision-enter-2d [obj k c]
  (kill-bird obj k))

(defn on-bird-trigger-enter-2d [obj k c]

  (let [cobj (gobj c)]
    (log "Triggered!!!!!!!!!!!! with: " cobj)
    (cond

      (= (.. cobj tag) "Coin")
      (let [c (cmpt cobj SpriteRenderer)]
        (score-point)
        (set! (..  c enabled) false))

      (= (.. cobj tag) "Skateboard")
      (let [skateboard cobj]
        (log "Scatebrd&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&")
        (role- skateboard :scroll)
        (log "removed roll")
        (set! (.. (cmpt skateboard BoxCollider2D) isTrigger) false)
        (set! (.. (cmpt skateboard Rigidbody2D) gravityScale) 1)
        (with-cmpt obj [fj FixedJoint2D]
          (log "Got fixed-joint")
          (set! (.. fj connectedBody) (cmpt skateboard Rigidbody2D))
          (log "set fixed-joint"))
        (update-state obj :flap (fn [s] (-> s
                                            (assoc :up-force with-skateboard-up-force)
                                            (assoc :skateboard-drop-time (+ skateboard-hold-time (Time/time))))))

        #_(set! (.. cobj transform position) (l/v3 0 0 0)))

      )))

(defn init-bird []
  (let [bird (object-named "Bird")
        rb (cmpt bird Rigidbody2D)]
    (role+ bird :flap {:state {:dead? false
                               :rigid-body rb
                               :up-force empty-up-force
                               :animator (cmpt bird Animator)
                               :skateboard-drop-time (float 9999999)}
                       :update #'on-bird-update
                       :on-collision-enter2d #'on-bird-collision-enter-2d
                       :on-trigger-enter2d #'on-bird-trigger-enter-2d})
    (set! (.. rb velocity) (l/v2 0 0))
    (set! (.. rb freezeRotation) true)
    (set! (.. bird transform position) (l/v3 0 0 0))))


(defn reposition-columns [obj]
  (let [cols (children (second (children obj)))]
    (doseq [c cols]
      (let [newy (UnityEngine.Random/Range (first col-height-range) (second col-height-range))
            pos (.. c transform position)
            new-pos (l/v3 (.. pos x) newy (.. pos z))]
        (set! (.. c transform position) new-pos))))
  (let [coins (children (second (rest (children obj))))]
    (doseq [c coins]
      (let [newy (UnityEngine.Random/Range (first col-height-range) (second col-height-range))
            pos (.. c transform position)
            new-pos (l/v3 (.. pos x) newy (.. pos z))]
        (set! (.. c transform position) new-pos)
        (set! (.. (cmpt c SpriteRenderer) enabled) true)))))

(defn scroll-update [obj k]
  (when-not @dead?
    (let [t (cmpt obj Transform)]
      (if (< (.. t position x) -20)
        (do
          (.Translate t (float (* 2 20.4)) 0 0)
          (reposition-columns obj))
        (.Translate t (float -0.08) 0 0)
        ))))

(def dynamic-func (atom nil))

(defn scroll-start [obj k]
  (log "Dynamic func: " (@dynamic-func 3))
  (let [t (cmpt obj Transform)]
    (set! (.. t position x) 0)))


(defn add-scroll-role [obj]
  (role+ obj :scroll
         {:state {:rigid-body (cmpt obj Rigidbody2D)}
          :update #'scroll-update
          :start #'scroll-start}))


(defn on-controller-start [obj k]
  (log "On conttroller start called!!!!!")
  (init-bird)
  (reset! dead? false)
  (reset! dynamic-func (eval '(fn [x] (+ x 1))))
  (add-scroll-role (object-named "Ground"))
  (add-scroll-role (object-named "Ground2"))
  (add-scroll-role (object-named "Skateboard"))

  (update-state obj k (fn [s] (merge s {:score-cont (object-named "Score")
                                        :status-cont (object-named "Status")
                                        :score 0})))
  (.SetActive (get-status-cont) false))


(defn init-controller []
  (let [cont (controller)]
    (role+ cont :init {:start #'on-controller-start
                       :state {:score 0
                               :score-cont nil
                               :status-cont nil}})))
