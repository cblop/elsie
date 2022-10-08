(ns elsie.core
  (:require
   [overtone.live :as ot]
   [overtone.midi :as midi]))

(def record-sequence
  (atom [[]]))

(def sinks
  (let [sink-devices (midi/midi-sinks)
        mf-3d (midi/midi-find-device sink-devices "M3D")
        mf-twister (midi/midi-find-device sink-devices "Twister")]
    {:mf-3d #p (midi/midi-out mf-3d)
     :mf-twister (midi/midi-out mf-twister)}))

(def mf-out (:mf-3d sinks))

(def bpm 70)

(def time-signature
  (atom [4 4]))

(def pulse (* 1000. (/ bpm 60 (first @time-signature))))

(def metro (ot/metronome bpm))

(def bank (atom 0))

(def colour-order
  [:red
   :orange
   :yellow
   :green
   :cyan
   :blue
   :purple
   :forest
   :pink])

(def colours
  {:red 13
   :dim-red 19
   :orange 25
   :dim-orange 31
   :yellow 37
   :dim-yellow 43
   :green 49
   :dim-green 55
   :forest 61
   :dim-forest 67
   :cyan 73
   :dim-cyan 79
   :blue 85
   :dim-blue 91
   :purple 97
   :dim-purple 103
   :pink 109
   :dim-pink 115
   :white 127
   :black 0})

(def leds
  (let [v1 [:c2 :e2 :g#2 :c3]
        v2 [:c#2 :f2 :a2 :c#3]
        v3 [:d2 :f#2 :a#2 :d3]
        v4 [:d#2 :g2 :b2 :d#3]
        all (concat v1 v2 v3 v4)]
    {:v1 v1
     :v2 v2
     :v3 v3
     :v4 v4
     :all all}))

(def buttons
  {:record (set (range 52 68))})

(defn flash-buttons
  [{:keys [notes colour duration]}]
  (doseq [note notes]
    (midi/midi-note mf-out (ot/note note) (get colours colour) duration 2)))

(defn buttons-on
  [{:keys [notes colour]}]
  (doseq [note notes]
    (midi/midi-note-on mf-out (cond-> note
                                (keyword? note) (ot/note note)) (get colours colour) 2)))

(defn buttons-off
  [notes]
  (doseq [note notes]
    (midi/midi-note-off mf-out (cond-> note
                                 (keyword? note) (ot/note note)) 2)))

(defn light-clock
  [beat]
  (ot/at (metro beat)
         (flash-buttons {:notes (case (mod beat (first @time-signature))
                                  0 (:all leds)
                                  1 (:v2 leds)
                                  2 (:v3 leds)
                                  3 (:v4 leds)
                                  [])
                         :colour :green
                         :duration pulse}))
  (ot/apply-by (metro (inc beat)) #'light-clock (inc beat) []))


(defn change-bank
  [bank-number]
  (let [bank-numbers [:c-1 :c#-1 :d-1 :d#-1]]
    (reset! bank 0)
    (midi/midi-note-on mf-out (ot/note (nth bank-numbers bank-number)) 127 3)))

(midi/midi-note-on mf-out (ot/note :c2) (:green colours) 2)
(midi/midi-note-off mf-out (ot/note :c2) 2)

(defn play
  []
  (change-bank 0)
  (ot/metro-bpm metro bpm)
  (ot/metro-start metro 0)
  (light-clock (metro)))

(defn stop
  []
  (ot/stop))

(def commands
  {:stop {:notes #{25 31 37 43}
          :command stop}
   :play {:notes #{22 28 34 40}
          :command play}})

(defn abs
  "(abs n) is the absolute value of n"
  [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException.
                             "abs requires a number"))
   (neg? n) (- n)
   :else n))

(defn position->button
  [pos]
  (abs (-
        (- 64)
        (mod (+ 64 pos) 4)
        (- (- pos (rem pos 4))))))

(defn button->position
  [note]
  (+
   (- 64 note)
   (mod note 4)
   (rem note 4)))

(button->position 52)

(defn add-record-phrase
  [note]
  (let [current-sequence @record-sequence
        last-count (count (last current-sequence))]
    (if
        (>= last-count 2) (swap! record-sequence conj [note])
        (do (swap! record-sequence (fn [s] (update s (dec (count current-sequence)) #(-> % (conj note) (sort)))))
            (let [new-sequence @record-sequence
                  [start finish] (last new-sequence)]
              (buttons-on {:notes #p (map position->button (range start (inc finish)))
                           :colour #p (nth colour-order (mod (dec (count new-sequence)) (count colour-order)))}))))
    (prn @record-sequence)))


(ot/on-event [:midi :note-on]
             (fn [{:keys [note]}]
               (doseq [{:keys [notes command]} (vals commands)]
                 (when (notes note) (command))))
             ::playback-handler)


(ot/on-event [:midi :note-on]
             (fn [{:keys [note]}]
               (when ((:record buttons) note) (add-record-phrase (button->position note))))
             ::sequence-record-handler)

(ot/on-event [:midi :note-on]
             (fn [{:keys [note]}]
               (when (<= 0 note 3) (reset! bank note)))
             ::bank-change-handler)

(defn init []
  (ot/stop)
  (buttons-off (range 0 128))
  (reset! record-sequence [[]])
  (ot/metro-bpm metro bpm)
  (ot/metro-start metro 0))

;; (init)

;; (ot/event-debug-off)

;; (do
;;   (ot/metro-bpm metro bpm)
;;   (ot/metro-start metro 0)
;;   (midi-clock (metro)))

;; (ot/stop)

;; (ot/metro-bpm metro 120)
;; (ot/metro-bpm metro 70)
