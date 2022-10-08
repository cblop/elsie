(ns elsie.core
  (:require
   [overtone.live :as ot]
   [overtone.midi :as midi]))

(def sinks
  (let [sink-devices (midi/midi-sinks)
        mf-3d (midi/midi-find-device sink-devices "M3D")
        mf-twister (midi/midi-find-device sink-devices "Twister")]
    {:mf-3d #p (midi/midi-out mf-3d)
     :mf-twister (midi/midi-out mf-twister)}))

(def bpm 70)

(def time-signature
  [4 4])

(def pulse (* 1000. (/ bpm 60 (first time-signature))))

(def metro (ot/metronome bpm))

(def colours
  {:red 13})

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

(defn flash-buttons
  [{:keys [notes colour duration]}]
  (let [sink (:mf-3d sinks)]
    (doseq [note notes]
      (midi/midi-note sink (ot/note note) colour duration 2))))

(defn midi-clock
  [beat]
  (ot/at (metro beat)
         (flash-buttons {:notes (case (mod beat 4)
                                  0 (:all leds)
                                  1 (:v2 leds)
                                  2 (:v3 leds)
                                  3 (:v4 leds)
                                  [])
                         :colour (:red colours)
                         :duration pulse}))
  (ot/apply-by (metro (inc beat)) #'midi-clock (inc beat) []))

(do
  (ot/metro-bpm metro bpm)
  (ot/metro-start metro 0)
  (midi-clock (metro)))

(ot/stop)

;; (ot/metro-bpm metro 120)
;; (ot/metro-bpm metro 70)
