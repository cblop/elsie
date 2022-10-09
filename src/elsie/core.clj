(ns elsie.core
  (:require
   [overtone.live :as ot]
   [overtone.midi :as midi]))

(def record-sequence
  (atom [[]]))

(def play-sequence
  (atom [[]]))

(def delete-mode? (atom false))

(def sinks
  (let [sink-devices (midi/midi-sinks)
        mf-3d (midi/midi-find-device sink-devices "M3D")
        mf-twister (midi/midi-find-device sink-devices "Twister")]
    {:mf-3d (midi/midi-out mf-3d)
     :mf-twister (midi/midi-out mf-twister)}))

(def mf-out (:mf-3d sinks))

(def bpm 70)

(def time-signature
  (atom [4 4]))

(def pulse (* 1000. (/ bpm 60 (first @time-signature))))

(def metro (ot/metronome bpm))

(def bank (atom 0))

(def banks [:play :record :sequence :jam])

(def note-banks (atom {:record 0
                       :sequence 0}))

(def colour-order
  [:purple
   :green
   :orange
   :cyan
   :red
   :yellow
   :blue
   :pink
   :forest])

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
  {:record {:sequence (set (range 52 68))
            :inc-bank 30
            :dec-bank 27}
   :sequence {:sequence (set (range 68 84))
              :inc-bank 36
              :dec-bank 33}})

(defn flash-buttons
  [{:keys [notes colour duration]}]
  (doseq [note notes]
    (midi/midi-note mf-out (ot/note note) (get colours colour) duration 2)))

(defn buttons-on
  [{:keys [notes colour]}]
  (doseq [note #p notes]
    (midi/midi-note-on mf-out (cond-> note
                                (keyword? note) (ot/note)) (get colours colour) 2)))

(defn buttons-off
  [notes]
  (doseq [note notes]
    (midi/midi-note-off mf-out (cond-> note
                                 (keyword? note) (ot/note)) 2)))

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

(defn play
  []
  (change-bank 0)
  (ot/metro-bpm metro bpm)
  (ot/metro-start metro 0)
  (buttons-off (range 0 128))
  (light-clock (metro)))

(defn stop
  []
  (ot/stop))

;; TODO remove?
(defn delete-mode
  []
  (swap! delete-mode? not))

(def commands
  {:stop {:notes #{25 31 37 43}
          :command stop}
   :play {:notes #{22 28 34 40}
          :command play}
   :delete-mode {:notes #{20 26 32 38}
                 :command delete-mode}})

(defn abs
  "(abs n) is the absolute value of n"
  [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException.
                             "abs requires a number"))
   (neg? n) (- n)
   :else n))

(defn current-seq
  []
  (nth banks @bank))

(def start-buttons
  {:record 64
   :sequence 80
   :play 48
   :jam 96})

(defn position->button
  [pos]
  (let [start (start-buttons (current-seq))]
    (abs (-
          (- start)
          (mod (+ start pos) 4)
          (- (- pos (rem pos 4)))
          (+ (* (get @note-banks (nth banks @bank)) 16))))))

(defn button->position
  [note]
  (let [start (start-buttons (current-seq))]
    (+
     (- start note)
     (mod note 4)
     (rem note 4)
     ;; TODO make dependend on current bank
     (* (get @note-banks (nth banks @bank)) 16))))


(defn button->sequence
  [note]
  (let [start (start-buttons (current-seq))]
    (+
     (- start note)
     (mod note 4)
     (rem note 4))))

(defn redraw-buttons
  []
  (let [seq-key (nth banks @bank)
        sequence (case seq-key
                   :record record-sequence
                   :sequence play-sequence)
        current-sequence #p @sequence
        start-n (- (get start-buttons seq-key) 12)
        end-n (+ start-n 16)
        shortened-sequence (filter (fn [[s f _]]
                                     (and (< (position->button s) end-n)
                                          (>= (position->button f) start-n)))
                                   current-sequence)]
    (buttons-off (range start-n end-n))
    (doseq [index-seq (map #(-> (apply list %1)
                                (conj %2)) shortened-sequence (range (count shortened-sequence)))]
      (let [i (first index-seq)
            seq-type? (= seq-key :sequence)
            start (nth index-seq 1)
            finish (nth index-seq 2)
            colour (if seq-type? (nth colour-order (mod (last index-seq) (count colour-order))) (nth colour-order (mod i (count colour-order))))]
        (buttons-on {:notes (map position->button (range start (inc finish)))
                     :colour colour})))))

(defn inc-note-bank
  [type]
  (swap! note-banks #(update % type inc))
  (redraw-buttons))

(defn dec-note-bank
  [type]
  (swap! note-banks #(update % type (fn [n]
                                      (cond-> n
                                        (> n 0) (dec)))))
  (redraw-buttons))

(def selected-phrase (atom 0))

;; TODO what about sorting?
(defn append-or-delete-phrase
  [note]
  (let [seq-key (nth banks @bank)
        sequence (case seq-key
                   :record record-sequence
                   :sequence play-sequence)
        current-sequence @sequence
        unsorted-phrase (-> current-sequence (last) (conj note))
        phrase (cond-> unsorted-phrase
                 (not= seq-key :sequence) (sort))]
    ;; TODO sort out play sequence deletion
    (if (some #{phrase} current-sequence)
      (do
        (reset! sequence (vec (butlast (remove #{phrase} current-sequence))))
        (redraw-buttons))
      (swap! sequence (fn [s] (assoc s (dec (count current-sequence)) phrase))))))

(defn add-or-remove-record-phrase
  [note]
  (let [current-sequence @record-sequence
        last-count (count (last current-sequence))]
    (if
        (>= last-count 2) (swap! record-sequence conj [note])
        (do
          (append-or-delete-phrase note)
          (let [new-sequence @record-sequence
                [start finish] (last new-sequence)]
              (buttons-on {:notes (map position->button (range start (inc finish)))
                           :colour (nth colour-order (mod (dec (count new-sequence)) (count colour-order)))}))))
    (println @record-sequence)))

(defn sequence-selection
  []
  (buttons-off (range 68 84))
  (doseq [i (range (count @record-sequence))]
    (buttons-on {:notes #{(position->button (+ i (* (get @note-banks (nth banks @bank)) 16)))}
                 :colour (nth colour-order (mod i (count colour-order)))})))

(defn add-or-remove-play-phrase
  [note]
  (let [current-sequence @play-sequence
        last-count (count (last current-sequence))
        transposed-note (button->position note)]
    (cond
      (>= last-count 3) (swap! play-sequence conj [transposed-note])
        (= last-count 2) (do
                           (append-or-delete-phrase (button->sequence note))
                           (redraw-buttons))
        (= last-count 1) (do
                           (append-or-delete-phrase transposed-note)
                           (sequence-selection))
        :else (append-or-delete-phrase transposed-note))
    (println @play-sequence)))

(defn bank-changed
  [bank]
  (case bank
    ;; 0 (buttons-off (range 52 68))
    1 (buttons-off (range 52 68))
    2 (buttons-off (range 68 84)))
  (redraw-buttons))

(ot/on-event [:midi :note-on]
             (fn [{:keys [note]}]
               (doseq [{:keys [notes command]} (vals commands)]
                 (when (notes note) (command))))
             ::playback-handler)

(ot/on-event [:midi :note-on]
             (fn [{:keys [note]}]
               (cond
                 (= note (get-in buttons [:record :inc-bank])) (inc-note-bank :record)
                 (= note (get-in buttons [:record :dec-bank])) (dec-note-bank :record)))
             ::record-note-bank-handler)

(ot/on-event [:midi :note-on]
             (fn [{:keys [note]}]
               (cond
                 (= note (get-in buttons [:sequence :inc-bank])) (inc-note-bank :sequence)
                 (= note (get-in buttons [:sequence :dec-bank])) (dec-note-bank :sequence)))
             ::sequence-note-bank-handler)

(ot/on-event [:midi :note-on]
             (fn [{:keys [note]}]
               (when ((get-in buttons [:record :sequence]) note)
                 (add-or-remove-record-phrase
                  (button->position note))))
             ::sequence-record-handler)

(ot/on-event [:midi :note-on]
             (fn [{:keys [note]}]
               (when ((get-in buttons [:sequence :sequence]) note)
                 (add-or-remove-play-phrase note)))
             ::sequence-play-handler)

(ot/on-event [:midi :note-on]
             (fn [{:keys [note]}]
               (when (<= 0 note 3)
                 (reset! bank note)
                 (bank-changed note)))
             ::bank-change-handler)

(defn init []
  (ot/stop)
  (buttons-off (range 0 128))
  (change-bank 0)
  (reset! record-sequence [[]])
  (reset! note-banks {:record 0
                      :sequence 0})
  (ot/metro-bpm metro bpm)
  (ot/metro-start metro 0))

(init)

(ot/event-debug-off)
;; (ot/event-debug-off)

;; (do
;;   (ot/metro-bpm metro bpm)
;;   (ot/metro-start metro 0)
;;   (midi-clock (metro)))

;; (ot/stop)

;; (ot/metro-bpm metro 120)
;; (ot/metro-bpm metro 70)
