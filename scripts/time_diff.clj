#!/usr/bin/env bb

(defn parse-time [time]
  (zipmap [:hour :minute :second :millisecond]
          (->> (str/split time #":")
               (map #(str/split % #"\."))
               (flatten)
               (map #(parse-long %)))))

(defn time->milliseconds [time]
  (let [{:keys [hour minute second millisecond]} (parse-time time)]
    (+ millisecond
       (* second 1000)
       (* minute 60 1000)
       (* hour 60 60 1000))))

(defn format-milliseconds [time-ms]
  (let [h (-> time-ms
              (quot 1000)
              (quot 60)
              (quot 60))
        m (-> (- time-ms (* h 60 60 1000))
              (quot 1000)
              (quot 60))
        s (-> (- time-ms (* h 60 60 1000) (* m 60 1000))
              (quot 1000))
        ms (- time-ms (* h 60 60 1000) (* m 60 1000) (* s 1000))]
    (str h "時間" m "分" s "秒" ms)))

(defn time-diff [t1 t2]
  (let [t1ms (time->milliseconds t1)
        t2ms (time->milliseconds t2)]
    (-> (- t1ms t2ms)
        abs 
        (format-milliseconds))))
 
(let [[t1 t2] *command-line-args*]
  (-> (time-diff t1 t2)
      println)
  )
