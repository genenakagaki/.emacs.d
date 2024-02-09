(ns web-ui.atom)

(defn display [{:keys [size]} child]
  (let [class (case size
                :lg "text-6xl tracking-tight"
                :md "text-5xl"
                :sm "text-4xl")]
    [:div {:class class} child]))

(defn display-lg [child]
  [display {:size :lg} child])

(defn display-md [child]
  [display {:size :md} child])

(defn display-sm [child]
  [display {:size :sm} child])

(defn headline [{:keys [size]} child]
  (let [class (case size
                :lg "text-4xl"
                :md "text-3xl"
                :sm "text-2xl")]
    [:div {:class class} child]))

(defn headline-lg [child]
  [headline {:size :lg} child])

(defn headline-md [child]
  [headline {:size :md} child])

(defn headline-sm [child]
  [headline {:size :sm} child])

(defn title [{:keys [size]} child]
  (let [class (case size
                :lg "text-xl"
                :md "text-lg font-medium"
                :sm "text-base font-medium")]
    [:div {:class class} child]))

(defn title-lg [child]
  [title {:size :lg} child])

(defn title-md [child]
  [title {:size :md} child])

(defn title-sm [child]
  [title {:size :sm} child])

(defn body [{:keys [size]} child]
  (let [class (case size
                :lg "text-base"
                :md "text-sm"
                :sm "text-xs")]
    [:div {:class class} child]))

(defn body-lg [child]
  [body {:size :lg} child])

(defn body-md [child]
  [body {:size :md} child])

(defn body-sm [child]
  [body {:size :sm} child])

(defn label [{:keys [size]} child]
  (let [text-size (case size
                :lg "text-sm"
                :md "text-xs"
                :sm "text-xs")]
    [:div {:class (str text-size " font-medium")} child]))

(defn label-lg [child]
  [label {:size :lg} child])

(defn label-md [child]
  [label {:size :md} child])

(defn label-sm [child]
  [label {:size :sm} child])

