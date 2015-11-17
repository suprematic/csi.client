(ns csi.etf.encode
  (:require [csi.etf.decode :as decode]
    [clojure.set :as set]))

(def tags
  (set/map-invert decode/tags))

(defn make-stream []
  (let [buffer (js/ArrayBuffer. 1024)]
    { :buffer buffer
      :dv (js/DataView. buffer)
      :pos 0}))

(defn get-buffer [{:keys [buffer pos]}]
  (.slice buffer 0 pos))

(defn seek-relative [stream offset]
  (update-in stream [:pos] + offset))

(defn write-uint8 [{:keys [dv pos] :as stream} b]
  (.setUint8 dv pos b)
  (seek-relative stream 1))

(defn write-uint16 [{:keys [dv pos] :as stream} b]
  (.setUint16 dv pos b)
  (seek-relative stream 2))

(defn write-uint32 [{:keys [dv pos] :as stream} value]
  (.setUint32 dv pos value)
  (seek-relative stream 4))

(defn write-int32 [{:keys [dv pos] :as stream} value]
  (.setInt32 dv pos value)
  (seek-relative stream 4))

(defn write-float64 [{:keys [dv pos] :as stream} value]
  (.setFloat64 dv pos value)
  (seek-relative stream 8))

;encoders

(defn encode-small-integer [stream si]
  (-> stream
    (write-uint8 (tags :small-integer))
    (write-uint8 si)))

(defn encode-integer [stream value]
  (-> stream
    (write-uint8 (tags :integer))
    (write-int32 value)))


;;; large-big and small-big
(defn- data->bytes [data]
  (if (= 0 data)
    nil
    (lazy-seq (cons (bit-and 255 data) (data->bytes (/ data 256) #_(bit-shift-right (+ 128 data) 8))))))

(defn- extract-bytes [data length]
  (reverse (take length (concat (data->bytes data) (repeat 0)))))

(defn- taged-seq [tag & data]
  (map byte (concat [(tags tag)] (apply concat data))))

(defn encode-big-integer [stream bi]
  (js/console.log "encode-big-integer" bi)
  (let [sign  (if (< bi 0) 1 0)
        bytes (data->bytes (js/Math.abs bi))
        size  (count bytes)]
    (reduce #(write-uint8 %1 %2) stream (if (> size 255)
                                          (taged-seq :large-big (extract-bytes size 4) (extract-bytes sign 1) bytes)
                                          (taged-seq :small-big (extract-bytes size 1) (extract-bytes sign 1) bytes)))))

;;;;;;;;;;;;;;;;;

(defn encode-float [stream value]
  (-> stream
    (write-uint8 (tags :new-float))
    (write-float64 value)))

(defn encode-nil [stream]
  (write-uint8 stream (tags :nil)))

(declare encode-term)

(defn encode-counted [stream vector & {:keys [tag count-writer-fn]}]
  (reduce
    (fn [stream item]
      (encode-term stream item))

    (-> stream
      (write-uint8 (tags tag))
      (count-writer-fn (count vector)))
    vector))

(defn encode-keyword [stream keyword]
  (let [name (name keyword)]
    (reduce
      (fn [stream pos]
        (write-uint8 stream (.charCodeAt name pos)))

      (-> stream
        (write-uint8 (tags :atom))
        (write-uint16 (count name)))

      (range (count name)))))

(defn encode-array-buffer [stream buffer]
  (let [length (.-byteLength buffer)]
    (reduce
      (fn [stream pos]
        (write-uint8 stream (aget buffer pos)))

      (-> stream
        (write-uint8 (tags :binary))
        (write-uint32 length))

      (range length))))

(defn encode-pid [stream {:keys [node id serial creation]}]
  (-> stream
    (write-uint8 (tags :pid))
    (encode-term node)
    (write-uint32 id)
    (write-uint32 serial)
    (write-uint8 creation)))

(defn encode-term [stream term]
  (cond

    (and (integer? term) (>= term 0) (< term 255))
      (encode-small-integer stream term)

    (and (integer? term)  (>= term  -2147483648) (< term 2147483648))
      (encode-integer stream term)

    ;; TODO: please check this
    (.isSafeInteger js/Number term)
      (encode-big-integer stream term)

    (number? term)
      (encode-float stream term)

    (keyword? term)
      (encode-keyword stream term)

    (instance? js/Uint8Array term)
      (encode-array-buffer stream term)

    (instance? decode/Pid term)
      (encode-pid stream term)

    (and (list? term) (empty? term))
      (encode-nil stream)

    (and (vector? term) (< (count term) 255))
      (encode-counted stream term :tag :small-tuple :count-writer-fn write-uint8)

    (and (vector? term))
      (encode-counted stream term :tag :large-tuple :count-writer-fn write-uint32)

    (list? term)
      (write-uint8
        (encode-counted stream term :tag :list :count-writer-fn write-uint32) (tags :nil))

    (map? term)
      (encode-counted stream
        (apply concat (into [] term)) :tag :map
          :count-writer-fn (fn [stream _] (write-uint32 stream (count term))))

    :else
      (throw (str "don't know how to encode: " (str term)))))

(defn encode* [term]
  (-> (make-stream)
    (write-uint8 (tags :term))
    (encode-term term)
    get-buffer))
