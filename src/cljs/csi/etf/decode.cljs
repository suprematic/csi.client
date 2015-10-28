(ns csi.etf.decode
  (:require
    [shodan.console :as log])
)

(def tags {
  70  :new-float
  97  :small-integer
  98  :integer
  100 :atom
  103 :pid
  104 :small-tuple
  105 :large-tuple
  106 :nil
  107 :string
  108 :list
  109 :binary
  115 :small-atom
  116 :map
  131 :term})

(def unsupported {
  77  :unsupported ; :bit-binary
  99  :unsupported ; :float
  101 :unsupported ; :reference
  102 :unsupported ; :port
  117 :unsupported ; :fun
  118 :unsupported ; :atom-utf
  119 :unsupported ; :small-atom-utf
  110 :unsupported ; :small_big
  111 :unsupported ; :large_big
  112 :unsupported ; :new-fun
  113 :unsupported ; :export
  114 :unsupported ; :new-reference
})

(defrecord Pid [node id serial creation])

(defn skip [dv offset]
  (js/DataView. (.-buffer dv) (+ offset (.-byteOffset dv))))

(defn uint32 [data]
  (.getUint32 data 0))

(defn int32 [data]
  (.getInt32 data 0))

(defn uint16 [data]
  (.getUint16 data 0))

(defn uint8 [data]
  (.getUint8 data 0))

(defmulti decode*
  (fn [tag _] tag))

(defmethod decode* :term [_ data]
  (let [type (uint8 data) tag (tags type)]
    (when-not tag
      (throw (str "unknown term type: " type)))

    (let [[decoded size] (decode* tag (skip data 1))]
      [decoded (+ 1 size)])))

;TODO check signed/unsigned processing
(defmethod decode* :small-integer [_ data]
  [(uint8 data) 1])

(defmethod decode* :integer [_ data]
  [(int32 data) 4])

(defmethod decode* :new-float [_ data]
  [(.getFloat64 data 0 false) 8])

(defmethod decode* :nil [_ data]
  ['() 0])

(defmethod decode* :pid [_ data]
  (let [[node size] (decode* :term data) data (skip data size)
        id (uint32 data) data (skip data 4)
        serial (uint32 data) data (skip data 4)
        creation (uint8 data)]
    [(Pid. node id serial creation) (+ size 9)]))

(defn decode-bytes [dv & {:keys [size-fn size-bytes]}]
  (let [size (size-fn dv)]
    [(js/Uint8Array. (.-buffer dv) (+ size-bytes (.-byteOffset dv)) size) (+ size-bytes size)]))

(defn bytes->list [dv]
  (map #(aget dv %) (range (.-byteLength dv))))

(defn list->string [codes]
  (apply str
    (map
      #(.fromCharCode js/String %) codes)))

(defn convert-value [[value size] & functions]
  (let [update (apply comp (reverse functions))]
    [(update value) size]))

(defmethod decode* :atom [_ data]
  (convert-value
    (decode-bytes data :size-fn uint16 :size-bytes 2) bytes->list list->string keyword))

(defmethod decode* :small-atom [_ data]
  (convert-value
    (decode-bytes data :size-fn uint8 :size-bytes 1) bytes->list list->string keyword))

(defmethod decode* :string [_ data]
  (convert-value (decode-bytes data :size-fn uint16 :size-bytes 2) bytes->list))

(defmethod decode* :binary [_ data]
  (decode-bytes data :size-fn uint32 :size-bytes 4))

(defn- decode-counted [data & {:keys [count-fn count-size tail-nil]}]
  (let [count (count-fn data)
        decoded
          (first
            (reduce
              (fn [[items data] item]
                (let [[decoded size] (decode* :term data)]
                  [(conj items [decoded size]) (skip data size)])
              ) [[] (skip data count-size)] (range count)))]
    [ (map first decoded)
      (+ count-size (reduce + (map second decoded)) (if tail-nil 1 0))]))

(defmethod decode* :list [_ data]
  (convert-value
    (decode-counted data :count-fn uint32 :count-size 4 :tail-nil true) #(apply list %)))

(defmethod decode* :small-tuple [_ data]
  (convert-value
    (decode-counted data :count-fn uint8 :count-size 1) #(apply vector %)))

(defmethod decode* :large-tuple [_ data]
  (convert-value
    (decode-counted data :count-fn uint32 :count-size 4) #(apply vector %)))

(defmethod decode* :map [_ data]
  (convert-value
    (decode-counted data :count-fn #(* 2 (uint32 %)) :count-size 4) #(apply hash-map %)))
