(ns csi.etf.core
  (:require
    [csi.etf.decode :as decode]
    [csi.etf.encode :as encode]))

(defn decode [bytes]
  (let [dv (js/DataView. bytes)]
    (let [[decoded size] (decode/decode* :term dv)]
      (assert (= size (.-byteLength bytes)) "decode buffer size mismatch") decoded)))

(def encode encode/encode*)
