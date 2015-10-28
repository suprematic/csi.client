(ns csi.etf.core
  (:require
    [shodan.console :as log]
    [csi.etf.decode :as decode]
    [csi.etf.encode :as encode]))

(defn decode [bytes]
  (let [dv (js/DataView. bytes)]
    (let [[decoded size] (decode/decode* :term dv)]
      (assert (= size (.-byteLength bytes)) "decode buffer size mismatch") decoded)))


(defn encode [form]
  (let [bytes (encode/encode* form)]
    bytes
  )
)

(defn string->utf8 [string]
  (js/Uint8Array. (.encode (js/TextEncoder. "utf-8") string)))

(defn utf8->string [buffer]
  (.decode (js/TextDecoder. "utf-8") buffer))

(def test-terms [
  [:abcd 1 '(2 3 4) [3 4] [] 6]
  {:a 10 :b [] :c :d}
])

(defn run-tests []
  (doseq [term test-terms]
    (let [encoded (encode term) decoded (decode encoded)]
      (log/info  (str term " -> " decoded)))))
