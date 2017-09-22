(ns csi.core
  (:require
    [cljs.core.async :as async]
    [shodan.console :as log]
    [chord.client :as chord]
    [csi.etf.core :as etf]
    [cljs.core.async.impl.protocols :as p])

  (:require-macros
    [cljs.core.async.macros :refer [go alt! go-loop]]))

(defprotocol IErlangMBox
  (close! [_self])
  (send! [_ pid message])
  (call* [_ func params])
  (self  [_]))

(defn erlang-mbox* [socket {:keys [self] :as params}]
  (log/debug (str "creating mbox with params: " params))

  (let [messages (async/chan) replies (async/chan) replies-mult (async/mult replies) correlation (atom 0)]
    (go-loop []
      (when-let [message (:message (<! socket))]
        (let [[type body] (etf/decode message)]
          (case type
            :message (>! messages body)
            :reply (>! replies body)
            (do
              (log/error (str "closing socket: got message of unexpected type " type))
              (async/close! socket)))
          (recur)))
      (log/debug "web socket closed")
      (async/close! messages)
      (async/close! replies))

    (reify
      p/ReadPort
        (take! [_ handler]
          (p/take! messages handler))

      IErlangMBox
        (self [_]
          self)

        (close! [_]
          (async/close! socket))

        (call* [_ func params]
          (go
            (let [replies (async/chan) correlation (swap! correlation inc)
                  module (namespace func) function (name func)]
              (assert function "invalid function")

              (async/tap replies-mult replies)
              (>! socket
                (etf/encode [:call correlation [(keyword (or module :erlang)) (keyword function)] (apply list params)]))

              (loop []
                (when-let [reply (<! replies)]
                  (let [[reply-correlation return] reply]
                    (if (= correlation reply-correlation) ; TODO: timeout handling
                      (do
                        (async/untap replies-mult replies)
                        ; Protection from deadlocks.
                        ; Look at "https://dev.clojure.org/jira/browse/ASYNC-58" for details.
                        (go-loop []
                          (if (some? (<! replies))
                            (recur)))
                        return)
                      (recur))))))))

        (send! [_ pid message]
          (let [encoded (etf/encode [:send pid message])]
            (go
              (>! socket encoded)))))))

(defn mbox [url]
  (go
    (if-let [socket (:ws-channel (<! (chord/ws-ch url {:format :str})))]
      (loop []
        (when-let [message (:message (<! socket))]
          (let [[type body] (etf/decode message)]
            (if (= type :setup)
              (erlang-mbox* socket body)
              (recur))))))))

(defn list-to-string [l]
  (apply str (map #(.fromCharCode js/String %) l)))

(defn string-to-list [s]
  (apply list
    (reduce
      (fn [acc ix]
        (conj acc
          (.charCodeAt s ix))) [] (range (count s)))))
