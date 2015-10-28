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
;  (close! [_self])

  (send! [_ message])
  (self  [_])
)


(defn erlang-mbox* [channel out {:keys [self] :as params}]
  (log/debug (str "got setup message with params: " params))

  (reify
    p/ReadPort
      (take! [_ handler]
        (p/take! out handler))

    IErlangMBox
      (self [_]
        self)

      (send! [_ message]
        (let [encoded (etf/encode message)]
          (go
            (>! channel encoded))))))

(defn erlang-mbox [channel]
  (let [out (async/chan) ready (async/chan)]
    (go-loop []
      (when-let [frame (<! channel)]
        (when-let [message (:message frame)]
          (let [[type body] (etf/decode message)]
            (case type
              :setup   (do
                         (>! ready (erlang-mbox* channel out body))
                         (async/close! ready))
              :message (>! out body))))
          (recur)))
      (log/info "web socket closed")

      (async/close! out)
    ready))


(defn run-mbox [mbox]
  (send! mbox [1 2 3 4 (self mbox)])

  (go-loop []
    (when-let [msg (<! mbox)]
      ;(log/info
        ;(str (bert/decode msg))

      ;)
      (recur))
  )
)


(defn main [& _args]
  (log/info "starting connection")

  (go
    (let [{:keys [ws-channel error]} (<! (chord/ws-ch "ws://localhost:8080/ws" {:format :str}))]
      (if-not error
        (let [mbox (<! (erlang-mbox ws-channel))]
          (run-mbox mbox))
        (js/console.log "Error:" (pr-str error)))))
)
