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
          (>! (case type :message messages :reply replies) body)
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
              (>! socket (etf/encode [:call correlation [(keyword (or module :erlang)) (keyword function)] (apply list params)]))

              (loop []
                (let [[rcorrelatin return] (<! replies)]
                  (if (= correlation rcorrelatin) ; TODO: timeout handling
                    (do
                      (async/untap replies-mult replies) return)
                    (recur)))))))

        (send! [_ pid message]
          (let [encoded (etf/encode [:send pid message])]
            (go
              (>! socket encoded)))))))

(defn erlang-mbox [socket]
  (go-loop []
    (when-let [message (:message (<! socket))]
      (let [[type body] (etf/decode message)]
        (if (= type :setup)
          (erlang-mbox* socket body)
          (recur))))))


; tests and examples
(defn run-mbox [mbox]
  #_(go-loop []
    (<! (async/timeout 1000))

    (send! mbox (self mbox) [1 2 3 4])
    (recur)
  )

  #_(go-loop []
    (<! (async/timeout 1000))
    (let [result (<! (call* mbox :erlang/now []))]
      (log/info (str "call result: " result))

    )
    (recur)
  )

  (go
    (<! (call* mbox :send_after [1000 (self mbox) [:hello :world]]))
    (log/info "set timer")
  )


  (go-loop []
    (when-let [msg (<! mbox)]
      (log/info (str msg))

      (close! mbox)

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
