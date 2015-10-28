(ns csi.core
  (:require
    [cljs.core.async :as async]
    [shodan.console :as log]
    [chord.client :as chord]
    [csi.etf.core :as bert]
    [cognitect.transit :as transit]
    [cljs.core.async.impl.protocols :as p])

  (:require-macros
    [cljs.core.async.macros :refer [go alt! go-loop]]))


(defprotocol IErlangNode
  (close! [self])
  (mbox [self]))

(defprotocol IErlangMBox
;  (close! [_self])
)


(defn erlang-mbox [channel]
  (let [in (async/chan) reader (transit/reader :json)]
    (go-loop []
      (when-let [{:keys [message]} (<! channel)]
        ;(>! in (transit/read reader message))
        (>! in message)
        (recur)
      )
    )

    (reify  ;IErlangMBox
      p/ReadPort
        (take! [_ handler]
          (p/take! in handler))

    )
  )
)


(defn run-mbox [mbox]
  (go-loop []
    (when-let [msg (<! mbox)]
      (log/info
        ;(str msg)
        (str (bert/decode msg))

        )

      (recur))
  )


)


(defn main [& _args]
  (log/info "starting connection")

  (bert/run-tests)

  #_(go
    (let [{:keys [ws-channel error]} (<! (chord/ws-ch "ws://localhost:8080/ws" {:format :str}))]
      (if-not error
        (let [mbox (erlang-mbox ws-channel)]
          (run-mbox mbox))
        (js/console.log "Error:" (pr-str error)))))
)
