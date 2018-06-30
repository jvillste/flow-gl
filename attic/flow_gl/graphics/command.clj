(ns flow-gl.graphics.command)

(defprotocol Command
  (create-runner [command gl]))

(defprotocol CombinableCommand
  (combine [command other-command]))

(defprotocol CommandRunner
  (delete [command-runner gl])
  (run [command-runner gl]))

(defn command-runners-for-commands [commands gl]
  (flatten (map (fn [command-group]
                  (if (satisfies? CombinableCommand (first command-group))
                    (-> (reduce combine command-group)
                        (create-runner gl))
                    (map (fn [command] (create-runner command gl))
                         command-group)))
                (partition-by type commands))))
