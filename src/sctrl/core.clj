(ns sctrl.core
  (:require
   [clojure.tools.cli :as cli]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [taoensso.encore :as e]
   [meander.epsilon :as r]
   [clojure.string :as str]
   [sctrl.db]
   [sctrl.ps]
   [sctrl.sway])
  (:gen-class))

(set! *warn-on-reflection* true)

(def version [0 0 1])
(def home-dir ^String (System/getProperty "user.home"))
(def config-dir ^String (e/path home-dir ".config" "sctrl"))
(def default-config-file ^String (e/path config-dir "config.edn"))


(def cli-opts
  [["-v" "--version"              "print version number"           ]
   ["-i" "--init"                 "init config file"               ]
   ["-k" "--kill-all-by-name RGX" "kill all procesess matching RGX"]
   ["-c" "--config-file PATH"     "start with config file"         ]
   ["-h" "--help"                 "show this help"                 ]])

(def usage
  (->> cli-opts
       (mapv (fn [[s l d]]
               (str s "    " l \newline "        " d)))
       (str/join (str \newline \newline))))


(defn -main [& args]
  (let [opts        (cli/parse-opts args cli-opts)
        config-file (or (get-in opts [:options :config-file]) default-config-file)
        config      (e/catching (edn/read-string (slurp config-file)))]
    (r/match [opts config]
      [{:options {:help true}} _]    (println usage)
      [{:options {:version true}} _] (println (str/join "." version))

      [{:options {:init true}} (r/not (r/some))]
      (if-not (.exists (io/as-file default-config-file))
        (do
          (io/make-parents default-config-file)
          (spit default-config-file (str {})))
        (println (format "file %s exists!" config-file)))

      [{:options {:kill-all-by-name (r/some ?rgx)}} _]
      (sctrl.ps/kill-processes-by-regex! (str/lower-case ?rgx))

      [{:options (r/pred empty?)} (r/some)]
      (do
        (println "start watching!")
        (sctrl.db/create-table-if-not-exists!)
        (sctrl.ps/run-process-watcher! config)
        (sctrl.sway/run-sway-watcher!)
        @(promise))

      [_ (r/not (r/some))]
      (println (format "config file %s not exists!" config-file))

      _
      (println (ex-info "error" {:opts opts})))))
