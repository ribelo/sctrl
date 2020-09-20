(ns sctrl.core
  (:require
   [clojure.tools.cli :as cli]
   [clojure.java.io :as io]
   [taoensso.encore :as e]
   [taoensso.timbre :as timbre]
   [meander.epsilon :as r]
   [lambdaisland.regal :as regal]
   [java-time :as jt]
   [clojure.string :as str]
   [malli.core :as m]
   [clojure.java.shell :refer [sh]])
  (:gen-class))

(def version [0 0 1])
(def home-dir (System/getProperty "user.home"))
(def config-dir (e/path home-dir ".config" "sctrl"))
(def default-config-file (e/path config-dir "config.edn"))

(defn parse-etime [s]
  (r/match (str/split s #":|-")
    [?d ?h ?m ?s] (e/ms :days (e/as-int ?d) :hours (e/as-int ?h) :mins (e/as-int ?m) :secs (e/as-int ?s))
    [?h ?m ?s] (e/ms :hours (e/as-int ?h) :mins (e/as-int ?m) :secs (e/as-int ?s))
    [?m ?s] (e/ms :mins (e/as-int ?m) :secs (e/as-int ?s))
    _ (timbre/errorf "bad etime: %s" s)))

(defn transform-process [[user pid etime cmd]]
  [(str/lower-case user) pid (parse-etime etime) (str/lower-case cmd)])

(defn parse-process [s]
  (->> (re-matches
         (regal/regex [:cat
                       :start
                       [:capture [:+ :non-whitespace]] ; user
                       [:+ :whitespace]
                       [:capture [:+ :digit]] ; pid
                       [:+ :whitespace]
                       [:capture
                        [:alt
                         [:cat [:+ :digit] "-" [:+ :digit]":" [:+ :digit] ":" [:+ :digit]]
                         [:cat [:+ :digit] ":" [:+ :digit] ":" [:+ :digit]]
                         [:cat [:+ :digit] ":" [:+ :digit]]]] ; etime
                       [:+ :whitespace]
                       [:capture [:+ :any]] ;cmd
                       :end]) s)
       (drop 1)
       (transform-process)
       (zipmap [:user :pid :etime :cmd])))

(defn list-processes []
  (->> (sh "ps" "axo" "user,pid,etime,cmd")
       :out
       (str/split-lines)
       (drop 1)
       (mapv parse-process)))

(defn filter-processes-by-regex [s]
  (->> (list-processes)
       (filter (fn [{:keys [cmd]}] (re-find (re-pattern s) cmd)))))

(defn kill-process-by-pid! [pid]
  (sh "kill" pid))

(defn kill-processes-by-regex! [s]
  (doseq [{:keys [pid]} (filter-processes-by-regex s)]
    (do
      (kill-process-by-pid! pid)
      (timbre/infof "kill process %s pid: %s" s pid))))

(defn ->hours&minutes [s]
  (->> (str/split s #":")
       (mapv e/as-int)))

(defn match-weekday [day]
  (r/match     day
    :monday    #{1}
    :tuesday   #{2}
    :wednesday #{3}
    :thursday  #{4}
    :friday    #{5}
    :saturday  #{6}
    :sunday    #{7}
    :all       #{1 2 3 4 5 6 7}
    :workday   #{1 2 3 4 5}
    :holiday   #{6 7}))

(defn parse-rule [rule]
  (r/match rule
    [(r/pred string? ?name) [(r/cata !rules) ...]]
    [?name (vec (mapcat identity !rules))]

    [(r/pred keyword? !days) ... (r/pred string? ?begin) (r/pred string? ?end)]
    (->> !days (mapv (fn [day] [(match-weekday day) (->hours&minutes ?begin) (->hours&minutes ?end)])))

    _ :invalid-rule))

(defn parse-config [config]
  (mapv #(parse-rule %) config))

(defn execute-rule! [[cmd rule]]
  (doseq [[days [bh bm] [eh em]] rule]
    (let [now   (jt/local-date-time)
          dow   (jt/as now :day-of-week)
          begin (jt/adjust (jt/local-date-time) (jt/local-time bh bm))
          end   (jt/adjust (jt/local-date-time) (jt/local-time eh em))]
      (when (and (contains? days dow)
                 (or (and (jt/before? end begin)
                          (or (jt/before? now end)
                              (jt/after? now begin)))
                     (and (jt/after? end begin)
                          (or (jt/after? now end)
                              (jt/before? now begin)))))
        (kill-processes-by-regex! cmd)))))

(defn watch! []
  (loop []
    (let [config (parse-config (e/read-edn (slurp default-config-file)))]
      (doseq [rule config]
        (execute-rule! rule)))
    (Thread/sleep 1000)
    (recur)))

(def cli-opts
  [["-v" "--version" nil]
   [nil "--init" nil]
   ["-k" "--kill-all-by-name RGX" nil]
   ["-c" "--config-file PATH" nil]])

(defn -main [& args]
  (let [opts        (cli/parse-opts args cli-opts)
        config-file (or (get-in opts [:options :config-file]) default-config-file)
        config      (e/catching (e/read-edn (slurp config-file)))]
    (r/match [opts config]

      [{:options {:init true}} (r/not (r/some))]
      (if-not (.exists (io/as-file default-config-file))
        (do
          (io/make-parents default-config-file)
          (spit default-config-file (e/pr-edn {})))
        (timbre/errorf "file %s exists!" config-file))

      [{:options {:kill-all-by-name (r/some ?rgx)}} _]
      (kill-processes-by-regex! (str/lower-case ?rgx))

      [{:options (r/pred empty?)} (r/some)]
      (do
        (timbre/info "start watching!")
        (watch!))

      [_ (r/not (r/some))]
      (timbre/infof "config file %s not exists!" config-file))))
