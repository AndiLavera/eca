(ns eca.features.tools.shell-parser
  "Parses shell command strings into structured breakdowns via shfmt's JSON AST.
   Gracefully returns nil when shfmt is not installed or parsing fails."
  (:require
   [babashka.process :as p]
   [cheshire.core :as json]
   [clojure.string :as string]
   [eca.logger :as logger]))

(set! *warn-on-reflection* true)

(def ^:private logger-tag "[SHELL-PARSER]")

(def ^:private shfmt-timeout-ms 5000)

;; Lazily checked once per JVM session
(let [available* (delay
                   (try
                     (let [result @(p/process {:cmd ["shfmt" "--version"]
                                               :out :string
                                               :err :string})]
                       (zero? (:exit result)))
                     (catch Exception _
                       false)))]
  (defn available?
    "Returns true if shfmt is installed and available on PATH."
    []
    @available*))

;; --- AST walking ---

(defn- resolve-part
  "Extract the string value from a single Part node in the shfmt AST.
   Returns the literal text or a placeholder for complex expressions."
  ^String [part]
  (case (:Type part)
    "Lit"        (:Value part)
    "SglQuoted"  (:Value part)
    "DblQuoted"  (->> (:Parts part)
                       (map resolve-part)
                       (string/join))
    "CmdSubst"   (:Lit part "$(...)")        ; edge-case: backtick literals
    "ParamExp"   (str "${" (:Param part "") "}")
    "ProcSubst"  (:Lit part "<(...)")
    "ArithmExp"  (:Lit part "$((...))")
    ;; Fallback: return an empty string for unknown types
    ""))

(defn- resolve-arg
  "Extract the full string value of an Arg node (array of Parts)."
  ^String [arg]
  (->> (:Parts arg)
       (map resolve-part)
       (string/join)))

(defn- extract-from-cmd
  "Recursively walk a Cmd node, returning a flat vector of {:command :args} maps
   for every CallExpr found. Handles BinaryCmd (&&, ||, ;, |), Subshell, IfClause,
   and other node types."
  [cmd]
  (let [type (:Type cmd)]
    (case type
      "CallExpr"
      (let [args (:Args cmd)]
        (when (seq args)
          [{:command (resolve-arg (first args))
            :args (mapv resolve-arg (rest args))}]))

      "BinaryCmd"
      (into (extract-from-cmd (:Cmd (:X cmd)))
            (extract-from-cmd (:Cmd (:Y cmd))))

      "Subshell"
      (mapcat #(extract-from-cmd (:Cmd %))
              (:Stmts cmd))

      "Block"
      (mapcat #(extract-from-cmd (:Cmd %))
              (:Stmts cmd))

      "IfClause"
      (concat (mapcat #(extract-from-cmd (:Cmd %))
                      (:Cond cmd))
              (mapcat #(extract-from-cmd (:Cmd %))
                      (:Then cmd))
              (mapcat #(extract-from-cmd (:Cmd %))
                      (:Else cmd)))

      "WhileClause"
      (mapcat #(extract-from-cmd (:Cmd %))
              (:Do cmd))

      "ForClause"
      (mapcat #(extract-from-cmd (:Cmd %))
              (:Do cmd))

      "CaseClause"
      (mapcat (fn [item]
                (mapcat #(extract-from-cmd (:Cmd %))
                        (:Stmts item)))
              (:Items cmd))

      "DeclClause"
      (extract-from-cmd (:Cmd cmd))

      "TimeClause"
      (when-let [inner (:Stmt cmd)]
        (extract-from-cmd (:Cmd inner)))

      "CoprocClause"
      (when-let [inner (:Stmt cmd)]
        (extract-from-cmd (:Cmd inner)))

      ;; For unknown/leaf nodes, return nothing
      nil)))

;; --- Public API ---

(defn parse
  "Parse a shell command string into a structured breakdown.
   Returns nil when shfmt is not available or parsing fails.
   On success returns:
     {:commands [{:command \"ls\" :args [\"-la\" \"*.clj\"]}
                 {:command \"grep\" :args [\"-r\" \"foo\" \".\"]}]}"
  [^String command]
  (when (and (available?) command (not (string/blank? command)))
    (try
      (let [proc (p/process {:cmd ["shfmt" "-tojson"]
                             :in command
                             :out :bytes
                             :err :string})
            result (deref proc ^long shfmt-timeout-ms ::timeout)]
        (when (and (not= result ::timeout)
                   (zero? (:exit result)))
          (let [ast (json/parse-string (String. ^bytes (:out result)) true)
                commands (->> (:Stmts ast)
                              (mapcat #(extract-from-cmd (:Cmd %)))
                              (vec))]
            (when (seq commands)
              {:commands commands}))))
      (catch Exception e
        (logger/debug logger-tag "shfmt parsing failed, falling back to raw command display"
                      {:message (.getMessage e)})
        nil))))
