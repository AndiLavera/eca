(ns eca.features.tools.shell-parser-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [eca.features.tools.shell-parser :as parser]))

(deftest parse-simple-command
  (when (parser/available?)
    (testing "simple single command"
      (is (= {:commands [{:command "echo" :args ["hello"]}]}
             (parser/parse "echo hello"))))

    (testing "chained commands with &&"
      (is (= {:commands [{:command "cd" :args ["/tmp"]}
                         {:command "ls" :args ["-la" "*.clj"]}
                         {:command "grep" :args ["-r" "foo" "."]}]}
             (parser/parse "cd /tmp && ls -la *.clj && grep -r \"foo\" ."))))

    (testing "piped commands"
      (let [result (parser/parse "cat file.txt | grep foo")]
        (is (= [{:command "cat" :args ["file.txt"]}
                {:command "grep" :args ["foo"]}]
               (:commands result)))))

    (testing "empty string returns nil"
      (is (nil? (parser/parse "")))

    (testing "nil returns nil"
      (is (nil? (parser/parse nil)))))))
