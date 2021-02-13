; *neode.onsave* setgo CLASSPATH=./src:$HOME/.m2/repository/http-kit/http-kit/2.2.0/http-kit-2.2.0.jar:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/rt.jar:$HOME/.m2/repository/ysera/ysera/1.2.0/ysera-1.2.0.jar clojure autodoc.clj indexhtml

(ns autodoc
    (:require [org.httpkit.server :refer [run-server]]
              [clojure.pprint :refer [pprint]]
              [clojure.string :as cljstr :refer [join split trim]]
              [firestone.api]
              [firestone.construct]
              [firestone.core]
              [firestone.damage-entity]
              [firestone.definitions]
              [firestone.definitions-loader]
              [firestone.events]
              [firestone.info]
              [firestone.mana]
              [firestone.spec]))


(defn autodoc-usage []
    (do
    (println "Usage: lein doc [TARGET]                  (executing through Leiningen/lein-exec)")
    (println "       clojure autodoc.clj [TARGET]       (plain execution)")
    (println "  ")
    (println "  Where TARGET is one of: bare, index, html, server")
    (println "  ")
    (println "  Available targets:")
    (println "    bare         print a bare function index")
    (println "    index        print a function index including docstrings")
    (println "    html         generate a dynamic html document containing a searchable function index")
    (println "    ")
    (println "    server       serve the dynamic html document on http://localhost:9999")
    (println "                   (reload the page to pull in new functions from the code!)")
    (println "  ")
    (println "  Examples:")
    (println "    lein doc bare | grep /get-")
    (println "    lein doc index > index.txt")
    (println "    lein doc html > index.html && chrome index.html")
    (println "    (lein doc server >/dev/null &); sleep 3s; firefox \"http://localhost:9999\"")))


(defn ns-publics-by-string
    [nsstr]
    (sort (keys (ns-publics (symbol nsstr)))))


(defn meta-by-string
    [funstr]
    (meta (resolve (symbol funstr))))


(defn function-header-by-string
    [fnstr]
    (let [metadata (meta-by-string fnstr)
          argstr (str (:arglists metadata))]
         (if (> (count argstr) 0)
            (str fnstr (subs argstr 1 (- (count argstr) 1)))
            fnstr)))


(defn function-docstring-by-string
    [fnstr]
    (let [docstr (:doc (meta-by-string fnstr))]
        (if (= nil docstr)
            ""
            docstr)))


(defn single-line-docstring
    [fnstr]
    (cljstr/replace (trim (function-docstring-by-string fnstr)) #"\n\s*" " "))


(defn autodoc-index-ns
    ([nsstr withdocstr]
    (do
    (println (str nsstr ":"))
    (println (str "  "
                  (join "\n  " (map (fn [fnstr] 
                                        (if (= true withdocstr)
                                            (str "; " (single-line-docstring (str nsstr "/" fnstr))
                                                 "\n  "
                                                 (function-header-by-string (str nsstr "/" fnstr))
                                                 "\n")
                                            (function-header-by-string (str nsstr "/" fnstr))))
                                        (ns-publics-by-string nsstr)))))
        (println)))
    
    ([nsstr] (autodoc-index-ns nsstr false)))


(defn autodoc-index
    ([withdocstr]
    (do
    (autodoc-index-ns "firestone.api" withdocstr)
    (autodoc-index-ns "firestone.construct" withdocstr)
    (autodoc-index-ns "firestone.core" withdocstr)
    (autodoc-index-ns "firestone.damage-entity" withdocstr)
    (autodoc-index-ns "firestone.definitions" withdocstr)
    (autodoc-index-ns "firestone.definitions-loader" withdocstr)
    (autodoc-index-ns "firestone.events" withdocstr)
    (autodoc-index-ns "firestone.info" withdocstr)
    (autodoc-index-ns "firestone.mana" withdocstr)
    (autodoc-index-ns "firestone.spec" withdocstr)))
    
    ([] (autodoc-index false)))


(defn autodoc-indexhtml
    []
    (do
    (println "<!DOCTYPE html>")
    (println "<html>")
    (println "    <head>")
    (println "        <style type=\"text/css\">")
    (println "            input { margin:4px; padding:4px; font-size:12pt; }")
    (println "            p { margin:0px; padding:0px; }")
    (println "            div.ns { background:#aaf; padding:2px; margin-bottom:8px; }")
    (println "            div.ns .nsname { padding:8px; background:#eef; border:1px solid #bbf; }")
    (println "            div.fn { margin:0px; padding:8px; background:#fefeff; margin-bottom:2px; margin-left:8px; }")
    (println "            div.fn .fnname { font-weight:bold; }")
    (println "            div.fn .fnargs { font-weight:bold; color:#779; }")
    (println "            div.fn .fndoc { padding:8px; }")
    (println "        </style>")
    (println "        <script src=\"https://cdnjs.cloudflare.com/ajax/libs/jquery/3.3.1/jquery.min.js\"></script>")
    (println "        <script>")
    (println (str "            docsrc = \"" (cljstr/replace (cljstr/replace (cljstr/replace (with-out-str (autodoc-index true)) "\"" "\\\"") "\r\n" "\n") "\n" "\\n") "\";"))
    (println      "            docdb = {current_ns: \"\", current_docstr: \"\", ns: {}};")
    (println      "            ")
    (println      "            for (let line of docsrc.split(\"\\n\"))")
    (println      "            {")
    (println      "                trimmed_line = line.trim();")
    (println      "                ")
    (println      "                if (trimmed_line == \"\")")
    (println      "                    continue;")
    (println      "                ")
    (println      "                if (line.startsWith(\"  \"))")
    (println      "                {")
    (println      "                    if (line.startsWith(\"  ;\"))")
    (println      "                    {")
    (println      "                        //console.log('docstring: ' + line);")
    (println      "                        docdb.current_docstr = trimmed_line.substring(1).trim();")
    (println      "                    }")
    (println      "                    else")
    (println      "                    {")
    (println      "                        //console.log('header: ' + line);")
    (println      "                        var fnname = trimmed_line.replace(/(^[^\\/]+\\/|\\[.+)/gi, \"\");")
    (println      "                        var fnargs = trimmed_line.replace(/(^[^\\[]+)/gi, \"\");")
    (println      "                        docdb.ns[docdb.current_ns].push({docstr:docdb.current_docstr, header:trimmed_line, name:fnname, args:fnargs});")
    (println      "                    }")
    (println      "                }")
    (println      "                else")
    (println      "                {")
    (println      "                    //console.log('ns: ' + line);")
    (println      "                    docdb.current_ns = trimmed_line.substring(0, trimmed_line.length - 1);")
    (println      "                    docdb.ns[docdb.current_ns] = [];")
    (println      "                }")
    (println      "            }")
    (println      "            ")
    (println      "            $(document).ready(function()")
    (println      "            {")
    (println      "                var output = [];")
    (println      "                ")
    (println      "                for (nskey in docdb.ns)")
    (println      "                {")
    (println      "                    output.push(\"<div class=\\\"ns\\\">\");")
    (println      "                    output.push(\"<p class=\\\"nsname\\\">\" + nskey + \"</p>\");")
    (println      "                    ")
    (println      "                    for (let fn of docdb.ns[nskey])")
    (println      "                    {")
    (println      "                        output.push(\"<div class=\\\"fn\\\">\");")
    (println      "                        output.push(\"<p class=\\\"fnhead\\\">\");")
    (println      "                        output.push(\"<span class=\\\"fnname\\\">\" + fn.name + \"</span>\");")
    (println      "                        output.push(\"<span class=\\\"fnargs\\\">\" + fn.args + \"</span>\");")
    (println      "                        output.push(\"</p>\");")
    (println      "                        output.push(\"<p class=\\\"fndoc\\\">\" + fn.docstr + \"</p>\");")
    (println      "                        output.push(\"</div>\");")
    (println      "                    }")
    (println      "                    ")
    (println      "                    output.push(\"</div>\");")
    (println      "                }")
    (println      "                ")
    (println      "                $(\"#index\").html(output.join(\"\\n\"));")
    (println      "                ")
    (println      "                $(\"#search input\").keyup(function()")
    (println      "                {")
    (println      "                    var query = $(\"#search input\").val().split(\" \");")
    (println      "                    ")
    (println      "                    $(\".fn\").each(function(index, element)")
    (println      "                    {")
    (println      "                        var should_show = true;")
    (println      "                        ")
    (println      "                        for (let word of query)")
    (println      "                        {")
    (println      "                            if (!($(element).find(\".fnname\").text().includes(word) || $(element).find(\".fnargs\").text().includes(word)))")
    (println      "                            {")
    (println      "                                should_show = false;")
    (println      "                            }")
    (println      "                        }")
    (println      "                        ")
    (println      "                        if (should_show)")
    (println      "                        {")
    (println      "                            $(element).show();")
    (println      "                        }")
    (println      "                        else")
    (println      "                        {")
    (println      "                            $(element).hide();")
    (println      "                        }")
    (println      "                    });")
    (println      "                });")
    (println      "            });")
    (println "        </script>")
    (println "    </head>")
    (println "    <body>")
    (println "        <div id=\"search\">")
    (println "            <form>")
    (println "                <input style=\"width:100%\" type=\"text\" />")
    (println "            </form>")
    (println "        </div>")
    (println "        <div id=\"index\">")
    (println "            Hello, World!")
    (println "        </div>")
    (println "    </body>")
    (println "</html>")))


; server code stolen from src/firestone/server.clj
(defonce server (atom nil))

(defn handle-server-request!
  [request]
  (do
    (remove-ns 'firestone.definition.card)
    (remove-ns 'firestone.definition.hero)
    (remove-ns 'firestone.api)
    (remove-ns 'firestone.construct)
    (remove-ns 'firestone.core)
    (remove-ns 'firestone.damage-entity)
    (remove-ns 'firestone.definitions)
    (remove-ns 'firestone.definitions-loader)
    (remove-ns 'firestone.events)
    (remove-ns 'firestone.info)
    (remove-ns 'firestone.mana)
    (remove-ns 'firestone.spec)
    (use 'autodoc :reload-all)
    {:status 200,
     :headers {"Content-Type" "text/html; charset=utf-8"},
     :body (with-out-str (autodoc-indexhtml))}))

(defn stop-server!
  []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn start-server!
  []
  (reset! server
          (run-server (fn [request] (handle-server-request! request)) {:port 9999})))


; fix for lein-exec passing filename in command-line-args
(def myargs (if (= "autodoc.clj" (first *command-line-args*))
                (drop 1 *command-line-args*)
                *command-line-args*))

(cond
    (= (first myargs) "bare") (autodoc-index)
    (= (first myargs) "index") (autodoc-index true)
    (= (first myargs) "html") (autodoc-indexhtml)
    (= (first myargs) "server") (start-server!)
      :else (autodoc-usage))
