{:user {:plugins [[cider/cider-nrepl "0.8.0-SNAPSHOT"]
                  [codox "0.8.10"]]
        :dependencies [[spyscope "0.1.4"]
                       [org.clojure/tools.namespace "0.2.7"]
                       [leiningen #=(leiningen.core.main/leiningen-version)]                       
                       [im.chit/iroh "0.1.11"]
                       [io.aviso/pretty "0.1.12"]
                       [im.chit/vinyasa "0.2.2"]]
   :injections 
   [(require 'spyscope.core)
    (require '[vinyasa.inject :as inject])
    (require 'io.aviso.repl)
    (inject/in ;; the default injected namespace is `.` 

               ;; note that `:refer, :all and :exclude can be used
               [vinyasa.inject :refer [inject [in inject-in]]]  
               [vinyasa.lein :exclude [*project*]]  

               ;; imports all functions in vinyasa.pull
               [vinyasa.pull :all]      

               ;; same as [cemerick.pomegranate 
               ;;           :refer [add-classpath get-classpath resources]]
               [cemerick.pomegranate add-classpath get-classpath resources] 

               ;; inject into clojure.core 
               clojure.core
               [iroh.core .> .? .* .% .%>]
               [clojure.tools.namespace.repl refresh]

               ;; inject into clojure.core with prefix
               clojure.core >
               [clojure.pprint pprint]
               [clojure.java.shell sh]
               [clojure.repl doc]
               [clojure.repl source]
               )]}}

        
