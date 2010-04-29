(ede-cpp-root-project "library"
                      :file (or load-file-name (buffer-file-name))
                      :include-path '("/"
                                      "/dumpxml"
                                      "/dumpxml/boost"
                                      "/dumpxml/tinyxml"
                                      "/encrypt"
                                      "/libsock/include"
                                      "/libthread/include"
                                      "/libutil/include"
                                      "/loki"
                                      "/mail"
                                      "/win"
                                      )
                      ;; :system-include-path '("/user/include")
                      ;; :spp-table '(("isUnix" . "")
                      ;;              ("BOOST_TEST_DYN_LINK" . ""))
                      )
