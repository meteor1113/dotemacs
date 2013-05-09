(when (fboundp 'ede-cpp-root-project)
  (ede-cpp-root-project "common/library/cpp"
                        :name "common/library/cpp"
                        :version "0.2"
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
                        ))

(eval-after-load "filesets"
  '(add-to-list 'filesets-data
                (list "common/library/cpp/"
                      (list :tree (file-name-directory
                                   (or load-file-name (buffer-file-name)))
                            "^.+\\.*$"))))
