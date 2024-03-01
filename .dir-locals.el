;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eglot-workspace-configuration
          . (:nil
             (:nix
              (:maxMemoryMB nil
                            :flake (:autoArchive nil
                                                 ;; FIXME: errors when trying to
                                                 ;; eval private asahi
                                                 ;; firmware repo
                                                 ;; :autoEvalInputs t
                                                 )))))))
 (nix-ts-mode . ((apheleia-formatter . alejandra))))
