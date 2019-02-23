(in-package #:xyz.lonjil.discord/base)


(defconstant +dispatch+ 0) ;receive
(defconstant +heartbeat+ 1) ;both
(defconstant +identify+ 2) ;send
(defconstant +status+ 3) ;send
(defconstant +voice-state+ 4) ;send
(defconstant +voice-ping+ 5) ;send
(defconstant +resume+ 6) ;send
(defconstant +reconnect+ 7) ;receive
(defconstant +request-guild-members+ 8) ;send
(defconstant +invalid-session+ 9) ;receive
(defconstant +hello+ 10) ;receive
(defconstant +beat-ack+ 11) ;receive
