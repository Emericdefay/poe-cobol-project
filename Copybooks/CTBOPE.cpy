       01 ZAOPE-ZCMA.
           05 ZAOPE-FONCTION         PIC X(03).
           05 ZAOPE-DONNEES.
               10 ZAOPE-COPE         PIC X(03).
               10 ZAOPE-LOPE         PIC X(03).
               10 ZAOPE-MNTMIN       PIC S9(9)V9(2) USAGE COMP-3.
               10 ZAOPE-MNTMAX       PIC S9(9)V9(2) USAGE COMP-3.
           05 ZAOPE-RETOUR.
               10 ZAOPE-CODRET       PIC X(02).
               10 ZAOPE-SQLCODE      PIC S9(3).
               10 ZAOPE-LIBRET       PIC X(30).
