       01 ZADEV-ZCMA.
           05 ZADEV-FONCTION         PIC X(03).
           05 ZADEV-DONNEES.
               10 ZADEV-CDEV         PIC X(03).
               10 ZADEV-CPAYS        PIC X(03).
               10 ZADEV-ACHAT        PIC S9(6)V9(3) USAGE COMP-3.
               10 ZADEV-VENTE        PIC S9(6)V9(3) USAGE COMP-3.
           05 ZADEV-RETOUR.
               10 ZADEV-CODRET       PIC X(02).
               10 ZADEV-SQLCODE      PIC S9(3).
               10 ZADEV-LIBRET       PIC X(30).
