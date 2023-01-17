       01 ZAHIS-ZCMA.
           05 ZAHIS-FONCTION         PIC X(03).
           05 ZAHIS-DONNEES.
               10 ZAHIS-COMPTE       PIC X(11).
               10 ZAHIS-REFOPE       PIC X(10).
               10 ZAHIS-CODOPE       PIC X(03).
               10 ZAHIS-LIBOPE       PIC X(20).
               10 ZAHIS-DTOPER       PIC X(10).
               10 ZAHIS-MNTOPE       PIC S9(13)V9(2) USAGE COMP-3.
           05 ZAHIS-RETOUR.
               10 ZAHIS-CODRET       PIC X(02).
               10 ZAHIS-SQLCODE      PIC S9(3).
               10 ZAHIS-LIBRET       PIC X(30).
