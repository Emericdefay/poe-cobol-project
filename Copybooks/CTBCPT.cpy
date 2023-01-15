       01 ZACPT-ZCMA.
           05 ZACPT-FONCTION         PIC X(03).
           05 ZACPT-DONNEES.
               10 ZACPT-COMPTE       PIC X(11).
               10 ZACPT-NOM          PIC X(20).
               10 ZACPT-SOLDE        PIC S9(13)V9(2) USAGE COMP-3.
               10 ZACPT-DDMVT        PIC X(10).
               10 ZACPT-DDMAJ        PIC X(10).
               10 ZACPT-HDMAJ        PIC X(08).
           05 ZACPT-RETOUR.
               10 ZACPT-CODRET       PIC X(02).
               10 ZACPT-SQLCODE      PIC S9(3).
               10 ZACPT-LIBRET       PIC X(30).