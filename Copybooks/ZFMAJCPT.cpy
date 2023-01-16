       01 ZF-MAJCPT.
           05 FILLER             PIC X(02).
           05 ZF-COMPTE          PIC X(11).
           05 ZF-CODOPE          PIC X(03).
               88 IS-ADD-OPE     VALUES "PRL" OR
                                        "RMB" OR 
                                        "VIR" OR 
                                        "RET" OR 
                                        "RTD" OR 
                                        "AGI".
               88 IS-SUB-OPE     VALUES "VER" OR
                                        "VRD" OR
                                        "INT" OR
                                        "VVF".
           05 ZF-CODDEV          PIC X(03).
           05 ZF-REFOPE          PIC X(03).
           05 FILLER             PIC X(04).
           05 ZF-MNTOPE          PIC S9(11)V99 COMP-3.
       01 ZF-RETOUR.
            05 ZF-CODRET         PIC X(02).
               88 CODRET-OK      VALUE "00".
            05 ZF-SQLCODE        PIC S9(3) COMP-3.
            05 ZF-LIBRET         PIC X(30).
