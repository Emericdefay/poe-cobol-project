       01  F1-ENREG-00.
               05 F1-TYPE-00           PIC X(02).
               05 F1-ORIGINE           PIC X(03).
               05 F1-DATE              PIC X(10).
               05 FILLER               PIC X(65).
       01  F1-ENREG-10 REDEFINES F1-ENREG-00.
               05 F1-TYPE-10           PIC X(02).
               05 F1-COMPTE            PIC X(11).
               05 F1-CODE-OPER         PIC X(03).
               05 F1-REF-OPER          PIC X(10).
               05 F1-CODE-DEV          PIC X(03).
               05 F1-MONTANT-OPER      PIC 9(11)V99.
               05 FILLER               PIC X(38).
       01  F1-ENREG-99 REDEFINES F1-ENREG-00.
               05 F1-TYPE-99           PIC X(02).
               05 F1-NB-OPERATIONS     PIC 9(06).
               05 F1-MT-GLOBAL         PIC 9(11)V99.
               05 FILLER               PIC X(59).
