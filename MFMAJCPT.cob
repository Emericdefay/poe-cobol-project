      *PROCESS TEST
      ******************************************************************
      * Program name   : MFMAJCPT                               
      * Original author: DEFAY E.                                
      *
      * Description    : 
      *
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 11/01/23  EDEFAY        Create first version       
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MFMAJCPT.
       AUTHOR.        DEFAY E. 
       INSTALLATION.  COBOL DEVELOPMENT CENTER. 
       DATE-WRITTEN.  11/01/23. 
       DATE-COMPILED. 11/01/23. 
       SECURITY.      NON-CONFIDENTIAL.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *  Copies
       COPY "./Copybooks/CTBCPT.cpy".
       COPY "./Copybooks/CTBHIS.cpy".
       COPY "./Copybooks/CTBDEV.cpy".
       COPY "./Copybooks/CTBOPE.cpy".
      *  Accessors auth
       01 AUTH-QUERY           PIC 9(02) VALUE 0.

      ******************************************************************
       LINKAGE SECTION.
      *  Actions
       COPY "./Copybooks/ZFMAJCPT.cpy".

      ******************************************************************
      *  Program : Setup, run main routine and exit.
      *    
      *    Main purpose
      *    - 0xxx : Input/Output section
      *    - 1xxx : Main element
      *    - 2xxx : Verifications   
      *    - 8xxx : SQL Handling
      *    - 9xxx : Close files
      *
      *    Input/Output managment
      *    - x1xx : Perform a READ
      *    - x2xx : Perform a WRITE
      *    - x3xx : Perform a FETCH
      *    - x5xx : Perform Comparisons
      *    - x7xx : Perform a UPDATE
      *    - x9xx : Perform a CLOSE
      *
      *    Specials
      *    - xxxx : OTHERS
      *    - Dxxx : Displays
      *    - Cxxx : Calls
      ******************************************************************
       PROCEDURE DIVISION USING BY REFERENCE ZF-MAJCPT,
                                BY REFERENCE ZF-RETOUR.
           PERFORM 1000-Main.
           GOBACK.

       1000-Main.
      ******************************************************************EDEFAY
      *  This routine should follow the logic of the program purpose.
           INITIALIZE ZF-RETOUR
      *    Verify CODOPE
           PERFORM VERIF-CODOPE
      *    Verify CODDEV
           IF CODRET-OK
               PERFORM VERIF-CODDEV
           ELSE
               CALL "ABEND PGM"
           END-IF
      *    Verify COMPTE
           IF CODRET-OK
               PERFORM VERIF-COMPTE
           ELSE
               CALL "ABEND PGM"
           END-IF           
      *    All checks passed
           IF CODRET-OK
               PERFORM TRAITEMENT
           ELSE
               CALL "ABEND PGM"
           END-IF
           GOBACK
           .

       VERIF-CODOPE.
      ******************************************************************EDEFAY
      *  Verify if CODOPE exist
           MOVE ZF-CODOPE TO ZAOPE-COPE
           MOVE "SEL" TO ZAOPE-FONCTION
           CALL "MAOPE" USING ZAOPE-ZCMA, AUTH-QUERY
           MOVE ZAOPE-RETOUR TO ZF-RETOUR
           .

       VERIF-CODDEV.
      ******************************************************************EDEFAY
      *  Verify if CODDEV exist
           MOVE ZF-CODDEV TO ZADEV-CDEV
           MOVE "SEL" TO ZADEV-FONCTION
           CALL "MADEV" USING ZADEV-ZCMA, AUTH-QUERY
           MOVE ZADEV-RETOUR TO ZF-RETOUR
           .

       VERIF-COMPTE.
      ******************************************************************EDEFAY
      *  verify if account exist
           MOVE ZF-COMPTE TO ZACPT-COMPTE
           MOVE "SEL" TO ZACPT-FONCTION
           CALL "MACPT" USING ZACPT-ZCMA, AUTH-QUERY
           MOVE ZACPT-RETOUR TO ZF-RETOUR
           .

       TRAITEMENT.
      ******************************************************************EDEFAY
      *  Check what kind of operation is it, then update account & hist
           IF IS-SUB-OPE THEN
               COMPUTE ZACPT-SOLDE = 
                       ZACPT-SOLDE - ( ZADEV-ACHAT * ZF-MNTOPE ) 
           END-IF
           IF IS-ADD-OPE THEN
               COMPUTE ZACPT-SOLDE = 
                       ZACPT-SOLDE + ( ZADEV-ACHAT * ZF-MNTOPE )
           END-IF
           PERFORM MAJ-SOLDE
           IF ZF-CODRET = "00" 
               PERFORM MAJ-HISTORIQUE
           END-IF
           .

       MAJ-SOLDE.
      ******************************************************************EDEFAY
      *  Update the account
           MOVE "UPD" TO ZACPT-FONCTION
           CALL "MACPT" USING ZACPT-ZCMA, AUTH-QUERY
           MOVE ZACPT-RETOUR TO ZF-RETOUR
           . 

       MAJ-HISTORIQUE.
      ******************************************************************EDEFAY
      *  Add the operation to the history 
           MOVE "INS" TO ZAHIS-FONCTION
           CALL "MAHIS" USING ZAHIS-ZCMA, AUTH-QUERY
           MOVE ZAHIS-RETOUR TO ZF-RETOUR
           . 
