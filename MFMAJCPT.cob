      *PROCESS TEST
      ******************************************************************
      * Program name   : MFMAJCPT                               
      * Original author: DEFAY E.                                
      *
      * Description    : This routine reads actions to perform on
      *                  accounts. The updates it.
      *
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 11/01/23  EDEFAY        Create first version       
      * 13/01/23  EDEFAY        Documentation     
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
           PERFORM 2001-VERIF-CODOPE
           IF ZF-CODRET = '00'
      *        Verify CODDEV
               PERFORM 2002-VERIF-CODDEV
           ELSE
               CALL "ABENDOPE"
           END-IF
           IF ZF-CODRET = '00'
      *        Verify COMPTE
               PERFORM 2003-VERIF-COMPTE
           ELSE
               CALL "ABENDDEV"
           END-IF           
           IF ZF-CODRET = '00'
      *        All checks passed
               PERFORM 1500-TRAITEMENT
           ELSE
               CALL "ABENDCPT"
           END-IF
           GOBACK
           .

       1500-TRAITEMENT.
      ******************************************************************EDEFAY
      *  Check what kind of operation is it, then update account & hist
           IF ZF-CODOPE = "VER" OR
                          "VRD" OR
                          "INT" OR
                          "VVF" THEN
               COMPUTE ZACPT-SOLDE = 
                       ZACPT-SOLDE - ( ZADEV-ACHAT * ZF-MNTOPE ) 
           END-IF
           IF ZF-CODOPE = "PRL" OR
                          "RMB" OR 
                          "VIR" OR 
                          "RET" OR 
                          "RTD" OR 
                          "AGI" THEN
               COMPUTE ZACPT-SOLDE = 
                       ZACPT-SOLDE + ( ZADEV-ACHAT * ZF-MNTOPE )
           END-IF
           PERFORM 1701-MAJ-SOLDE
           IF ZF-CODRET = "00" 
               PERFORM 1702-MAJ-HISTORIQUE
           END-IF
           .

       1701-MAJ-SOLDE.
      ******************************************************************EDEFAY
      *  Update the account
           MOVE "UPD" TO ZACPT-FONCTION
           CALL "MACPT" USING ZACPT-ZCMA, AUTH-QUERY
           MOVE ZACPT-RETOUR TO ZF-RETOUR
           . 

       1702-MAJ-HISTORIQUE.
      ******************************************************************EDEFAY
      *  Add the operation to the history 
           MOVE "INS" TO ZAHIS-FONCTION
           CALL "MAHIS" USING ZAHIS-ZCMA, AUTH-QUERY
           MOVE ZAHIS-RETOUR TO ZF-RETOUR
           . 

       2001-VERIF-CODOPE.
      ******************************************************************EDEFAY
      *  Verify if CODOPE exist
           MOVE ZF-CODOPE TO ZAOPE-COPE
           MOVE "SEL" TO ZAOPE-FONCTION
           CALL "MAOPE" USING ZAOPE-ZCMA, AUTH-QUERY
           MOVE ZAOPE-RETOUR TO ZF-RETOUR
           .

       2002-VERIF-CODDEV.
      ******************************************************************EDEFAY
      *  Verify if CODDEV exist
           MOVE ZF-CODDEV TO ZADEV-CDEV
           MOVE "SEL" TO ZADEV-FONCTION
           CALL "MADEV" USING ZADEV-ZCMA, AUTH-QUERY
           MOVE ZADEV-RETOUR TO ZF-RETOUR
           .

       2003-VERIF-COMPTE.
      ******************************************************************EDEFAY
      *  Verify if account exist
           MOVE ZF-COMPTE TO ZACPT-COMPTE
           MOVE "SEL" TO ZACPT-FONCTION
           CALL "MACPT" USING ZACPT-ZCMA, AUTH-QUERY
           MOVE ZACPT-RETOUR TO ZF-RETOUR
           .
