      *PROCESS TEST
      ******************************************************************
      * Program name   : TSTMAJ01                               
      * Original author: DEFAY E.                                
      *
      * Description    : This routine check if authorizations are
      *                  correctly given for each accessors
      *
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 09/01/23  EDEFAY        Create tests        
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSTMAJ01.
       AUTHOR.        DEFAY E. 
       INSTALLATION.  COBOL DEVELOPMENT CENTER. 
       DATE-WRITTEN.  09/01/23. 
       DATE-COMPILED. 09/01/23. 
       SECURITY.      NON-CONFIDENTIAL.
      ******************************************************************
      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      ******************************************************************
       WORKING-STORAGE SECTION.
      *  PGM called for tests
	   01  FILEIN-DDNAME       PIC X(30).
      *  Errors
       01  NUM-ERRORS          PIC 9(02) VALUE 0.
      *  Accessors auth
       01 AUTH-QUERY           PIC 9(02).
           88 SHOULD-BE-AUTH     VALUE 0.
           88 SHOULD-NOT-BE-AUTH VALUE 1.
      *  Accessors struct
       01 ZCMA.
           05 FONCTION         PIC X(03).
           05 DONNEES.
               10 COMPTE       PIC X(11).
               10 NOM          PIC X(20).
               10 SOLDE        PIC S9(13)V9(2) USAGE COMP-3.
               10 DDMVT        PIC X(10).
               10 DDMAJ        PIC X(10).
               10 HDMAJ        PIC X(08).
           05 RETOUR.
               10 CODRET       PIC X(02).
               10 SQLCODE      PIC S9(3).
               10 LIBRET       PIC X(30).
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
      *    - xTnn : Tests with "nn" RC expected
      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-Main.
           GOBACK.

       1000-Main.
      ******************************************************************EDEFAY
      *  This routine should follow the logic of the program purpose
           DISPLAY "Executing tests :"
           PERFORM CT01-MACPT-SELECT
           PERFORM CT02-MACPT-INSERT
           PERFORM CT03-MACPT-UPDATE
           PERFORM CT04-MACPT-DELETE
           PERFORM CT05-MACPT-UNKNOWN
           PERFORM CT01-MAHIS-SELECT
           PERFORM CT02-MAHIS-INSERT
           PERFORM CT03-MAHIS-UPDATE
           PERFORM CT04-MAHIS-DELETE
           PERFORM CT05-MAHIS-UNKNOWN
           PERFORM CT01-MADEV-SELECT
           PERFORM CT02-MADEV-INSERT
           PERFORM CT03-MADEV-UPDATE
           PERFORM CT04-MADEV-DELETE
           PERFORM CT05-MADEV-UNKNOWN
           PERFORM CT01-MAOPE-SELECT
           PERFORM CT02-MAOPE-INSERT
           PERFORM CT03-MAOPE-UPDATE
           PERFORM CT04-MAOPE-DELETE
           PERFORM CT05-MAOPE-UNKNOWN

           DISPLAY "Ending tests."
           PERFORM 2500-CHECK-NO-ERROR
           .

       1200-INC-ERROR.
      ******************************************************************EDEFAY
      *  This routine should increment number of errors saw in tests
           ADD 1 TO NUM-ERRORS
           .

       2500-CHECK-NO-ERROR.    
      ******************************************************************EDEFAY
      *  This routine should exec failure if there is an error in tests
           IF NUM-ERRORS > 0 THEN
               PERFORM 9999-MAKING-ERROR-CODE
           END-IF
           .

       9999-MAKING-ERROR-CODE.
      ******************************************************************EDEFAY
      *  This routine should generate an error, making workflow failing
           CALL "TEST-FAILED"
           .

      ******************************************************************EDEFAY
      *    TESTS MACPT
      ******************************************************************EDEFAY

       CT01-MACPT-SELECT.
      ******************************************************************EDEFAY
      *  This routine should test if MACPT is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "SEL" TO FONCTION
           CALL "MACPT" USING FILEIN-DDNAME, AUTH-QUERY
           DISPLAY AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST CPT-01 PASSED.'
           ELSE
               DISPLAY '    TEST CPT-01 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT02-MACPT-INSERT.
      ******************************************************************EDEFAY
      *  This routine should test if MACPT is authorized to use func
           INITIALIZE ZCMA
           MOVE "INS" TO FONCTION
           CALL "MACPT" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-BE-AUTH THEN
               DISPLAY '    TEST CPT-02 PASSED.'
           ELSE
               DISPLAY '    TEST CPT-02 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .  

       CT03-MACPT-UPDATE.
      ******************************************************************EDEFAY
      *  This routine should test if MACPT is authorized to use func
           INITIALIZE ZCMA
           MOVE "UPD" TO FONCTION
           CALL "MACPT" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-BE-AUTH THEN
               DISPLAY '    TEST CPT-03 PASSED.'
           ELSE
               DISPLAY '    TEST CPT-03 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT04-MACPT-DELETE.
      ******************************************************************EDEFAY
      *  This routine should test if MACPT is authorized to use func
           INITIALIZE ZCMA
           MOVE "DEL" TO FONCTION
           CALL "MACPT" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-BE-AUTH THEN
               DISPLAY '    TEST CPT-04 PASSED.'
           ELSE
               DISPLAY '    TEST CPT-04 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT05-MACPT-UNKNOWN.
      ******************************************************************EDEFAY
      *  This routine should test if MACPT is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "UNK" TO FONCTION
           CALL "MACPT" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST CPT-05 PASSED.'
           ELSE
               DISPLAY '    TEST CPT-05 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

      ******************************************************************EDEFAY
      *    TESTS MAHIS
      ******************************************************************EDEFAY
  
       CT01-MAHIS-SELECT.
      ******************************************************************EDEFAY
      *  This routine should test if MAHIS is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "SEL" TO FONCTION
           CALL "MAHIS" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST HIS-01 PASSED.'
           ELSE
               DISPLAY '    TEST HIS-01 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT02-MAHIS-INSERT.
      ******************************************************************EDEFAY
      *  This routine should test if MAHIS is authorized to use func
           INITIALIZE ZCMA
           MOVE "INS" TO FONCTION
           CALL "MAHIS" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-BE-AUTH THEN
               DISPLAY '    TEST HIS-02 PASSED.'
           ELSE
               DISPLAY '    TEST HIS-02 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .  

       CT03-MAHIS-UPDATE.
      ******************************************************************EDEFAY
      *  This routine should test if MAHIS is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "UPD" TO FONCTION
           CALL "MAHIS" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST HIS-03 PASSED.'
           ELSE
               DISPLAY '    TEST HIS-03 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT04-MAHIS-DELETE.
      ******************************************************************EDEFAY
      *  This routine should test if MAHIS is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "DEL" TO FONCTION
           CALL "MAHIS" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST HIS-04 PASSED.'
           ELSE
               DISPLAY '    TEST HIS-04 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT05-MAHIS-UNKNOWN.
      ******************************************************************EDEFAY
      *  This routine should test if MAHIS is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "UNK" TO FONCTION
           CALL "MAHIS" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST HIS-05 PASSED.'
           ELSE
               DISPLAY '    TEST HIS-05 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .


      ******************************************************************EDEFAY
      *    TESTS MADEV
      ******************************************************************EDEFAY

       CT01-MADEV-SELECT.
      ******************************************************************EDEFAY
      *  This routine should test if MADEV is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "SEL" TO FONCTION
           CALL "MADEV" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST DEV-01 PASSED.'
           ELSE
               DISPLAY '    TEST DEV-01 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT02-MADEV-INSERT.
      ******************************************************************EDEFAY
      *  This routine should test if MADEV is authorized to use func
           INITIALIZE ZCMA
           MOVE "INS" TO FONCTION
           CALL "MADEV" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-BE-AUTH THEN
               DISPLAY '    TEST DEV-02 PASSED.'
           ELSE
               DISPLAY '    TEST DEV-02 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .  

       CT03-MADEV-UPDATE.
      ******************************************************************EDEFAY
      *  This routine should test if MADEV is authorized to use func
           INITIALIZE ZCMA
           MOVE "UPD" TO FONCTION
           CALL "MADEV" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-BE-AUTH THEN
               DISPLAY '    TEST DEV-03 PASSED.'
           ELSE
               DISPLAY '    TEST DEV-03 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT04-MADEV-DELETE.
      ******************************************************************EDEFAY
      *  This routine should test if MADEV is authorized to use func
           INITIALIZE ZCMA
           MOVE "DEL" TO FONCTION
           CALL "MADEV" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-BE-AUTH THEN
               DISPLAY '    TEST DEV-04 PASSED.'
           ELSE
               DISPLAY '    TEST DEV-04 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT05-MADEV-UNKNOWN.
      ******************************************************************EDEFAY
      *  This routine should test if MADEV is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "UNK" TO FONCTION
           CALL "MADEV" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST DEV-05 PASSED.'
           ELSE
               DISPLAY '    TEST DEV-05 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

      ******************************************************************EDEFAY
      *    TESTS MAOPE
      ******************************************************************EDEFAY

       CT01-MAOPE-SELECT.
      ******************************************************************EDEFAY
      *  This routine should test if MAOPE is authorized to use func
           INITIALIZE ZCMA
           MOVE "SEL" TO FONCTION
           CALL "MAOPE" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-BE-AUTH THEN
               DISPLAY '    TEST OPE-01 PASSED.'
           ELSE
               DISPLAY '    TEST OPE-01 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT02-MAOPE-INSERT.
      ******************************************************************EDEFAY
      *  This routine should test if MAOPE is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "INS" TO FONCTION
           CALL "MAOPE" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST OPE-02 PASSED.'
           ELSE
               DISPLAY '    TEST OPE-02 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .  

       CT03-MAOPE-UPDATE.
      ******************************************************************EDEFAY
      *  This routine should test if MAOPE is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "UPD" TO FONCTION
           CALL "MAOPE" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST OPE-03 PASSED.'
           ELSE
               DISPLAY '    TEST OPE-03 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT04-MAOPE-DELETE.
      ******************************************************************EDEFAY
      *  This routine should test if MAOPE is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "DEL" TO FONCTION
           CALL "MAOPE" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST OPE-04 PASSED.'
           ELSE
               DISPLAY '    TEST OPE-04 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT05-MAOPE-UNKNOWN.
      ******************************************************************EDEFAY
      *  This routine should test if MAOPE is unauthorized to use func
           INITIALIZE ZCMA
           MOVE "UNK" TO FONCTION
           CALL "MAOPE" USING FILEIN-DDNAME, AUTH-QUERY
           IF SHOULD-NOT-BE-AUTH THEN
               DISPLAY '    TEST OPE-05 PASSED.'
           ELSE
               DISPLAY '    TEST OPE-05 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .
