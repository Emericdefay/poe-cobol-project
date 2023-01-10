      *PROCESS TEST
      ******************************************************************
      * Program name   : MAXXX                               
      * Original author: DEFAY E.                                
      *
      * Description    : Template MAXXX of CPT, HIS, DEV & OPE
      *
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 10/01/23  EDEFAY        Template created          
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MAXXX.
       AUTHOR.        DEFAY E. 
       INSTALLATION.  COBOL DEVELOPMENT CENTER. 
       DATE-WRITTEN.  10/01/23. 
       DATE-COMPILED. 10/01/23. 
       SECURITY.      NON-CONFIDENTIAL.
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AUTHORIZATION-QUERIES-TYPE.
           10 SELECT-XXX PIC X   VALUE "O".
                 88 SELECT-AUTH  VALUE "O".
           10 INSERT-XXX PIC X   VALUE "O".
                 88 INSERT-AUTH  VALUE "O".
           10 UPDATE-XXX PIC X   VALUE "O".
                 88 UPDATE-AUTH  VALUE "O".
           10 DELETE-XXX PIC X   VALUE "O".
                 88 DELETE-AUTH  VALUE "O".
       01  SQLCODE       PIC S9(3) VALUE 0.

       LINKAGE SECTION.
       01 AUTH-QUERY PIC 9(2).
       01 ZAXXX-ZCMA.
           05 ZAXXX-FONCTION         PIC X(03).
           05 ZAXXX-DONNEES.
               10 ZAXXX-COMPTE       PIC X(11).
               10 ZAXXX-NOM          PIC X(20).
               10 ZAXXX-SOLDE        PIC S9(13)V9(2) USAGE COMP-3.
               10 ZAXXX-DDMVT        PIC X(10).
               10 ZAXXX-DDMAJ        PIC X(10).
               10 ZAXXX-HDMAJ        PIC X(8).
           05 ZAXXX-RETOUR.
               10 ZAXXX-CODRET       PIC X(02).
               10 ZAXXX-SQLCODE      PIC S9(3).
               10 ZAXXX-LIBRET       PIC X(30).
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
      *    - x4xx : Perform a INSERT
      *    - x5xx : Perform Comparisons
      *    - x7xx : Perform a UPDATE
      *    - x8xx : Perform a DELETE
      *    - x9xx : Perform a CLOSE
      *
      *    Specials
      *    - xxxx : OTHERS
      *    - Dxxx : Displays
      *    - Cxxx : Calls
      ******************************************************************
       PROCEDURE DIVISION USING ZAXXX-ZCMA, AUTH-QUERY
                          RETURNING         AUTH-QUERY.
           PERFORM 0000-INITIALIZATIONS
           PERFORM 2500-ROOTER
           GOBACK.

       0000-INITIALIZATIONS.
      ******************************************************************EDEFAY 
      *  Initialize values
           SET AUTH-QUERY TO 0
           .

       2500-ROOTER.
      ******************************************************************EDEFAY 
      * Perform the different operations based on the value of FONCTION
           EVALUATE TRUE
               WHEN ZAXXX-FONCTION = 'SEL'
                   IF SELECT-AUTH THEN
                       PERFORM 8100-SELECT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZAXXX-FONCTION = 'INS'
                   IF INSERT-AUTH THEN
                       PERFORM 8400-INSERT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZAXXX-FONCTION = 'UPD'
                   IF UPDATE-AUTH THEN
                       PERFORM 8700-UPDATE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZAXXX-FONCTION = 'DEL'
                   IF DELETE-AUTH THEN
                       PERFORM 8800-DELETE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN OTHER
                   MOVE -1 TO SQLCODE
                   PERFORM 2501-CHECK-SQLCODE
           END-EVALUATE
           .

       2501-CHECK-SQLCODE.
      ******************************************************************EDEFAY 
      *  Verify SQLCODE, returning Error code and message if SQLCODE<>0
           EVALUATE SQLCODE
               WHEN 0
                   MOVE SQLCODE TO ZAXXX-CODRET
                   MOVE "SPACE" TO ZAXXX-LIBRET
                   MOVE SQLCODE TO ZAXXX-SQLCODE
               WHEN 20
                   MOVE SQLCODE TO ZAXXX-CODRET
                   MOVE "LIGNE EN DOUBLE" TO ZAXXX-LIBRET
                   MOVE SQLCODE TO ZAXXX-SQLCODE
               WHEN 30
                   MOVE SQLCODE TO ZAXXX-CODRET
                   MOVE "XXX" TO ZAXXX-LIBRET
                   MOVE SQLCODE TO ZAXXX-SQLCODE
               WHEN 40
                   MOVE SQLCODE TO ZAXXX-CODRET
                   MOVE "UPDATE D'UNE LIGNE INEXISTANTE" TO ZAXXX-LIBRET
                   MOVE SQLCODE TO ZAXXX-SQLCODE
               WHEN 50
                   MOVE SQLCODE TO ZAXXX-CODRET
                   MOVE "DELETE D'UNE LIGNE INEXISTANTE" TO ZAXXX-LIBRET
                   MOVE SQLCODE TO ZAXXX-SQLCODE
               WHEN 90
                   MOVE SQLCODE TO ZAXXX-CODRET
                   MOVE "SQLCA" TO ZAXXX-LIBRET
                   MOVE SQLCODE TO ZAXXX-SQLCODE
               WHEN OTHER
                   MOVE SQLCODE TO ZAXXX-CODRET
                   MOVE "SQL ERROR UNHANDLED" TO ZAXXX-LIBRET
                   MOVE SQLCODE TO ZAXXX-SQLCODE
           END-EVALUATE
           .

       7777-UNAUTHORIZED-QUERY-TYPE.
      ******************************************************************EDEFAY 
      *  Update AUTH-QUERY, since Query type is unauthorized
           ADD 1 TO AUTH-QUERY
           EXIT PROGRAM
           .

       8100-SELECT.
      ******************************************************************EDEFAY 
      *Code for SELECT operation
                  EXEC SQL
                      SELECT ...
                      INTO ...
                      FROM ...
                      WHERE ...
                  END-EXEC
           .

       8400-INSERT.
      ******************************************************************EDEFAY 
      *Code for INSERT operation
                   EXEC SQL
                       INSERT INTO ...
                       VALUES ...
                   END-EXEC
           .

       8700-UPDATE.
      ******************************************************************EDEFAY 
      *Code for UPDATE operation
                   EXEC SQL
                       UPDATE ...
                       SET ...
                       WHERE ...
                   END-EXEC
           .

       8800-DELETE.
      ******************************************************************EDEFAY 
      *Code for DELETE operation
                  EXEC SQL
                      DELETE FROM ...
                      WHERE ...
                  END-EXEC
           .
