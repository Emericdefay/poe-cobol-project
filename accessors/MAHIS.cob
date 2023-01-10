      *PROCESS TEST
      ******************************************************************
      * Program name   : MAHIS                               
      * Original author: DEFAY E.                                
      *
      * Description    : This routine is an accessor allowed to :
      *                    - [X] SELECT
      *                    - [O] INSERT
      *                    - [O] UPDATE
      *                    - [O] DELETE
      *
      *                  It uses ZCMA copy replacing () by ZAHIS.
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 10/01/23  EDEFAY        Created from MAXXX         
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MAHIS.
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
           10 SELECT-HIS PIC X   VALUE "X".
                 88 SELECT-AUTH  VALUE "O".
           10 INSERT-HIS PIC X   VALUE "O".
                 88 INSERT-AUTH  VALUE "O".
           10 UPDATE-HIS PIC X   VALUE "O".
                 88 UPDATE-AUTH  VALUE "O".
           10 DELETE-HIS PIC X   VALUE "O".
                 88 DELETE-AUTH  VALUE "O".
       01  SQLCODE       PIC S9(3) VALUE 0.
           
       LINKAGE SECTION.
       01 AUTH-QUERY PIC 9(2).
       01 ZAHIS-ZCMA.
           05 ZAHIS-FONCTION         PIC X(03).
           05 ZAHIS-DONNEES.
               10 ZAHIS-COMPTE       PIC X(11).
               10 ZAHIS-NOM          PIC X(20).
               10 ZAHIS-SOLDE        PIC S9(13)V9(2) USAGE COMP-3.
               10 ZAHIS-DDMVT        PIC X(10).
               10 ZAHIS-DDMAJ        PIC X(10).
               10 ZAHIS-HDMAJ        PIC X(8).
           05 ZAHIS-RETOUR.
               10 ZAHIS-CODRET       PIC X(02).
               10 ZAHIS-SQLCODE      PIC S9(3).
               10 ZAHIS-LIBRET       PIC X(30).
      ******************************************************************
      *  Program : Setup, run main routine and exit.
      *    
      *    Main purpose
      *    - 0HIS : Input/Output section
      *    - 1HIS : Main element
      *    - 2HIS : Verifications   
      *    - 8HIS : SQL Handling
      *    - 9HIS : Close files
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
      *    - HISx : OTHERS
      *    - DHIS : Displays
      *    - CHIS : Calls
      ******************************************************************
       PROCEDURE DIVISION USING ZAHIS-ZCMA, AUTH-QUERY
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
               WHEN ZAHIS-FONCTION = 'SEL'
                   IF SELECT-AUTH THEN
                       PERFORM 8100-SELECT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZAHIS-FONCTION = 'INS'
                   IF INSERT-AUTH THEN
                       PERFORM 8400-INSERT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZAHIS-FONCTION = 'UPD'
                   IF UPDATE-AUTH THEN
                       PERFORM 8700-UPDATE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZAHIS-FONCTION = 'DEL'
                   IF DELETE-AUTH THEN
                       PERFORM 8800-DELETE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN OTHER
                   MOVE -1 TO SQLCODE
                   PERFORM 2501-CHECK-SQLCODE
                   PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
           END-EVALUATE
           .

       2501-CHECK-SQLCODE.
      ******************************************************************EDEFAY 
      *  Verify SQLCODE, returning Error code and message if SQLCODE<>0
           EVALUATE SQLCODE
               WHEN 0
                   MOVE SQLCODE TO ZAHIS-CODRET
                   MOVE "SPACE" TO ZAHIS-LIBRET
                   MOVE SQLCODE TO ZAHIS-SQLCODE
               WHEN 20
                   MOVE SQLCODE TO ZAHIS-CODRET
                   MOVE "LIGNE EN DOUBLE" TO ZAHIS-LIBRET
                   MOVE SQLCODE TO ZAHIS-SQLCODE
               WHEN 30
                   MOVE SQLCODE TO ZAHIS-CODRET
                   MOVE "HIS" TO ZAHIS-LIBRET
                   MOVE SQLCODE TO ZAHIS-SQLCODE
               WHEN 40
                   MOVE SQLCODE TO ZAHIS-CODRET
                   MOVE "UPDATE D'UNE LIGNE INEXISTANTE" TO ZAHIS-LIBRET
                   MOVE SQLCODE TO ZAHIS-SQLCODE
               WHEN 50
                   MOVE SQLCODE TO ZAHIS-CODRET
                   MOVE "DELETE D'UNE LIGNE INEXISTANTE" TO ZAHIS-LIBRET
                   MOVE SQLCODE TO ZAHIS-SQLCODE
               WHEN 90
                   MOVE SQLCODE TO ZAHIS-CODRET
                   MOVE "SQLCA" TO ZAHIS-LIBRET
                   MOVE SQLCODE TO ZAHIS-SQLCODE
               WHEN OTHER
                   MOVE SQLCODE TO ZAHIS-CODRET
                   MOVE "SQL ERROR UNHANDLED" TO ZAHIS-LIBRET
                   MOVE SQLCODE TO ZAHIS-SQLCODE
           END-EVALUATE
           .

       7777-UNAUTHORIZED-QUERY-TYPE.
      ******************************************************************EDEFAY 
      *  Update AUTH-QUERY, since Query type is unauthorized
           ADD 1 TO AUTH-QUERY
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
