      *PROCESS TEST
      ******************************************************************
      * Program name   : MADEV                               
      * Original author: DEFAY E.                                
      *
      * Description    : This routine is an accessor allowed to :
      *                    - [X] SELECT
      *                    - [O] INSERT
      *                    - [X] UPDATE
      *                    - [X] DELETE
      *
      *                  It uses ZCMA copy replacing () by ZADEV.
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 10/01/23  EDEFAY        Created from MAXXX         
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MADEV.
       AUTHOR.        DEFAY E. 
       INSTALLATION.  COBOL DEVELOPMENT CENTER. 
       DATE-WRITTEN.  10/01/23. 
       DATE-COMPILED. 10/01/23. 
       SECURITY.      NON-CONFIDENTIAL.
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE.
       01 AUTHORIZATION-QUERIES-TYPE.
           10 SELECT-DEV PIC X   VALUE "X".
                 88 SELECT-AUTH  VALUE "O".
           10 INSERT-DEV PIC X   VALUE "O".
                 88 INSERT-AUTH  VALUE "O".
           10 UPDATE-DEV PIC X   VALUE "X".
                 88 UPDATE-AUTH  VALUE "O".
           10 DELETE-DEV PIC X   VALUE "X".
                 88 DELETE-AUTH  VALUE "O".
           
       LINKAGE SECTION.
       01 AUTH-QUERY PIC 9(2).
       01 ZADEV-ZCMA.
           05 ZADEV-FONCTION         PIC X(03).
           05 ZADEV-DONNEES.
               10 ZADEV-COMPTE       PIC X(11).
               10 ZADEV-NOM          PIC X(20).
               10 ZADEV-SOLDE        PIC S9(13)V9(2) USAGE COMP-3.
               10 ZADEV-DDMVT        PIC X(10).
               10 ZADEV-DDMAJ        PIC X(10).
               10 ZADEV-HDMAJ        PIC X(8).
           05 ZADEV-RETOUR.
               10 ZADEV-CODRET       PIC X(02).
               10 ZADEV-SQLCODE      PIC S9(3).
               10 ZADEV-LIBRET       PIC X(30).
      ******************************************************************
      *  Program : Setup, run main routine and exit.
      *    
      *    Main purpose
      *    - 0DEV : Input/Output section
      *    - 1DEV : Main element
      *    - 2DEV : Verifications   
      *    - 8DEV : SQL Handling
      *    - 9DEV : Close files
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
      *    - DEVx : OTHERS
      *    - DDEV : Displays
      *    - CDEV : Calls
      ******************************************************************
       PROCEDURE DIVISION USING ZADEV-ZCMA, AUTH-QUERY.
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
           EVALUATE ZADEV-FONCTION
               WHEN 'SEL'
                   IF -AUTH THEN
                       PERFORM 8100-SELECT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN 'INS'
                   IF -AUTH THEN
                       PERFORM 8400-INSERT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN 'UPD'
                   IF -AUTH THEN
                       PERFORM 8700-UPDATE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN 'DEL'
                   IF -AUTH THEN
                       PERFORM 8800-DELETE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN OTHER
                   MOVE -1 TO SQLCODE
                   PERFORM 2501-CHECK-SQLCODE
           END-EVALUATE.

       2501-CHECK-SQLCODE.
      ******************************************************************EDEFAY 
      *  Verify SQLCODE, returning Error code and message if SQLCODE<>0
           EVALUATE SQLCODE
               WHEN 0
                   MOVE SQLCODE TO ZADEV-CODRET
                   MOVE "SPACE" TO ZADEV-LIBRET
                   MOVE SQLCODE TO ZADEV-SQLCODE
               WHEN 20
                   MOVE SQLCODE TO ZADEV-CODRET
                   MOVE "LIGNE EN DOUBLE" TO ZADEV-LIBRET
                   MOVE SQLCODE TO ZADEV-SQLCODE
               WHEN 30
                   MOVE SQLCODE TO ZADEV-CODRET
                   MOVE "DEV" TO ZADEV-LIBRET
                   MOVE SQLCODE TO ZADEV-SQLCODE
               WHEN 40
                   MOVE SQLCODE TO ZADEV-CODRET
                   MOVE "UPDATE D'UNE LIGNE INEXISTANTE" TO ZADEV-LIBRET
                   MOVE SQLCODE TO ZADEV-SQLCODE
               WHEN 50
                   MOVE SQLCODE TO ZADEV-CODRET
                   MOVE "DELETE D'UNE LIGNE INEXISTANTE" TO ZADEV-LIBRET
                   MOVE SQLCODE TO ZADEV-SQLCODE
               WHEN 90
                   MOVE SQLCODE TO ZADEV-CODRET
                   MOVE "SQLCA" TO ZADEV-LIBRET
                   MOVE SQLCODE TO ZADEV-SQLCODE
               WHEN OTHER
                   MOVE SQLCODE TO ZADEV-CODRET
                   MOVE "SQL ERROR UNHANDLED" TO ZADEV-LIBRET
                   MOVE SQLCODE TO ZADEV-SQLCODE
           END-EVALUATE

       7777-UNAUTHORIZED-QUERY-TYPE.
      ******************************************************************EDEFAY 
      *  Update AUTH-QUERY, since Query type is unauthorized
           MOVE 1 TO AUTH-QUERY
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
