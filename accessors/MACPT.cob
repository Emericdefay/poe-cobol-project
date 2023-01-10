      *PROCESS TEST
      ******************************************************************
      * Program name   : MACPT                               
      * Original author: DEFAY E.                                
      *
      * Description    : This routine is an accessor allowed to :
      *                    - [X] SELECT
      *                    - [O] INSERT
      *                    - [O] UPDATE
      *                    - [O] DELETE
      *
      *                  It uses ZCMA copy replacing () by ZACPT.
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 10/01/23  EDEFAY        Created from MAXXX         
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MACPT.
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
           10 SELECT-CPT  PIC X  VALUE "X".
           10 SELECT-AUTH PIC X     VALUE "O".
           10 INSERT-CPT  PIC X  VALUE "O".
           10 INSERT-AUTH PIC X     VALUE "O".
           10 UPDATE-CPT  PIC X  VALUE "O".
           10 UPDATE-AUTH PIC X     VALUE "O".
           10 DELETE-CPT  PIC X  VALUE "O".
           10 DELETE-AUTH PIC X     VALUE "O".
       01  SQLCODE        PIC S9(3) VALUE 0.

       LINKAGE SECTION.
       01 AUTH-QUERY PIC 9(2).
       01 ZACPT-ZCMA.
           05 ZACPT-FONCTION         PIC X(03).
           05 ZACPT-DONNEES.
               10 ZACPT-COMPTE       PIC X(11).
               10 ZACPT-NOM          PIC X(20).
               10 ZACPT-SOLDE        PIC S9(13)V9(2) USAGE COMP-3.
               10 ZACPT-DDMVT        PIC X(10).
               10 ZACPT-DDMAJ        PIC X(10).
               10 ZACPT-HDMAJ        PIC X(8).
           05 ZACPT-RETOUR.
               10 ZACPT-CODRET       PIC X(02).
               10 ZACPT-SQLCODE      PIC S9(3).
               10 ZACPT-LIBRET       PIC X(30).
      ******************************************************************
      *  Program : Setup, run main routine and exit.
      *    
      *    Main purpose
      *    - 0CPT : Input/Output section
      *    - 1CPT : Main element
      *    - 2CPT : Verifications   
      *    - 8CPT : SQL Handling
      *    - 9CPT : Close files
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
      *    - CPTx : OTHERS
      *    - DCPT : Displays
      *    - CCPT : Calls
      ******************************************************************
       PROCEDURE DIVISION USING ZACPT-ZCMA, AUTH-QUERY.
           PERFORM 0000-INITIALIZATIONS
           PERFORM 2500-ROOTER
           GOBACK.

       0000-INITIALIZATIONS.
      ******************************************************************EDEFAY 
      *  Initialize values
      *    INITIALIZE AUTHORIZATION-QUERIES-TYPE
      *    DISPLAY AUTHORIZATION-QUERIES-TYPE
           .

       2500-ROOTER.
      ******************************************************************EDEFAY 
      * Perform the different operations based on the value of FONCTION
           DISPLAY ZACPT-FONCTION
           DISPLAY SELECT-CPT
           DISPLAY SELECT-AUTH
           EVALUATE TRUE
               WHEN ZACPT-FONCTION = 'SEL'
                   IF SELECT-CPT = SELECT-AUTH THEN
                       DISPLAY "x"
                       PERFORM 8100-SELECT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZACPT-FONCTION = 'INS'
                   IF INSERT-CPT = INSERT-AUTH THEN
                       DISPLAY "o"
                       PERFORM 8400-INSERT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZACPT-FONCTION = 'UPD'
                   IF UPDATE-CPT = UPDATE-AUTH THEN
                       DISPLAY "o"
                       PERFORM 8700-UPDATE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZACPT-FONCTION = 'DEL'
                   IF DELETE-CPT = DELETE-AUTH THEN
                       DISPLAY "o"
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
                   MOVE SQLCODE TO ZACPT-CODRET
                   MOVE "SPACE" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN 20
                   MOVE SQLCODE TO ZACPT-CODRET
                   MOVE "LIGNE EN DOUBLE" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN 30
                   MOVE SQLCODE TO ZACPT-CODRET
                   MOVE "CPT" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN 40
                   MOVE SQLCODE TO ZACPT-CODRET
                   MOVE "UPDATE D'UNE LIGNE INEXISTANTE" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN 50
                   MOVE SQLCODE TO ZACPT-CODRET
                   MOVE "DELETE D'UNE LIGNE INEXISTANTE" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN 90
                   MOVE SQLCODE TO ZACPT-CODRET
                   MOVE "SQLCA" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN OTHER
                   MOVE SQLCODE TO ZACPT-CODRET
                   MOVE "SQL ERROR UNHANDLED" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
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
