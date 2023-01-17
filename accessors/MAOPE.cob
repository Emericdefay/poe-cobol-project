      *PROCESS TEST
      ******************************************************************
      * Program name   : MAOPE                               
      * Original author: DEFAY E.                                
      *
      * Description    : This routine is an accessor allowed to :
      *                    - [O] SELECT
      *                    - [X] INSERT
      *                    - [X] UPDATE
      *                    - [X] DELETE
      *
      *                  It uses ZCMA copy replacing () by ZAOPE.
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 10/01/23  EDEFAY        Created from MAXXX          
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MAOPE.
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
           10 SELECT-CPT  PIC X  VALUE "O".
           10 SELECT-AUTH PIC X     VALUE "O".
           10 INSERT-CPT  PIC X  VALUE "X".
           10 INSERT-AUTH PIC X     VALUE "O".
           10 UPDATE-CPT  PIC X  VALUE "X".
           10 UPDATE-AUTH PIC X     VALUE "O".
           10 DELETE-CPT  PIC X  VALUE "X".
           10 DELETE-AUTH PIC X     VALUE "O".
       01  SQLCODE       PIC S9(3) VALUE 0.

           EXEC SQL 
               INCLUDE SQLCA 
           END-EXEC.
      *  DCLTBOPE init for avoid workflow errors
       01  DCLTBOPE PIC X(255).
      *  DECLARATION DU DCLGEN DE LA TABLE TBOPE
           EXEC SQL 
               INCLUDE DCLTBOPE 
           END-EXEC.

       LINKAGE SECTION.
       01 AUTH-QUERY PIC 9(2).
       COPY "./Copybooks/CTBOPE.cpy".

      ******************************************************************
      *  Program : Setup, run main routine and exit.
      *    
      *    Main purpose
      *    - 0OPE : Input/Output section
      *    - 1OPE : Main element
      *    - 2OPE : Verifications   
      *    - 8OPE : SQL Handling
      *    - 9OPE : Close files
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
      *    - OPEx : OTHERS
      *    - DOPE : Displays
      *    - COPE : Calls
      ******************************************************************
       PROCEDURE DIVISION USING ZAOPE-ZCMA, AUTH-QUERY
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
               WHEN ZAOPE-FONCTION = 'SEL'
                   IF SELECT-CPT = SELECT-AUTH THEN
                       PERFORM 8100-SELECT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZAOPE-FONCTION = 'INS'
                   IF INSERT-CPT = INSERT-AUTH THEN
                       PERFORM 8400-INSERT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZAOPE-FONCTION = 'UPD'
                   IF UPDATE-CPT = UPDATE-AUTH THEN
                       PERFORM 8700-UPDATE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZAOPE-FONCTION = 'DEL'
                   IF DELETE-CPT = DELETE-AUTH THEN
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
           MOVE "00" TO ZAOPE-CODRET
           MOVE "SPACE" TO ZAOPE-LIBRET
           MOVE 0 TO ZAOPE-SQLCODE

           IF SQLCODE NOT = 0 THEN
               EVALUATE SQLCODE ALSO ZAOPE-FONCTION
                   WHEN -803    ALSO 'INS'
                       MOVE 20 TO ZAOPE-CODRET
                       MOVE "LIGNE EN DOUBLE" TO ZAOPE-LIBRET
                       MOVE SQLCODE TO ZAOPE-SQLCODE
                   WHEN +100    ALSO 'SEL'
                       MOVE 30 TO ZAOPE-CODRET
                       MOVE "OPE" TO ZAOPE-LIBRET
                       MOVE SQLCODE TO ZAOPE-SQLCODE
                   WHEN +100    ALSO 'UPD'
                       MOVE 40 TO ZAOPE-CODRET
                       MOVE "UPDATE D'UNE LIGNE INEXISTANTE"
                           TO ZAOPE-LIBRET
                       MOVE SQLCODE TO ZAOPE-SQLCODE
                   WHEN +100    ALSO 'DEL'
                       MOVE 50 TO ZAOPE-CODRET
                       MOVE "DELETE D'UNE LIGNE INEXISTANTE"
                           TO ZAOPE-LIBRET
                       MOVE SQLCODE TO ZAOPE-SQLCODE
                   WHEN OTHER
                       MOVE 90 TO ZAOPE-CODRET
                       MOVE "SQLCA" TO ZAOPE-LIBRET
                       MOVE SQLCODE TO ZAOPE-SQLCODE
               END-EVALUATE
           END-IF
           .

       7777-UNAUTHORIZED-QUERY-TYPE.
      ******************************************************************EDEFAY 
      *  Update AUTH-QUERY, since Query type is unauthorized
           ADD 1 TO AUTH-QUERY
           .

       8100-SELECT.
      ******************************************************************EDEFAY 
      *Code for SELECT operation
           MOVE ZAOPE-DONNEES TO DCLTBOPE
           EXEC SQL
             SELECT
                 COPE   ,
                 LOPE   ,
                 MNTMIN ,
                 MNTMAX 
             INTO
                :ZAOPE-COPE   ,
                :ZAOPE-LOPE   ,
                :ZAOPE-MNTMIN ,
                :ZAOPE-MNTMAX 
             FROM TBOPE
             WHERE COPE=:ZAOPE-COPE
           END-EXEC
           IF SQLCODE = ZERO
              MOVE DCLTBOPE TO ZAOPE-DONNEES
           END-IF
           .

       8400-INSERT.
      ******************************************************************EDEFAY 
      *Code for INSERT operation
           DISPLAY "INSERT NOT ALLOWED"
           .

       8700-UPDATE.
      ******************************************************************EDEFAY 
      *Code for UPDATE operation
           DISPLAY "UPDATE NOT ALLOWED"
           .

       8800-DELETE.
      ******************************************************************EDEFAY 
      *Code for DELETE operation
           DISPLAY "DELETE NOT ALLOWED"
           .
