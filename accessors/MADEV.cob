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
       WORKING-STORAGE SECTION.
       01 AUTHORIZATION-QUERIES-TYPE.
           10 SELECT-CPT  PIC X  VALUE "O".
           10 SELECT-AUTH PIC X     VALUE "O".
           10 INSERT-CPT  PIC X  VALUE "O".
           10 INSERT-AUTH PIC X     VALUE "O".
           10 UPDATE-CPT  PIC X  VALUE "O".
           10 UPDATE-AUTH PIC X     VALUE "O".
           10 DELETE-CPT  PIC X  VALUE "O".
           10 DELETE-AUTH PIC X     VALUE "O".
       01  SQLCODE       PIC S9(3) VALUE 0.

           EXEC SQL 
               INCLUDE SQLCA 
           END-EXEC.
      *  DCLTBDEV init for avoid workflow errors
       01  DCLTBDEV PIC X(255).
      * DECLARATION DU DCLGEN DE LA TABLE TBDEV
           EXEC SQL 
               INCLUDE DCLTBDEV 
           END-EXEC.

       LINKAGE SECTION.
       01 AUTH-QUERY PIC 9(2).
       COPY "./Copybooks/CTBDEV.cpy".
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
       PROCEDURE DIVISION USING ZADEV-ZCMA, AUTH-QUERY
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
               WHEN ZADEV-FONCTION = 'SEL'
                   IF SELECT-CPT = SELECT-AUTH THEN
                       PERFORM 8100-SELECT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZADEV-FONCTION = 'INS'
                   IF INSERT-CPT = INSERT-AUTH THEN
                       PERFORM 8400-INSERT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZADEV-FONCTION = 'UPD'
                   IF UPDATE-CPT = UPDATE-AUTH THEN
                       PERFORM 8700-UPDATE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZADEV-FONCTION = 'DEL'
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
           MOVE 0 TO ZADEV-CODRET
           MOVE "SPACE" TO ZADEV-LIBRET
           MOVE 0 TO ZADEV-SQLCODE

           IF SQLCODE NOT = 0 THEN
               EVALUATE SQLCODE ALSO ZADEV-FONCTION
                   WHEN -803    ALSO 'INS'
                       MOVE 20 TO ZADEV-CODRET
                       MOVE "LIGNE EN DOUBLE" TO ZADEV-LIBRET
                       MOVE SQLCODE TO ZADEV-SQLCODE
                   WHEN +100    ALSO 'SEL'
                       MOVE 30 TO ZADEV-CODRET
                       MOVE "DEV" TO ZADEV-LIBRET
                       MOVE SQLCODE TO ZADEV-SQLCODE
                   WHEN +100    ALSO 'UPD'
                       MOVE 40 TO ZADEV-CODRET
                       MOVE "UPDATE D'UNE LIGNE INEXISTANTE"
                           TO ZADEV-LIBRET
                       MOVE SQLCODE TO ZADEV-SQLCODE
                   WHEN +100    ALSO 'DEL'
                       MOVE 50 TO ZADEV-CODRET
                       MOVE "DELETE D'UNE LIGNE INEXISTANTE"
                           TO ZADEV-LIBRET
                       MOVE SQLCODE TO ZADEV-SQLCODE
                   WHEN OTHER
                       MOVE 90 TO ZADEV-CODRET
                       MOVE "SQLCA" TO ZADEV-LIBRET
                       MOVE SQLCODE TO ZADEV-SQLCODE
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
           MOVE ZADEV-DONNEES TO DCLTBDEV
           EXEC SQL
             SELECT
                 CDEV  ,
                 CPAYS ,
                 ACHAT ,
                 VENTE 
             INTO
                :ZADEV-CDEV  ,
                :ZADEV-CPAYS ,
                :ZADEV-ACHAT ,
                :ZADEV-VENTE 
             FROM TBDEV
             WHERE CDEV=:ZADEV-CDEV
           END-EXEC
           IF SQLCODE = ZERO
              MOVE DCLTBDEV TO ZADEV-DONNEES
           END-IF
           .

       8400-INSERT.
      ******************************************************************EDEFAY 
      *Code for INSERT operation
           MOVE ZADEV-DONNEES TO DCLTBDEV
           EXEC SQL
                INSERT INTO TBDEV VALUES
               (:ZADEV-CDEV   ,
                :ZADEV-CPAYS  ,
                :ZADEV-ACHAT  ,
                :ZADEV-VENTE )
           END-EXEC
           .

       8700-UPDATE.
      ******************************************************************EDEFAY 
      *Code for UPDATE operation
           MOVE ZADEV-DONNEES TO DCLTBDEV
           EXEC SQL
                UPDATE TBDEV
           SET   CDEV  =:ZADEV-CDEV  ,
                 CPAYS =:ZADEV-CPAYS ,
                 ACHAT =:ZADEV-ACHAT ,
                 VENTE =:ZADEV-VENTE ,
           WHERE CDEV  =:ZADEV-CDEV
           END-EXEC
           .

       8800-DELETE.
      ******************************************************************EDEFAY 
      *Code for DELETE operation
           MOVE ZADEV-DONNEES TO DCLTBDEV
           EXEC SQL
             DELETE FROM TBDEV
           WHERE CDEV  =:ZADEV-CDEV
           END-EXEC
           .
