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

           EXEC SQL 
               INCLUDE SQLCA 
           END-EXEC.
      *  DCLTBCPT init for avoid workflow errors
       01 DCLTBCPT PIC X(255).
      * DECLARATION DU DCLGEN DE LA TABLE TBCPT
           EXEC SQL 
               INCLUDE DCLTBCPT 
           END-EXEC.

       LINKAGE SECTION.
       01 AUTH-QUERY PIC 9(2).
       COPY "./Copybooks/CTBCPT.cpy".
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
           EVALUATE TRUE
               WHEN ZACPT-FONCTION = 'SEL'
                   IF SELECT-CPT = SELECT-AUTH THEN
                       PERFORM 8100-SELECT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZACPT-FONCTION = 'INS'
                   IF INSERT-CPT = INSERT-AUTH THEN
                       PERFORM 8400-INSERT
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZACPT-FONCTION = 'UPD'
                   IF UPDATE-CPT = UPDATE-AUTH THEN
                       PERFORM 8700-UPDATE
                       PERFORM 2501-CHECK-SQLCODE
                   ELSE
                       PERFORM 7777-UNAUTHORIZED-QUERY-TYPE
                   END-IF
               WHEN ZACPT-FONCTION = 'DEL'
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
           MOVE 0 TO ZACPT-CODRET
           MOVE "SPACE" TO ZACPT-LIBRET
           MOVE 0 TO ZACPT-SQLCODE

           EVALUATE SQLCODE ALSO ZACPT-FONCTION
               WHEN -803    ALSO 'INS'
                   MOVE 20 TO ZACPT-CODRET
                   MOVE "LIGNE EN DOUBLE" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN +100    ALSO 'SEL'
                   MOVE 30 TO ZACPT-CODRET
                   MOVE "CPT" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN +100    ALSO 'UPD'
                   MOVE 40 TO ZACPT-CODRET
                   MOVE "UPDATE D'UNE LIGNE INEXISTANTE" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN +100    ALSO 'DEL'
                   MOVE 50 TO ZACPT-CODRET
                   MOVE "DELETE D'UNE LIGNE INEXISTANTE" TO ZACPT-LIBRET
                   MOVE SQLCODE TO ZACPT-SQLCODE
               WHEN OTHER
                   MOVE 90 TO ZACPT-CODRET
                   MOVE "SQLCA" TO ZACPT-LIBRET
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
      *  Code for SELECT operation
           DISPLAY "SELECT NOT ALLOWED."
           .

       8400-INSERT.
      ******************************************************************EDEFAY 
      *  Code for INSERT operation
               MOVE ZACPT-DONNEES TO DCLTBCPT
               EXEC SQL
                    INSERT INTO TBCPT VALUES
                   (:HC-COMPTE  ,
                    :HC-NOM     ,
                    :HC-SOLDE   ,
                    :HC-DDMVT   ,
                    :HC-DDMAJ   ,
                    :HC-HDMAJ   )
               END-EXEC
           .

       8700-UPDATE.
      ******************************************************************EDEFAY 
      *  Code for UPDATE operation
           MOVE ZACPT-DONNEES TO DCLTBCPT
           EXEC SQL
                UPDATE TBCPT
           SET   COMPTE =:HC-COMPTE ,
                 NOM    =:HC-NOM    ,
                 SOLDE  =:HC-SOLDE  ,
                 DDMVT  =:HC-DDMVT  ,
                 DDMAJ  =:HC-DDMAJ  ,
                 HDMAJ  =:HC-HDMAJ  ,
           WHERE COMPTE =:HC-COMPTE
           END-EXEC
           .

       8800-DELETE.
      ******************************************************************EDEFAY 
      *  Code for DELETE operation
           MOVE ZACPT-DONNEES TO DCLTBCPT
           EXEC SQL
             DELETE FROM TBCPT
           WHERE COMPTE=:HC-COMPTE
           END-EXEC
           .
