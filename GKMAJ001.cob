      *PROCESS TEST
      ******************************************************************
      * Program name   : GKMAJ001                               
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
       PROGRAM-ID.    GKMAJ001.
       AUTHOR.        DEFAY E. 
       INSTALLATION.  COBOL DEVELOPMENT CENTER. 
       DATE-WRITTEN.  11/01/23. 
       DATE-COMPILED. 11/01/23. 
       SECURITY.      NON-CONFIDENTIAL.
      ******************************************************************
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
      *    FLUX     
           SELECT FLUX-FILE
           ASSIGN TO './FLUX.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
      *    REJECT     
           SELECT REJETS-FILE
           ASSIGN TO './REJETS.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD FLUX-FILE.
           01 FLUX-ENREG.
               05 F1-ENREG-00 PIC X(2).
               05 F1-ENREG-10 PIC X(80).
       FD REJETS-FILE.
           01 REJETS-ENREG.
               05 R-10-F1     PIC X(80).
               05 R-MOTIF     PIC X(30).
               05 R-SQLCODE   PIC S9(3) COMP-3.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *   Status file
       01  WS-EOF PIC X(1) VALUE 'N'.
      *   Copybooks
       COPY "./Copybooks/ZFMAJCPT.cpy".
      ******************************************************************
       LINKAGE SECTION.

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
       PROCEDURE DIVISION.

           PERFORM 1000-Main.
           EXIT PROGRAM.

       0000-OFILES.
      ******************************************************************EDEFAY
      *  This routine should open file(s).          
            OPEN INPUT FLUX-FILE
            OPEN OUTPUT REJETS-FILE
           .

       0100-READ-FILEIN.
      ******************************************************************EDEFAY
      *  This routine should read FILEIN file.
            READ FLUX-FILE
                AT END MOVE 'Y' TO WS-EOF
            END-READ
           .

       1000-Main.
      ******************************************************************EDEFAY
      *  This routine should follow the logic of the program purpose.
           PERFORM 0000-OFILES
           PERFORM 0100-READ-FILEIN
           PERFORM 1100-FLUX-ACTIONS
           PERFORM 9999-CFILES
           .

       1001-UPDATE-MAJCPT.
      ******************************************************************EDEFAY
      *  This routine should feed ZF-MAJCPT with data from input file
           MOVE F1-ENREG-10 TO ZF-MAJCPT
           .

       1100-FLUX-ACTIONS.
      ******************************************************************EDEFAY
      *  This routine should read input file to choose the action to do
           PERFORM UNTIL WS-EOF = 'Y'
               IF F1-ENREG-00 = '10' THEN
                   PERFORM 1001-UPDATE-MAJCPT
                   CALL 'MFMAJCPT' USING ZF-MAJCPT, ZF-RETOUR
                   IF ZF-CODRET NOT = '00' THEN
                       PERFORM 1299-WRITE-REJECTS
                   END-IF
               END-IF
               PERFORM 0100-READ-FILEIN
           END-PERFORM
           .

       1299-WRITE-REJECTS.
      ******************************************************************EDEFAY
      *  This routine should write errors in output file
           MOVE F1-ENREG-10 TO R-10-F1
           MOVE ZF-LIBRET   TO R-MOTIF
           MOVE ZF-SQLCODE  TO R-SQLCODE
           WRITE REJETS-ENREG
           .

       9999-CFILES.
      ******************************************************************EDEFAY
      *  This routine should close file(s).
            CLOSE FLUX-FILE
            CLOSE REJETS-FILE
           .
