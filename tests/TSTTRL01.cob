      *PROCESS TEST
      ******************************************************************
      * Program name   : TSTTRL01                               
      * Original author: DEFAY E.                                
      *
      * Description    : This routine test RC returned by a PGM
      * Rules handling : 
      *             RC = 1 : DDNAME issue
      *             RC = 2 : Length DSNAME issue
      *             RC = 3 : Other open file issue
      *             RC = 4 : FileIN empty issue
      *             RC = 5 : number operands != footer expected operands
      *             RC = 6 : F1-MONTANT-OPER != F1-MT-GLOBAL 
      *             RC = 7 : No header issue
      *             RC = 8 : No footer issue
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 09/01/23  EDEFAY        RC 00 to 08         
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSTTRL01.
       AUTHOR.        DEFAY E. 
       INSTALLATION.  COBOL DEVELOPMENT CENTER. 
       DATE-WRITTEN.  09/01/23. 
       DATE-COMPILED. 09/01/23. 
       SECURITY.      NON-CONFIDENTIAL.
      ******************************************************************
      *ENVIRONMENT DIVISION. 
      *INPUT-OUTPUT SECTION. 
      *FILE-CONTROL. 
      *    SELECT FILEIN ASSIGN TO FILEIN
      *    FILE STATUS IS FS-FILEIN.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
      *FILE SECTION.
      *FD  FILEIN RECORDING MODE F
      *    RECORD CONTAINS 80 CHARACTERS.
      *01  FILEIN-STRUCT.        
      *   05  FILLER     PIC X(80).

      ******************************************************************
       WORKING-STORAGE SECTION.
	  / PGM called for tests
	   01  WS-TEST-PGM PIC X(08) VALUE "GKCTRL01".
      / FILES STATUS 

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
      *    USING 
      *        BY REFERENCE LK-DUMMY-REF,
      *        BY VALUE     LK-DUMMY-VAL
      *    .
           PERFORM 0000-OFILES.
           PERFORM 1000-Main.
           PERFORM 9999-CFILES.
           GOBACK.

       0000-OFILES.
      ******************************************************************EDEFAY
      *  This routine should open file(s)
      *    OPEN INPUT FILEIN
		   DISPLAY "DO SOMETHING"
           .

       1000-Main.
      ******************************************************************EDEFAY
      *  This routine should follow the logic of the program purpose
           DISPLAY "DO SOMETHING"
           .

       9999-CFILES.
      ******************************************************************EDEFAY
      *  This routine should close file(s)
      *    CLOSE FILEIN
		   DISPLAY "DO SOMETHING"
           .

       CT00-FILE-OK.
      ******************************************************************EDEFAY
      *  This routine should 
		   DISPLAY "DO SOMETHING"
           .

       CT01-DDNAME.
      ******************************************************************EDEFAY
      *  This routine should
		   DISPLAY "DO SOMETHING"
		   .

       CT02-DSNAME-LEN.
      ******************************************************************EDEFAY
      *  This routine should
		   DISPLAY "DO SOMETHING"
		   .

       CT03-OTHER-OFILE.
      ******************************************************************EDEFAY
      *  This routine should
           DISPLAY "DO SOMETHING"
           .

       CT04-EMPTY-FILEIN.
      ******************************************************************EDEFAY
      *  This routine should
		   DISPLAY "DO SOMETHING"
		   .

       CT05-OPE-DIFF-FOOTER-SAID.
      ******************************************************************EDEFAY
      *  This routine should
		   DISPLAY "DO SOMETHING"
		   .

       CT06-SUM-OPER-DIFF-MT-FOOTER.
      ******************************************************************EDEFAY
      *  This routine should
           DISPLAY "DO SOMETHING"
           .

       CT07-NO-HEADER.
      ******************************************************************EDEFAY
      *  This routine should
		   DISPLAY "DO SOMETHING"
		   .

       CT08-NO-FOOTER.
      ******************************************************************EDEFAY
      *  This routine should
		   DISPLAY "DO SOMETHING"
		   .
