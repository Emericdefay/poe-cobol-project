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
      * 09/01/23  EDEFAY        Making logic       
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
      * 01  FILEIN-STRUCT.        
      *   05  FILLER     PIC X(80).

      ******************************************************************
       WORKING-STORAGE SECTION.
      *  PGM called for tests
	   01  FILEIN-DDNAME PIC X(30).
      *  Errors
       01 NUM-ERRORS     PIC 9(02) VALUE 0.
      *  RC checked
       01  RC            PIC X(02) VALUE '00'.
           88 RC-00-EXPECTED       VALUE '00'.
           88 RC-01-EXPECTED       VALUE '01'.
           88 RC-02-EXPECTED       VALUE '02'.
           88 RC-03-EXPECTED       VALUE '03'.
           88 RC-04-EXPECTED       VALUE '04'.
           88 RC-05-EXPECTED       VALUE '05'.
           88 RC-06-EXPECTED       VALUE '06'.
           88 RC-07-EXPECTED       VALUE '07'.
           88 RC-08-EXPECTED       VALUE '08'.

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
           PERFORM CT00-FILE-OK
           PERFORM CT01-DDNAME
           PERFORM CT02-DSNAME-LEN
           PERFORM CT03-OTHER-OFILE
           PERFORM CT04-EMPTY-FILEIN
           PERFORM CT05-OPE-DIFF-FOOTER-SAID
           PERFORM CT06-SUM-OPER-DIFF-MT-FOOTER
           PERFORM CT07-NO-HEADER
           PERFORM CT08-NO-FOOTER

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

       CT00-FILE-OK.
      ******************************************************************EDEFAY
      *  This routine should make RC = 00
           MOVE "tests/FLUX-00.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           IF RC-00-EXPECTED THEN
               DISPLAY '    TEST 00 PASSED.'
           ELSE
               DISPLAY '    TEST 00 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT01-DDNAME.
      ******************************************************************EDEFAY
      *  This routine should make RC = 01
           MOVE "tests/FLUX-01.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           IF RC-01-EXPECTED THEN
               DISPLAY '    TEST 01 PASSED.'
           ELSE
               DISPLAY '    TEST 01 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
		   .

       CT02-DSNAME-LEN.
      ******************************************************************EDEFAY
      *  This routine should make RC = 02
           MOVE "tests/FLUX-02.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           DISPLAY    '    TEST 02 DENIED FOR LINUX.'
      *    IF RC-02-EXPECTED THEN
      *        DISPLAY '    TEST 02 PASSED.'
      *    ELSE
      *        DISPLAY '    TEST 02 FAILED.'
      *        PERFORM 1200-INC-ERROR
      *    END-IF
           .

       CT03-OTHER-OFILE.
      ******************************************************************EDEFAY
      *  This routine should make RC = 03
           MOVE "tests/FLUX-03.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           IF RC-03-EXPECTED THEN
               DISPLAY '    TEST 03 PASSED.'
           ELSE
               DISPLAY '    TEST 03 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT04-EMPTY-FILEIN.
      ******************************************************************EDEFAY
      *  This routine should make RC = 04
           MOVE "tests/FLUX-04.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           IF RC-04-EXPECTED THEN
               DISPLAY '    TEST 04 PASSED.'
           ELSE
               DISPLAY '    TEST 04 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
		   .

       CT05-OPE-DIFF-FOOTER-SAID.
      ******************************************************************EDEFAY
      *  This routine should make RC = 05
           MOVE "tests/FLUX-05.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           IF RC-05-EXPECTED THEN
               DISPLAY '    TEST 05 PASSED.'
           ELSE
               DISPLAY '    TEST 05 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
		   .

       CT06-SUM-OPER-DIFF-MT-FOOTER.
      ******************************************************************EDEFAY
      *  This routine should make RC = 06
           MOVE "tests/FLUX-06.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           IF RC-06-EXPECTED THEN
               DISPLAY '    TEST 06 PASSED.'
           ELSE
               DISPLAY '    TEST 06 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .

       CT07-NO-HEADER.
      ******************************************************************EDEFAY
      *  This routine should make RC = 07
           MOVE "tests/FLUX-07.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           IF RC-07-EXPECTED THEN
               DISPLAY '    TEST 07 PASSED.'
           ELSE
               DISPLAY '    TEST 07 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
		   .

       CT08-NO-FOOTER.
      ******************************************************************EDEFAY
      *  This routine should make RC = 08
           MOVE "tests/FLUX-08.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           IF RC-08-EXPECTED THEN
               DISPLAY '    TEST 08 PASSED.'
           ELSE
               DISPLAY '    TEST 08 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
		   .
