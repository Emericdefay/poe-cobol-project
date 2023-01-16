      *PROCESS TEST
      ******************************************************************
      * Program name   : TSTMAIN                               
      * Original author: DEFAY E.                                
      *
      * Description    : This routine is a functionnal test for the
      *                  entire program.
      *
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 13/01/23  EDEFAY        Create functionnal test        
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSTMAIN.
       AUTHOR.        DEFAY E. 
       INSTALLATION.  COBOL DEVELOPMENT CENTER. 
       DATE-WRITTEN.  13/01/23. 
       DATE-COMPILED. 13/01/23. 
       SECURITY.      NON-CONFIDENTIAL.
      ******************************************************************
      ******************************************************************
       DATA DIVISION.
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
           DISPLAY "  Functionnal test :"
           PERFORM CT00-FILE-OK
           PERFORM 2500-CHECK-NO-ERROR
           PERFORM CT00-FUNCTIONNAL-OK
           DISPLAY "Ending tests."
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
           MOVE "./FLUX.txt" TO FILEIN-DDNAME
           MOVE '00' TO RC
           CALL "GKCTRL01" USING FILEIN-DDNAME, RC
           IF RC-00-EXPECTED THEN
               DISPLAY '    TEST 00 PASSED.'
           ELSE
               DISPLAY '    TEST 00 FAILED.'
               PERFORM 1200-INC-ERROR
           END-IF
           .
       
       CT00-FUNCTIONNAL-OK.
      ******************************************************************EDEFAY
      *  This routine should return no error.
           CALL 'GKMAJ001'
           DISPLAY '    TEST MAIN PASSED.'
           .
