      *PROCESS TEST
      ******************************************************************
      * Program name   : GKCTRL01                               
      * Original author: DEFAY E.                                
      *
      * Description    : This routine check the operands flow file.
      *                  Returning RC, according to some rules.
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
      *
      * Using           : 
      *    FILEIN       :
      *        FDNAME   : FILEIN
      *        DDNAME   : FILEIN
      *
      *    COPYBOOKS    : 
      *        CFLUX    : Operands flow file Data structure
      *
      * Return          : (RC)
      *
      *                ---------------------------------                
      * Maintenance Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 09/01/23  EDEFAY        Create a blank template        
      * 09/01/23  EDEFAY        Making dynamic file calls
      *                                                               
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GKCTRL01.
       AUTHOR.        DEFAY E. 
       INSTALLATION.  COBOL DEVELOPMENT CENTER. 
       DATE-WRITTEN.  09/01/23. 
       DATE-COMPILED. 09/01/23. 
       SECURITY.      NON-CONFIDENTIAL.
      ******************************************************************
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT FILEIN-FDNAME
           ASSIGN TO DYNAMIC FILEIN-NAME
           ORGANIZATION IS LINE SEQUENTIAL.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD  FILEIN-FDNAME RECORDING MODE F
           RECORD CONTAINS 80 CHARACTERS.
       01  FILEIN-RECORD.        
          05  FILLER     PIC X(80).

      ******************************************************************
       WORKING-STORAGE SECTION.
      / FILES STATUS 
       01  FILEIN-NAME PIC X(255).
      / FILES STATUS 
       01  FS-FILEIN PIC X(02).
           88 FS-FILEIN-END VALUE "10".

      / IMPORT SQLCA
      *    EXEC SQL INCLUDE SQLCA 
      *    END-EXEC.

      / DECLARATIONS DCLGEN
      *    EXEC SQL INCLUDE DCLDUMMY  END-EXEC.
       LINKAGE SECTION.
      ******************************************************************
      /  DYNAMIC FILE 
       01  FILEIN-DDNAME PIC X(12).
      *01  RC PIC 9(04).

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
       PROCEDURE DIVISION USING     FILEIN-DDNAME
                          RETURNING RC.
           PERFORM 0000-OFILES.
           PERFORM 1000-Main.
           PERFORM 9999-CFILES.
           GOBACK.

       0000-OFILES.
      ******************************************************************EDEFAY
      *  This routine should open file(s)
           MOVE FILEIN-DDNAME TO FILEIN-NAME
           OPEN INPUT FILEIN-FDNAME
           .

       0100-READ-FILEIN.
      ******************************************************************EDEFAY
      *  This routine should read FILEIN file
           READ FILEIN-FDNAME
           INTO FILEIN-RECORD
           END-READ
           .

       1000-Main.
      ******************************************************************EDEFAY
      *  This routine should follow the logic of the program purpose
           DISPLAY "DO SOMETHING"
           .

       9999-CFILES.
      ******************************************************************EDEFAY
      *  This routine should close file(s)
           CLOSE FILEIN-FDNAME
           .
