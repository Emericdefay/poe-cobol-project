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
           SELECT FILEIN-FDNAME
           ASSIGN TO DYNAMIC FILEIN-NAME
           FILE STATUS IS WS-FS-FLUX
           ORGANIZATION IS LINE SEQUENTIAL.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD  FILEIN-FDNAME RECORDING MODE F.
       01  FILEIN-RECORD.        
          05  FILLER      PIC X(80).

      ******************************************************************
       WORKING-STORAGE SECTION.
      /  FILE
       01  FILEIN-NAME    PIC X(255).
       01  WS-FS-FLUX     PIC X(02).
      *    STATUS OK 
           88 FS-FLUX-OK  VALUE '00'.
      *    STATUS END FILE 
           88 FS-FLUX-END VALUE '10'.
      *    STATUS DDNAME not found 
           88 FS-FLUX-DDN VALUE '35'.
      *    STATUS LENGHT or TYPE different 
           88 FS-FLUX-LEN VALUE '39'.
       01  WS-VARS.
      /  Counters
           05  WS-LUS-00      PIC 9(06).
           05  WS-LUS-10      PIC 9(06).
           05  WS-LUS-99      PIC 9(06).
      /  Operations
           05  WS-MT-GLOBAL   PIC 9(11)V99.
      /  Copybook
       COPY 'CFLUX.cpy'.

      ******************************************************************
       LINKAGE SECTION.
      /  DYNAMIC FILE 
       01  FILEIN-DDNAME PIC X(30).
      /  RETURN CODE
       01  RC            PIC X(02).
           88 RC-IS-00   VALUE '00'.

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
       PROCEDURE DIVISION USING 
                            FILEIN-DDNAME,
                            RC.
           PERFORM 1000-Main.
           EXIT PROGRAM.

       0000-OFILES.
      ******************************************************************EDEFAY
      *  This routine should open file(s).          
           MOVE FILEIN-DDNAME TO FILEIN-NAME
           OPEN INPUT FILEIN-FDNAME
      
           IF FS-FLUX-DDN
               MOVE '01' TO RC
               PERFORM 1999-FIN
           END-IF
           IF FS-FLUX-LEN THEN
               MOVE '02' TO RC
               PERFORM 1999-FIN
           END-IF
           .

       0100-READ-FILEIN.
      ******************************************************************EDEFAY
      *  This routine should read FILEIN file.
           READ FILEIN-FDNAME
           INTO FILEIN-RECORD
           END-READ
           .

       1000-Main.
      ******************************************************************EDEFAY
      *  This routine should follow the logic of the program purpose.
           PERFORM 1001-DEBUT
           IF RC-IS-00 THEN
               PERFORM UNTIL (FS-FLUX-END)
                   PERFORM 1500-TRAITEMENT
               END-PERFORM
           END-IF
           PERFORM 1999-FIN
           EXIT PROGRAM
           .

       1001-DEBUT.
      ******************************************************************EDEFAY
      *  This routine should initialize vars and check if file is empty.
      *    Initialize vars
           INITIALIZE WS-VARS
      *    Setup file
           PERFORM 0000-OFILES
           IF RC-IS-00 THEN
               PERFORM 0100-READ-FILEIN
           END-IF
           IF FS-FLUX-END THEN
               MOVE '04' TO RC
               PERFORM 1999-FIN
           END-IF
           .

       1500-TRAITEMENT.
      ******************************************************************EDEFAY
      *  This routine should increment WS-LUS-xx vars and price into
      *  OPER AMOUNT vars. Updating RC if needed.  
           MOVE FILEIN-RECORD TO F1-ENREG-00
           EVALUATE TRUE
               WHEN F1-TYPE-00 = '00'
                   ADD 1 TO WS-LUS-00
               WHEN F1-TYPE-00 = '10'
                   ADD 1 TO WS-LUS-10
                   ADD F1-MONTANT-OPER TO WS-MT-GLOBAL
               WHEN F1-TYPE-00 = '99'
                   ADD 1 TO WS-LUS-99
                   IF F1-NB-OPERATIONS NOT = WS-LUS-10    THEN
                       MOVE '05' TO RC
                       PERFORM 1999-FIN
                   END-IF
                   IF F1-MT-GLOBAL     NOT = WS-MT-GLOBAL THEN
                       MOVE '06' TO RC
                       PERFORM 1999-FIN
                   END-IF
               WHEN OTHER
                   MOVE '03' TO RC
                   PERFORM 1999-FIN
           END-EVALUATE
           PERFORM 0100-READ-FILEIN
           .

       1999-FIN.
      ******************************************************************EDEFAY
      *  This routine should end the program, updating RC if needed.
           IF RC-IS-00 THEN
      *        No header issue
               IF WS-LUS-00 = 0 THEN
                   MOVE '07' TO RC
               END-IF
      *        No footer issue
               IF WS-LUS-99     = 0
              AND WS-LUS-00 NOT = 0 THEN
                   MOVE '08' TO RC
               END-IF
           END-IF
      *    Close file
           PERFORM 9999-CFILES
      *    Exit program
           EXIT PROGRAM
           .

       9999-CFILES.
      ******************************************************************EDEFAY
      *  This routine should close file(s).
           CLOSE FILEIN-FDNAME
           .
