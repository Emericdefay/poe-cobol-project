//EXEPLAN JOB 'MASTER',                         
//            'MAC',                            
//            CLASS=A,                          
//            MSGCLASS=X,                       
//            NOTIFY=&SYSUID                    
//***********************************************************
//*                     LAUNCHER JCL
//* The JCL script is used to execute the GKMAJ001 program on
//* an IBM mainframe system.
//* The script includes a step called "CHECKFI" which runs 
//* the CHECKINGPGM program and checks for errors in the FLUX 
//* input file. 
//* If the return code (RC) is 0, the script will proceed to 
//* the next step called "RUNMAIN". This step runs the IKJEFT01 
//* program to execute the GKMAJ001 program, update the 
//* accounts according to the FLUX file and create the REPORT 
//* file if error(s) are still detected.
//*
//* The script includes several parameters that are used to
//* configure the job:
//*     CHECKINGPGM: specifies the program used for checking 
//*                  errors in the FLUX input file.
//*     FILEIN     : specifies the name of the input file.
//*     FILEOUT    : specifies the name of the output file.
//*     FOLDER     : specifies the folder where the input and 
//*                  output files are located.
//*     LIBLOAD    : specifies the library where the programs
//*                  and load modules are located.
//*     RUNLIB     : specifies the library where the ZOS
//*                  functionalities are located.
//* 
//* They are passed to the job and the program uses them to 
//* access the necessary files and libraries.
//***********************************************************
//*=========================PARAMETERS=======================    
// SET CHECKINGPGM=GKCTRL01       
// SET FILEIN=FLUX       
// SET FILEOUT=REPORT       
// SET FOLDER=MASTER.FILES          
// SET LIBLOAD=MASTER.PROG.LOAD        
//*====================ZOS-FUNCTIONNALITIES================== 
// SET RUNLIB=DSN910.DB9G.RUNLIB.LOAD           
//*==========================================================    
//*
//***********************************************************
//* Checking if errors found in FLUX input file
//***********************************************************
//CHECKFI EXEC PGM=&CHECKINGPGM
//STEPLIB DD DISP=SHR,DSN=&LIBLOAD  
//SYSOUT  DD SYSOUT=*
//FILEIN  DD DISP=SHR,DSN=&FOLDER(&FILEIN)
//*
//***********************************************************
//* Update accounts according to FLUX if no error found
//***********************************************************
//RUNMAIN IF RC=0 THEN
//STEP001 EXEC PGM=IKJEFT01                     
//STEPLIB   DD DISP=SHR,DSN=&RUNLIB             
//          DD DISP=SHR,DSN=&LIBLOAD            
//FILEIN    DD DISP=SHR,DSN=&FOLDER(&FILEIN)
//FILEOUT   DD DISP=SHR,DSN=&FOLDER(&FILEOUT)
//SYSTSPRT  DD SYSOUT=*                         
//SYSPRINT  DD SYSOUT=*                         
//SYSTSIN   DD  *                               
 DSN SYSTEM(DB9G)                               
     RUN -                    
        PROGRAM   (GKMAJ001) - 
        PLAN      (PLANCOB)  
     END                      
 END                         
/*  
// ENDIF