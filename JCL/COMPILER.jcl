//DB2COMP JOB 'MASTER',                                      
//            'MAC',                                         
//            CLASS=A,                                       
//            MSGCLASS=X,                                    
//            NOTIFY=&SYSUID                                 
//*================ACCESSORS=================                
//      SET PGMCPT=MACPT
//      SET PGMOPE=MAOPE
//      SET PGMDEV=MADEV
//      SET PGMHIS=MAHIS
//      SET ACCPATH=MASTER.SQL.ACCESSORS
//*=================PROGRAM==================                           
//      SET PGMPATH=MASTER.PROG.CBL                          
//      SET COPYPATH=MASTER.PROG.COPY                        
//      SET LOADPATH=MASTER.PROG.LOAD                 
//      SET FILEIN=FILEIN                           
//      SET FILEOUT=FILEOUT                        
//      SET FOLDER=MASTER.FILES                       
//*==============DCLGEN PATH=================         
//      SET DCLGENPT=MASTER.SQL.DCLGEN                
//*==============DBRM PATH===================         
//      SET DBRMPTH=MASTER.SQL.DBRM                   
//*===========IGY SPECIFICATION==============         
//      SET IGYSPE=IGY410.SIGYCOMP                    
//      SET SCLKED=CEE.SCEELKED                    
//*==============runlib spec=================         
//      SET RUNLIB=DSN910.DB9G.RUNLIB.LOAD            
//*==========================================    
//*                                                          
//* Binding                                                  
//*                                                          
//DBPLBND    EXEC PGM=IKJEFT01                               
//STEPLIB      DD DISP=SHR,DSN=&RUNLIB                       
//DBRMLIB      DD DISP=SHR,DSN=&DBRMPTH                      
//SYSTSPRT     DD SYSOUT=*                                   
//SYSPRINT     DD SYSOUT=*                                   
//SYSTSIN      DD  *                                         
 DSN SYSTEM(DB9G)                                            
    BIND -                                                   
        PLAN      (PLANCOB) -                                
        OWNER     (IBMUSER) -                                
        PKLIST    (COL1.*)                                   
    END                                                      
 END                                                         
/*                                     
//******************MACPT********************  
//* Precompilation                                    
//*                                                   
//PRECOMPL   EXEC DSNHICOB,MEM=&PGMCPT               
//PC.SYSIN     DD DISP=SHR,DSN=&ACCPATH(&MEM)         
//PC.SYSLIB    DD DISP=SHR,DSN=&DCLGENPT              
//PC.DBRMLIB   DD DISP=SHR,DSN=&DBRMPTH(&MEM)         
//*                                                   
//* Compilation                                              
//*                                                          
//COB.STEPLIB  DD DISP=SHR,DSN=&IGYSPE
//COB.SYSLIB   DD DISP=SHR,DSN=&COPYPATH
//LKED.SYSLMOD DD DISP=SHR,DSN=&LOADPATH(&MEM)
//*      
//* Bind package     
//*  
//DBPKBND    EXEC PGM=IKJEFT01                               
//STEPLIB      DD DISP=SHR,DSN=&RUNLIB                       
//DBRMLIB      DD DISP=SHR,DSN=&DBRMPTH                      
//SYSTSPRT     DD SYSOUT=*                                   
//SYSPRINT     DD SYSOUT=*                                   
//SYSTSIN      DD *                                          
 DSN SYSTEM(DB9G)                                            
    BIND -                                                   
        PACKAGE   (COL1)    -                          
        MEMBER    (MACPT)   -                          
        OWNER     (IBMUSER) -                          
        QUALIFIER (IBMUSER) -                          
        ACTION    (REPLACE) -                          
        VALIDATE  (BIND)    -                          
        ENCODING  (EBCDIC)  -                          
        ISOLATION (CS)                                 
    END                                                
 END                                                   
/*                        
//******************MAOPE********************       
//* Precompilation                                    
//*                                                   
//PRECOMPL   EXEC DSNHICOB,MEM=&PGMOPE                
//PC.SYSIN     DD DISP=SHR,DSN=&ACCPATH(&MEM)         
//PC.SYSLIB    DD DISP=SHR,DSN=&DCLGENPT              
//PC.DBRMLIB   DD DISP=SHR,DSN=&DBRMPTH(&MEM)         
//*                                                   
//* Compilation                                              
//*                                                          
//COB.STEPLIB  DD DISP=SHR,DSN=&IGYSPE
//COB.SYSLIB   DD DISP=SHR,DSN=&COPYPATH
//LKED.SYSLMOD DD DISP=SHR,DSN=&LOADPATH(&MEM)
//*      
//* Bind package     
//*      
//DBPKBND    EXEC PGM=IKJEFT01                               
//STEPLIB      DD DISP=SHR,DSN=&RUNLIB                       
//DBRMLIB      DD DISP=SHR,DSN=&DBRMPTH                      
//SYSTSPRT     DD SYSOUT=*                                   
//SYSPRINT     DD SYSOUT=*                                   
//SYSTSIN      DD *                                          
 DSN SYSTEM(DB9G)                                            
    BIND -                                                   
        PACKAGE   (COL1)    -                          
        MEMBER    (MAOPE)   -                          
        OWNER     (IBMUSER) -                          
        QUALIFIER (IBMUSER) -                          
        ACTION    (REPLACE) -                          
        VALIDATE  (BIND)    -                          
        ENCODING  (EBCDIC)  -                          
        ISOLATION (CS)                                 
    END                                                
 END                                                   
/*                        
//******************MADEV********************           
//* Precompilation                                    
//*                                                   
//PRECOMPL   EXEC DSNHICOB,MEM=&PGMDEV
//PC.SYSIN     DD DISP=SHR,DSN=&ACCPATH(&MEM)         
//PC.SYSLIB    DD DISP=SHR,DSN=&DCLGENPT              
//PC.DBRMLIB   DD DISP=SHR,DSN=&DBRMPTH(&MEM)         
//*                                                   
//* Compilation                                              
//*                                                          
//COB.STEPLIB  DD DISP=SHR,DSN=&IGYSPE
//COB.SYSLIB   DD DISP=SHR,DSN=&COPYPATH
//LKED.SYSLMOD DD DISP=SHR,DSN=&LOADPATH(&MEM)
//*      
//* Bind package     
//*               
//DBPKBND    EXEC PGM=IKJEFT01                               
//STEPLIB      DD DISP=SHR,DSN=&RUNLIB                       
//DBRMLIB      DD DISP=SHR,DSN=&DBRMPTH                      
//SYSTSPRT     DD SYSOUT=*                                   
//SYSPRINT     DD SYSOUT=*                                   
//SYSTSIN      DD *                                          
 DSN SYSTEM(DB9G)                                            
    BIND -                                                   
        PACKAGE   (COL1)    -                          
        MEMBER    (MADEV)   -                          
        OWNER     (IBMUSER) -                          
        QUALIFIER (IBMUSER) -                          
        ACTION    (REPLACE) -                          
        VALIDATE  (BIND)    -                          
        ENCODING  (EBCDIC)  -                          
        ISOLATION (CS)                                 
    END                                                
 END                                                   
/*                        
//******************MAHIS********************              
//* Precompilation                                    
//*                                                   
//PRECOMPL   EXEC DSNHICOB,MEM=&PGMHIS               
//PC.SYSIN     DD DISP=SHR,DSN=&ACCPATH(&MEM)         
//PC.SYSLIB    DD DISP=SHR,DSN=&DCLGENPT              
//PC.DBRMLIB   DD DISP=SHR,DSN=&DBRMPTH(&MEM)         
//*                                                   
//* Compilation                                              
//*                                                          
//COB.STEPLIB  DD DISP=SHR,DSN=&IGYSPE
//COB.SYSLIB   DD DISP=SHR,DSN=&COPYPATH
//LKED.SYSLMOD DD DISP=SHR,DSN=&LOADPATH(&MEM)
//*      
//* Bind package     
//*       
//DBPKBND    EXEC PGM=IKJEFT01                               
//STEPLIB      DD DISP=SHR,DSN=&RUNLIB                       
//DBRMLIB      DD DISP=SHR,DSN=&DBRMPTH                      
//SYSTSPRT     DD SYSOUT=*                                   
//SYSPRINT     DD SYSOUT=*                                   
//SYSTSIN      DD *                                          
 DSN SYSTEM(DB9G)                                            
    BIND -                                                   
        PACKAGE   (COL1)    -                          
        MEMBER    (MAHIS)   -                          
        OWNER     (IBMUSER) -                          
        QUALIFIER (IBMUSER) -                          
        ACTION    (REPLACE) -                          
        VALIDATE  (BIND)    -                          
        ENCODING  (EBCDIC)  -                          
        ISOLATION (CS)                                 
    END                                                
 END                                                   
/*                        
//*******************************************      
//*                                                   
//* Compilation CTRL                                             
//*                      
// SET PGMCBL=GKCTRL01                                  
//CALLIGY    EXEC IGYWCL
//STEPLIB      DD DISP=SHR,DSN=&IGYSPE
//COBOL.SYSIN  DD DISP=SHR,DSN=&PGMPATH(&PGMCBL)
//LKED.SYSLIB  DD DISP=SHR,DSN=&LOADPATH
//             DD DISP=SHR,DSN=&SCLKED
//LKED.SYSLMOD DD DISP=SHR,DSN=&LOADPATH(&PGMCBL)
//
//*                                                   
//* Compilation UPDATE                                             
//*                                                      
// SET PGMCBL=MFMAJCPT                                   
//CALLIGY    EXEC IGYWCL
//STEPLIB      DD DISP=SHR,DSN=&IGYSPE
//COBOL.SYSIN  DD DISP=SHR,DSN=&PGMPATH(&PGMCBL)
//LKED.SYSLIB  DD DISP=SHR,DSN=&LOADPATH
//             DD DISP=SHR,DSN=&SCLKED
//LKED.SYSLMOD DD DISP=SHR,DSN=&LOADPATH(&PGMCBL)
//*                                                   
//* Compilation MAIN                                             
//*                                                  
// SET PGMCBL=GKMAJ001                                   
//CALLIGY    EXEC IGYWCL
//STEPLIB      DD DISP=SHR,DSN=&IGYSPE
//COBOL.SYSIN  DD DISP=SHR,DSN=&PGMPATH(&PGMCBL)
//LKED.SYSLIB  DD DISP=SHR,DSN=&LOADPATH
//             DD DISP=SHR,DSN=&SCLKED
//LKED.SYSLMOD DD DISP=SHR,DSN=&LOADPATH(&PGMCBL)
//*******************************************      