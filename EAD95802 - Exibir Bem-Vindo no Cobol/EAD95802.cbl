 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95802.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       ********************************************                      
 000700       *        EXIBIR BEM-VINDO NO COBOL         *                      
 000800       ********************************************                      
 000810       *                                                                 
 000900        ENVIRONMENT DIVISION.                                            
 001000       *                                                                 
 001100        DATA DIVISION.                                                   
 001110        WORKING-STORAGE SECTION.                                         
 001120        01  DATA-SIST.                                                   
 001130            03 ANO-SIST    PIC 9(2)  VALUE ZEROS.                        
 001140            03 MES-SIST    PIC 9(2)  VALUE ZEROS.                        
 001150            03 DIA-SIST    PIC 9(2)  VALUE ZEROS.                                               
 001160        01  DATA-EXIBE.                                                  
 001170            05 DIA-EXIBE   PIC 9(2)  VALUE ZEROS.                        
 001190            05 FILLER      PIC X     VALUE '/'.                          
 001191            05 MES-EXIBE   PIC 9(2)  VALUE ZEROS.                        
 001192            05 FILLER      PIC X(3)  VALUE '/20'.                        
 001193            05 ANO-EXIBE   PIC 9(2)  VALUE ZEROS.                        
 001194        01 HORA-SIST.                                                    
 001195           05 HOR-SIST     PIC 9(2)  VALUE ZEROS.                        
 001196           05 MIN-SIST     PIC 9(2)  VALUE ZEROS.                        
 001197           05 SEG-SIST     PIC 9(2)  VALUE ZEROS.                        
 001198           05 CEN-SIST     PIC 9(2)  VALUE ZEROS.                        
 001199        01 HORA-EXIBE.                                                   
 001200           05 HOR-EXIBE    PIC 9(2)  VALUE ZEROS.                        
 001201           05 FILLER       PIC X     VALUE ':'.                          
 001202           05 MIN-EXIBE    PIC 9(2)  VALUE ZEROS.                        
 001203           05 FILLER       PIC X     VALUE ':'.                          
 001204           05 SEG-EXIBE    PIC 9(2)  VALUE ZEROS.                                                
 001205        77 NOME            PIC A(10) VALUE SPACES.                       
 001210       *                                                                 
 001300        PROCEDURE DIVISION.                                              
 001400        UNICA SECTION.                                                   
 001500        INICIO.                                                          
 001600            ACCEPT NOME      FROM SYSIN.                                 
 001610            ACCEPT DATA-SIST FROM DATE.                                  
 001620            ACCEPT HORA-SIST FROM TIME.                                  
 001630            MOVE   DIA-SIST  TO DIA-EXIBE.                               
 001640            MOVE   MES-SIST  TO MES-EXIBE.                               
 001650            MOVE   ANO-SIST  TO ANO-EXIBE.                               
 001660            MOVE   HOR-SIST  TO HOR-EXIBE.                               
 001670            MOVE   MIN-SIST  TO MIN-EXIBE.                               
 001680            MOVE   SEG-SIST  TO SEG-EXIBE.                               
 001690            DISPLAY 'OLA ' NOME                                          
 001691                    ', BEM-VINDO AO CURSO DE COBOL'.                     
 001692            DISPLAY 'A DATA EH: ' DATA-EXIBE.                                                    
 001693            DISPLAY 'HORA: '      HORA-EXIBE.                            
 001700            STOP RUN.                                                    
