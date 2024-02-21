 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95806.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       **************************************************                
 000700       *       CALCULAR O SALDO DE INVESTIMENTO         *                
 000800       **************************************************                
 000900       *                                                                 
 001000        ENVIRONMENT DIVISION.                                            
 001100        CONFIGURATION SECTION.                                           
 001200        SPECIAL-NAMES.                                                   
 001300            DECIMAL-POINT IS COMMA.                                      
 001400       *                                                                 
 001500        DATA DIVISION.                                                   
 001600        WORKING-STORAGE SECTION.                                         
 001700        77  DEPOSITO       PIC 9(5)V99  VALUE ZEROS.                                         
 001800        77  PRAZO          PIC 9(3)         VALUE ZEROS.                 
 001900        77  TAXA           PIC 9V99         VALUE ZEROS.                 
 001910        77  SALDO          PIC 9(7)V99      VALUE ZEROS.                 
 001920        77  SALDO-EDIT     PIC Z.ZZZ.ZZ9,99 VALUE ZEROS.                 
 001930        77  RENDA          PIC 9(5)V99      VALUE ZEROS.                 
 001940        77  MES            PIC 9(3)         VALUE ZEROS.                 
 002000       *                                                                 
 002100        PROCEDURE DIVISION.                                              
 002200        UNICA SECTION.                                                   
 002300        INICIO.                                                          
 002400            ACCEPT DEPOSITO FROM SYSIN.                                  
 002500            ACCEPT PRAZO    FROM SYSIN.                                  
 002600            ACCEPT TAXA     FROM SYSIN.                                  
 002700            PERFORM CALCULO PRAZO TIMES.                                 
 002800            MOVE SALDO      TO SALDO-EDIT.                               
 002900            DISPLAY 'SALDO APOS ' PRAZO                                  
 003000                    ' MESES : '   SALDO-EDIT.                                                        
 003010            STOP RUN.                                                    
 003100        CALCULO.                                                         
 003200            COMPUTE RENDA = SALDO * TAXA / 100.                          
 003300            ADD RENDA    TO SALDO.                                       
 003400            ADD DEPOSITO TO SALDO.                                       
 003500            ADD 1        TO MES.                                         
 003600            MOVE SALDO   TO SALDO-EDIT.                                  
 003700            DISPLAY 'SALDO NO MES ' MES                                  
 003800                    ' = '           SALDO-EDIT.                          
              