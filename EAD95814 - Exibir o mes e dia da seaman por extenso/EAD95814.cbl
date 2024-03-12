 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95814                                                     
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000900       *                                                                 
 000910       ************************************************************      
 000920       *       EXIBIR O MES E DIA DA SEMANA POR EXTENSO           *      
 000930       ************************************************************      
 001000        ENVIRONMENT DIVISION.                                            
 001100        CONFIGURATION SECTION.                                           
 001200        SPECIAL-NAMES.                                                   
 001300            DECIMAL-POINT IS COMMA.                                      
 001400        INPUT-OUTPUT SECTION.                                            
 001500        FILE-CONTROL.                                                    
 001800       *                                                                 
 001900        DATA DIVISION.                                                                                                    
 002000        FILE SECTION.                                                    
 002900        WORKING-STORAGE SECTION.                                         
 003000        01  WK-DATA-ACCEPT.                                              
 003100            05 WK-ANO-ACCEPT          PIC 99    VALUE ZEROS.             
 003200            05 WK-MES-ACCEPT          PIC 99    VALUE ZEROS.             
 003300            05 WK-DIA-ACCEPT          PIC 99    VALUE ZEROS.             
 003400        77  WK-DIASEM-ACCEPT          PIC 9     VALUE ZEROS.             
 003500        01  WK-AREA-COM.                                                 
 003600            05 WK-COD-MES-COM         PIC 99    VALUE ZEROS.             
 003700            05 WK-COD-DIASEM-COM      PIC 9     VALUE ZEROS.             
 003800            05 WK-NOME-MES-COM        PIC X(9)  VALUE SPACES.            
 003900            05 WK-NOME-DIASEM-COM     PIC X(13) VALUE SPACES.            
 003910        77  WK-NOME-PROG              PIC X(8)  VALUE SPACES.            
 004000       *                                                                 
 004100        PROCEDURE DIVISION.                                              
 004200        000-PRINCIPAL SECTION.                                           
 004300        001-PRINCIPAL.                                                                                                     
 004400            PERFORM 101-INICIAR.                                         
 004500            PERFORM 201-PROCESSAR.                                       
 004600            PERFORM 901-FINALIZAR.                                       
 004700            STOP RUN.                                                    
 004800       ***********************************************************       
 004900        100-INICIAR SECTION.                                             
 005000        101-INICIAR.                                                     
 005100            ACCEPT WK-DATA-ACCEPT FROM DATE.                             
 005200            ACCEPT WK-DIASEM-ACCEPT FROM DAY-OF-WEEK.                    
 006600       ***********************************************                   
 006700        200-PROCESSAR SECTION.                                           
 006800        201-PROCESSAR.                                                   
 006810            MOVE WK-MES-ACCEPT    TO WK-COD-MES-COM.                     
 006820            MOVE WK-DIASEM-ACCEPT TO WK-COD-DIASEM-COM.                  
 006900            MOVE 'EAD95815'       TO WK-NOME-PROG.                       
 007100            CALL WK-NOME-PROG USING WK-AREA-COM.                         
 009900       ***********************************************                                     
 010000        900-FINALIZAR SECTION.                                           
 010100        901-FINALIZAR.                                                   
 010200            DISPLAY 'DIA : '  WK-DIA-ACCEPT.                             
 010300            DISPLAY 'MES : '  WK-NOME-MES-COM.                           
 010400            DISPLAY 'ANO :20' WK-ANO-ACCEPT.                             
 010500            DISPLAY 'DIA DA SEMANA : ' WK-NOME-DIASEM-COM.  