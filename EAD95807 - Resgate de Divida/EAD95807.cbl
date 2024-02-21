 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95807.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       **************************************************                
 000700       *       RESGATE DE DIVIDA                        *                
 000800       **************************************************                
 000900       *                                                                 
 001000        ENVIRONMENT DIVISION.                                            
 001100        CONFIGURATION SECTION.                                           
 001200        SPECIAL-NAMES.                                                   
 001300            DECIMAL-POINT IS COMMA.                                      
 001400       *                                                                 
 001500        DATA DIVISION.                                                   
 001600        WORKING-STORAGE SECTION.                                         
 001700        77  PRESTACAO      PIC 9(5)V99      VALUE ZEROS.                                         
 001900        77  TAXA           PIC 9V99         VALUE ZEROS.                 
 002000        77  DIVIDA         PIC 9(7)V99      VALUE ZEROS.                 
 002200        77  JUROS          PIC 9(5)V99      VALUE ZEROS.                 
 002300        77  MES            PIC 9(3)         VALUE ZEROS.                 
 002310        77  DIVIDA-EDIT    PIC Z.ZZZ.ZZ9,99 VALUE ZEROS.                 
 002320        77  PRESTACAO-EDIT PIC ZZ.ZZ9,99    VALUE ZEROS.                 
 002330        77  JUROS-EDIT     PIC ZZ.ZZ9,99    VALUE ZEROS.                 
 002400       *                                                                 
 002500        PROCEDURE DIVISION.                                              
 002600        UNICA SECTION.                                                   
 002700        INICIO.                                                          
 002710            ACCEPT DIVIDA    FROM SYSIN.                                 
 002800            ACCEPT PRESTACAO FROM SYSIN.                                 
 003000            ACCEPT TAXA      FROM SYSIN.                                 
 003100            PERFORM CALCULO UNTIL DIVIDA = 0.                            
 003300                DISPLAY 'A DIVIDA FOI PAGA ' MES                         
 003400                        ' MESES '.                                                                                                                                             
 003500            STOP RUN.                                                    
 003600        CALCULO.                                                         
 003700            COMPUTE JUROS = DIVIDA * TAXA / 100.                         
 003710            IF JUROS >= PRESTACAO                                        
 003720                DISPLAY 'PRESTACAO MUITO BAIXA'                          
 003730                STOP RUN                                                 
 003740            END-IF.                                                      
 003800            ADD JUROS    TO DIVIDA.                                      
 003810            IF DIVIDA < PRESTACAO                                        
 003820                MOVE DIVIDA TO PRESTACAO                                 
 003830            END-IF.                                                      
 003900            SUBTRACT PRESTACAO FROM DIVIDA.                              
 004000            ADD 1        TO MES.                                         
 004100            MOVE DIVIDA     TO DIVIDA-EDIT.                              
 004110            MOVE PRESTACAO  TO PRESTACAO-EDIT.                           
 004120            MOVE JUROS      TO JUROS-EDIT.                                                              
 004200            DISPLAY 'NO MES ' MES                                        
 004300                    'O JURO = ' JUROS-EDIT                               
 004400                    ', A PRESTACAO = ' PRESTACAO-EDIT                    
 004500                    'E A DIVIDA = ' DIVIDA-EDIT.                         
