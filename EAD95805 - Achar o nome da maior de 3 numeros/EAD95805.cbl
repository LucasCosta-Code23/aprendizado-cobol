 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95805.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       **************************************************                
 000700       *       ACHAR O NOME DA MAIOR DE 3 NUMEROS       *                
 000800       **************************************************                
 000900       *                                                                 
 001000        ENVIRONMENT DIVISION.                                            
 001100        CONFIGURATION SECTION.                                           
 001200        SPECIAL-NAMES.                                                   
 001300            DECIMAL-POINT IS COMMA.                                      
 001400       *                                                                 
 001500        DATA DIVISION.                                                   
 001600        WORKING-STORAGE SECTION.                                         
 001700        77  N1             PIC 9(2)     VALUE ZEROS.                                         
 001800        77  N2             PIC 9(2)     VALUE ZEROS.                     
 001900        77  N3             PIC 9(2)     VALUE ZEROS.                     
 002000       *                                                                 
 002100        PROCEDURE DIVISION.                                              
 002200        UNICA SECTION.                                                   
 002300        INICIO.                                                          
 002400            ACCEPT N1 FROM SYSIN.                                        
 002500            ACCEPT N2 FROM SYSIN.                                        
 002600            ACCEPT N3 FROM SYSIN.                                        
 002700            DISPLAY 'NUMEROS INFORMADOS: ' N1                            
 002800                    ', '                   N2                            
 002900                    ' E '                  N3.                           
 002910            EVALUATE TRUE                                                
 003000                WHEN N1 > N2 AND N1 > N3                                 
 003100                    DISPLAY 'A MAIOR VARIAVEL EH N1'                     
 003300                WHEN N2 > N1 AND N2 > N3                                 
 003400                    DISPLAY 'A MAIOR VARIAVEL EH N2'                                          
 003600                WHEN N3 > N1 AND N3 > N2                                 
 003700                    DISPLAY 'A MAIOR VARIAVEL EH N3'                     
 003900                WHEN N1 = N2 AND N1 > N3                                 
 004000                    DISPLAY 'AS MAIORES VARIAVEIS SAO N1 E N2'           
 004200                WHEN N1 = N3 AND N1 > N2                                 
 004210                    DISPLAY 'AS MAIORES VARIAVEIS SAO N1 E N3'           
 004230                WHEN N2 = N3 AND N2 > N1                                 
 004240                    DISPLAY 'AS MAIORES VARIAVEIS SAO N2 E N3'           
 004250                WHEN OTHER                                               
 004260                    DISPLAY 'AS MAIORES VARIAVEIS SAO N1,N2 E N3'        
 004270            END-EVALUATE.                                                
 004300            STOP RUN.                                                    
                                           