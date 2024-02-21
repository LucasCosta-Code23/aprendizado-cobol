 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95804.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       ********************************************                      
 000700       *       ACHAR O MAIOR DE 3 NUMEROS         *                      
 000800       ********************************************                      
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
 002700       *                                                                 
 002800        PROCEDURE DIVISION.                                              
 002900        UNICA SECTION.                                                   
 003000        INICIO.                                                          
 003100            ACCEPT N1 FROM SYSIN.                                        
 003200            ACCEPT N2 FROM SYSIN.                                        
 003300            ACCEPT N3 FROM SYSIN.                                        
 003310            DISPLAY 'NUMEROS INFORMADOS: ' N1                            
 003320                    ', '                   N2                            
 003330                    ' E '                  N3.                           
 003400            IF N1 GREATER N2                                             
 003500                IF N1 GREATER N3                                         
 003600                    DISPLAY 'O MAIOR NUMERO EH ' N1                      
 003700                ELSE                                                     
 003800                    DISPLAY 'O MAIOR NUMERO EH ' N3                                          
 003900                END-IF                                                   
 004000            ELSE                                                         
 004100                IF N2 GREATER N3                                         
 004200                    DISPLAY 'O MAIOR NUMERO EH ' N2                      
 004300                ELSE                                                     
 004400                    DISPLAY 'O MAIOR NUMERO EH ' N3                      
 004500                END-IF                                                   
 004600            END-IF.                                                      
 008400            STOP RUN.                                                    
