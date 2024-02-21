 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95803.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       ********************************************                      
 000700       *          OPERACOES ARITMETICAS           *                      
 000800       ********************************************                      
 000900       *                                                                 
 001000        ENVIRONMENT DIVISION.                                            
 001010        CONFIGURATION SECTION.                                           
 001020        SPECIAL-NAMES.                                                   
 001030            DECIMAL-POINT IS COMMA.                                      
 001100       *                                                                 
 001200        DATA DIVISION.                                                   
 001300        WORKING-STORAGE SECTION.                                         
 001400        77  N1             PIC 9(2)V9     VALUE ZEROS.                                     
 001500        77  N2             PIC 9(2)V9     VALUE ZEROS.                   
 001600        77  N3             PIC S9(3)V9    VALUE ZEROS.                   
 001700        77  N1-EDIT        PIC Z9,9       VALUE ZEROS.                   
 001800        77  N2-EDIT        PIC Z9,9       VALUE ZEROS.                   
 001900        77  N3-EDIT        PIC -ZZ9,9     VALUE ZEROS.                   
 002000        77  N4             PIC 9(2)       VALUE ZEROS.                   
 002100        77  N5             PIC 9(2)       VALUE ZEROS.                   
 002200        77  N6             PIC 9(2)       VALUE ZEROS.                   
 002300        77  N7             PIC 9(2)       VALUE ZEROS.                   
 003600       *                                                                 
 003700        PROCEDURE DIVISION.                                              
 003800        UNICA SECTION.                                                   
 003900        INICIO.                                                          
 004000            ACCEPT N1 FROM SYSIN.                                        
 004100            ACCEPT N2 FROM SYSIN.                                        
 004101            ACCEPT N4 FROM SYSIN.                                        
 004102            ACCEPT N5 FROM SYSIN.                                        
 004110            MOVE   N1 TO   N1-EDIT.                                      
 004120            MOVE   N2 TO   N2-EDIT.                                      
 004200       *      SOMA USANDO TO (ACUMULAR)                                  
 004300            MOVE 10   TO N3.                                             
 004400            ADD N1 N2 TO N3.                                             
 004410            MOVE N3   TO N3-EDIT.                                        
 004500            DISPLAY 'A SOMA DE '  N1-EDIT                                
 004600                    ' COM '       N2-EDIT                                
 004700                    ' MAIS 10 = ' N3-EDIT.                               
 004800       *      SOMA USANDO GIVING (N O ACUMULAR)                          
 005000            ADD N1 N2 GIVING N3.                                         
 005100            MOVE N3   TO N3-EDIT.                                        
 005200            DISPLAY 'A SOMA DE '  N1-EDIT                                
 005210                    ' COM '       N2-EDIT                                
 005220                    ' = '         N3-EDIT.                               
 005230       *      SUBTRACAO                                                  
 005240            SUBTRACT N2 FROM N1 GIVING N3.                                                             
 005250            MOVE N3   TO N3-EDIT.                                        
 005260            DISPLAY 'A DIFERENCA DE ' N1-EDIT                            
 005270                    ' E '             N2-EDIT                            
 005280                    ' = '             N3-EDIT.                           
 005290       *      MULTIPLICACAO                                              
 005291            MULTIPLY N1 BY N2 GIVING N3 ROUNDED                          
 005292                ON SIZE ERROR                                            
 005293                    DISPLAY 'ESTOURO DO RESULTADO DA MULTIPLICACAO'      
 005294                NOT ON SIZE ERROR                                        
 005297                    MOVE N3   TO N3-EDIT                                 
 005298                    DISPLAY 'O PRODUTO DE '   N1-EDIT                    
 005299                            ' E '             N2-EDIT                    
 005300                            ' = '             N3-EDIT                    
 005310            END-MULTIPLY.                                                
 005320       *      DIVISAO                                                    
 005330            DIVIDE N1 BY N2 GIVING N3.                                   
 005340            MOVE N3   TO N3-EDIT.                                                                               
 005350            DISPLAY 'A DIVISAO DE ' N1-EDIT                              
 005360                    ' POR '         N2-EDIT                              
 005370                    ' = '           N3-EDIT.                             
 005380       *      DIVISAO COM RESTO                                          
 005390            DIVIDE N4 BY N5 GIVING N6 REMAINDER N7.                      
 005392            DISPLAY 'A DIVISAO DE ' N4                                   
 005393                    ' POR '         N5                                   
 005394                    ' = '           N6                                   
 005395                    ' COM RESTO = ' N7.                                  
 005396       *      CALCULAR A MEDIA ENTRE N1 E N2                             
 005397            COMPUTE N3 = (N1 + N2) / 2.                                  
 005398            MOVE N3   TO N3-EDIT.                                        
 005399            DISPLAY 'A MEDIA DE ' N1-EDIT                                
 005400                    ' E '         N2-EDIT                                
 005410                    ' = '         N3-EDIT.                               
 005500            STOP RUN.                                                    
             