 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95808.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       **************************************************                
 000700       *         LER E EXIBIR ARQUIVO CLIENTES          *                
 000800       **************************************************                
 000900       *                                                                 
 001000        ENVIRONMENT DIVISION.                                            
 001100        CONFIGURATION SECTION.                                           
 001200        SPECIAL-NAMES.                                                   
 001300            DECIMAL-POINT IS COMMA.                                      
 001310        INPUT-OUTPUT SECTION.                                            
 001320        FILE-CONTROL.                                                    
 001330            SELECT ARQ-CLIENTES ASSIGN TO CLIENTES                       
 001340                FILE STATUS IS WK-FS-CLIENTES.                                                    
 001400       *                                                                 
 001500        DATA DIVISION.                                                   
 001510        FILE SECTION.                                                    
 001520        FD   ARQ-CLIENTES                                                
 001530             RECORDING MODE IS F.                                        
 001540        01  REG-CLIENTES.                                                
 001550            05 FD-AGENCIA-CLIENTES     PIC X(4).                         
 001560            05 FD-CONTA-CLIENTES       PIC 9(5).                         
 001570            05 FD-NOME-CLIENTES        PIC A(20).                        
 001580            05 FD-SALDO-CLIENTES       PIC 9(6)V99.                      
 001600        WORKING-STORAGE SECTION.                                         
 001700        77  WK-FS-CLIENTES PIC XX           VALUE SPACES.                
 001800        77  WK-SALDO-EDIT  PIC ZZZ.ZZ9,99   VALUE ZEROS.                 
 002500       *                                                                 
 002600        PROCEDURE DIVISION.                                              
 002700        000-PRINCIPAL SECTION.                                           
 002800        001-PRINCIPAL.                                                      
 002810            PERFORM 101-INICIAR.                                         
 002820            PERFORM 201-PROCESSAR UNTIL WK-FS-CLIENTES = '10'.           
 002830            PERFORM 901-FINALIZAR.                                       
 003600            STOP RUN.                                                    
 003700       ***********************************************************       
 003800        100-INICIAR SECTION.                                             
 003900        101-INICIAR.                                                     
 004000            OPEN INPUT ARQ-CLIENTES.                                     
 004100            EVALUATE WK-FS-CLIENTES                                      
 004200                WHEN '00'                                                
 004300                    PERFORM 301-LER-CLIENTES                             
 004400                WHEN '35'                                                
 004500                    DISPLAY 'ARQUIVO CLIENTES NAO ENCONTRADO'            
 004600                    MOVE 12 TO RETURN-CODE                               
 004700                    STOP RUN                                             
 004800                WHEN OTHER                                               
 004810                    DISPLAY 'ERRO ' WK-FS-CLIENTES                                              
 004820                            ' NO COMANDO OPEN CLIENTES'                  
 005000                    MOVE 12 TO RETURN-CODE                               
 005100                    STOP RUN                                             
 005200            END-EVALUATE.                                                
 005300       ***********************************************                   
 005400        200-PROCESSAR SECTION.                                           
 005500        201-PROCESSAR.                                                   
 005600            DISPLAY 'AGENCIA: ' FD-AGENCIA-CLIENTES.                     
 005700            DISPLAY 'CONTA  : ' FD-CONTA-CLIENTES.                       
 005800            DISPLAY 'NOME   : ' FD-NOME-CLIENTES.                        
 005810            MOVE FD-SALDO-CLIENTES TO WK-SALDO-EDIT.                     
 005900            DISPLAY 'SALDO  : ' WK-SALDO-EDIT.                           
 006000            DISPLAY ' '.                                                 
 006100            PERFORM 301-LER-CLIENTES.                                    
 006200       ***********************************************                   
 006300        300-LER-CLIENTES SECTION.                                        
 006400        301-LER-CLIENTES.                                                                                                
 006500            READ ARQ-CLIENTES.                                           
 006600            EVALUATE WK-FS-CLIENTES                                      
 006700                WHEN '00'                                                
 006800                    CONTINUE                                             
 006900                WHEN '10'                                                
 007000                    DISPLAY 'FIM DO ARQUIVO'                             
 007100                WHEN OTHER                                               
 007200                    DISPLAY 'ERRO ' WK-FS-CLIENTES                       
 007210                            ' NO COMANDO READ CLIENTES'                  
 007300                    MOVE 12 TO RETURN-CODE                               
 007400                    STOP RUN                                             
 007500            END-EVALUATE.                                                
 007600       ***********************************************                   
 007700        900-FINALIZAR SECTION.                                           
 007800        901-FINALIZAR.                                                   
 007900            CLOSE ARQ-CLIENTES.                                          
