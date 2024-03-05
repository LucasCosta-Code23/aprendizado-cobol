 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95810.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       **************************************************                
 000700       *         CRIAR RELATORIO DE CLIENTES            *                
 000800       **************************************************                
 000900       *                                                                 
 001000        ENVIRONMENT DIVISION.                                            
 001100        CONFIGURATION SECTION.                                           
 001200        SPECIAL-NAMES.                                                   
 001300            DECIMAL-POINT IS COMMA.                                      
 001400        INPUT-OUTPUT SECTION.                                            
 001500        FILE-CONTROL.                                                    
 001600            SELECT ARQ-CLIENTES ASSIGN TO CLIENTES                       
 001700                FILE STATUS IS WK-FS-CLIENTES.                                                     
 001800            SELECT ARQ-RELATO   ASSIGN TO RELATO.                        
 002200       *                                                                 
 002300        DATA DIVISION.                                                   
 002400        FILE SECTION.                                                    
 002500        FD   ARQ-CLIENTES                                                
 002600             RECORDING MODE IS F.                                        
 002700        01  REG-CLIENTES.                                                
 002800            05 FD-CHAVE-CLIENTES.                                        
 002900                10 FD-AGENCIA-CLIENTES     PIC X(4).                     
 003000                10 FD-CONTA-CLIENTES       PIC 9(5).                     
 003100            05 FD-NOME-CLIENTES        PIC A(20).                        
 003200            05 FD-SALDO-CLIENTES       PIC 9(6)V99.                      
 003300        FD   ARQ-RELATO                                                  
 003400             RECORDING MODE IS F.                                        
 003500        01  REG-RELATO                 PIC X(50).                        
 005000        WORKING-STORAGE SECTION.                                         
 005010        01  WK-CABEC1.                                                                                                     
 005020            05 FILLER      PIC X(04) VALUE SPACES.                       
 005030            05 FILLER      PIC X(21) VALUE 'RELATORIO DE CLIENTES'.      
 005040            05 FILLER      PIC X(08) VALUE SPACES.                       
 005050            05 FILLER      PIC X(04) VALUE 'PAG '.                       
 005060            05 WK-NUMPAG   PIC 9(03) VALUE ZEROS.                        
 005070        01  WK-CABEC2.                                                   
 005080            05 FILLER       PIC X(09) VALUE SPACES.                      
 005090            05 FILLER       PIC X(10) VALUE 'AGENCIA : '.                
 005091            05 WK-AG-CABEC2 PIC X(04) VALUE SPACES.                      
 005092        01  WK-CABEC3.                                                   
 005093            05 FILLER      PIC X(05) VALUE 'CONTA'.                      
 005094            05 FILLER      PIC X(02) VALUE SPACES.                       
 005095            05 FILLER      PIC X(04) VALUE 'NOME'.                       
 005096            05 FILLER      PIC X(22) VALUE SPACES.                       
 005097            05 FILLER      PIC X(05) VALUE 'SALDO'.                      
 005098        01  WK-DETALHE.                                                  
 005099            05 WK-CONTA-DET PIC 9(05)       VALUE ZEROS.                                  
 005100            05 FILLER       PIC X(02)       VALUE SPACES.                
 005101            05 WK-NOME-DET  PIC A(20)       VALUE SPACES.                
 005102            05 FILLER       PIC X(01)       VALUE SPACES.                
 005103            05 WK-SALDO-DET PIC ZZZ.ZZ9,99  VALUE ZEROS.                 
 005104        01  WK-RODAPE.                                                   
 005105            05 FILLER        PIC X(17) VALUE 'TOTAL DA AGENCIA '.        
 005106            05 WK-AG-RODAPE  PIC X(04) VALUE SPACES.                     
 005107            05 FILLER        PIC X(04) VALUE ' -> '.                     
 005108            05 WK-TOT-RODAPE PIC ZZ.ZZZ.ZZ9,99 VALUE ZEROS.              
 005110        77  WK-FS-CLIENTES PIC XX           VALUE SPACES.                
 005120        77  WK-AG-CORRENTE PIC X(04)        VALUE SPACES.                
 005130        77  WK-TOT-AGENCIA PIC 9(08)V99     VALUE ZEROS.                 
 005140        77  WK-CONTALIN    PIC 9            VALUE ZEROS.                 
 005600       *                                                                 
 005700        PROCEDURE DIVISION.                                              
 005800        000-PRINCIPAL SECTION.                                           
 005900        001-PRINCIPAL.                                                                                                     
 006000            PERFORM 101-INICIAR.                                         
 006100            PERFORM 201-PROCESSAR UNTIL WK-FS-CLIENTES = '10'.           
 006300            PERFORM 901-FINALIZAR.                                       
 006400            STOP RUN.                                                    
 006500       ***********************************************************       
 006510        100-INICIAR SECTION.                                             
 006600        101-INICIAR.                                                     
 007200            OPEN INPUT ARQ-CLIENTES.                                     
 007300            EVALUATE WK-FS-CLIENTES                                      
 007400                WHEN '00'                                                
 007500                    PERFORM 301-LER-CLIENTES                             
 007510                    MOVE FD-AGENCIA-CLIENTES  TO WK-AG-CORRENTE          
 007600                WHEN '35'                                                
 007700                    DISPLAY 'ARQUIVO CLIENTES NAO ENCONTRADO'            
 007800                    MOVE 12 TO RETURN-CODE                               
 007900                    STOP RUN                                             
 008000                WHEN OTHER                                                                                              
 008100                    DISPLAY 'ERRO ' WK-FS-CLIENTES                       
 008200                            ' NO COMANDO OPEN CLIENTES'                  
 008300                    MOVE 12 TO RETURN-CODE                               
 008400                    STOP RUN                                             
 008500            END-EVALUATE.                                                
 008600            OPEN OUTPUT ARQ-RELATO.                                      
 011200       ***********************************************                   
 011300        200-PROCESSAR SECTION.                                           
 011400        201-PROCESSAR.                                                   
 011500            PERFORM 202-IMPRIME-CLIENTES.                                
 011600            PERFORM 301-LER-CLIENTES.                                    
 011700        202-IMPRIME-CLIENTES.                                            
 011800            IF FD-AGENCIA-CLIENTES NOT EQUAL WK-AG-CORRENTE              
 011900                PERFORM 203-TOTALIZA                                     
 012000            END-IF.                                                      
 012100            IF WK-CONTALIN = 0 OR WK-CONTALIN >= 5                       
 012200                PERFORM 204-CABECALHO                                                                      
 012300            END-IF.                                                      
 012400            PERFORM 205-IMPRIME.                                         
 012500        203-TOTALIZA.                                                    
 012600            MOVE WK-AG-CORRENTE  TO WK-AG-RODAPE.                        
 012700            MOVE WK-TOT-AGENCIA  TO WK-TOT-RODAPE.                       
 012800            WRITE REG-RELATO FROM WK-RODAPE AFTER 2 LINES.               
 012900            MOVE 0 TO  WK-TOT-AGENCIA WK-CONTALIN.                       
 013000            MOVE FD-AGENCIA-CLIENTES TO WK-AG-CORRENTE.                  
 013100        204-CABECALHO.                                                   
 013200            ADD 1 TO     WK-NUMPAG.                                      
 013300            MOVE WK-AG-CORRENTE   TO WK-AG-CABEC2.                       
 013400            WRITE REG-RELATO FROM WK-CABEC1 AFTER PAGE.                  
 013500            WRITE REG-RELATO FROM WK-CABEC2.                             
 013600            WRITE REG-RELATO FROM WK-CABEC3 AFTER 2 LINES.               
 013700            MOVE 0 TO  WK-CONTALIN.                                      
 013800        205-IMPRIME.                                                     
 013900            MOVE FD-CONTA-CLIENTES TO WK-CONTA-DET.                                            
 014000            MOVE FD-NOME-CLIENTES  TO WK-NOME-DET.                       
 014100            MOVE FD-SALDO-CLIENTES TO WK-SALDO-DET.                      
 014200            WRITE REG-RELATO FROM  WK-DETALHE.                           
 014300            ADD 1                  TO WK-CONTALIN.                       
 014400            ADD FD-SALDO-CLIENTES  TO WK-TOT-AGENCIA.                    
 015800       ***********************************************                   
 015900        300-LER-CLIENTES SECTION.                                        
 016000        301-LER-CLIENTES.                                                
 016100            READ ARQ-CLIENTES.                                           
 016200            EVALUATE WK-FS-CLIENTES                                      
 016300                WHEN '00'                                                
 016400                    CONTINUE                                             
 016900                WHEN '10'                                                
 017000                    CONTINUE                                             
 017100                WHEN OTHER                                               
 017200                    DISPLAY 'ERRO ' WK-FS-CLIENTES                       
 017300                            ' NO COMANDO READ CLIENTES'                                   
 017400                    MOVE 12 TO RETURN-CODE                               
 017500                    STOP RUN                                             
 017600            END-EVALUATE.                                                
 019000       ***********************************************                   
 019100        900-FINALIZAR SECTION.                                           
 019200        901-FINALIZAR.                                                   
 019210            PERFORM 203-TOTALIZA.                                        
 019300            CLOSE ARQ-CLIENTES.                                          
 019400            CLOSE ARQ-RELATO.                                            
