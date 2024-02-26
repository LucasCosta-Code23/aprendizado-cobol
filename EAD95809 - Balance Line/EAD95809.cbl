 EDIT       GR.EAD958.COBLIB(EAD95809) - 01.07              Columns 00001 00072 
 ****** ***************************** Top of Data ******************************
 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95809.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       **************************************************                
 000700       *                 BALANCE LINE                   *                
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
 001710            SELECT ARQ-LANCAM   ASSIGN TO LANCAM                         
 001720                FILE STATUS IS WK-FS-LANCAM.                             
 001730            SELECT ARQ-CLIENTEN ASSIGN TO CLIENTEN                       
 001740                FILE STATUS IS WK-FS-CLIENTEN.                           
 001800       *                                                                 
 001900        DATA DIVISION.                                                   
 002000        FILE SECTION.                                                    
 002100        FD   ARQ-CLIENTES                                                
 002200             RECORDING MODE IS F.                                        
 002300        01  REG-CLIENTES.                                                
 002310            05 FD-CHAVE-CLIENTES.                                        
 002400                10 FD-AGENCIA-CLIENTES     PIC X(4).                     
 002500                10 FD-CONTA-CLIENTES       PIC 9(5).                     
 002600            05 FD-NOME-CLIENTES        PIC A(20).                        
 002700            05 FD-SALDO-CLIENTES       PIC 9(6)V99.                      
 002710        FD   ARQ-LANCAM                                                  
 002720             RECORDING MODE IS F.                                                                               
 002730        01  REG-LANCAM.                                                  
 002740            05 FD-CHAVE-LANCAM.                                          
 002750                10 FD-AGENCIA-LANCAM       PIC X(4).                     
 002760                10 FD-CONTA-LANCAM         PIC 9(5).                     
 002761            05 FD-DOC-LANCAM           PIC 9(4).                         
 002770            05 FD-TIPO-LANCAM          PIC A.                            
 002780            05 FD-VALOR-LANCAM         PIC 9(6)V99.                      
 002790        FD   ARQ-CLIENTEN                                                
 002791             RECORDING MODE IS F.                                        
 002792        01  REG-CLIENTEN.                                                
 002793            05 FD-CHAVE-CLIENTEN.                                        
 002794                10 FD-AGENCIA-CLIENTEN     PIC X(4).                     
 002795                10 FD-CONTA-CLIENTEN       PIC 9(5).                     
 002798            05 FD-NOME-CLIENTEN        PIC A(20).                        
 002799            05 FD-SALDO-CLIENTEN       PIC 9(6)V99.                      
 002800        WORKING-STORAGE SECTION.                                         
 002900        77  WK-FS-CLIENTES PIC XX           VALUE SPACES.                                
 002910        77  WK-FS-LANCAM   PIC XX           VALUE SPACES.                
 002920        77  WK-FS-CLIENTEN PIC XX           VALUE SPACES.                
 003000        77  WK-SALDO-EDIT  PIC ZZZ.ZZ9,99   VALUE ZEROS.                 
 003010        77  WK-VALOR-EDIT  PIC ZZZ.ZZ9,99   VALUE ZEROS.                 
 003100       *                                                                 
 003200        PROCEDURE DIVISION.                                              
 003300        000-PRINCIPAL SECTION.                                           
 003400        001-PRINCIPAL.                                                   
 003500            PERFORM 101-INICIAR.                                         
 003600            PERFORM 201-PROCESSAR UNTIL WK-FS-CLIENTES = '10'            
 003610                                    AND WK-FS-LANCAM   = '10'.           
 003700            PERFORM 901-FINALIZAR.                                       
 003800            STOP RUN.                                                    
 003900       ***********************************************************       
 004000        100-INICIAR SECTION.                                             
 004100        101-INICIAR.                                                     
 004110            PERFORM 102-ABRIR-CLIENTES.                                                                   
 004120            PERFORM 103-ABRIR-LANCAM.                                    
 004130            PERFORM 104-ABRIR-CLIENTEN.                                  
 004140        102-ABRIR-CLIENTES.                                              
 004200            OPEN INPUT ARQ-CLIENTES.                                     
 004300            EVALUATE WK-FS-CLIENTES                                      
 004400                WHEN '00'                                                
 004500                    PERFORM 301-LER-CLIENTES                             
 004600                WHEN '35'                                                
 004700                    DISPLAY 'ARQUIVO CLIENTES NAO ENCONTRADO'            
 004800                    MOVE 12 TO RETURN-CODE                               
 004900                    STOP RUN                                             
 005000                WHEN OTHER                                               
 005100                    DISPLAY 'ERRO ' WK-FS-CLIENTES                       
 005200                            ' NO COMANDO OPEN CLIENTES'                  
 005300                    MOVE 12 TO RETURN-CODE                               
 005400                    STOP RUN                                             
 005500            END-EVALUATE.                                                                                                
 005510        103-ABRIR-LANCAM.                                                
 005520            OPEN INPUT ARQ-LANCAM.                                       
 005530            EVALUATE WK-FS-LANCAM                                        
 005540                WHEN '00'                                                
 005550                    PERFORM 302-LER-LANCAM                               
 005560                WHEN '35'                                                
 005570                    DISPLAY 'ARQUIVO LANCAM NAO ENCONTRADO'              
 005580                    MOVE 12 TO RETURN-CODE                               
 005590                    STOP RUN                                             
 005591                WHEN OTHER                                               
 005592                    DISPLAY 'ERRO ' WK-FS-LANCAM                         
 005593                            ' NO COMANDO OPEN LANCAMENTOS'               
 005594                    MOVE 12 TO RETURN-CODE                               
 005595                    STOP RUN                                             
 005596            END-EVALUATE.                                                
 005597        104-ABRIR-CLIENTEN.                                              
 005598            OPEN OUTPUT ARQ-CLIENTEN.                                                                      
 005599            EVALUATE WK-FS-CLIENTEN                                      
 005600                WHEN '00'                                                
 005601                    CONTINUE                                             
 005606                WHEN OTHER                                               
 005607                    DISPLAY 'ERRO ' WK-FS-CLIENTEN                       
 005608                            ' NO COMANDO OPEN CLIENTEN'                  
 005609                    MOVE 12 TO RETURN-CODE                               
 005610                    STOP RUN                                             
 005611            END-EVALUATE.                                                
 005620       ***********************************************                   
 005700        200-PROCESSAR SECTION.                                           
 005800        201-PROCESSAR.                                                   
 005900            EVALUATE TRUE                                                
 006000                WHEN FD-CHAVE-CLIENTES < FD-CHAVE-LANCAM                 
 006100                    PERFORM 202-GRAVAR-CLIENTEN                          
 006200                    PERFORM 301-LER-CLIENTES                             
 006300                WHEN FD-CHAVE-CLIENTES = FD-CHAVE-LANCAM                                 
 006400                    PERFORM 203-EXEC-LANCAM                              
 006500                    PERFORM 302-LER-LANCAM                               
 006510                WHEN OTHER                                               
 006520                    DISPLAY 'CHAVE LANCAMENTO : ' FD-CHAVE-LANCAM        
 006530                            ' ESTA ERRADA NO DOCUMENTO ' FD-DOC-LANCAM   
 006540                    PERFORM 302-LER-LANCAM                               
 006550            END-EVALUATE.                                                
 006560        202-GRAVAR-CLIENTEN.                                             
 006561            MOVE FD-SALDO-CLIENTES TO WK-SALDO-EDIT.                     
 006570            DISPLAY '    SALDO FINAL = ' WK-SALDO-EDIT.                  
 006571            DISPLAY '------------------------------'.                    
 006580            MOVE REG-CLIENTES  TO REG-CLIENTEN.                          
 006590            WRITE REG-CLIENTEN.                                          
 006591            IF WK-FS-CLIENTEN NOT EQUAL '00'                             
 006592                DISPLAY 'ERRO ' WK-FS-CLIENTEN                           
 006593                        ' NO COMANDO WRITE CLIENTEN'                     
 006594                MOVE 12 TO RETURN-CODE                                                                    
 006595                STOP RUN                                                 
 006596            END-IF.                                                      
 006597        203-EXEC-LANCAM.                                                 
 006598            EVALUATE FD-TIPO-LANCAM                                      
 006599                WHEN 'C'                                                 
 006600                    ADD FD-VALOR-LANCAM  TO FD-SALDO-CLIENTES            
 006601                    MOVE FD-VALOR-LANCAM TO WK-VALOR-EDIT                
 006602                    DISPLAY '        CREDITO : ' WK-VALOR-EDIT           
 006603                WHEN 'D'                                                 
 006604                    IF FD-VALOR-LANCAM > FD-SALDO-CLIENTES               
 006605                        DISPLAY 'SALDO INSUFICIENTE NO DOCUMENTO '       
 006606                                FD-DOC-LANCAM                            
 006607                    ELSE                                                 
 006608                        SUBTRACT FD-VALOR-LANCAM FROM FD-SALDO-CLIENTES  
 006609                        MOVE FD-VALOR-LANCAM TO WK-VALOR-EDIT            
 006610                        DISPLAY '        DEBITO : ' WK-VALOR-EDIT        
 006611                    END-IF                                                                                             
 006612                WHEN OTHER                                               
 006613                    DISPLAY 'TIPO LANCAMENTO : ' FD-TIPO-LANCAM          
 006614                            ' ESTA ERRADO NO DOCUMENTO ' FD-DOC-LANCAM   
 006615            END-EVALUATE.                                                
 006620       ***********************************************                   
 006700        300-LER-CLIENTES SECTION.                                        
 006800        301-LER-CLIENTES.                                                
 006900            READ ARQ-CLIENTES.                                           
 007000            EVALUATE WK-FS-CLIENTES                                      
 007100                WHEN '00'                                                
 007110                    MOVE FD-SALDO-CLIENTES TO WK-SALDO-EDIT              
 007200                    DISPLAY FD-AGENCIA-CLIENTES ' '                      
 007210                            FD-CONTA-CLIENTES ' '                        
 007220                            FD-NOME-CLIENTES ' '                         
 007240                    DISPLAY '    SALDO INICIAL = ' WK-SALDO-EDIT         
 007300                WHEN '10'                                                
 007400                    MOVE HIGH-VALUES TO FD-CHAVE-CLIENTES                               
 007500                WHEN OTHER                                               
 007600                    DISPLAY 'ERRO ' WK-FS-CLIENTES                       
 007700                            ' NO COMANDO READ CLIENTES'                  
 007800                    MOVE 12 TO RETURN-CODE                               
 007900                    STOP RUN                                             
 008000            END-EVALUATE.                                                
 008010        302-LER-LANCAM.                                                  
 008020            READ ARQ-LANCAM.                                             
 008030            EVALUATE WK-FS-LANCAM                                        
 008040                WHEN '00'                                                
 008050                    CONTINUE                                             
 008091                WHEN '10'                                                
 008093                    MOVE HIGH-VALUES TO FD-CHAVE-LANCAM                  
 008094                WHEN OTHER                                               
 008095                    DISPLAY 'ERRO ' WK-FS-LANCAM                         
 008096                            ' NO COMANDO READ LANCAM'                    
 008097                    MOVE 12 TO RETURN-CODE                                                             
 008098                    STOP RUN                                             
 008099            END-EVALUATE.                                                
 008100       ***********************************************                   
 008200        900-FINALIZAR SECTION.                                           
 008300        901-FINALIZAR.                                                   
 008400            CLOSE ARQ-CLIENTES.                                          
 010700            CLOSE ARQ-LANCAM.                                            
 010800            CLOSE ARQ-CLIENTEN.                                          
 010900            IF WK-FS-CLIENTEN NOT EQUAL '00'                             
 011000                DISPLAY 'ERRO ' WK-FS-CLIENTEN                           
 011100                        ' NO COMANDO CLOSE CLIENTEN'                     
 011110                DISPLAY 'ERRO AO SALVAR ARQUIVO'                         
 011200                MOVE 12 TO RETURN-CODE                                   
 011300            END-IF.                                                      
                               