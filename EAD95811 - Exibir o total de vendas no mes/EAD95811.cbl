 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95811.                                                    
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       **************************************************                
 000700       *     EXIBIR TOTAL DE VENDAS POR MES             *                
 000800       **************************************************                
 000900       *                                                                 
 001000        ENVIRONMENT DIVISION.                                            
 001100        CONFIGURATION SECTION.                                           
 001200        SPECIAL-NAMES.                                                   
 001300            DECIMAL-POINT IS COMMA.                                      
 001400        INPUT-OUTPUT SECTION.                                            
 001500        FILE-CONTROL.                                                    
 001600            SELECT ARQ-VENDAS ASSIGN TO VENDAS                           
 001700                FILE STATUS IS WK-FS-VENDAS.                                                       
 001900       *                                                                 
 002000        DATA DIVISION.                                                   
 002100        FILE SECTION.                                                    
 002200        FD   ARQ-VENDAS                                                  
 002300             RECORDING MODE IS F.                                        
 002400        01  REG-VENDAS.                                                  
 002500            05 FD-NOTA                 PIC 9(6).                         
 002800            05 FD-MES                  PIC 99.                           
 002900            05 FD-VALOR                PIC 9(6)V99.                      
 003300        WORKING-STORAGE SECTION.                                         
 006100        77  WK-FS-VENDAS PIC XX             VALUE SPACES.                
 006200        77  WK-TOT-01    PIC 9(8)V99        VALUE ZEROS.                 
 006300        77  WK-TOT-02    PIC 9(8)V99        VALUE ZEROS.                 
 006400        77  WK-TOT-03    PIC 9(8)V99        VALUE ZEROS.                 
 006410        77  WK-TOT-04    PIC 9(8)V99        VALUE ZEROS.                 
 006420        77  WK-TOT-05    PIC 9(8)V99        VALUE ZEROS.                 
 006430        77  WK-TOT-06    PIC 9(8)V99        VALUE ZEROS.                                  
 006440        77  WK-TOT-07    PIC 9(8)V99        VALUE ZEROS.                 
 006450        77  WK-TOT-08    PIC 9(8)V99        VALUE ZEROS.                 
 006460        77  WK-TOT-09    PIC 9(8)V99        VALUE ZEROS.                 
 006470        77  WK-TOT-10    PIC 9(8)V99        VALUE ZEROS.                 
 006480        77  WK-TOT-11    PIC 9(8)V99        VALUE ZEROS.                 
 006490        77  WK-TOT-12    PIC 9(8)V99        VALUE ZEROS.                 
 006491        77  WK-TOT-EDIT  PIC ZZ.ZZZ.ZZ9,99  VALUE ZEROS.                 
 006492        77  WK-MES       PIC 99             VALUE ZEROS.                 
 006500       *                                                                 
 006600        PROCEDURE DIVISION.                                              
 006700        000-PRINCIPAL SECTION.                                           
 006800        001-PRINCIPAL.                                                   
 006900            PERFORM 101-INICIAR.                                         
 007000            PERFORM 201-PROCESSAR UNTIL WK-FS-VENDAS = '10'.             
 007100            PERFORM 901-FINALIZAR.                                       
 007200            STOP RUN.                                                                                                       
 007300       ***********************************************************       
 007400        100-INICIAR SECTION.                                             
 007500        101-INICIAR.                                                     
 007600            OPEN INPUT ARQ-VENDAS.                                       
 007700            EVALUATE WK-FS-VENDAS                                        
 007800                WHEN '00'                                                
 007900                    PERFORM 301-LER-VENDAS                               
 008100                WHEN '35'                                                
 008200                    DISPLAY 'ARQUIVO VENDAS NAO ENCONTRADO'              
 008300                    MOVE 12 TO RETURN-CODE                               
 008400                    STOP RUN                                             
 008500                WHEN OTHER                                               
 008600                    DISPLAY 'ERRO ' WK-FS-VENDAS                         
 008700                            ' NO COMANDO OPEN VENDAS'                    
 008800                    MOVE 12 TO RETURN-CODE                               
 008900                    STOP RUN                                             
 009000            END-EVALUATE.                                                                                                
 009200       ***********************************************                   
 009300        200-PROCESSAR SECTION.                                           
 009400        201-PROCESSAR.                                                   
 009500            EVALUATE FD-MES                                              
 009600                WHEN 01                                                  
 009700                    ADD FD-VALOR TO WK-TOT-01                            
 009800                WHEN 02                                                  
 009900                    ADD FD-VALOR TO WK-TOT-02                            
 010000                WHEN 03                                                  
 010100                    ADD FD-VALOR TO WK-TOT-03                            
 010200                WHEN 04                                                  
 010300                    ADD FD-VALOR TO WK-TOT-04                            
 010400                WHEN 05                                                  
 010500                    ADD FD-VALOR TO WK-TOT-05                            
 010600                WHEN 06                                                  
 010700                    ADD FD-VALOR TO WK-TOT-06                            
 010800                WHEN 07                                                                                                   
 010900                    ADD FD-VALOR TO WK-TOT-07                            
 011000                WHEN 08                                                  
 011100                    ADD FD-VALOR TO WK-TOT-08                            
 011200                WHEN 09                                                  
 011300                    ADD FD-VALOR TO WK-TOT-09                            
 011400                WHEN 10                                                  
 011500                    ADD FD-VALOR TO WK-TOT-10                            
 011600                WHEN 11                                                  
 011700                    ADD FD-VALOR TO WK-TOT-11                            
 011800                WHEN 12                                                  
 011900                    ADD FD-VALOR TO WK-TOT-12                            
 012000            END-EVALUATE.                                                
 012100            PERFORM 301-LER-VENDAS.                                      
 012500       ***********************************************                   
 012600        300-LER-VENDAS SECTION.                                          
 012700        301-LER-VENDAS.                                                  
 012800            READ ARQ-VENDAS.                                                                                        
 012900            EVALUATE WK-FS-VENDAS                                        
 013000                WHEN '00'                                                
 013100                    CONTINUE                                             
 013200                WHEN '10'                                                
 013300                    CONTINUE                                             
 013400                WHEN OTHER                                               
 013500                    DISPLAY 'ERRO ' WK-FS-VENDAS                         
 013600                            ' NO COMANDO READ VENDAS'                    
 013700                    MOVE 12 TO RETURN-CODE                               
 013800                    STOP RUN                                             
 013900            END-EVALUATE.                                                
 014000       ***********************************************                   
 014100        900-FINALIZAR SECTION.                                           
 014200        901-FINALIZAR.                                                   
 014300            PERFORM VARYING WK-MES FROM 1 BY 1 UNTIL WK-MES > 12         
 014400                EVALUATE WK-MES                                          
 014410                    WHEN 01                                                                                        
 014411                        MOVE WK-TOT-01    TO WK-TOT-EDIT                 
 014420                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014430                                ' = ' WK-TOT-EDIT                        
 014440                    WHEN 02                                              
 014450                        MOVE WK-TOT-02    TO WK-TOT-EDIT                 
 014460                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014470                                ' = ' WK-TOT-EDIT                        
 014480                    WHEN 03                                              
 014490                        MOVE WK-TOT-03    TO WK-TOT-EDIT                 
 014491                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014492                                ' = ' WK-TOT-EDIT                        
 014493                    WHEN 04                                              
 014494                        MOVE WK-TOT-04    TO WK-TOT-EDIT                 
 014495                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014496                                ' = ' WK-TOT-EDIT                        
 014497                    WHEN 05                                              
 014498                        MOVE WK-TOT-05    TO WK-TOT-EDIT                                 
 014499                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014500                                ' = ' WK-TOT-EDIT                        
 014510                    WHEN 06                                              
 014520                        MOVE WK-TOT-06    TO WK-TOT-EDIT                 
 014530                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014540                                ' = ' WK-TOT-EDIT                        
 014550                    WHEN 07                                              
 014560                        MOVE WK-TOT-07    TO WK-TOT-EDIT                 
 014570                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014580                                ' = ' WK-TOT-EDIT                        
 014590                    WHEN 08                                              
 014591                        MOVE WK-TOT-08    TO WK-TOT-EDIT                 
 014592                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014593                                ' = ' WK-TOT-EDIT                        
 014594                    WHEN 09                                              
 014595                        MOVE WK-TOT-09    TO WK-TOT-EDIT                 
 014596                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES                 
 014597                                ' = ' WK-TOT-EDIT                        
 014598                    WHEN 10                                              
 014599                        MOVE WK-TOT-10    TO WK-TOT-EDIT                 
 014600                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014610                                ' = ' WK-TOT-EDIT                        
 014620                    WHEN 11                                              
 014630                        MOVE WK-TOT-11    TO WK-TOT-EDIT                 
 014640                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014650                                ' = ' WK-TOT-EDIT                        
 014660                    WHEN 12                                              
 014670                        MOVE WK-TOT-12    TO WK-TOT-EDIT                 
 014680                        DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES         
 014690                                ' = ' WK-TOT-EDIT                        
 014691                END-EVALUATE                                             
 014692            END-PERFORM.                                                 
 014700            CLOSE ARQ-VENDAS.                                            
