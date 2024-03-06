 000001        IDENTIFICATION DIVISION.                                         
 000002        PROGRAM-ID.                                                      
 000003            EAD95812                                                     
 000004        AUTHOR.                                                          
 000005            LUCAS.                                                       
 000006       ******************************************************            
 000007       *    EXIBIR TOTAL DE VENDAS POR MES(USANDO TABELA)   *            
 000008       ******************************************************            
 000009       *                                                                 
 000010        ENVIRONMENT DIVISION.                                            
 000011        CONFIGURATION SECTION.                                           
 000012        SPECIAL-NAMES.                                                   
 000013            DECIMAL-POINT IS COMMA.                                      
 000014        INPUT-OUTPUT SECTION.                                            
 000015        FILE-CONTROL.                                                    
 000016            SELECT ARQ-VENDAS ASSIGN TO VENDAS                           
 000017                FILE STATUS IS WK-FS-VENDAS.                                                         
 000018       *                                                                 
 000019        DATA DIVISION.                                                   
 000020        FILE SECTION.                                                    
 000021        FD   ARQ-VENDAS                                                  
 000022             RECORDING MODE IS F.                                        
 000023        01  REG-VENDAS.                                                  
 000024            05 FD-NOTA                 PIC 9(6).                         
 000025            05 FD-MES                  PIC 99.                           
 000026            05 FD-VALOR                PIC 9(6)V99.                      
 000027        WORKING-STORAGE SECTION.                                         
 000028        77  WK-FS-VENDAS PIC XX            VALUE SPACES.                 
 000029        01  TABELA-TOTAIS.                                               
 000030            03 TOTAL     PIC 9(8)V99    OCCURS 12 TIMES.                 
 000031        77  WK-TOT-EDIT  PIC ZZ.ZZZ.ZZ9,99  VALUE ZEROS.                 
 000032        77  WK-MES       PIC 99             VALUE ZEROS.                 
 000033       *                                                                 
 000034        PROCEDURE DIVISION.                                                                                           
 000035        000-PRINCIPAL SECTION.                                           
 000036        001-PRINCIPAL.                                                   
 000037            PERFORM 101-INICIAR.                                         
 000038            PERFORM 201-PROCESSAR UNTIL WK-FS-VENDAS = '10'.             
 000039            PERFORM 901-FINALIZAR.                                       
 000040            STOP RUN.                                                    
 000041       ***********************************************************       
 000042        100-INICIAR SECTION.                                             
 000043        101-INICIAR.                                                     
 000044            OPEN INPUT ARQ-VENDAS.                                       
 000045            EVALUATE WK-FS-VENDAS                                        
 000046                WHEN '00'                                                
 000047                    PERFORM 301-LER-VENDAS                               
 000048                WHEN '35'                                                
 000049                    DISPLAY 'ARQUIVO VENDAS NAO ENCONTRADO'              
 000050                    MOVE 12 TO RETURN-CODE                               
 000051                    STOP RUN                                                                                         
 000052                WHEN OTHER                                               
 000053                    DISPLAY 'ERRO ' WK-FS-VENDAS                         
 000054                            ' NO COMANDO OPEN VENDAS'                    
 000055                    MOVE 12 TO RETURN-CODE                               
 000056                    STOP RUN                                             
 000057            END-EVALUATE.                                                
 000058       ***********************************************                   
 000059        200-PROCESSAR SECTION.                                           
 000060        201-PROCESSAR.                                                   
 000061            ADD FD-VALOR  TO TOTAL(FD-MES).                              
 000062            PERFORM 301-LER-VENDAS.                                      
 000063       ***********************************************                   
 000064        300-LER-VENDAS SECTION.                                          
 000065        301-LER-VENDAS.                                                  
 000066            READ ARQ-VENDAS.                                             
 000067            EVALUATE WK-FS-VENDAS                                        
 000068                WHEN '00'                                                                                              
 000069                    CONTINUE                                             
 000070                WHEN '10'                                                
 000071                    CONTINUE                                             
 000072                WHEN OTHER                                               
 000073                    DISPLAY 'ERRO ' WK-FS-VENDAS                         
 000074                            ' NO COMANDO READ VENDAS'                    
 000075                    MOVE 12 TO RETURN-CODE                               
 000076                    STOP RUN                                             
 000077            END-EVALUATE.                                                
 000078       ***********************************************                   
 000079        900-FINALIZAR SECTION.                                           
 000080        901-FINALIZAR.                                                   
 000081            PERFORM VARYING WK-MES FROM 1 BY 1 UNTIL WK-MES > 12         
 000082                MOVE TOTAL(WK-MES) TO WK-TOT-EDIT                        
 000083                DISPLAY 'TOTAL DE VENDAS DO MES ' WK-MES                 
 000084                        ' = ' WK-TOT-EDIT                                
 000085            END-PERFORM.                                                                                                  
 000086            CLOSE ARQ-VENDAS.                                                                                                     