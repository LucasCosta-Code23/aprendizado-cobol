 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95813                                                     
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       *****************************************************             
 000710       *EXIBIR TOTAL DE VENDAS POR MES(3 NIVEIS DE TABELAS )*             
 000800       *****************************************************             
 000900       *                                                                 
 001000        ENVIRONMENT DIVISION.                                            
 001100        CONFIGURATION SECTION.                                           
 001200        SPECIAL-NAMES.                                                   
 001300            DECIMAL-POINT IS COMMA.                                      
 001400        INPUT-OUTPUT SECTION.                                            
 001500        FILE-CONTROL.                                                    
 001600            SELECT ARQ-VENDAS3N ASSIGN TO VENDAS3N                             
 001700                FILE STATUS IS WK-FS-VENDAS3N.                           
 001800       *                                                                 
 001900        DATA DIVISION.                                                   
 002000        FILE SECTION.                                                    
 002100        FD   ARQ-VENDAS3N                                                
 002200             RECORDING MODE IS F.                                        
 002300        01  REG-VENDAS3N.                                                
 002301            05 FD-VENDEDOR             PIC 99.                           
 002302            05 FD-ESTADO               PIC XX.                           
 002310            05 FD-MES                  PIC 99.                           
 002400            05 FD-NOTA                 PIC 9(6).                         
 002600            05 FD-VALOR                PIC 9(6)V99.                      
 002700        WORKING-STORAGE SECTION.                                         
 002800        77  WK-FS-VENDAS3N PIC XX          VALUE SPACES.                 
 002900        01  TABELAS.                                                     
 002910            02 TOT-VENDEDOR             OCCURS 2  TIMES.                                  
 002920               03 TOT-ESTADO            OCCURS 3  TIMES.                 
 002930                  04 TOT-MES            OCCURS 4  TIMES.                 
 003000                     05 TOTAL     PIC 9(8)V99.                           
 003100        77  WK-TOT-EDIT  PIC ZZ.ZZZ.ZZ9,99  VALUE ZEROS.                 
 003200        77  WK-VENDEDOR  PIC 99             VALUE ZEROS.                 
 003210        77  WK-ESTADO    PIC 99             VALUE ZEROS.                 
 003220        77  WK-MES       PIC 99             VALUE ZEROS.                 
 003300       *                                                                 
 003400        PROCEDURE DIVISION.                                              
 003500        000-PRINCIPAL SECTION.                                           
 003600        001-PRINCIPAL.                                                   
 003700            PERFORM 101-INICIAR.                                         
 003800            PERFORM 201-PROCESSAR UNTIL WK-FS-VENDAS3N = '10'.           
 003900            PERFORM 901-FINALIZAR.                                       
 004000            STOP RUN.                                                    
 004100       ***********************************************************            
 004200        100-INICIAR SECTION.                                             
 004300        101-INICIAR.                                                     
 004400            OPEN INPUT ARQ-VENDAS3N.                                     
 004500            EVALUATE WK-FS-VENDAS3N                                      
 004600                WHEN '00'                                                
 004700                    PERFORM 301-LER-VENDAS3N                             
 004800                WHEN '35'                                                
 004900                    DISPLAY 'ARQUIVO VENDAS3N NAO ENCONTRADO'            
 005000                    MOVE 12 TO RETURN-CODE                               
 005100                    STOP RUN                                             
 005200                WHEN OTHER                                               
 005300                    DISPLAY 'ERRO ' WK-FS-VENDAS3N                       
 005400                            ' NO COMANDO OPEN VENDAS3N'                  
 005500                    MOVE 12 TO RETURN-CODE                               
 005600                    STOP RUN                                             
 005700            END-EVALUATE.                                                                                                
 005710            INITIALIZE TABELAS.                                          
 005800       ***********************************************                   
 005900        200-PROCESSAR SECTION.                                           
 006000        201-PROCESSAR.                                                   
 006010            PERFORM 202-FD-WK-ESTADO.                                    
 006100            ADD FD-VALOR  TO TOTAL(FD-VENDEDOR, WK-ESTADO, FD-MES)       
 006200            PERFORM 301-LER-VENDAS3N.                                    
 006210        202-FD-WK-ESTADO.                                                
 006220            EVALUATE FD-ESTADO                                           
 006230                WHEN 'SP'                                                
 006240                    MOVE 01 TO WK-ESTADO                                 
 006250                WHEN 'RJ'                                                
 006260                    MOVE 02 TO WK-ESTADO                                 
 006270                WHEN 'MG'                                                
 006280                    MOVE 03 TO WK-ESTADO                                 
 006281                WHEN OTHER                                                                                             
 006282                    DISPLAY 'ESTADO ' FD-ESTADO ' INVALIDO'              
 006283                    STOP RUN                                             
 006290            END-EVALUATE.                                                
 006300       ***********************************************                   
 006400        300-LER-VENDAS3N SECTION.                                        
 006500        301-LER-VENDAS3N.                                                
 006600            READ ARQ-VENDAS3N.                                           
 006700            EVALUATE WK-FS-VENDAS3N                                      
 006800                WHEN '00'                                                
 006900                    CONTINUE                                             
 007000                WHEN '10'                                                
 007100                    CONTINUE                                             
 007200                WHEN OTHER                                               
 007300                    DISPLAY 'ERRO ' WK-FS-VENDAS3N                       
 007400                            ' NO COMANDO READ VENDAS3N'                  
 007500                    MOVE 12 TO RETURN-CODE                                                             
 007600                    STOP RUN                                             
 007700            END-EVALUATE.                                                
 007800       ***********************************************                   
 007900        900-FINALIZAR SECTION.                                           
 008000        901-FINALIZAR.                                                   
 008100            PERFORM VARYING WK-VENDEDOR FROM 1 BY 1                      
 008101                            UNTIL WK-VENDEDOR > 2                        
 008102                PERFORM VARYING WK-ESTADO FROM 1 BY 1                    
 008103                            UNTIL WK-ESTADO > 3                          
 008104                    PERFORM VARYING WK-MES FROM 1 BY 1                   
 008105                            UNTIL WK-MES > 4                             
 008106                       PERFORM 902-WK-FD-ESTADO                          
 008200                       MOVE TOTAL(WK-VENDEDOR, WK-ESTADO, WK-MES)        
 008210                                   TO WK-TOT-EDIT                        
 008300                       DISPLAY 'TOTAL DE VENDAS DO VENDEDOR ' WK-VENDEDOR
 008310                        ' NO ESTADO '                  FD-ESTADO   
 008320                        ', NO MES '                    WK-MES            
 008400                        ' = ' WK-TOT-EDIT                                
 008500                    END-PERFORM                                          
 008510                END-PERFORM                                              
 008520            END-PERFORM.                                                 
 008600            CLOSE ARQ-VENDAS3N.                                          
 008700        902-WK-FD-ESTADO.                                                
 008800            EVALUATE WK-ESTADO                                           
 008900                WHEN 01                                                  
 009000                    MOVE 'SP' TO FD-ESTADO                               
 009100                WHEN 02                                                  
 009200                    MOVE 'RJ' TO FD-ESTADO                               
 009300                WHEN 03                                                  
 009400                    MOVE 'MG' TO FD-ESTADO                               
 009800            END-EVALUATE.                                                                                                               