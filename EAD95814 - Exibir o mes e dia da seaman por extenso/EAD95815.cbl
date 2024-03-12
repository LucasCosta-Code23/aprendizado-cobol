 000100        IDENTIFICATION DIVISION.                                         
 000200        PROGRAM-ID.                                                      
 000300            EAD95815                                                     
 000400        AUTHOR.                                                          
 000500            LUCAS.                                                       
 000600       *                                                                 
 000700       ************************************************************      
 000800       *       MONTAR O MES E DIA DA SEMANA POR EXTENSO           *      
 000900       ************************************************************      
 001000        ENVIRONMENT DIVISION.                                            
 001100        CONFIGURATION SECTION.                                           
 001200        SPECIAL-NAMES.                                                   
 001300            DECIMAL-POINT IS COMMA.                                      
 001400        INPUT-OUTPUT SECTION.                                            
 001500        FILE-CONTROL.                                                    
 001600       *                                                                 
 001700        DATA DIVISION.                                                                                                   
 001800        FILE SECTION.                                                    
 001900        WORKING-STORAGE SECTION.                                         
 002000        01  NOMES-MES.                                                   
 002100            03 FILLER               PIC X(9)  VALUE 'JANEIRO'.           
 002200            03 FILLER               PIC X(9)  VALUE 'FEVEREIRO'.         
 002300            03 FILLER               PIC X(9)  VALUE 'MARCO'.             
 002400            03 FILLER               PIC X(9)  VALUE 'ABRIL'.             
 002500            03 FILLER               PIC X(9)  VALUE 'MAIO'.              
 002600            03 FILLER               PIC X(9)  VALUE 'JUNHO'.             
 002800            03 FILLER               PIC X(9)  VALUE 'JULHO'.             
 002900            03 FILLER               PIC X(9)  VALUE 'AGOSTO'.            
 003000            03 FILLER               PIC X(9)  VALUE 'SETEMBRO'.          
 003001            03 FILLER               PIC X(9)  VALUE 'OUTUBRO'.           
 003002            03 FILLER               PIC X(9)  VALUE 'NOVEMBRO'.          
 003003            03 FILLER               PIC X(9)  VALUE 'DEZEMBRO'.          
 003004        01  TABELA-MESES  REDEFINES NOMES-MES.                           
 003005            03 NOME-MES             PIC X(9)  OCCURS 12 TIMES.                    
 003006        01  NOMES-SEM.                                                   
 003007            03 FILLER               PIC X(13) VALUE 'SEGUNDA-FEIRA'.     
 003008            03 FILLER               PIC X(13) VALUE 'TERCA-FEIRA'.       
 003009            03 FILLER               PIC X(13) VALUE 'QUARTA-FEIRA'.      
 003010            03 FILLER               PIC X(13) VALUE 'QUINTA-FEIRA'.      
 003011            03 FILLER               PIC X(13) VALUE 'SEXTA-FEIRA'.       
 003012            03 FILLER               PIC X(13) VALUE 'SABADO'.            
 003013            03 FILLER               PIC X(13) VALUE 'DOMINGO'.           
 003014        01  TABELA-SEMANA REDEFINES NOMES-SEM.                           
 003015            03 NOME-SEM             PIC X(13) OCCURS 7  TIMES.           
 003016        LINKAGE SECTION.                                                 
 003020        01  LK-AREA-COM.                                                 
 003030            05 LK-COD-MES-COM         PIC 99.                            
 003040            05 LK-COD-DIASEM-COM      PIC 9.                             
 003050            05 LK-NOME-MES-COM        PIC X(9).                          
 003060            05 LK-NOME-DIASEM-COM     PIC X(13).                         
 003100       *                                                                                                                                
 003200        PROCEDURE DIVISION USING LK-AREA-COM.                            
 003300        000-PRINCIPAL SECTION.                                           
 003400        001-PRINCIPAL.                                                   
 003500            PERFORM 101-INICIAR.                                         
 003600            PERFORM 201-PROCESSAR.                                       
 003700            PERFORM 901-FINALIZAR.                                       
 003800            GOBACK.                                                      
 003900       ***********************************************************       
 004000        100-INICIAR SECTION.                                             
 004100        101-INICIAR.                                                     
 004200            EXIT.                                                        
 004400       ***********************************************                   
 004500        200-PROCESSAR SECTION.                                           
 004600        201-PROCESSAR.                                                   
 004700            MOVE NOME-MES(LK-COD-MES-COM)     TO LK-NOME-MES-COM.        
 004800            MOVE NOME-SEM(LK-COD-DIASEM-COM)  TO LK-NOME-DIASEM-COM.     
 005100       ***********************************************                                   
 005200        900-FINALIZAR SECTION.                                           
 005300        901-FINALIZAR.                                                   
 005400            EXIT.                                