#################################################################
######################wrang_br_bcb_sicor_saldo################
#################################################################
rm(list=ls())
options(scipen = 999)

#libs
library(tidyverse)


#funcao para criar diretorios
dir_create_ano <- function(ano){
  
  dir.create(paste0("~/sicor_bd/Template Dados/output/microdados_saldo/ano=",ano),
             showWarnings = FALSE,
             recursive = T)
}
dir_create_mes <- function(mes){
  
  dir.create(paste0("~/Template Dados/output/microdados_saldo/ano=",ano,"/mes=",mes),
             showWarnings = FALSE,
             recursive = T)
}


#ler path dos arquivos de input
arquivos_brutos_saldo <- list.files("C:/Users/gabri/OneDrive/vida_profissional/Projetos/amazonDB/sicor_bd/Template Dados/input",
                                       full.names = T,
                                       pattern = 'SALDO')



#--breve explicacao da estrutura do script:
#uma dos acompanhamentos de uma operacao de crédito é evolucao do saldo.
#o BACEN armazena os saldos de todas as operacoes contratadas em um mesmo ano
#junto com a evolucao de saldo para os seguintes. 
#Ex: O arquivo 'SALDOS_2015' contém as contratações de 2015 e seus respectivos saldos
#até o momento em que a operacao for quitada. Como resultado, esse arquivo de 2015
#contem saldos para 2015,2016,2017,2018,2019....

#Por isso separei o script em 2 partes.
#1. um primeiro loop le os arquivos e os particiona por ano e mes.

#2. Em seguida um segundo loop utliza as funcoes "create_list" e "save_tibble"
#para criar uma lista com todos os arquivos de um mes e ano, fazer um bind_rows
#e salvar em uma tabela formato csv.

#loop inicial particionar os dados por ano e mes
for(arquivo in arquivos_brutos_saldo){
  
  file <- read_delim(arquivo,
                     delim = ';') %>% 
    mutate(ANO_BASE = as.integer(ANO_BASE),
           MES_BASE = as.integer(MES_BASE))
  
  print(paste0("base_","lida"))
  
   for(ano in unique(file$ANO_BASE)){
     dir_create_ano(ano)
     a <- ano
     
     for(mes in unique(file$MES_BASE)){
      dir_create_mes(mes)
       b <- mes
       print(paste0("selec", mes))  
     
       file_particionado <- file %>% 
       filter(ANO_BASE %in% a &
              MES_BASE %in% b)
     
     #gerar um identificador unico para um conjunto de arquivos particionados
     z <- arquivo %>% 
       as_tibble()
     
     z <- z %>% 
       mutate(nome_tabela = sub('.*\\/', '', z$value),
            nome_tabela = str_remove_all(nome_tabela, '.csv')) 
     
     z <- z$nome_tabela
     
     write.table(file_particionado,
                 paste0("~/output/microdados_saldo/ano=",ano,"/mes=",mes,"/microdados_saldo_",ano,"_",mes,z,".csv"),
                 na = '',
                 quote = FALSE,
                 row.names = FALSE,
                 sep = ",")
     
     print(paste0("microdados_", ano,"_",mes, "_criado"))
     rm(file_particionado)
   }  
}
}

#
  

create_list <- function(ano,mes){
  #le os arquivos de um ano e mes adicionando-os a uma lista
  arquivos <- list.files(paste0("~/ano=",ano,"/mes=",mes),
                         full.names = T)
  
  lista_arquivos <- list()
  
  for(arquivo in arquivos){
    
    arq <- read_csv(arquivo) %>% 
      select(
        id_referencia_bacen = `#REF_BACEN`,
        numero_ordem = NU_ORDEM,
        id_situacao_operacao = CD_SITUACAO_OPERACAO,
        ano = ANO_BASE,
        mes = MES_BASE,
        valor_medio_diario = VL_MEDIO_DIARIO,
        valor_medio_diario_vincendo = VL_MEDIO_DIARIO_VINCENDO, 
        valor_ultimo_dia = VL_ULTIMO_DIA
      ) %>% 
      mutate(
        ano = as.integer(ano),
        id_situacao_operacao = as.character(id_situacao_operacao), 
        mes = as.integer(mes),
        numero_ordem = as.character(numero_ordem),
        id_referencia_bacen = as.character(id_referencia_bacen),
        valor_medio_diario = as.double(valor_medio_diario),
        valor_medio_diario_vincendo = as.double(valor_medio_diario_vincendo), 
        valor_ultimo_dia = as.double(valor_ultimo_dia)
      )
    
    #adiciona arquivo na lista
    lista_arquivos[[arquivo]] <- arq
    
    print("arquivo add a lista")
  }
  return(lista_arquivos)
}

save_tibble <- function(lista,ano,mes){
  #salva os arquivos de um ano num arquivo unico
  lista_completa <- bind_rows(lista) 
  
  write.table(lista_completa,
              paste0("~/Template Dados/output/microdados_saldos/ano=",ano,"/mes=",mes,"/microdados_saldo_",ano,"_",mes,".csv"),
              na = '',
              quote = FALSE,
              row.names = FALSE,
              sep = ",")
  
  rm(lista_completa)
}

vec_ano <- c(2013:2022)
vec_mes <- c(1:12)

#loop para ler cada pasta, dar bin_rows no arquivos de cada ano e mes e salvar.
for(ano in vec_ano){
  for(mes in vec_mes){
  c <- create_list(ano,mes)
  save_tibble(c, ano, mes)  
}
}

