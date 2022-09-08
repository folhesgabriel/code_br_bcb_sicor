#################################################################
######################wrang_br_bcb_sicor_liberacao###############
#################################################################
rm(list=ls())
options(scipen = 999)

#libs
library(tidyverse)

#funcoes para criar diretorios particionados
dir_create_ano <- function(ano){
  
  dir.create(paste0("~/Template Dados/output/microdados_liberacao/ano=",ano),
             showWarnings = FALSE,
             recursive = T)
}
dir_create_mes <- function(mes){
  
  dir.create(paste0("~/Template Dados/output/microdados_liberacao/ano=",ano,"/mes=",mes),
             showWarnings = FALSE,
             recursive = T)
}


arquivos_brutos_liberacao <- list.files("~/Template Dados/input",
                                        full.names = T,
                                        pattern = 'LIBERACAO'
                                        )


for(arquivo in arquivos_brutos_liberacao){
  
  file <- read_delim(arquivo, 
                     delim = ';') %>% 
    select( 
      data_liberacao = `#LIR_DT_LIBERACAO`,
      valor_liberado =  LIR_VL_LIBERADO, 
      id_referencia_bacen = REF_BACEN,     
      numero_ordem =  NU_ORDEM
    )  %>%  
    mutate(
      data_liberacao = as.Date(paste0(str_sub(data_liberacao, 7, 10), "/", str_sub(data_liberacao, 1, 5))),
      valor_liberado = as.double(valor_liberado),
      id_referencia_bacen = as.character(id_referencia_bacen),
      numero_ordem = as.double(numero_ordem),
      ano = as.integer(str_sub(data_liberacao, 1,4)),
      mes = as.integer(str_sub(data_liberacao,6,7))
    ) %>% 
    #esse filtro foi realizado pois na base bruta existem anos preenchidos errados. 
    #201 aparece uma vez; 1753 aparece outra vez...
    #201 - 1; 1753 - 6;2000-1;2011- 1;2023-2;2049-1;2106-1;2109;2173-1;2501-1.
    filter(ano %in% c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022))

    
      for(ano in unique(file$ano)){
        a <- ano
        dir_create_ano(ano)
    print(paste0('dir', ano,'criado'))
         for(mes in unique(file$mes)){
         dir_create_mes(mes)     
      b <- mes
      
      file_particionado <- file %>% 
        filter(ano %in% a &
               mes %in% b)
      
      write.csv(file_particionado,
                paste0("~/output/microdados_liberacao/ano=",ano,"/mes=",mes,"/microdados_liberacao.csv"),
                na = '',
                row.names = F,
                fileEncoding = "UTF-8")
    
    print(paste0('base', mes, "criada"))
    
  }
  }
}

rm(file)


