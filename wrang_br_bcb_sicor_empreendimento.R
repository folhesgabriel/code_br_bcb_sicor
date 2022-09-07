################################################
#########wrang_br_bcb_sicor_empreendimetos######
################################################
rm(list=ls())
options(scipen = 999)

#libs
library(tidyverse)


#ler path do arquivo de input
arquivo_bruto_empreendimento <- list.files("~/Template Dados/input",
                                    full.names = T,
                                    pattern = 'Empreendimento')
    
#ler arquivo
empreendimentos <- read_csv(arquivo_bruto_empreendimento,
                            #especificar o encoding do arquivo
                            locale = locale(encoding = "LATIN1"))


#funcao para remover acentos retirada do stackoverflow (https://stackoverflow.com/questions/39148759/remove-accents-from-a-dataframe-column-in-r)
rm_accent<- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}



#tratamento dos dados
empreendimentos <- empreendimentos %>% 
 select(
   #ordenar e renomear variaveis
   id_empreendimento = `#CODIGO`,
     data_inicio_empreendimento = DATA_INICIO,
     data_fim_empreendimento = DATA_FIM,
     finalidade = FINALIDADE,
     atividade = ATIVIDADE,
     modalidade = MODALIDADE,
     produto = PRODUTO,
     variedade = VARIEDADE,
     cesta_safra = CESTA,
     zoneamento = ZONEAMENTO,
     unidade_medida = UNIDADE_MEDIDA,
     unidade_medida_previsao_producao = UNIDADE_MEDIDA_PREVISAO,
     consorcio = CONSORCIO,
     cedula_mae = CEDULA_MAE,
     id_tipo_cultura = CD_TIPO_CULTURA
 ) %>% 

  mutate(#modificar tipos e tratar variaveis 
         id_empreendimento = as.character(id_empreendimento),
          data_inicio_empreendimento = if_else(is.na(data_inicio_empreendimento) == F,
            paste0(str_sub(data_inicio_empreendimento, 7,10), 
                                               str_sub(data_inicio_empreendimento, 3,6),
                                               str_sub(data_inicio_empreendimento, 1,2)),
                                               data_inicio_empreendimento),
          data_fim_empreendimento = if_else(is.na(data_fim_empreendimento) == F,
                                               paste0(str_sub(data_fim_empreendimento, 7,10), 
                                                      str_sub(data_fim_empreendimento, 3,6),
                                                      str_sub(data_fim_empreendimento, 1,2)),
                                               data_fim_empreendimento),
          finalidade = as.character(finalidade),
          atividade = as.character(atividade),
          modalidade = as.character(modalidade),
          produto = as.character(produto),
          variedade = as.character(variedade),
          cesta_safra = as.character(cesta_safra),
          zoneamento = as.character(zoneamento),
          unidade_medida = as.character(unidade_medida),
          unidade_medida_previsao_producao = as.character(unidade_medida_previsao_producao),
          consorcio = as.character(consorcio),
          cedula_mae = as.character(cedula_mae),
          id_tipo_cultura = as.character(id_tipo_cultura)) %>% 
  #mutar todas as colunas para caixa baixa
  mutate_all(funs(str_to_lower(.))) %>% 
  #retirar acentos de todas as colunas 
  mutate_all(funs(rm_accent(.)))


#criar diretorio
dir.create("~/Template Dados/output/empreendimento/",
                  showWarnings = FALSE,
                  recursive = T)

#salvar arquivo tratado
write.table(empreendimentos,
            "~/Template Dados/output/empreendimento/empreendimento.csv",
            na = '',
            quote = FALSE,
            row.names = FALSE,
            sep = ",")


