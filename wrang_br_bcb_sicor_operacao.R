#################################################################
######################wrang_br_bcb_sicor_operacao################
#################################################################
rm(list=ls())
options(scipen = 999)

#libs
library(tidyverse)



#ler path dos arquivos
arquivos_brutos_operacao <- list.files("~/Template Dados/input",
                                       full.names = T,
                                       pattern = 'OPERACAO')



for(arquivo in arquivos_brutos_operacao){
   
  file <- read_delim(arquivo, delim = ';')
  
  #criar a variavel temporaria ano nos dados brutos para iterar e criar particao
  
  file <- file %>% 
  select(
    id_referencia_bacen = `#REF_BACEN`, 
    numero_ordem = NU_ORDEM,
    sigla_uf = CD_ESTADO,
    id_categoria_emitente = CD_CATEG_EMITENTE,
    id_empreendimento = CD_EMPREENDIMENTO,
    id_fase_ciclo_producao = CD_FASE_CICLO_PRODUCAO, 
    id_fonte_recurso = CD_FONTE_RECURSO, 
    id_instrumento_credito = CD_INST_CREDITO,
    id_programa = CD_PROGRAMA,
    id_referencia_bacen_investimento = CD_REF_BACEN_INVESTIMENTO, 
    id_subprograma  = CD_SUBPROGRAMA, 
    id_tipo_agricultura = CD_TIPO_AGRICULTURA, 
    id_tipo_cultivo = CD_TIPO_CULTIVO,
    id_tipo_encargo_financeiro = CD_TIPO_ENCARG_FINANC, 
    id_tipo_grao_semente = CD_TIPO_GRAO_SEMENTE, 
    id_tipo_integracao_consorcio = CD_TIPO_INTGR_CONSOR, 
    id_tipo_irrigacao = CD_TIPO_IRRIGACAO, 
    id_tipo_seguro = CD_TIPO_SEGURO, 
    cnpj_agente_investimento = CNPJ_AGENTE_INVEST, 
    cnpj_instituicao_financeira = CNPJ_IF,
    data_emissao = DT_EMISSAO, 
    data_vencimento = DT_VENCIMENTO, 
    data_fim_colheita = DT_FIM_COLHEITA, 
    data_fim_plantio = DT_FIM_PLANTIO, 
    data_inicio_colheita = DT_INIC_COLHEITA, 
    data_inicio_plantio = DT_INIC_PLANTIO,
    area_financiada = VL_AREA_FINANC, 
    valor_aliquota_proagro = VL_ALIQ_PROAGRO, 
    valor_parcela_credito = VL_PARC_CREDITO, 
    valor_prestacao_investimento = VL_PRESTACAO_INVESTIMENTO, 
    valor_recurso_proprio = VL_REC_PROPRIO, 
    valor_receita_bruta_esperada = VL_RECEITA_BRUTA_ESPERADA, 
    valor_recurso_proprio_srv = VL_REC_PROPRIO_SRV, 
    valor_quantidade_itens_financiados = VL_QUANTIDADE, 
    valor_produtividade_obtida = VL_PRODUTIV_OBTIDA, 
    valor_previsao_producao = VL_PREV_PROD, 
    taxa_juro  = VL_JUROS, 
    taxa_juro_encargo_financeiro_posfixado = VL_JUROS_ENC_FINAN_POSFIX, 
    valor_percentual_risco_stn = VL_PERC_RISCO_STN,
    valor_percentual_custo_efetivo_total = VL_PERC_CUSTO_EFET_TOTAL, 
    valor_percentual_risco_fundo_constitucional = VL_PERC_RISCO_FUNDO_CONST 
  ) %>% 
    
    mutate(
      id_referencia_bacen = as.character(id_referencia_bacen), 
      numero_ordem = as.character(numero_ordem), 
      cnpj_instituicao_financeira = as.character(cnpj_instituicao_financeira), 
      data_emissao = as.Date(paste0(str_sub(data_emissao, 7, 10), "/", str_sub(data_emissao, 1, 5))), 
      data_vencimento = as.Date(paste0(str_sub(data_vencimento, 7, 10), "/", str_sub(data_vencimento, 1, 5))), 
      id_instrumento_credito = as.character(id_instrumento_credito),  
      id_categoria_emitente = as.character(id_categoria_emitente), 
      id_fonte_recurso = as.character(id_fonte_recurso), 
      cnpj_agente_investimento = as.character(cnpj_agente_investimento), 
      sigla_uf = as.character(sigla_uf),
      id_referencia_bacen_investimento = as.character(id_referencia_bacen_investimento), 
      id_tipo_seguro = as.character(id_tipo_seguro), 
      id_empreendimento = as.character(id_empreendimento), 
      id_programa = as.character(id_programa), 
      id_tipo_encargo_financeiro = as.character(id_tipo_encargo_financeiro), 
      id_tipo_irrigacao = as.character(id_tipo_irrigacao), 
      id_tipo_agricultura = as.character(id_tipo_agricultura), 
      id_fase_ciclo_producao = as.character(id_fase_ciclo_producao), 
      id_tipo_cultivo = as.character(id_tipo_cultivo),
      id_tipo_integracao_consorcio = as.character(id_tipo_integracao_consorcio), 
      id_tipo_grao_semente = as.character(id_tipo_grao_semente), 
      valor_aliquota_proagro = as.double(valor_aliquota_proagro), 
      taxa_juro  = as.double(taxa_juro), 
      valor_prestacao_investimento = as.double(valor_prestacao_investimento), 
      valor_previsao_producao = as.double(valor_previsao_producao), 
      valor_quantidade_itens_financiados = as.double(valor_quantidade_itens_financiados), 
      valor_receita_bruta_esperada = as.double(valor_receita_bruta_esperada), 
      valor_parcela_credito = as.double(valor_parcela_credito), 
      valor_recurso_proprio = as.double(valor_recurso_proprio), 
      valor_percentual_risco_stn = as.double(valor_percentual_risco_stn), 
      valor_percentual_risco_fundo_constitucional = as.double(valor_percentual_risco_fundo_constitucional), 
      valor_recurso_proprio_srv = as.double(valor_recurso_proprio_srv), 
      area_financiada = as.double(area_financiada), 
      id_subprograma  = as.character(id_programa), 
      valor_produtividade_obtida = as.double(valor_produtividade_obtida), 
      data_fim_colheita = as.Date(paste0(str_sub(data_fim_colheita, 7, 10), "/", str_sub(data_fim_colheita, 1, 5))), 
      data_fim_plantio = as.Date(paste0(str_sub(data_fim_plantio, 7, 10), "/", str_sub(data_fim_plantio, 1, 5))), 
      data_inicio_colheita = as.Date(paste0(str_sub(data_inicio_colheita, 7, 10), "/", str_sub(data_inicio_colheita, 1, 5))), 
      data_inicio_plantio = as.Date(paste0(str_sub(data_inicio_plantio, 7, 10), "/", str_sub(data_inicio_plantio, 1, 5))),
      taxa_juro_encargo_financeiro_posfixado = as.double(taxa_juro_encargo_financeiro_posfixado), 
      valor_percentual_custo_efetivo_total = as.double(valor_percentual_custo_efetivo_total),
      ano = as.integer(str_sub(data_emissao, 1, 4)),
      mes = as.integer(str_sub(data_liberacao,6,7))
  
    )

    for(ano in unique(file$ano)){
    
    dir.create(paste0("~/Template Dados/output/microdados_operacao/ano=",ano),
               showWarnings = FALSE,
               recursive = T)
      
      print(paste0('dir', ano,'criado'))
      
      for(mes in unique(file_particionado$mes)){
        dir.create(paste0("~/Template Dados/output/microdados_operacao/ano=",ano,"/mes=",mes),
                   showWarnings = FALSE,
                   recursive = T)
        
        print(paste0('dir', mes,'criado'))
        
  
      for(sigla_uf in unique(file$sigla_uf)){
         dir.create(paste0(~/Template Dados/output/microdados_operacao/ano=",ano,"/mes=",mes,"/uf=", sigla_uf),
                   showWarnings = FALSE,
                   recursive = T)
          
         a <- ano
         b <- mes
         b <- sigla_uf
        # print(paste0('var', a))
        # print(paste0('var', b))
         
        print(paste0('dir','_',ano,"_" ,sigla_uf, '_', 'criado'))
        
         file_particionado <- file %>% 
           filter(ano %in% a  &
                    mes %in% b  &
                  sigla_uf %in% c) %>% 
            
            #Essa variável foi idealizada para facilitar o uso da base para possíveis usuários. 
            #A política de crédito rural nacional é estipulada de acordo com o ano safra, que se extende
            #de julho a junho do ano seguinte. O montante de recursos subsidiados, taxas de juros e afins são
            #definido para o ano safra e não para o ano civil. 
           
           mutate(#caso sejam os 6 primeiros meses do ano, escreva ano-1 '/' ano corrente,
                  #caso contrario escreva ano corrente '/' ano+1 .
                   ano_safra_emissao = if_else(mes %in% c(1,2,3,4,5,6),
                                                                     paste0(ano-1, "/",ano),
                                                                     paste0(ano,"/",ano+1)),
                  plano_safra_vencimento = if_else(mes %in% c(1,2,3,4,5,6),
                                                   paste0(ano-1, "/",ano),
                                                   paste0(ano,"/",ano+1)))
           
        write.csv(file_particionado,
                    paste0("~/Template Dados/output/microdados_operacao/ano=",ano,"/uf=", sigla_uf,"/microdados_operacao.csv"),
                    na = "",
                    fileEncoding = "UTF-8",
                    row.names = FALSE
                   )
        print(paste0('base', ano, mes, sigla_uf, "criada"))
        
                    
  }  
 }
}
}

rm(file)
