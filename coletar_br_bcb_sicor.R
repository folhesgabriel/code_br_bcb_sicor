#############################################################
######################extracao dados bancen sicor############
#############################################################
rm(list=ls())
options(scipen = 999)

#libs
library(tidyverse)
library(netstat)
library(RSelenium)
library(xml2)

#setar driver
driver <- rsDriver(
  browser = 'chrome',
  chromever = '104.0.5112.79',
  verbose = F,
  port = free_port())

#criar var driver
driver <- driver$client

#abrir driver
driver$open()

#carregar pg com tabelas do sicor bacen
driver$navigate("https://www.bcb.gov.br/estabilidadefinanceira/creditorural?modalAberto=tabelas_sicor")

#raspar html 
html <- driver$getPageSource()
#fechar o drive 
driver$close()

#ler html
html1 <- read_html(html[[1]]) 

#selecionar css selector do html referente as tbls de microdados

#br_bcb_sicor:microdados_operacao
vec_operacao <- c(
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(2) > ul > li:nth-child(1) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(2) > ul > li:nth-child(2) > a'
                  )

#br_bcb_sicor:microdados_saldo
vec_saldos <- c(
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(1) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(2) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(3) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(4) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(5) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(6) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(7) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(8) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(9) > a',
                  'body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(3) > ul > li:nth-child(10) > a'
                   )

#br_bcb_sicor:microdados_liberacao
vec_liberacao <- c('body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(5) > li:nth-child(4) > a')

#br_bcb_sicor:empreendimento
vec_empreendimento <- c('body > modal-container > div.modal-dialog.modal-xl > div > div.modal-body > p > dynamic-comp > div > ul:nth-child(3) > li:nth-child(6) > a')


#juntar vetores
vec <- c(vec_liberacao, vec_saldos, vec_operacao, vec_empreendimento)#adicionar vec empreendimentos


#criar df para armazenar os links que serao extraidos do codigo html
tbl_bcb_sicor <- as_tibble(matrix(nrow = 1, ncol = 1))
#setar nomes das colunas do df
colnames(tbl_bcb_sicor) <- c( 'link')

#loop para extrair todos os links do codigo html da pg
for(i in vec){
  #itera o css nos vetores e extrai os links de downloads das tabelas
  link <- html1 %>%
    html_nodes(i) %>%
    html_attr('href')
  #alimenta a tibble acima com os links
  tbl_bcb_sicor  <- rbind(tbl_bcb_sicor, link)
}

#extrair nome das tabelas do link
tbl_bcb_sicor <- tbl_bcb_sicor %>% 
  #criar o nome das tabelas extraindo a ultima parte da URL (string depois do ultimo '/')
  mutate(nome_tabela = sub('.*\\/', '', tbl_bcb_sicor$link),
         #remover o tipo de formato do arquivo do nome da tabela
         nome_tabela = str_remove_all(nome_tabela, '.gz')) %>% 
  na.omit()


#remover vetores utilizados para criar tabela com links
rm(vec, vec_liberacao, vec_operacao, vec_saldos, vec_empreendimento)


#setar wd
setwd('~/Template Dados/input')

#baixar arquivos e salvar no diretorio
lapply(seq_along(tbl_bcb_sicor$link), function(x)
  download.file(tbl_bcb_sicor$link[x], 
                paste0(getwd(), "/", tbl_bcb_sicor$nome_tabela[x], ".csv")))







