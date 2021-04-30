#====================================================================
# Definição do Problema a ser resolvido  
# Previsão da Ocorrorência de Óbito em casos de SRAG por Covid-19.
#====================================================================

#===================================================================
# Baixando o dataset, realizando uma primeira análise e, definindo 
# quais as variáveis podem ser imediatamente descartadas por não 
# influenciar os objetivos deste estudo.
#===================================================================

# Configurando o diretório de trabalho
setwd("c:\\TCC_DC_PUC\\dados")
#setwd("/home/noel/Documentos/anderson/srag")#


# Verificando o diretório configurado com o comando setwd
getwd()

# Carregando as Bibliotecas
library(data.table)
library(ggplot2)
library(scales)
library(magrittr)
library(dplyr)
library(forcats)
library(tidyverse)
library(zoo)
library(caret)
library(randomForest)
library(rpart)
library(ROCR)

#===================================================================
#== Obtendo os dados e realizando as primeiras preparações
#===================================================================

#------------------
# Fonte do Dataset
#------------------
# https://covid.saude.gov.br/
# dados SRAG - Síndrome Respiratória Grave - Casos Hospitalizados
# Dataset: https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD-01-03-2021.csv
# Ficha de Notificação: https://opendatasus.saude.gov.br/dataset/ae90fa8f-3e94-467e-a33f-94adbb66edf8/resource/54a46c6d-e0b5-40b7-8b74-85450d22ace3/download/ficha-srag-final-27.07.2020_final.pdf
# Dicionário de Dados: https://opendatasus.saude.gov.br/dataset/ae90fa8f-3e94-467e-a33f-94adbb66edf8/resource/8f571374-c555-4ec0-8e44-00b1e8b11c25/download/dicionario-de-dados-srag-hospitalizado-27.07.2020-final.pdf

#----------------------
# Carregando o Dataset
#----------------------
existe_arquivo <- if(file.exists("INFLUD-01-03-2021.csv")) {
  "INFLUD-01-03-2021.csv"  
} else {
  "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD-01-03-2021.csv"
}
srag <- fread(existe_arquivo) # carrega os dados no formato data.table

#-----------------------------------
# Explorando  dataset - Visão geral
#-----------------------------------

# Verificar as dimensões do dataset
dim(srag) 


# Verificando os nomes das variáveis
srag_names <- names(srag) #criando um array com o nome das variáveis
#View(srag_names)


# Visualização dos tipos atribuídos as variáveis pelo R no carregamento do dataset
str(srag)


# Verificando valores missing (NA) nas variáveis do dataset
sapply(srag, function(x)sum(is.na(x)))


#-----------------------------------
# Identificação das variáveis target => EVOLUCAO = 1-Cura, 2-óbito
# Aplicação dos pressupostos: Serão analaisado os dados do dataset que:
# 1) SG_UF_NOT = Ocorreram no estado de SP
# 2) CLASSI_FIN = 5 - SRAG por COVID-19
#-----------------------------------


# Criação de arrays com aplicação de filtros (pressupostos)  
srag_sp <- subset(srag, SG_UF_NOT == "SP")
srag_sp_covid <- subset(srag_sp, CLASSI_FIN == "5")
srag_sp_covid_v1 <- filter(srag_sp_covid, EVOLUCAO %in% c("1", "2"))


# Excluindo variáveis que não possuem relação com objetivo do problema.
srag_sp_covid_v1$COD_IDADE = NULL
srag_sp_covid_v1$DT_NOTIFIC = NULL
srag_sp_covid_v1$SEM_NOT = NULL
srag_sp_covid_v1$SEM_PRI = NULL
srag_sp_covid_v1$ID_MUNICIP = NULL
srag_sp_covid_v1$SG_UF_NOT = NULL
srag_sp_covid_v1$CO_MUN_NOT = NULL
srag_sp_covid_v1$ID_REGIONA = NULL
srag_sp_covid_v1$CO_REGIONA = NULL
srag_sp_covid_v1$ID_UNIDADE = NULL
srag_sp_covid_v1$CO_UNI_NOT = NULL
srag_sp_covid_v1$NU_IDADE_N = NULL
srag_sp_covid_v1$TP_IDADE = NULL
srag_sp_covid_v1$CS_GESTANT = NULL
srag_sp_covid_v1$CS_ETINIA = NULL
srag_sp_covid_v1$ID_PAIS = NULL
srag_sp_covid_v1$CO_PAIS = NULL
srag_sp_covid_v1$SG_UF = NULL
srag_sp_covid_v1$ID_RG_RESI = NULL
srag_sp_covid_v1$CO_RG_RESI = NULL
srag_sp_covid_v1$ID_MN_RESI = NULL
srag_sp_covid_v1$CO_MUN_RES = NULL
srag_sp_covid_v1$CS_ZONA = NULL
srag_sp_covid_v1$HISTO_VGM = NULL
srag_sp_covid_v1$PAIS_VGM = NULL
srag_sp_covid_v1$CO_PS_VGM = NULL
srag_sp_covid_v1$LO_PS_VGM = NULL
srag_sp_covid_v1$DT_VGM = NULL
srag_sp_covid_v1$DT_RT_VGM = NULL
srag_sp_covid_v1$SURTO_SG = NULL
srag_sp_covid_v1$NOSOCOMIAL = NULL
srag_sp_covid_v1$AVE_SUINO = NULL
srag_sp_covid_v1$OUT_ANIM = NULL
srag_sp_covid_v1$OUTRO_DES = NULL
srag_sp_covid_v1$OBES_IMC = NULL
srag_sp_covid_v1$MORB_DESC = NULL
srag_sp_covid_v1$VACINA = NULL
srag_sp_covid_v1$DT_UT_DOSE = NULL
srag_sp_covid_v1$MAE_VAC = NULL
srag_sp_covid_v1$DT_VAC_MAE = NULL
srag_sp_covid_v1$M_AMAMENTA = NULL
srag_sp_covid_v1$DT_DOSEUNI = NULL
srag_sp_covid_v1$DT_1_DOSE = NULL
srag_sp_covid_v1$DT_2_DOSE = NULL
srag_sp_covid_v1$TP_ANTIVIR = NULL
srag_sp_covid_v1$OUT_ANTIV = NULL
srag_sp_covid_v1$DT_ANTIVIR = NULL
srag_sp_covid_v1$SG_UF_INTE = NULL
srag_sp_covid_v1$ID_RG_INTE = NULL
srag_sp_covid_v1$CO_RG_INTE = NULL
srag_sp_covid_v1$ID_MN_INTE = NULL
srag_sp_covid_v1$CO_MU_INTE = NULL
srag_sp_covid_v1$RAIOX_RES = NULL
srag_sp_covid_v1$RAIOX_OUT = NULL
srag_sp_covid_v1$DT_RAIOX = NULL
srag_sp_covid_v1$TOMO_OUT = NULL
srag_sp_covid_v1$DT_TOMO = NULL
srag_sp_covid_v1$AMOSTRA = NULL
srag_sp_covid_v1$DT_COLETA = NULL
srag_sp_covid_v1$TP_AMOSTRA = NULL
srag_sp_covid_v1$OUT_AMOST = NULL
srag_sp_covid_v1$TP_TES_AN = NULL
srag_sp_covid_v1$DT_RES_AN = NULL
srag_sp_covid_v1$POS_AN_FLU = NULL
srag_sp_covid_v1$TP_FLU_AN = NULL
srag_sp_covid_v1$POS_AN_OUT = NULL
srag_sp_covid_v1$AN_SARS2 = NULL
srag_sp_covid_v1$AN_VSR = NULL
srag_sp_covid_v1$AN_PARA1 = NULL
srag_sp_covid_v1$AN_PARA2 = NULL
srag_sp_covid_v1$AN_PARA3 = NULL
srag_sp_covid_v1$AN_ADENO = NULL
srag_sp_covid_v1$AN_OUTRO = NULL
srag_sp_covid_v1$DS_AN_OUT = NULL
srag_sp_covid_v1$PCR_RESUL = NULL
srag_sp_covid_v1$DT_PCR = NULL
srag_sp_covid_v1$POS_PCRFLU = NULL
srag_sp_covid_v1$TP_FLU_PCR = NULL
srag_sp_covid_v1$PCR_FLUASU = NULL
srag_sp_covid_v1$FLUASU_OUT = NULL
srag_sp_covid_v1$PCR_FLUBLI = NULL
srag_sp_covid_v1$FLUBLI_OUT = NULL
srag_sp_covid_v1$POS_PCROUT = NULL
srag_sp_covid_v1$PCR_VSR = NULL
srag_sp_covid_v1$PCR_PARA1 = NULL
srag_sp_covid_v1$PCR_PARA2 = NULL
srag_sp_covid_v1$PCR_PARA3 = NULL
srag_sp_covid_v1$PCR_PARA4 = NULL
srag_sp_covid_v1$PCR_ADENO = NULL
srag_sp_covid_v1$PCR_METAP = NULL
srag_sp_covid_v1$PCR_BOCA = NULL
srag_sp_covid_v1$PCR_RINO = NULL
srag_sp_covid_v1$PCR_OUTRO = NULL
srag_sp_covid_v1$DS_PCR_OUT = NULL
srag_sp_covid_v1$TP_AM_SOR = NULL
srag_sp_covid_v1$DT_CO_SOR = NULL
srag_sp_covid_v1$TP_SOR = NULL
srag_sp_covid_v1$OUT_SOR = NULL
srag_sp_covid_v1$SOR_OUT = NULL
srag_sp_covid_v1$RES_IGG = NULL
srag_sp_covid_v1$RES_IGM = NULL
srag_sp_covid_v1$RES_IGA = NULL
srag_sp_covid_v1$DT_RES = NULL
srag_sp_covid_v1$CLASSI_FIN = NULL
srag_sp_covid_v1$CLASSI_OUT = NULL
srag_sp_covid_v1$CRITERIO = NULL
srag_sp_covid_v1$DT_ENCERRA = NULL
srag_sp_covid_v1$DT_DIGITA = NULL
srag_sp_covid_v1$PAC_COCBO = NULL
srag_sp_covid_v1$PAC_DSCBO = NULL


# Criação do novo dataset com as variáveis e valores selecionados
write.csv(srag_sp_covid_v1, "srag_sp_covid_v1.csv")


# Carregando o novo dataset
srag_sp_v1 <- fread("srag_sp_covid_v1.csv")


# eliminando variável criada pelo R ao salva novo arquivo
srag_sp_v1$V1 <- NULL

# Verificando a nova dimensão e nome das variáveis relacionadas ao objetivo do estudo
dim(srag_sp_v1)
names(srag_sp_v1)

# continuando os trabalhos com o próximo script
# source('2-Tratamento_Variaveis.R')