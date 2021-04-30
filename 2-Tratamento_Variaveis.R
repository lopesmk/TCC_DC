#====================================================================
# Definição do Problema a ser resolvido  
# Previsão da Ocorrorência de Óbito em casos de SRAG por Covid-19.
#====================================================================

#====================================================================
# Tratamento dos tipos de variaveis que serão trabalhadas bem como 
# dos valores missing (na)
#====================================================================


# Configurando o diretório de trabalho
setwd("c:\\TCC_DC_PUC\\dados")
#setwd("/home/noel/Documentos/anderson/srag")#

# Carregando Pacotes
library(data.table) # Provê a função fread, carrega arquivos pesados usando múltiplas threads
library(ggplot2) # Pacote para a criação de gráficos
library(scales)  # For percentage scales
library(magrittr) # Habilitando o operador %>%
library(dplyr) # Pacote para manipulação de dados
library(forcats) # Funções de frequência: fct_infreq
library(tidyverse) # Manipulação de data
library(zoo) # Manipulação de data
library(caret) # Pacote com algoritmos de machine learning
library(randomForest) # Pacote com algoritmo Random Forest
library(rpart) # Pacote com métodos de classificação e regressão usando árvores
library(ROCR) # Pacote para criação de curvas de avaliação para métodos de machine learning
library(gridExtra)


# Carregando o Dataset
srag_sp_v1 <- fread("srag_sp_covid_v1.csv") # carrega os dados no formato data.table

# eliminando variável criada pelo R ao salva novo arquivo
srag_sp_v1$V1 <- NULL

# Verifcando algumas informações iniciais do dataset.
dim(srag_sp_v1) #Dimensão do dataset: Nº de registro x variáveis.
str(srag_sp_v1) #Visão geral do dateset: variáveis, tipo de dados, nível de variação dos dados etc.
summary(srag_sp_v1) #Demostra cada variável com as quantidades de cada nível de registro.
sapply(srag_sp_v1, function(x)sum(is.na(x))) # Verificando os valores missing em cada variável.

#==================================================
#== Tratando os valores missing das variáveis com valores numéricos
#==================================================
    srag_sp_v1$CS_RACA <- coalesce(srag_sp_v1$CS_RACA,9)
    srag_sp_v1$CS_ESCOL_N <- coalesce(srag_sp_v1$CS_ESCOL_N,9)
    srag_sp_v1$FEBRE <- coalesce(srag_sp_v1$FEBRE,9)
    srag_sp_v1$TOSSE <- coalesce(srag_sp_v1$TOSSE,9)
    srag_sp_v1$GARGANTA <- coalesce(srag_sp_v1$GARGANTA,9)
    srag_sp_v1$DISPNEIA <- coalesce(srag_sp_v1$DISPNEIA,9)
    srag_sp_v1$DESC_RESP <- coalesce(srag_sp_v1$DESC_RESP,9)
    srag_sp_v1$SATURACAO <- coalesce(srag_sp_v1$SATURACAO,9)
    srag_sp_v1$DIARREIA <- coalesce(srag_sp_v1$DIARREIA,9)
    srag_sp_v1$VOMITO <- coalesce(srag_sp_v1$VOMITO,9)
    srag_sp_v1$OUTRO_SIN <- coalesce(srag_sp_v1$OUTRO_SIN,9)
    srag_sp_v1$PUERPERA <- coalesce(srag_sp_v1$PUERPERA,9)
    srag_sp_v1$CARDIOPATI <- coalesce(srag_sp_v1$CARDIOPATI,9)
    srag_sp_v1$HEMATOLOGI <- coalesce(srag_sp_v1$HEMATOLOGI,9)
    srag_sp_v1$SIND_DOWN <- coalesce(srag_sp_v1$SIND_DOWN,9)
    srag_sp_v1$HEPATICA <- coalesce(srag_sp_v1$HEPATICA,9)
    srag_sp_v1$ASMA <- coalesce(srag_sp_v1$ASMA,9)
    srag_sp_v1$DIABETES <- coalesce(srag_sp_v1$DIABETES,9)
    srag_sp_v1$NEUROLOGIC <- coalesce(srag_sp_v1$NEUROLOGIC,9)
    srag_sp_v1$PNEUMOPATI <- coalesce(srag_sp_v1$PNEUMOPATI,9)
    srag_sp_v1$IMUNODEPRE <- coalesce(srag_sp_v1$IMUNODEPRE,9)
    srag_sp_v1$RENAL <- coalesce(srag_sp_v1$RENAL,9)
    srag_sp_v1$OBESIDADE <- coalesce(srag_sp_v1$OBESIDADE,9)
    srag_sp_v1$OUT_MORBI <- coalesce(srag_sp_v1$OUT_MORBI,9)
    srag_sp_v1$ANTIVIRAL <- coalesce(srag_sp_v1$ANTIVIRAL,9)
    srag_sp_v1$HOSPITAL <- coalesce(srag_sp_v1$HOSPITAL,9)
    srag_sp_v1$UTI <- coalesce(srag_sp_v1$UTI,9)
    srag_sp_v1$SUPORT_VEN <- coalesce(srag_sp_v1$SUPORT_VEN,9)
    srag_sp_v1$PCR_SARS2 <- coalesce(srag_sp_v1$PCR_SARS2,9)
    srag_sp_v1$DOR_ABD <- coalesce(srag_sp_v1$DOR_ABD,9)
    srag_sp_v1$FADIGA <- coalesce(srag_sp_v1$FADIGA,9)
    srag_sp_v1$PERD_OLFT <- coalesce(srag_sp_v1$PERD_OLFT,9)
    srag_sp_v1$PERD_PALA <- coalesce(srag_sp_v1$PERD_PALA,9)
    srag_sp_v1$TOMO_RES <- coalesce(srag_sp_v1$TOMO_RES,9)
    srag_sp_v1$RES_AN <- coalesce(srag_sp_v1$RES_AN,9)
    
    # Verificando valores missing (NA) nas variáveis do dataset
    sapply(srag_sp_v1, function(x)sum(is.na(x)))
    str(srag_sp_v1)
#==================================================
#== FIM - Tratando os valores missing das variáveis numéricas
#==================================================


#==================================================
#== Tratando os valores missing das variáveis com valores de texto
#==================================================
    # atribuindo valores na variável sexo conforme equivalente no dicionário
    srag_sp_v1$CS_SEXO[srag_sp_v1$CS_SEXO == "M"] <- 1
    srag_sp_v1$CS_SEXO[srag_sp_v1$CS_SEXO == "F"] <- 2
    srag_sp_v1$CS_SEXO[srag_sp_v1$CS_SEXO == "I"] <- 9
    
    # atribuindo valores na variável fator de risco conforme equivalente no dicionário
    srag_sp_v1$FATOR_RISC[srag_sp_v1$FATOR_RISC == "S"] <- 1
    srag_sp_v1$FATOR_RISC[srag_sp_v1$FATOR_RISC == "N"] <- 2
#==================================================
#== FIM - Tratando os valores missing das variáveis de texto
#==================================================


#==================================================
#== Ajustando os labels das variáveis fatores conforme dicionário
#==================================================
    srag_sp_v1$CS_SEXO = factor(srag_sp_v1$CS_SEXO, labels = c("Masculino","Feminino","Ignorado"))
    srag_sp_v1$FATOR_RISC = factor(srag_sp_v1$FATOR_RISC, labels = c("Sim","Não"))
    srag_sp_v1$CS_RACA = factor(srag_sp_v1$CS_RACA, labels = c("Branca","Preta","Amarela","Parda","Indigena","Ignorado"))
    srag_sp_v1$CS_ESCOL_N = factor(srag_sp_v1$CS_ESCOL_N, labels = c("Sem escolaridade","Fundamental 1","Fundamental 2","Médio","Superior","Não se aplica","Ignorado"))
    srag_sp_v1$FEBRE = factor(srag_sp_v1$FEBRE, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$TOSSE = factor(srag_sp_v1$TOSSE, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$GARGANTA = factor(srag_sp_v1$GARGANTA, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$DISPNEIA = factor(srag_sp_v1$DISPNEIA, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$DESC_RESP = factor(srag_sp_v1$DESC_RESP, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$SATURACAO = factor(srag_sp_v1$SATURACAO, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$DIARREIA = factor(srag_sp_v1$DIARREIA, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$VOMITO = factor(srag_sp_v1$VOMITO, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$DOR_ABD = factor(srag_sp_v1$DOR_ABD, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$FADIGA = factor(srag_sp_v1$FADIGA, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$PERD_OLFT = factor(srag_sp_v1$PERD_OLFT, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$PERD_PALA = factor(srag_sp_v1$PERD_PALA, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$OUTRO_SIN = factor(srag_sp_v1$OUTRO_SIN, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$PUERPERA = factor(srag_sp_v1$PUERPERA, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$CARDIOPATI = factor(srag_sp_v1$CARDIOPATI, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$HEMATOLOGI = factor(srag_sp_v1$HEMATOLOGI, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$SIND_DOWN = factor(srag_sp_v1$SIND_DOWN, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$HEPATICA = factor(srag_sp_v1$HEPATICA, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$ASMA = factor(srag_sp_v1$ASMA, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$DIABETES = factor(srag_sp_v1$DIABETES, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$NEUROLOGIC = factor(srag_sp_v1$NEUROLOGIC, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$PNEUMOPATI = factor(srag_sp_v1$PNEUMOPATI, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$IMUNODEPRE = factor(srag_sp_v1$IMUNODEPRE, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$RENAL = factor(srag_sp_v1$RENAL, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$OBESIDADE = factor(srag_sp_v1$OBESIDADE, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$OUT_MORBI = factor(srag_sp_v1$OUT_MORBI, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$ANTIVIRAL = factor(srag_sp_v1$ANTIVIRAL, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$HOSPITAL = factor(srag_sp_v1$HOSPITAL, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$UTI = factor(srag_sp_v1$UTI, labels = c("Sim","Não","Ignorado"))
    srag_sp_v1$SUPORT_VEN = factor(srag_sp_v1$SUPORT_VEN, labels = c("Sim, invasivo","Sim, não invasivo","Não","Ignorado"))
    srag_sp_v1$EVOLUCAO = factor(srag_sp_v1$EVOLUCAO, labels = c("Cura","Óbito"))
    srag_sp_v1$PCR_SARS2 = factor(srag_sp_v1$PCR_SARS2, labels = c("marcado pelo usuário","Não marcado"))
    srag_sp_v1$TOMO_RES = factor(srag_sp_v1$TOMO_RES, labels = c("Tipico COVID-19","Indeterminado COVID-19","Atípico COVID-19", "Negativo para Pneumonia", "Outro", "Não realizado", "Ignorado"))
    srag_sp_v1$RES_AN = factor(srag_sp_v1$RES_AN, labels = c("positivo","Negativo","Inconclusivo", "Não realizado", "Aguardando resultado", "Ignorado" ))
#==================================================
#== FIM - Ajustando os labels das variáveis fatores
#==================================================



#==================================================
#== Tratando o tipo das variáveis de data 
#==================================================
    
    # Vetor com os campos de datas que desejamos transformar do tipo "chr" para date.
    variaveis.data <- c('DT_SIN_PRI','DT_NASC','DT_INTERNA','DT_ENTUTI','DT_SAIDUTI','DT_EVOLUCA')
    
    
    # Função para a conversão das variáveis para o tipo data
    to.date <- function(df, variables){
      for (variable in variables){
        df[[variable]] <- as.Date(df[[variable]], "%d/%m/%Y") 
      }
      return(df)
    }
    
    # Executa a função "to.date" com os valores do vetor "variaveis.data" e grava no mesmo dataset.
    srag_sp_v1 <- to.date(srag_sp_v1, variaveis.data)
    
    # Conferência da alteração dos tipos das variáveis para date
    str(srag_sp_v1$DT_SIN_PRI)  #Data dos primeiros sintomas
    str(srag_sp_v1$DT_NASC)     #Data de nascimento
    str(srag_sp_v1$DT_INTERNA)  #Data da internação
    str(srag_sp_v1$DT_ENTUTI)   #Data da entrada na UTI
    str(srag_sp_v1$DT_SAIDUTI)  #Data da sa?da da UTI
    str(srag_sp_v1$DT_EVOLUCA)  #Data da evolução (alta médica ou óbito)
    
    #-------------------------------------------------------
    # Analisando valores missing das variáveis do tipo data
    #-------------------------------------------------------
        # Não podemos substitur valores missing nos campos de data por zero. 
        # Isso aferaria os resultados dos modelos de forma negativa. 
        # Então, após a análise destas variáveis, vamos decidir por:
        # Excluir os registros vazios se as quantidades forem percentalmente baixa;
        # Deixar de utilizar a variável, caso tenha muitos registros missing e a sua exclusão ocasione baixa significativa no numero de registro restantes.
    
    #verificar numeros de registros missing no dataset. Neste momento, somente os campos de data apresentam registro missing, os demais já foram tratados.
    sapply(srag_sp_v1, function(x)sum(is.na(x)))
    
    # DT_SIN_PRI
    dim(filter(srag_sp_v1, is.na(DT_SIN_PRI) & EVOLUCAO == "Cura")) 
    dim(filter(srag_sp_v1, is.na(DT_SIN_PRI) & EVOLUCAO == "Óbito"))
    #  Não há valores missing, portanto, faremos uso desta variável.

    # DT_NASC 
    dim(filter(srag_sp_v1, is.na(DT_NASC) & EVOLUCAO == "Cura"))
    dim(filter(srag_sp_v1, is.na(DT_NASC) & EVOLUCAO == "Óbito"))
        # Existem 100 registro missing nesta variável. 
        # Destes, 64 evoluiram para a cura e 36 para óbito. 
        # Considerando que isso representa somente 0,053%, optamos pela exclusão dos 
        # registros missing e faremos uso desta variável.
        # Considerando que a maioria dos casos a serem excluídos estão na categoria
        # de evolução cura, a exclusão deste registro contribui para o balanceamento do dataset.
    
    # DT_INTERNA
    dim(filter(srag_sp_v1, is.na(DT_INTERNA) & EVOLUCAO == "Cura"))
    dim(filter(srag_sp_v1, is.na(DT_INTERNA) & EVOLUCAO == "Óbito"))
        # Existem nesta variável 6228 valores missing.  
        # Destes, 3846 evoluiram para a cura e 2382 para óbito. 
        # Considerando que isso representa somente 3,33%, poderiamos fazer a exclusão dos
        # registros missing e utilizar a variável, entretanto, como faremos o descarte de 
        # outras variáveis do tipo data que tem alto número de registros missing, está 
        # variável deixará de ser útil, portanto, optamos por descartar está variável em nossos modelos.
    
    # DT_ENTUTI
    dim(filter(srag_sp_v1, is.na(DT_ENTUTI) & EVOLUCAO == "Cura"))
    dim(filter(srag_sp_v1, is.na(DT_ENTUTI) & EVOLUCAO == "Óbito"))
        # Existem nesta variável 127138 registro missing. 
        # Destes, 103330 evoluiram para a cura e 23808 para óbito. 
        # Não se trata necessariamente de que os dados estão sem qualidade, mas sim, 
        # que a maioria dos casos não seguem para UTI, portanto sem data de entrada 
        # e saída da UTI. 
        # Considerando que a exclusão dos 127138 representam 68% do
        # do total de registros, optamos por descartar está variável em nossos modelos.
    
    # DT_SAIDUTI 
    dim(filter(srag_sp_v1, is.na(DT_SAIDUTI) & EVOLUCAO == "Cura"))
    dim(filter(srag_sp_v1, is.na(DT_SAIDUTI) & EVOLUCAO == "Óbito"))
        # Existem nesta variável 151717 registro missing. 
        # Destes, 115339 evoluiram para a cura e 36378 para óbito.     
        # Não se trata necessariamente de que os dados estão sem qualidade, mas sim, 
        # que a maioria dos casos não seguem para UTI, portanto sem data de entrada 
        # e saída da UTI. 
        # Considerando que a exclusão dos 127138 representam 81% do
        # do total de registros, optamos por descartar está variável em nossos modelos.
    
    # DT_EVOLUCA 
    dim(filter(srag_sp_v1, is.na(DT_EVOLUCA) & EVOLUCAO == "Cura")) 
    dim(filter(srag_sp_v1, is.na(DT_EVOLUCA) & EVOLUCAO == "Óbito"))
        # Existem nesta variável 7973 registro missing.
        # # Destes, 7933 evoluiram para a cura e 40 para óbito.     
        # Considerando que nossa variável target é a evolução (cura ou óbito) e que
        # os valores missing representam 4,26% dos registros, optamos pela exclusão dos 
        # registros missing e faremos uso desta variável.
        # Além disso, Considerando que a maioria dos casos a serem excluídos estão na categoria
        # de evolução cura, a exclusão deste registro contribui para o balanceamento do dataset.
    
    
    # DT_NASC - Eliminando valores NA que impossibilitam o cálculo da idade
    # Deletado 100 registros
    srag_sp_v1 <- subset(srag_sp_v1, !is.na(DT_NASC))
    dim(srag_sp_v1)
    
    # DT_EVOLUCA - Eliminando valores NA que impossibilitam o cálculo do período patogênico
    # Deletado 7973 registros 
    srag_sp_v1 <- subset(srag_sp_v1, !is.na(DT_EVOLUCA))
    dim(srag_sp_v1)
    
    # Eliminando as variáveis de data cujo optamos por não utilizar em nossos modelos
    srag_sp_v1$DT_INTERNA <- NULL
    srag_sp_v1$DT_ENTUTI <- NULL
    srag_sp_v1$DT_SAIDUTI <- NULL
    
    # Certificando-se de que não há nenhuma variável com valores missing.
    sapply(srag_sp_v1, function(x)sum(is.na(x)))
    
    str(srag_sp_v1)
#==================================================
#== FIM - Tratando o tipo das variáveis de data 
#==================================================
    


#==================================================
#== Tratando o tipo das variáveis fator
#==================================================

    # Vetor com os campos que desejamos transformar em factor
    variaveis.fator <- c('CS_RACA','CS_ESCOL_N','FEBRE','TOSSE','GARGANTA','DISPNEIA', 'DESC_RESP',
                         'SATURACAO','DIARREIA','VOMITO','OUTRO_SIN','PUERPERA', 
                         'CARDIOPATI','HEMATOLOGI','SIND_DOWN','HEPATICA','ASMA','DIABETES',
                         'NEUROLOGIC','PNEUMOPATI','IMUNODEPRE','RENAL','OBESIDADE','OUT_MORBI',
                         'ANTIVIRAL','HOSPITAL','UTI','SUPORT_VEN','EVOLUCAO','PCR_SARS2','DOR_ABD',
                         'FADIGA','PERD_OLFT','PERD_PALA','TOMO_RES','RES_AN','CS_SEXO','FATOR_RISC' )
    
    
    # Função para converter as variáveis do vetor "variaveis.fator" para o tipo fator
    to.fator <- function(df, variables){
      for (variable in variables){
        df[[variable]] <- factor(df[[variable]]) 
      }
      return(df)
    }
    
    # Executa a função "to.fator" com os valores do vetor "variaveis.fator" e grava no mesmo dataset.
    srag_sp_v1 <- to.fator(srag_sp_v1, variaveis.fator)

#==================================================
#== FIM - Tratando o tipo das variáveis fator
#==================================================



#==================================================
#== INICIO - Engenharia de atributos
#==================================================

# Transformando a variável srag_sp_v1 em data.frame
    srag_sp_v1 <- data.frame(srag_sp_v1)

# Utilizando variáveis de data para a criação de novas variáveis.

# Criando variável da período patogênico. 
# Periodo em dias da data dos primeiros sintomas até a cura ou óbito
# Calculado pela diferença em dias das variáveis DT_SIN_PRI e DT_EVOLUCA
    srag_sp_v1$PERIODO_PATOGENICO  <- difftime(srag_sp_v1$DT_EVOLUCA,srag_sp_v1$DT_SIN_PRI, units = c("days"))
    
# Criando variável de IDADE_EM_DIAS.
# Período em dias referente a idade que o paciente tinha na data dos primeiros sintomas.
# calculado pela diferença em dias das variáveis DT_SIN_PRI e DT_NASC
    srag_sp_v1$IDADE_EM_DIAS  <- difftime(srag_sp_v1$DT_SIN_PRI,srag_sp_v1$DT_NASC, units = c("days"))
    
# Transformando a variável período patogênico para o tipo numérica inteiro (int)
    srag_sp_v1$PERIODO_PATOGENICO <- as.numeric(srag_sp_v1$PERIODO_PATOGENICO, units="days")
   
# Transformando a variável idade em dias para o tipo numérica inteiro (int)
    srag_sp_v1$IDADE_EM_DIAS <- as.numeric(srag_sp_v1$IDADE_EM_DIAS, units="days")
     
# Eliminando as variáveis de data que foram utilizadas acima e não serão mais úteis
     srag_sp_v1$DT_SIN_PRI <- NULL
     srag_sp_v1$DT_EVOLUCA <- NULL
     srag_sp_v1$DT_NASC <- NULL

#==================================================
#== FIM - Engenharia de atributos
#==================================================

# Salvando as alterações em novo arquivo    
write.csv(srag_sp_v1, "srag_sp_covid_v2.csv")
     
     