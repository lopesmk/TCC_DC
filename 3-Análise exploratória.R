#====================================================================
# Análise e Exploração dos Dados
#====================================================================

# Configurando o diretório de trabalho
setwd("c:\\TCC_DC_PUC\\dados")
#setwd("/home/noel/Documentos/anderson")#

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
srag_sp_v1 <- fread("srag_sp_covid_v2.csv") # carrega os dados no formato data.table

# Transformando o datable em dataframe.
srag_sp_v1 <- data.frame(srag_sp_v1)

# Eliminando variável criada pelo R ao salva novo arquivo
srag_sp_v1$V1 <- NULL

# visão geral do dataset com as alterações
str(srag_sp_v1)
dim(srag_sp_v1)
# 179021 registro e 40 variáveis

#==================================================
#== Análise da variável Target - Evolução
#==================================================

# Transformando a variável EVOLUCAO em fator
srag_sp_v1$EVOLUCAO <- factor(srag_sp_v1$EVOLUCAO) 


# Verificando informações estatisticas básicas
summary(srag_sp_v1$EVOLUCAO)
# Cura                Óbito 
# 125971 = 70,37%     53050 = 29,63%

# Gráfico de Barras
ggplot(srag_sp_v1, aes(y= EVOLUCAO)) + geom_bar() +  
  labs(title="Variável target - Evolução", x="", y="")
# verificamos o dataset se encontra desbalanceado, apresentando 70,37% da variável target referentes a cura
# É importante fazer o balanceamento desta variável pois, do contrário, o modelo preditivo terá uma visão distorcida dos dados.

#==================================================
# Balanceamento do dataset
#==================================================

# Separando dados de óbito e cura
srag_sp_v1_obito <- filter(srag_sp_v1, EVOLUCAO == "Óbito")
dim(srag_sp_v1_obito)

srag_sp_v1_cura <- filter(srag_sp_v1, EVOLUCAO == "Cura")
dim(srag_sp_v1_cura)

# Obtendo amostragem de 53.050 casos de cura de forma aleatória.
# A aleatoriedade de sample, pode gerar ligeirar variação a cada execução.
# Devido a aleatoriedade, vamos salvar novo arquivo, para que possamos utilizar sempre os mesmo valores em todo o trabalho.
indexcura <- sample(1:nrow(srag_sp_v1_cura), size = 53050)
srag_sp_v1_cura <- srag_sp_v1_cura[indexcura,]

# unindo os dados de óbito e cura para o dataset balanceado
srag_sp_v1_balanceada <- rbind(srag_sp_v1_obito, srag_sp_v1_cura)
dim(srag_sp_v1_balanceada)


# Salvando o dataset balanceado (comentado para não sobrescrever o que vamos trabalhar até o fim do estudo)   
# write.csv(srag_sp_v1_balanceada, "srag_sp_covid_v3_balan.csv")

#==================================================
# Checkpoint-1: Deste ponto até o próximo checkpoint, iniciamos já com o dataset balanceado
# onde vamos trabalhar com as variáveis numéricas e excluir outliers.
#==================================================

# Carregando o Dataset balanceado
srag_sp_v1 <- fread("srag_sp_covid_v3_balan.csv") # carrega os dados no formato data.table

# Transformando o datable em dataframe.
srag_sp_v1 <- data.frame(srag_sp_v1)

# Eliminando variável criada pelo R ao salva novo arquivo
srag_sp_v1$V1 <- NULL

# Visão geral do dataset balanceado
dim(srag_sp_v1)

#==================================================
#== Tratando o tipo das variáveis fator após abertura do arquivo
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
#== INICIO - Análise Exploratória
#==================================================
     
# Obtendo a quantidade de cura e de óbito da variável evolução
cura <- filter(srag_sp_v1, EVOLUCAO == "Cura")     
obito <- filter(srag_sp_v1, EVOLUCAO == "Óbito")
dim(srag_sp_v1)
dim(cura)
dim(obito)

#==================================================
# Variáveis numéricas
#==================================================
#----------------------------------------
# PERIODO_PATOGENICO
#----------------------------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$PERIODO_PATOGENICO)
#Min.   1st Qu.   Median    Mean    3rd Qu.    Max. 
#0.00   10.00     15.00     18.43   22.00     317.00 

# Obtendo os valores para platagem do Histograma 
plot_periodo_patogenico <- qplot(PERIODO_PATOGENICO, data = srag_sp_v1, binwidth = 5, main = "Dataset completo", xlab = "Período em dias", ylab = "Qtd casos")
plot_periodo_patogenico_obito <- qplot(PERIODO_PATOGENICO, data = obito, binwidth = 5, main = "Somente óbito", xlab = "Período em dias", ylab = "Qtd casos")
plot_periodo_patogenico_alta <- qplot(PERIODO_PATOGENICO, data = cura, binwidth = 5, main = "Somente alta", xlab = "Período em dias", ylab = "Qtd casos")


# Plotando o histograma
grid.arrange(plot_periodo_patogenico, plot_periodo_patogenico_obito, plot_periodo_patogenico_alta, nrow = 1)
# Observamos que a curva do histograma está deslocada a esquerda, sugerindo a presença de outliers

# Plotando boxplot 
ggplot(srag_sp_v1, aes(x=PERIODO_PATOGENICO, y=EVOLUCAO)) + geom_boxplot() +  
  labs(title="Período patogênico",x="Duração em dias", y="")

# Identificamos 
count(filter(srag_sp_v1, PERIODO_PATOGENICO > 40))
count(filter(srag_sp_v1, PERIODO_PATOGENICO > 40 & EVOLUCAO == "Cura"))
count(filter(srag_sp_v1, PERIODO_PATOGENICO > 40 & EVOLUCAO == "Óbito"))

# O boxplot da variável PERIODO_PATOGENICO nos indica a presença de dados outliers. 
# Identificamos 6582 registros acima de 40 dias, 
# Destes, 2540 para dados de cura e 4042 para dados de óbito. 
# Podemos ajustar a normalidade dos dados para evitar overfiting no modelo

# Excluindo os dados com PERIODO_PATOGENICO de 40 para cima. 
srag_sp_v1 <- filter(srag_sp_v1, PERIODO_PATOGENICO <= 40)
dim(srag_sp_v1)

# Recalculando as quantidade da variável evolução
cura <- filter(srag_sp_v1, EVOLUCAO == "Cura")     
obito <- filter(srag_sp_v1, EVOLUCAO == "Óbito")

# Obtendo os novos valores após exclusão de parte dos outliers para platagem do Histograma 
plot_periodo_patogenico <- qplot(PERIODO_PATOGENICO, data = srag_sp_v1, binwidth = 5, main = "Dataset completo", xlab = "Período em dias", ylab = "Qtd casos")
plot_periodo_patogenico_obito <- qplot(PERIODO_PATOGENICO, data = obito, binwidth = 5, main = "Somente óbito", xlab = "Período em dias", ylab = "Qtd casos")
plot_periodo_patogenico_alta <- qplot(PERIODO_PATOGENICO, data = cura, binwidth = 5, main = "Somente alta", xlab = "Período em dias", ylab = "Qtd casos")

# Plotando o histograma após exclusão de parte dos outliers
grid.arrange(plot_periodo_patogenico, plot_periodo_patogenico_obito, plot_periodo_patogenico_alta, nrow = 1)

# Boxplot após exclusão de parte dos outliers
ggplot(srag_sp_v1, aes(x=PERIODO_PATOGENICO, y=EVOLUCAO)) + geom_boxplot() +  
  labs(title="Período patogênico",x="Duração em dias", y="")

# Padronização a variável PERIODO_PATOGENICO
srag_sp_v1$PERIODO_PATOGENICO <- scale(srag_sp_v1$PERIODO_PATOGENICO, center=T, scale=T)
summary(srag_sp_v1$PERIODO_PATOGENICO)

# Recalculando a quantidade de cura e de óbito após aliminação de parte dos outliers.
cura <- filter(srag_sp_v1, EVOLUCAO == "Cura")     
obito <- filter(srag_sp_v1, EVOLUCAO == "Óbito")


#-----------------------------------
# IDADE_EM_DIAS
#-----------------------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$IDADE_EM_DIAS)
#Min.   1st Qu.   Median    Mean    3rd Qu.    Max. 
#0      18453     23421     22836   27725     40312 

# Obtendo os valores para platagem do Histograma 
plot_idade_em_dias <- qplot(IDADE_EM_DIAS, data = srag_sp_v1, binwidth = 400, main = "Dataset completo", xlab = "Período em dias", ylab = "Qtd casos")
plot_idade_em_dias_obito <- qplot(IDADE_EM_DIAS, data = obito, binwidth = 400,  main = "Somente óbito", xlab = "Período em dias", ylab = "Qtd casos")
plot_idade_em_dias_alta <- qplot(IDADE_EM_DIAS, data = cura, binwidth = 400, main = "Somente alta", xlab = "Período em dias", ylab = "Qtd casos")

# Plotando o histograma
grid.arrange(plot_idade_em_dias, plot_idade_em_dias_obito ,plot_idade_em_dias_alta, nrow = 1)
# Observamos que a variável idade segue uma distribuição normal.

# Plotando boxplot 
ggplot(srag_sp_v1, aes(x=IDADE_EM_DIAS, y=EVOLUCAO)) + geom_boxplot() +  
  labs(title="Idade em dias",x="Dias", y="")


# Identificamos 
count(filter(srag_sp_v1, IDADE_EM_DIAS < 10000))
count(filter(srag_sp_v1, IDADE_EM_DIAS < 10000 & EVOLUCAO == "Cura"))
count(filter(srag_sp_v1, IDADE_EM_DIAS < 10000 & EVOLUCAO == "Óbito"))
# O boxplot da variável IDADE_EM_DIAS nos indica a presença de dados outliers. 
# Identificamos 2889 registros acima de 10.000 dias, 
# Deste 2521 para dados de cura e 368 para dados de óbito. 
# Podemos ajustar a normalidade dos dados para evitar overfiting no modelo


# Excluindo os dados com IDADE_EM_DIAS abaixo de 10000 dias. 
srag_sp_v1 <- filter(srag_sp_v1, IDADE_EM_DIAS >= 10000)

# Recalculando as quantidade da variável evolução
cura <- filter(srag_sp_v1, EVOLUCAO == "Cura")     
obito <- filter(srag_sp_v1, EVOLUCAO == "Óbito")


# Obtendo os novos valores após exclusão de parte dos outliers para platagem do Histograma 
plot_idade_em_dias <- qplot(IDADE_EM_DIAS, data = srag_sp_v1, binwidth = 400, main = "Dataset completo", xlab = "Período em dias", ylab = "Qtd casos")
plot_idade_em_dias_obito <- qplot(IDADE_EM_DIAS, data = obito, binwidth = 400,  main = "Somente óbito", xlab = "Período em dias", ylab = "Qtd casos")
plot_idade_em_dias_alta <- qplot(IDADE_EM_DIAS, data = cura, binwidth = 400, main = "Somente alta", xlab = "Período em dias", ylab = "Qtd casos")

# Plotando o histograma após exclusão de parte dos outliers
grid.arrange(plot_idade_em_dias, plot_idade_em_dias_obito ,plot_idade_em_dias_alta, nrow = 1)
# Observamos que a variável idade segue uma distribuição normal.

# Boxplot após exclusão de parte dos outliers
ggplot(srag_sp_v1, aes(x=IDADE_EM_DIAS, y=EVOLUCAO)) + geom_boxplot() +  
  labs(title="Idade em dias",x="Dias", y="") 

# padronização dos valores da variável IDADE_EM_DIAS
srag_sp_v1$IDADE_EM_DIAS <- scale(srag_sp_v1$IDADE_EM_DIAS, center=T, scale=T)
summary(srag_sp_v1$IDADE_EM_DIAS)

# Recalculando a quantidade de cura e de óbito após aliminação de parte dos outliers.
cura <- filter(srag_sp_v1, EVOLUCAO == "Cura")     
obito <- filter(srag_sp_v1, EVOLUCAO == "Óbito")


# Salvando o dataset balanceado, com outliers excluídos e normalizado    
write.csv(srag_sp_v1, "srag_sp_covid_v3_normal.csv")

#==================================================
# Checkpoint-2: Deste ponto em diante, iniciamos os trabalhos dataset balanceado, com outliers tratados e normalizados.
#==================================================

# Carregando o Dataset balanceado
srag_sp_v1 <- fread("srag_sp_covid_v3_normal.csv") # carrega os dados no formato data.table
# srag_sp_v1 <- fread("srag_sp_covid_v3_normal.csv", fileEncoding = 'UTF-8')
# Transformando o datable em dataframe.
srag_sp_v1 <- data.frame(srag_sp_v1)

# Eliminando variável criada pelo R ao salva novo arquivo
srag_sp_v1$V1 <- NULL

#==================================================
#== Tratando o tipo das variáveis fator após abertura do arquivo
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
# Variáveis discretas
#==================================================
#==================================================
#Variáveis do indivíduo
#==================================================

#-------------------
# CS_RACA
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$CS_RACA)
#Amarela   Branca   Ignorado    Indigena    Parda    Preta 
#1324      52046    19609       73          18601     4976 

# Gráfico de Barras 
ggplot(srag_sp_v1, aes(y = CS_RACA, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="CS_RACA",x="", y="") + 
  scale_x_continuous(breaks = scales::breaks_width(3000)) 
# Para todas as raça observamos que o numero de óbtos é maior que o da cura, exeto para o grupo inde está informação foi ignarada.
# observa-se também que para a raça branca, o numero de óbitos se destaca aos de cura.


#-------------------
# CS_SEXO
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$CS_SEXO)
# Feminino    Ignorado    Masculino 
# 41962        5           54662 

# Gráfico de Barras 
ggplot(srag_sp_v1, aes(y = CS_SEXO, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="CS_SEXO",x="", y="") + 
  scale_x_continuous(breaks = scales::breaks_width(3000))
# Observamos que os pacientes do sexo masculino são a maioria dos casos, bem como dos
# pouco ocorrem poucos mais óbitos que cura. Já o grupo feminino tem menor numero de casos e
# uma ligeira alta no número de cura.


#-------------------
# CS_ESCOL_N
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$CS_ESCOL_N)
# Fundamental 1    Fundamental 2    Ignorado      Médio       Sem escolaridade       Superior 
# 11058            6952             59378         11448       2019                    5774      

# Gráfico de Barras 
ggplot(srag_sp_v1, aes(x = CS_ESCOL_N, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="CS_ESCOL_N",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))
# Observamos que quanto menor o nível de escolaridade, maior é o número de morte. 
# O inverso é verdadeiro para a maior escolaridade e menor numero de morte.
# Isso pode passa pelas hipóteses de: Maior escolaridade = esclarecimentos e cuidados preventivos ou;
# Possibilidades de trabalho remoto e menor exposição a contaminação.





#==================================================
#Variáveis sinais e sintomas
#==================================================

#-------------------
# FEBRE
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$FEBRE)
# Ignorado      Não      Sim 
# 12465         27926    56238 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = FEBRE, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="FEBRE",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#TOSSE
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$TOSSE)
# Ignorado    Não      Sim 
# 9888        18944    67797 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = TOSSE, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="TOSSE",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#GARGANTA
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$GARGANTA)
# Ignorado      Não      Sim 
# 24063         57080    15486  

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = GARGANTA, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="GARGANTA",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#DISPNEIA
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$DISPNEIA)
# Ignorado      Não      Sim 
# 10026        17984    68619 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = DISPNEIA, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="DISPNEIA",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#DESC_RESP
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$DESC_RESP)
# Ignorado      Não      Sim 
# 15173        23578    57878 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = DESC_RESP, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="DESC_RESP",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#SATURACAO
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$SATURACAO)
# Ignorado      Não      Sim 
# 13518         21143    61968 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = SATURACAO, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="SATURACAO",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#DIARREIA
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$DIARREIA)
# Ignorado      Não      Sim 
# 25104         59095    12430 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = DIARREIA, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="DIARREIA",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#VOMITO
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$VOMITO)
# Ignorado      Não      Sim 
# 26547         63277     6805 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = VOMITO, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="VOMITO",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#DOR_ABD
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$DOR_ABD)
# Ignorado      Não      Sim 
# 58329        35679     2621 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = DOR_ABD, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="DOR_ABD",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#FADIGA
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$FADIGA)
# Ignorado      Não      Sim 
# 56860         27610    12159 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = FADIGA, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="FADIGA",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#PERD_OLFT
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$PERD_OLFT)
# Ignorado      Não      Sim 
# 58291       33208     5130

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = PERD_OLFT, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="PERD_OLFT",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#PERD_PALA
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$PERD_PALA)
# Ignorado      Não      Sim 
# 58311        32929     5389 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = PERD_PALA, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="PERD_PALA",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#OUTRO_SIN
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$OUTRO_SIN)
# Ignorado      Não      Sim 
# 27171        38676    30782

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = OUTRO_SIN, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="OUTRO_SIN",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))




#==================================================
#Variáveis grupo de risco e doenças pré-existentes
#==================================================

#-------------------
#FATOR_RISC
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$FATOR_RISC)
# Não     Sim 
# 28926   67703 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = FATOR_RISC, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="FATOR_RISC",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#PUERPERA
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$PUERPERA)
# Ignorado      Não      Sim 
# 54927         41578      124 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = PUERPERA, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="PUERPERA",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#CARDIOPATI
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$CARDIOPATI)
# Ignorado      Não      Sim 
# 40180         17487    38962 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = CARDIOPATI, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="CARDIOPATI",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#HEMATOLOGI
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$HEMATOLOGI)
# Ignorado      Não      Sim 
# 103736       70050     1502 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = HEMATOLOGI, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="HEMATOLOGI",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#SIND_DOWN
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$SIND_DOWN)
# Ignorado      Não      Sim 
# 54446        41947      236 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = SIND_DOWN, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="SIND_DOWN",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#HEPATICA
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$HEPATICA)
# Ignorado      Não        Sim 
# 54353         41242     1034 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = HEPATICA, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="HEPATICA",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#ASMA
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$ASMA)
# Ignorado      Não      Sim 
# 53807        40260     2562 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = ASMA, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="ASMA",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#DIABETES
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$DIABETES)
# Ignorado      Não      Sim 
# 44115       24460    28054 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = DIABETES, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="DIABETES",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#NEUROLOGIC
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$NEUROLOGIC)
# Ignorado      Não      Sim 
# 52766       38525     5338 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = NEUROLOGIC, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="NEUROLOGIC",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#PNEUMOPATI
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$PNEUMOPATI)
# Ignorado      Não      Sim 
# 53024         39091     4514 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x =PNEUMOPATI, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="PNEUMOPATI",x="", y="")


#-------------------
#IMUNODEPRE
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$IMUNODEPRE)
# Ignorado      Não      Sim 
# 53777       39912     2940

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = IMUNODEPRE, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="IMUNODEPRE",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#RENAL
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$RENAL)
# Ignorado      Não      Sim 
# 53163       38788     4678

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = RENAL, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="RENAL",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#OBESIDADE
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$OBESIDADE)
# Ignorado      Não      Sim 
# 53697       36526     6406 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = OBESIDADE, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="OBESIDADE",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#OUT_MORBI
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$OUT_MORBI)
# Ignorado      Não      Sim 
# 46361        22223    28045 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = OUT_MORBI, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="OUT_MORBI",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))







#==================================================
#Variáveis relacionadas ao tratamento
#==================================================

#-------------------
#ANTIVIRAL
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$ANTIVIRAL)
# Ignorado      Não      Sim 
# 19899       55365    21365 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = ANTIVIRAL, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="ANTIVIRAL",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#HOSPITAL
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$HOSPITAL)
# Ignorado      Não      Sim 
# 967           1948    93714 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = HOSPITAL, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="HOSPITAL",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(5000))


#-------------------
#UTI
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$UTI)
# Ignorado      Não      Sim 
# 9225        50038    37366 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = UTI, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="UTI",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#SUPORT_VEN
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$SUPORT_VEN)
# Ignorado    Não       Sim, invasivo     Sim, não invasivo 
# 11131       19456     20587             45455 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = SUPORT_VEN, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="SUPORT_VEN",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#EVOLUCAO
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$EVOLUCAO)
# Cura    Óbito 
# 47989   48640 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = EVOLUCAO, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="EVOLUCAO",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(5000))
# Variável target que está balanceada para o correto treinamento do modelo


#-------------------
#PCR_SARS2
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$PCR_SARS2)
# marcado pelo usuário          Não marcado 
# 82446                         14183 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = PCR_SARS2, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="PCR_SARS2",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#TOMO_RES
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$TOMO_RES)
# Atípico COVID-19      Ignorado      Indeterminado COVID-19       Não realizado      Negativo para Pneumonia       Outro        Tipico COVID-19 
# 802                   61336         1421                         5935               177                           2044                   24914 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = TOMO_RES, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="TOMO_RES",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))


#-------------------
#RES_AN
#-------------------
# Verificando informações estatisticas básicas
summary(srag_sp_v1$RES_AN)
# Aguardando resultado       Ignorado     Inconclusivo    Não realizado         Negativo             positivo 
# 39442                      10521        6               41394                 1761                 3505 

# Gráfico de Barras
ggplot(srag_sp_v1, aes(x = RES_AN, fill=EVOLUCAO)) + geom_bar(position = "dodge") +  
  labs(title="RES_AN",x="", y="") + 
  scale_y_continuous(breaks = scales::breaks_width(3000))

#==================================================
#== FIM - Análise Exploratória
#==================================================