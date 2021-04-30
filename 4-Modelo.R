# instalando bibliotecas necessárias
# install.packages(
#   "data.table",
#   "ggplot2",
#   "scales",
#   "magrittr",
#   "dplyr",
#   "forcats",
#   "tidyverse",
#   "zoo",
#   "caret",
#   "randomForest",
#   "rpart",
#   "ROCR",
#   "gridExtra",
#   "e1071"
# )


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
library(e1071)
# library(doMC) #somente para Linux
# registerDoMC(cores=7) #somente para Linux

# Carregando o Dataset
srag_sp_v1 <- fread("srag_sp_covid_v3_normal.csv") # carrega os dados no formato data.table

# Transformando so dados em dataframe e colcoando na mesma variável
srag_sp_v1 <- data.frame(srag_sp_v1)

# eliminando variável criada pelo R ao salva novo arquivo
srag_sp_v1$V1 <- NULL

# Colocando a variável evolução como última coluna do dataset. Isso facilitará na definição
# das variáveis de treino e teste.
srag_sp_v1$EVOLU <- srag_sp_v1$EVOLUCAO
srag_sp_v1$EVOLUCAO <- NULL

# verificando informações para prosseguir com estratégia do próximo passo.
dim(srag_sp_v1)
summary(srag_sp_v1)


#============================================
#== Tratando o tipo das variáveis fator
#============================================

    # Vetor com os campos que desejamos transformar em factor
    variaveis.fator <- c('CS_RACA','CS_ESCOL_N','FEBRE','TOSSE','GARGANTA','DISPNEIA', 'DESC_RESP',
                         'SATURACAO','DIARREIA','VOMITO','OUTRO_SIN','PUERPERA',
                         'CARDIOPATI','HEMATOLOGI','SIND_DOWN','HEPATICA','ASMA','DIABETES',
                         'NEUROLOGIC','PNEUMOPATI','IMUNODEPRE','RENAL','OBESIDADE','OUT_MORBI',
                         'ANTIVIRAL','HOSPITAL','UTI','SUPORT_VEN','PCR_SARS2','DOR_ABD',
                         'FADIGA','PERD_OLFT','PERD_PALA','TOMO_RES','RES_AN','CS_SEXO','FATOR_RISC', 'EVOLU' )


    # Função para converter as variáveis do vetor "variaveis.fator" para o tipo fator
    to.fator <- function(df, variables){
      for (variable in variables){
        df[[variable]] <- factor(df[[variable]])
      }
      return(df)
    }

    # Executa a função "to.fator" com os valores do vetor "variaveis.fator" e grava no mesmo dataset.
    srag_sp_v1 <- to.fator(srag_sp_v1, variaveis.fator)

#============================================
#== FIM - Tratando o tipo das variáveis fator
#============================================

    
#====================================================
#== Definindo a Amostragem - Dados de treino e teste
#====================================================
    #====================================================
    ##### Dados e variáveis de todo dataset 
    #====================================================
    
    # Dividindo os dados em treino e teste - 60:40 ratio (cross validate)
    indexes <- sample(1:nrow(srag_sp_v1), size = 0.6 * nrow(srag_sp_v1))
    
    # Preparando dados de treino e teste de todo dataset
    dados_treino_full <- srag_sp_v1[indexes,] # 60% dos dados para treinar o modelo 
    dados_teste_full <- srag_sp_v1[-indexes,] # 40% dos dados para testar o modelo
    
    # Salvando os dados de treino e de teste para que se possa chegar nos mesmos resultados apresentados no TCC.
    #write.csv(dados_treino_full, "dados_treino_full.csv")
    #write.csv(dados_teste_full, "dados_teste_full.csv")
  
    # Carregando os dados de treino e teste anteriormente salvos.
    dados_treino_full <- fread("dados_treino_full.csv")
    dados_teste_full <- fread("dados_teste_full.csv")
    
    # Transformandos os dados de treino e de teste em dataframe
    dados_treino_full <- data.frame(dados_treino_full)
    dados_teste_full <- data.frame(dados_teste_full)
    
    # Eliminando variável criada pelo R ao salva novo arquivo
    dados_treino_full$V1 <- NULL
    dados_teste_full$V1 <- NULL
    
    dados_treino_full <- to.fator(dados_treino_full, variaveis.fator)
    dados_teste_full <- to.fator(dados_teste_full, variaveis.fator)    
    
    # Preparando variáveis de teste
    var_teste_full <- dados_teste_full[,-40] 
    #pegando as 39 variáveis, menos a ultima que é a de evolução
    
    # Separando a variável de treino
    var_teste_full_target <- dados_teste_full[,40] 
    #pegando somente a variável target
    
    # Tranformando os dados para zero e um, compatibilizando com os dados do modelo.
    var_teste_full_target = factor(var_teste_full_target, labels = c(0, 1))
  
    
    #====================================================
    ##### Amostra de 10% dos dados para a seleção das variáveis mais relevantes
    #====================================================
    #Verificando informações da amostragem de 10% do dataset para a seleção de variáveis.
    iamostra <- sample(1:nrow(srag_sp_v1), size = 0.1 * nrow(srag_sp_v1))
    amostra_selecao_variaveis <- srag_sp_v1[iamostra,]
    
    
    #====================================================
    ##### Dados e somente as variáveis mais relevantes 
    #====================================================
    # Preparar o dataset  com as 11 mais relevantes + variável preditora
    list_var_maior_relevancia <- c('IDADE_EM_DIAS', 'SUPORT_VEN', 'UTI', 'PERIODO_PATOGENICO', 
    'SATURACAO', 'FATOR_RISC', 'CS_RACA', 'TOMO_RES', 'OUT_MORBI', 'DESC_RESP', 'CS_ESCOL_N', 'EVOLU')
    
    # Preparando dados de treino e teste
    dados_treino_maior_relevancia <- dados_treino_full[,list_var_maior_relevancia] 
    dados_teste_maior_relevancia <- dados_teste_full[,list_var_maior_relevancia] 
    
    # Preparando variáveis de teste
    var_teste_maior_relevancia <- dados_teste_maior_relevancia[,-12] #todas as variáveis menos a EVOLU
    var_teste_maior_relevancia_target <- dados_teste_maior_relevancia[,12] #Pega somente a variável EVOLU
    
    #tranformando os dados para zero e um, compatibilizando com os dados do modelo.
    var_teste_maior_relevancia_target = factor(var_teste_maior_relevancia_target, labels = c(0, 1))
    #=========================================================
    #== FIM - Dados e somente as variáveis mais relevantes 
    #=========================================================


#============================================
#== Feature Selection -  Seleção de Variáveis
#============================================
# Está sessão comentada devido a demora para conclusão do processo. Uma vez concluída, salvamos o resultado
# e carregamos este para uso.
    
#?rfe
#Função para seleção de variáveis
# run.feature.selection <- function(num.iters=20, feature.vars, class.var){
#   set.seed(10)
#   variable.sizes <- 1:39
#   control <- rfeControl(functions = rfFuncs, method = "cv",
#                         verbose = FALSE, returnResamp = "all",
#                         number = num.iters)
#   results.rfe <- rfe(x = feature.vars, y = class.var,
#                      sizes = variable.sizes,
#                      rfeControl = control)
#   return(results.rfe)
# }
# 
# train.data[,-40]
# 
# 
# # Executando a função
# rfe.results <- run.feature.selection(feature.vars = amostra_selecao_variaveis[,-40],
#                                      class.var = amostra_selecao_variaveis[,40])
# 
# #verificando os resultados da seleção de variváveis
# rfe.results
# 
# # Salvando os resultado da seleção das variáveis
# saveRDS(rfe.results, "rfe_results.rds")

#==============================================
#==  FIM - Feature Selection -  Seleção de Variáveis
#==============================================

#===================================================
#== Definindo a Amostragem - Dados de treino e teste
#====================================================

# Carregando resultado variáveis mais relevantes
rfe.results <- readRDS("rfe_results.rds", refhook = NULL)
      rfe.results

varImp((rfe.results))

#==============================================
# Criando e Avaliando o Modelo
#=============================================
library(caret) 
library(ROCR) 
#==============================================================================================
# modelo_glm_full_var - Construindo um modelo de regressão logística utilizando todas as 
# variáveis do dataset (39 variáveis) (exceto a variável target) 
#==============================================================================================
# Montando o modelo de regressão logistica, (GLM) modelo linear generalizado da familia binomial
target_formula <- "EVOLU ~ ."
target_formula <- as.formula(target_formula)
modelo_glm_full_var <- glm(formula = target_formula, data = dados_treino_full, 
                           family = "binomial")

# Salvando o modelo utilizado no TCC
#saveRDS(modelo_glm_full_var, "result_regressao_glm_full_var.rds")

# Carregando resultado modelo GLM com todas as variáveis
modelo_glm_full_var <- readRDS("result_regressao_glm_full_var.rds", refhook = NULL)

# Testando o modelo com os dados de teste
modelo_glm_full_predicao <- predict(modelo_glm_full_var, dados_teste_full, type="response")
modelo_glm_full_predicao <- round(modelo_glm_full_predicao)

# Avaliando o modelo com os valores da variável target
var_matriz_confusao_glm_full_var <- confusionMatrix(table(data = modelo_glm_full_predicao,
                                    reference = var_teste_full_target), positive = '1')

# Salvando a matriz de confusão do TCC
#saveRDS(var_matriz_confusao_glm_full_var, "matriz_confusao_glm_full_var.rds")

# Carregando o resultado da matriz confusão do TCC
arq_matriz_confusao_glm_full_var <- readRDS("matriz_confusao_glm_full_var.rds", refhook = NULL)

# Criando curva ROC
source("plot_utils.R")
ROC_modelo_glm_full_var <- modelo_glm_full_var
ROC_modelo_glm_full_var_valores <- predict(ROC_modelo_glm_full_var, var_teste_full, type = "response")
ROC_predicoes.glm.full <- prediction(ROC_modelo_glm_full_var_valores, var_teste_full_target)
plot.roc.curve(ROC_predicoes.glm.full, title.text = "Curva ROC")


#================================================================================================================
#==  modelo_glm_relevante_var - Construindo um modelo de regressão logística utilizando variáveis
# que tiveram pontuação geral maior que 10 no algoritmo recursive feature elimination - RFE
#================================================================================================================

# Montando o modelo de regressão logistica, (GLM) modelo linear generalizado da familia binomial
target_formula <- "EVOLU ~ ."
target_formula <- as.formula(target_formula)
modelo_glm_relevante_var <- glm(formula = target_formula, data = dados_treino_maior_relevancia, 
                                family = "binomial")

# Salvando o modelo utilizado no TCC
#saveRDS(modelo_glm_relevante_var, "result_regressao_glm_relevante_var.rds")

# Carregando resultado modelo GLM com as principais variáveis.
modelo_glm_relevante_var <- readRDS("result_regressao_glm_relevante_var.rds", refhook = NULL)

# Testando o modelo com os dados de teste
modelo_glm_relevantes_predicao <- predict(modelo_glm_relevante_var, 
                                dados_teste_maior_relevancia, type="response")
modelo_glm_relevantes_predicao <- round(modelo_glm_relevantes_predicao)

# Avaliando o modelo com os valores da variável target
var_matriz_confusao_glm_relevante_var <- confusionMatrix(table(data = modelo_glm_relevantes_predicao,
                                      reference = var_teste_maior_relevancia_target), positive = '1')

# Salvando a matriz de confusão do TCC
#saveRDS(var_matriz_confusao_glm_relevante_var, "matriz_confusao_glm_relevante_var.rds")

# Carregando o resultado da matriz confusão do TCC
arq_matriz_confusao_glm_relevante_var <- readRDS("matriz_confusao_glm_relevante_var.rds", refhook = NULL)

# Criando curva ROC
source("plot_utils.R")
ROC_modelo_glm_relevante_var <- modelo_glm_relevante_var
ROC_modelo_glm_relevante_var_valores <- predict(ROC_modelo_glm_relevante_var, var_teste_maior_relevancia, type = "response")
ROC_predicoes_glm_relevante_var <- prediction(ROC_modelo_glm_relevante_var_valores, var_teste_maior_relevancia_target)
plot.roc.curve(ROC_predicoes_glm_relevante_var, title.text = "Curva ROC")


#==============================================================================================
# modelo_naive_full_var - Construindo um modelo de regressão logística utilizando
# todas as variáveis do dataset (39 variáveis) (exceto a variável target) 
#==============================================================================================

# Montando o modelo classificador probalilístico 
target_formula <- "EVOLU ~ ."
target_formula <- as.formula(target_formula)
modelo_naive_full_var <- naiveBayes(formula = target_formula, data = dados_treino_full)

# Salvando o modelo utilizado no TCC
#saveRDS(modelo_naive_full_var, "result_regressao_naives_full_var.rds")

# Carregando resultado modelo NaivesBayes com todas as variáveis
modelo_naive_full_var <- readRDS("result_regressao_naives_full_var.rds", refhook = NULL)

# Testando o modelo com os dados de teste
modelo_naive_full_predicao <- predict(modelo_naive_full_var, dados_teste_full)

# Alterando os valores de texto para 0 e 1, sendo Cura = 0 e Óbito = 1
modelo_naive_full_predicao <- ifelse(modelo_naive_full_predicao=="Óbito", 1, 0)

# Avaliando o modelo com os valores da variável target
var_matriz_confusao_naive_full_var <- confusionMatrix(table(data = modelo_naive_full_predicao, 
                                          reference = var_teste_full_target), positive = '1')

# Salvando a matriz de confusão do TCC
#saveRDS(var_matriz_confusao_naive_full_var, "matriz_confusao_naive_full_var.rds")

# Carregando o resultado da matriz confusão do TCC
arq_matriz_confusao_naive_full_var <- readRDS("matriz_confusao_naive_full_var.rds", refhook = NULL)

# Criando curva ROC
source("plot_utils.R")
ROC_predicoes.naive_full <- prediction(modelo_naive_full_predicao, var_teste_full_target)
plot.roc.curve(ROC_predicoes.naive_full, title.text = "Curva ROC")


#================================================================================================================
#==  modelo_naive_relevante_var - Construindo um modelo de regressão logística utilizando variáveis
# que tiveram pontuação geral maior que 10 no algoritmo recursive feature elimination - RFE
#================================================================================================================

# Montando o modelo classificador probalilístico 
target_formula <- "EVOLU ~ ."
target_formula <- as.formula(target_formula)
modelo_naive_relevante_var <- naiveBayes(formula = target_formula, data = dados_treino_maior_relevancia)

# Salvando o modelo utilizado no TCC
#saveRDS(modelo_naive_relevante_var, "result_regressao_naives_relevante_var.rds")

# Carregando resultado modelo NaivesBayes com as principais variáveis.
modelo_naive_relevante_var <- readRDS("result_regressao_naives_relevante_var.rds", refhook = NULL)

# Testando o modelo com os dados de teste
modelo_naive_relevante_predicao <- predict(modelo_naive_relevante_var, dados_teste_maior_relevancia)

# Alterando os valores de texto para 0 e 1, sendo Cura = 0 e Óbito = 1
modelo_naive_relevante_predicao <- ifelse(modelo_naive_relevante_predicao=="Óbito", 1, 0)

# Avaliando o modelo com os valores da variável target
var_matriz_confusao_naive_relevante_var <- confusionMatrix(table(data = modelo_naive_relevante_predicao,
                                        reference = var_teste_maior_relevancia_target), positive = '1')

# Salvando a matriz de confusão do TCC
#saveRDS(var_matriz_confusao_naive_relevante_var, "matriz_confusao_naive_relevante_var.rds")

# Carregando o resultado da matriz confusão do TCC
arq_matriz_confusao_naive_relevante_var <- readRDS("matriz_confusao_naive_relevante_var.rds", refhook = NULL)

# Criando curva ROC
source("plot_utils.R")
ROC_predicoes.naive_relevante <- prediction(modelo_naive_relevante_predicao, var_teste_maior_relevancia_target)
plot.roc.curve(ROC_predicoes.naive_relevante, title.text = "Curva ROC")

#=====================================
#Curva ROV dos resultados
perf_glm_full <- performance(ROC_predicoes.glm.full, measure='tpr', x.measure='fpr')
perf_glm_relevante <- performance(ROC_predicoes_glm_relevante_var, measure='tpr', x.measure='fpr')
perf_naive_full <- performance(ROC_predicoes.naive_full, measure='tpr', x.measure='fpr')
perf_naive_relevante <- performance(ROC_predicoes.naive_relevante, measure='tpr', x.measure='fpr')

roc_glm_full <- data.frame(fpr=unlist(perf_glm_full@x.values), tpr=unlist(perf_glm_full@y.values))
roc_glm_full$method <- "GLM Full"

roc_glm_relevante <- data.frame(fpr=unlist(perf_glm_relevante@x.values), tpr=unlist(perf_glm_relevante@y.values))
roc_glm_relevante$method <- "GLM Relevantes"

roc_naive_full <- data.frame(fpr=unlist(perf_naive_full@x.values), tpr=unlist(perf_naive_full@y.values))
roc_naive_full$method <- "Naive Full"

roc_naive_relevante <- data.frame(fpr=unlist(perf_naive_relevante@x.values), tpr=unlist(perf_naive_relevante@y.values))
roc_naive_relevante$method <- "Naive Relevantes"

rbind(roc_glm_full, roc_glm_relevante, roc_naive_full, roc_naive_relevante) %>%
  ggplot(data=., aes(x=fpr, y=tpr, linetype=method, color=method)) + 
  geom_line() +
  geom_abline(a=1, b=0, linetype=2) +
  scale_x_continuous(labels=percent, lim=c(0,1)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  theme(legend.position=c(0.8,0.2), legend.title=element_blank())
