
### INSTALANDO PACOTE NO R ###

# install.packages("dplyr")
# install.packages('MASS')
library(dplyr)
library(MASS)


### IMPORTAR BASE .CVS NO R ###

base_unica = read.csv2("C:\\Users\\Public\\Tibolão\\01 - Particular\\08 - Palestras e Cursos\\UFBA - 2024\\01 - Mini Curso\\01 - Dados\\base_unica.csv", sep = ";")


### VISUALIZAR A BASE NO R ###

View(base_unica)


### TRANSFORMAR A VARIÁVEL NUMERICA EM CATEGORICA E REFERENCIAR O NEUTRO ###

# C_tipo_trabalho
# C_metodo_pagamento
# C_status_pre_analise
# C_troca
# C_renda
# C_parcela
# C_parcela_perc_renda
# C_preco_troca
# C_ano_troca
# C_entrada
# C_entrada_perc_preco_veiculo
# C_veiculo_preco
# C_veiculo_ano
# C_Distancia_SJC
# C_Idade

base_unica$C_tipo_trabalho <- factor(base_unica$C_tipo_trabalho)
base_unica$C_tipo_trabalho <- relevel(base_unica$C_tipo_trabalho, ref = '99')
levels(base_unica$C_tipo_trabalho)

base_unica$C_metodo_pagamento <- factor(base_unica$C_metodo_pagamento)
base_unica$C_metodo_pagamento <- relevel(base_unica$C_metodo_pagamento, ref = '99')
levels(base_unica$C_metodo_pagamento)

base_unica$C_status_pre_analise <- factor(base_unica$C_status_pre_analise)
base_unica$C_status_pre_analise <- relevel(base_unica$C_status_pre_analise, ref = '99')
levels(base_unica$C_status_pre_analise)

base_unica$C_troca <- factor(base_unica$C_troca)
base_unica$C_troca <- relevel(base_unica$C_troca, ref = '99')
levels(base_unica$C_troca)

base_unica$C_renda <- factor(base_unica$C_renda)
base_unica$C_renda <- relevel(base_unica$C_renda, ref = '99')
levels(base_unica$C_renda)

base_unica$C_parcela <- factor(base_unica$C_parcela)
base_unica$C_parcela <- relevel(base_unica$C_parcela, ref = '99')
levels(base_unica$C_parcela)

base_unica$C_parcela_perc_renda <- factor(base_unica$C_parcela_perc_renda)
base_unica$C_parcela_perc_renda <- relevel(base_unica$C_parcela_perc_renda, ref = '99')
levels(base_unica$C_parcela_perc_renda)

base_unica$C_preco_troca <- factor(base_unica$C_preco_troca)
base_unica$C_preco_troca <- relevel(base_unica$C_preco_troca, ref = '99')
levels(base_unica$C_preco_troca)

base_unica$C_ano_troca <- factor(base_unica$C_ano_troca)
base_unica$C_ano_troca <- relevel(base_unica$C_ano_troca, ref = '99')
levels(base_unica$C_ano_troca)

base_unica$C_entrada <- factor(base_unica$C_entrada)
base_unica$C_entrada <- relevel(base_unica$C_entrada, ref = '99')
levels(base_unica$C_entrada)

base_unica$C_entrada_perc_preco_veiculo <- factor(base_unica$C_entrada_perc_preco_veiculo)
base_unica$C_entrada_perc_preco_veiculo <- relevel(base_unica$C_entrada_perc_preco_veiculo, ref = '99')
levels(base_unica$C_entrada_perc_preco_veiculo)

base_unica$C_veiculo_preco <- factor(base_unica$C_veiculo_preco)
base_unica$C_veiculo_preco <- relevel(base_unica$C_veiculo_preco, ref = '99')
levels(base_unica$C_veiculo_preco)

base_unica$C_veiculo_ano <- factor(base_unica$C_veiculo_ano)
base_unica$C_veiculo_ano <- relevel(base_unica$C_veiculo_ano, ref = '99')
levels(base_unica$C_veiculo_ano)

base_unica$C_Distancia_SJC <- factor(base_unica$C_Distancia_SJC)
base_unica$C_Distancia_SJC <- relevel(base_unica$C_Distancia_SJC, ref = '99')
levels(base_unica$C_Distancia_SJC)

base_unica$C_Idade <- factor(base_unica$C_Idade)
base_unica$C_Idade <- relevel(base_unica$C_Idade, ref = '99')
levels(base_unica$C_Idade)


### SELEÇÃO DE VARIÁVEIS USANDO STEPWISE NO R ###

mod_unica = glm(data = base_unica, vendido ~ C_renda + C_parcela + C_parcela_perc_renda + C_preco_troca + C_ano_troca +
C_entrada + C_entrada_perc_preco_veiculo + C_veiculo_preco + C_veiculo_ano + C_Distancia_SJC + C_Idade + C_metodo_pagamento+
C_troca + C_status_pre_analise + C_tipo_trabalho,
family = binomial(link='logit'))

step(mod_unica, direction = 'both')

# Modelo selecionado pelo Stepwise
# Call:  glm(formula = vendido ~ C_renda + C_parcela_perc_renda + C_ano_troca + 
#    C_entrada + C_entrada_perc_preco_veiculo + C_veiculo_ano + 
#    C_Distancia_SJC + C_Idade + C_metodo_pagamento + C_status_pre_analise + 
#    C_tipo_trabalho, family = binomial(link = "logit"), data = base_unica)


### AJUSTANDO O MODELO ###

mod_unica = glm(formula = vendido ~ C_renda + C_parcela_perc_renda + C_ano_troca + 
    C_entrada + C_entrada_perc_preco_veiculo + C_veiculo_ano + 
    C_Distancia_SJC + C_Idade + C_metodo_pagamento + C_status_pre_analise + 
    C_tipo_trabalho, family = binomial(link = "logit"), data = base_unica)

summary(mod_unica)

### VERIFICANDO VARIÁVEL POR VARIÁVEL ###

mod_unica = glm(formula = vendido ~ C_renda, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_parcela_perc_renda, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_ano_troca, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_entrada, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_entrada_perc_preco_veiculo, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_veiculo_ano, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_Distancia_SJC, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_Idade, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_metodo_pagamento, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_status_pre_analise, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)

mod_unica = glm(formula = vendido ~ C_tipo_trabalho, family = binomial(link = "logit"), data = base_unica)
summary(mod_unica)


### NOVA SELEÇÃO DE VARIÁVEIS TIRANDO OS PERCENTUAIS ###

mod_unica = glm(data = base_unica, vendido ~ C_renda + C_parcela + C_preco_troca + C_ano_troca +
C_entrada + C_veiculo_preco + C_veiculo_ano + C_Distancia_SJC + C_Idade + C_metodo_pagamento+
C_troca + C_status_pre_analise + C_tipo_trabalho,
family = binomial(link='logit'))

step(mod_unica, direction = 'both')


### AJUSTANDO O MODELO ###

mod_unica = glm(formula = vendido ~ C_renda + C_parcela + C_entrada + C_veiculo_ano + 
    C_Distancia_SJC + C_Idade + C_metodo_pagamento + C_status_pre_analise + 
    C_tipo_trabalho, family = binomial(link = "logit"), data = base_unica)

summary(mod_unica)

