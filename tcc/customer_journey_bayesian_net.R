library(tidyverse)
library(bnlearn)
library(Rgraphviz)

setwd("C:/dev/bayesian-nets/tcc")
# este csv está o google drive, na pasta TCC/Dados. O SQL tb está lá
journey <- read.csv("customer_journey.csv", header = TRUE)
head(journey)
colnames(journey)
str(journey)

# TODO: buscar usar metodo hibrido (continua + discreta)
# TODO: limpar a base para pegar apenas clientes que fizeram signup
#   dados faltantes podem ser um problema
# tentar usar log, pq preciso ter normalidade
# raiz quadrada/cubica pode ser melhor pq tem mto zero
hist(sqrt(journey$first_hr_sessions_minutes))
hist(journey[journey$first_hr_sessions_minutes > 0,]$first_hr_sessions_minutes)

journey <- journey %>% select(c(professional_email, did_sign_up,  invited_someone, returned_another_day, 
                     completed_tour, is_lead_contacted, is_demo_scheduled, is_became_customer
                     ))
str(journey)
# converting columns to factor
journey$professional_email <- as.factor(journey$professional_email)
journey$did_sign_up <- as.factor(journey$did_sign_up)
journey$invited_someone <- as.factor(journey$invited_someone)
journey$returned_another_day <- as.factor(journey$returned_another_day)
journey$completed_tour <- as.factor(journey$completed_tour)
journey$is_lead_contacted <- as.factor(journey$is_lead_contacted)
journey$is_demo_scheduled <- as.factor(journey$is_demo_scheduled)
journey$is_became_customer <- as.factor(journey$is_became_customer)
str(journey)

colnames(journey) <- c('ProEm', 'SgUp', 'Inv', 'Rtrn', 'Tour', 'Ctc', 'Demo', 'Buy')

########################
# ESTIMANDO ESTRUTURA
########################
learned <- hc(journey)
graphviz.plot(learned)


# creating black list
bl = data.frame(from = c("Buy", "Rtrn", "Ctc"), to = c("ProEm", "ProEm", "ProEm"))
learned <- hc(journey, blacklist=bl) # TODO: tentar Tabu. hc tem problemas de minimos locais
graphviz.plot(learned)

# improving the black list
bl = data.frame(from = c("Buy", "Rtrn", "Ctc", "Tour", "Demo", "Demo"), to = c("ProEm", "ProEm", "ProEm", "ProEm", "ProEm", "Ctc"))
learned <- hc(journey, blacklist=bl)
graphviz.plot(learned)

# improving the black list 2
bl = data.frame(from = c("Buy", "Rtrn", "Ctc", "Tour", "Demo", "Demo", "Inv"), to = c("ProEm", "ProEm", "ProEm", "ProEm", "ProEm", "Ctc", "ProEm"))
learned <- hc(journey, blacklist=bl)
graphviz.plot(learned)
graphviz.plot(learned, layout="fdp") # TODO: usar esse layout e tentar mudar fonte e tam do circulo
graphviz.plot(learned, layout="circo")


#################################################
# ESTIMANDO PROBABILIDADES CONDICIONAIS LOCAIS
#################################################
bn.mle <- bn.fit(learned, data = journey, method = "mle")
str(bn.mle)
bn.mle$Buy



bn.fit.barchart(bn.mle$Buy, main = "Buy",xlab = "Pr(Buy | Rtn,Demo)", ylab = "")


# Shiny:
# focar na exploracao do modelo e nao na estimacao
# vou deixar o modelo pronto e os gestores vao poder explorar
# estimacao interativa do modelo ficaria como uma sugestao de continuacao do trabalho
# para este trabalho, so se sobrar tempo

# TODO: estimacao via boostrap pois ele vai dar métricas que vao me ajudar e ver se faz sentido os arcos ou nao

# a parte da estimação vai ser importante pra eu estudar a teoria do que estou estimando
# tem muitos estimadores
# na teoria, me concentrar nos pressupostos e testa-los


