##################### TRABALHO SOBREVIVENCIA #####################

#### PACOTES ####

library(tidyverse)
library(survival)
library(AdequacyModel)

#### DADOS ####

tree <- read.csv("tree_data.csv")

# O banco de dados e composto por 3024 mudas de quatro especies arboreas submetidas 
# a diferentes condicoes de luminosidade e origem do solo. O objetivo e avaliar fatores
# associados a sobrevivencia das mudas ao longo do tempo. Para cada individuo registrou-se
# o tempo de acompanhamento (Time) e a ocorrencia do evento de interesse (Event), 
# definido como a morte da muda. Individuos que permaneceram vivos ate o final do 
# estudo ou foram removidos para medicoes laboratoriais foram tratados como observacoes
# censuradas.

#### ANALISE DESCRITIVA E EXPLORATORIA ####

### FUNCOES E GRAFICOS DE SOBREVIVENCIA E RISCO DOS DADOS SEM CONSIDERAR COVARIAVEIS ###

## KAPLER MEIER ##

km <- survfit(Surv(Time, Event) ~ 1, data = tree, conf.int = F); summary(km)
plot(km, conf.int = F)

## FUNCAO DE RISCO ACUMULADA ##

sobrevivencia_km <- km$surv
H_km <- -log(sobrevivencia_km)
plot(H_km, type = "s")

## GRAFICO TTT ##

TTT(tree$Time, col = "red", lwd = 2.5, grid = TRUE, lty = 2)

### FUNCOES E GRAFICOS DE SOBREVIVENCIA E RISCO DAS COVARIAVEIS SEPARADAMENTE ###

## PLOT ##

km_plot <- survfit(Surv(Time, Event) ~ Plot, data = tree, conf.int = F)
summary(km_plot)
teste_plot <- survdiff(Surv(Time, Event) ~ Plot, data = tree);teste_plot
# Aqui o grafico e dificil de ver, ja que sao 18 categorias. Porem, pelo teste de
# comparacao de curvas, existe diferenca entre as categorias.

## SUBPLOT ##

km_subplot <- survfit(Surv(Time, Event) ~ Subplot, data = tree, conf.int = F)
summary(km_subplot)
teste_subplot <- survdiff(Surv(Time, Event) ~ Subplot, rho = 1, data = tree);teste_subplot
plot(km_subplot, conf.int = F, col = rainbow(5), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Subplot)),
  col = rainbow(5),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor de 0.8) e do  (pelo teste de wilcoxom p-valor=0.6)
# grafico de sobrevivencia, as diferentes categorias desta variavel nao diferem,
# portanto nao devem ser consideradas para analises futuras

## ESPECIE ##

km_especie <- survfit(Surv(Time, Event) ~ Species, data = tree, conf.int = F)
summary(km_especie)
teste_especie <- survdiff(Surv(Time, Event) ~ Species,rho = 1, data = tree); teste_especie
plot(km_especie, conf.int = F, col = rainbow(4), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Species)),
  col = rainbow(4),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do
# grafico de sobrevivencia, existe evidencia de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.
# Uma coisa interessante e que existe "duplas" no grafico, mostrando que duas duplas
# de categorias tem comportamentos muito parecidos

## LUZ ##

km_luz <- survfit(Surv(Time, Event) ~ Light_Cat, data = tree, conf.int = F)
summary(km_luz)
teste_luz <- survdiff(Surv(Time, Event) ~ Light_Cat,rho = 1, data = tree); teste_luz
plot(km_luz, conf.int = F, col = rainbow(3), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Light_Cat)),
  col = rainbow(3),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor = 0.1) e do (pelo teste de wilcoxom p-valor=0.2)
# grafico de sobrevivencia, existe evidencia, apesar de pouca, de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.

## CORE ##

km_core <- survfit(Surv(Time, Event) ~ Core, data = tree, conf.int = F)
summary(km_core)
teste_core <- survdiff(Surv(Time, Event) ~ Core, rho = 1, data = tree); teste_core
plot(km_core, conf.int = F, col = rainbow(2), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Core)),
  col = rainbow(2),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do (pelo teste de wilcoxom p-valor=0.06)
# grafico de sobrevivencia, existe evidencia de que as categorias desta
# variavel diferem, portanto deve ser considerada para analises futuras.

## SOLO ##

km_solo <- survfit(Surv(Time, Event) ~ Soil, data = tree, conf.int = F)
summary(km_solo)
teste_solo <- survdiff(Surv(Time, Event) ~ Soil, rho = 1, data = tree); teste_solo
plot(km_solo, conf.int = F, col = rainbow(7), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Soil)),
  col = rainbow(7),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.

## ADULT ??? ##

## ESTERELIZACAO ##

km_este <- survfit(Surv(Time, Event) ~ Sterile, data = tree, conf.int = F)
summary(km_este)
teste_este <- survdiff(Surv(Time, Event) ~ Sterile, rho = 1, data = tree); teste_este
plot(km_este, conf.int = F, col = rainbow(2), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Sterile)),
  col = rainbow(2),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que as categorias desta
# variavel diferem, portanto deve ser considerada para analises futuras.

## CONSPECIFIC ##

km_conspecific <- survfit(Surv(Time, Event) ~ Conspecific, data = tree, conf.int = F)
summary(km_conspecific)
teste_conspecific <- survdiff(Surv(Time, Event) ~ Conspecific, rho = 1, data = tree); teste_conspecific
plot(km_conspecific, conf.int = F, col = rainbow(3), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Conspecific)),
  col = rainbow(2),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.

## MYCO ##

km_myco <- survfit(Surv(Time, Event) ~ Myco, data = tree, conf.int = F)
summary(km_myco)
teste_myco <- survdiff(Surv(Time, Event) ~ Myco , rho = 0, data = tree); teste_myco
plot(km_myco, conf.int = F, col = rainbow(2), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Myco)),
  col = rainbow(2),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que as categorias desta
# variavel diferem, portanto deve ser considerada para analises futuras.

## SOILMYCO ##

km_soilmyco <- survfit(Surv(Time, Event) ~ SoilMyco, data = tree, conf.int = F)
summary(km_soilmyco)
teste_soilmyco <- survdiff(Surv(Time, Event) ~ SoilMyco, rho = 1, data = tree); teste_soilmyco
plot(km_soilmyco, conf.int = F, col = rainbow(3), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$SoilMyco)),
  col = rainbow(3),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.

### CONCLUSAO DA ANALISE DESCRITIVA E EXPLORATORIA ###

#### MODELAGEM ####

### ESTIMACAO ###

## SURVREG ##

modelo_weibull <- survreg(Surv(Time, Event) ~ 1, data = tree, dist = "weibull"); summary(modelo_weibull)

modelo_loglogi <- survreg(Surv(Time, Event) ~ 1, data = tree, dist = "loglogistic"); summary(modelo_loglogi)

## METODO GRAFICO ##

# TEMPO #

tempo <- seq(0, max(tree$Time))

# WEIBULL #

gammaW <- 1/modelo_weibull$scale
alphaW <- exp(coef(modelo_weibull))

sW <- exp(-(tempo/alphaW)^gammaW)

# LOG LOGISTIC #

gammaLL <- 1/modelo_loglogi$scale
alphaLL <- exp(coef(modelo_loglogi))

sLL <- 1/(1 + (tempo/alphaLL)^gammaLL)

# GAMMA #

# GRAFICO DE COMPARACAO #

plot(km,
     conf.int = FALSE,
     xlab = "Tempo",
     ylab = "S(t)",
     col = "black",
     lwd = 2)

lines(tempo, sW, col = "blue", lwd = 2)
lines(tempo, sLL, col = "red", lwd = 2)

# Log-logística

legend("topright",
       legend = c("Kaplan-Meier",
                  "Weibull",
                  "Log-Logística"),
       col = c("black", "blue", "red"),
       lwd = 2)

## AIC, BIC, AICc ##

# AIC e BIC #

tab_criterios <- data.frame(
  Modelo = c("Weibull", "loglogistc"),
  AIC = c(AIC(modelo_weibull, modelo_loglogi)
  ),
  BIC = c(BIC(modelo_weibull, modelo_loglogi)
  )
)

tab_criterios

# AICc #

# definindo n e k

n <- nrow(tree)

k_weibull <- length(coef(modelo_weibull)) + 1

AICc <- function(modelo, k, n){
  aic <- AIC(modelo)
  aic + (2*k*(k+1))/(n-k-1)
}

AICc(modelo_weibull, k_weibull, n)

# A análise descritiva e exploratória dos dados permitiu caracterizar o comportamento
# inicial da sobrevivência das mudas e identificar possíveis fatores associados ao 
# tempo até a ocorrência do evento de interesse. A inspeção dos gráficos TTT e da 
# função de risco acumulado indicou um comportamento compatível com uma taxa de falha
# crescente ao longo do tempo, sugerindo que distribuições paramétricas capazes de 
# acomodar esse padrão constituem alternativas adequadas para a modelagem dos dados. 
# Dessa forma, nas etapas seguintes serão avaliados modelos assumindo as distribuições
# de Weibull e Gama, visando identificar aquela que melhor descreve o processo de 
# sobrevivência observado. Além disso, a variável Subplot não apresentou evidências
# de associação com a sobrevivência durante a análise exploratória, sendo, portanto,
# desconsiderada nas etapas subsequentes de modelagem, de modo a favorecer um modelo
# mais parcimonioso sem perda significativa de informação.