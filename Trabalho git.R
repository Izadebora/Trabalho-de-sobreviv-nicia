##################### TRABALHO SOBREVIVENCIA #####################

#### PACOTES ####

library(tidyverse)
library(survival)

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
teste_subplot <- survdiff(Surv(Time, Event) ~ Subplot, data = tree);teste_subplot
plot(km_subplot, conf.int = F, col = rainbow(5), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Subplot)),
  col = rainbow(5),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor de 0.8) e do 
# grafico de sobrevivencia, as diferentes categorias desta variavel nao diferem,
# portanto nao devem ser consideradas para analises futuras

## ESPECIE ##

km_especie <- survfit(Surv(Time, Event) ~ Species, data = tree, conf.int = F)
summary(km_especie)
teste_especie <- survdiff(Surv(Time, Event) ~ Species, data = tree); teste_especie
plot(km_especie, conf.int = F, col = rainbow(4), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Species)),
  col = rainbow(4),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.01) e do 
# grafico de sobrevivencia, existe evidencia de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.


## LUZ ##

km_luz <- survfit(Surv(Time, Event) ~ Light_Cat, data = tree, conf.int = F)
summary(km_luz)
teste_especie <- survdiff(Surv(Time, Event) ~ Species, data = tree, rho = 1); teste_especie
plot(km_luz, conf.int = F, col = rainbow(3), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Light_Cat)),
  col = rainbow(3),
  lty = 2,
  bty = "n",
  cex = 0.8
)

#