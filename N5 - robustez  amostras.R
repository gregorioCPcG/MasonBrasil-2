#N5 - robustez  PCA sem P50 e sem imputação nas amostras separadas(questão interv/golpe)

# !!!!!!!!!! Nao rodar nada - tudo do zero


#obs - projeto BASE NOVA

library(tidyverse)
rm(list=ls())
library(sjPlot)
library(wesanderson)
library(memisc)
library(huxtable)
df <- read_csv("df.csv")




#
#removendo NA´s das questões P50 a P56 e também da P58

table(df$P50)#
df$P50_recod <- factor(ifelse(df$P50 == 1, "Tanto Faz",
                              ifelse(df$P50 == 2, "Preferível",
                                     ifelse(df$P50 == 3, "Governo autoritário",
                                            "NS/NR"))))
table(df$P50_recod)
df$P50_recod_numeric <- as.numeric(ifelse(is.na(df$P50) | df$P50 == 4 | df$P50 == 5, NA, df$P50))
table(df$P50_recod_numeric)
df$P50_antidemoc_positivo <- ifelse(df$P50_recod_numeric == 2, 0, 1)
table(df$P50_antidemoc_positivo)

table(df$P51)#
df$P51_recod_numeric <- as.numeric(df$P51)
df$P51_recod_numeric[df$P51_recod_numeric %in% c(11, 12)] <- NA
table(df$P51_recod_numeric)
df$P51_recod <- as.character(df$P51)
df$P51_recod[df$P51_recod == "11"] <- "NS/NR"
df$P51_recod[df$P51_recod == "12"] <- "NS/NR"
df$P51_recod[!is.na(df$P51_recod_numeric)] <- as.character(as.numeric(df$P51_recod_numeric[!is.na(df$P51_recod_numeric)]))
df$P51_recod <- factor(df$P51_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "NS/NR"))
table(df$P51_recod)

table(df$P52)#
df$P52_recod_numeric <- as.numeric(df$P52)
df$P52_recod_numeric[df$P52_recod_numeric %in% c(11, 12)] <- NA
table(df$P52_recod_numeric)
df$P52_recod <- as.character(df$P52)
df$P52_recod[df$P52_recod == "11"] <- "NS/NR"
df$P52_recod[df$P52_recod == "12"] <- "NS/NR"
df$P52_recod[!is.na(df$P52_recod_numeric)] <- as.character(as.numeric(df$P52_recod_numeric[!is.na(df$P52_recod_numeric)]))
df$P52_recod <- factor(df$P52_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P52_recod)

table(df$P53)#
df$P53_recod_numeric <- as.numeric(df$P53)
df$P53_recod_numeric[df$P53_recod_numeric %in% c(11, 12)] <- NA
table(df$P53_recod_numeric)
df$P53_recod <- as.character(df$P53)
df$P53_recod[df$P53_recod == "11"] <- "NS/NR"
df$P53_recod[df$P53_recod == "12"] <- "NS/NR"
df$P53_recod[!is.na(df$P53_recod_numeric)] <- as.character(as.numeric(df$P53_recod_numeric[!is.na(df$P53_recod_numeric)]))
df$P53_recod <- factor(df$P53_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P53_recod)

table(df$P54)#
df$P54_recod_numeric <- as.numeric(df$P54)
df$P54_recod_numeric[df$P54_recod_numeric %in% c(11, 12)] <- NA
table(df$P54_recod_numeric)
df$P54_recod <- as.character(df$P54)
df$P54_recod[df$P54_recod == "11"] <- "NS/NR"
df$P54_recod[df$P54_recod == "12"] <- "NS/NR"
df$P54_recod[!is.na(df$P54_recod_numeric)] <- as.character(as.numeric(df$P54_recod_numeric[!is.na(df$P54_recod_numeric)]))
df$P54_recod <- factor(df$P54_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P54_recod)

table(df$P55)#
df$P55_recod_numeric <- as.numeric(df$P55)
df$P55_recod_numeric[df$P55_recod_numeric %in% c(11, 12)] <- NA
table(df$P55_recod_numeric)
df$P55_recod <- as.character(df$P55)
df$P55_recod[df$P55_recod == "11"] <- "NS/NR"
df$P55_recod[df$P55_recod == "12"] <- "NS/NR"
df$P55_recod[!is.na(df$P55_recod_numeric)] <- as.character(as.numeric(df$P55_recod_numeric[!is.na(df$P55_recod_numeric)]))
df$P55_recod <- factor(df$P55_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P55_recod)

table(df$P56)
df$P56_recod_numeric_apoio_positivo <- df$P56
df$P56_recod_numeric_apoio_positivo[df$P56 == 4 | df$P56 == 5] <- NA
df$P56_recod_numeric_apoio_positivo[df$P56 == 1 | df$P56 == 2] <- 1
df$P56_recod_numeric_apoio_positivo[df$P56 == 3] <- 0
table(df$P56_recod_numeric_apoio_positivo)
df$P56_recod_8jan <- factor(df$P56, levels = c(1, 2, 3, 4, 5), labels = c("Apoio", "Apoio", "Não Apoio", "NS/NR", "NS/NR"))
table(df$P56_recod_8jan)


table(df$P58)#
df$P58_recod_numeric <- as.numeric(df$P58)
df$P58_recod_numeric[df$P58_recod_numeric %in% c(11, 12)] <- NA
table(df$P58_recod_numeric)
df$P58_recod <- as.character(df$P58)
df$P58_recod[df$P58_recod == "11"] <- "NS/NR"
df$P58_recod[df$P58_recod == "12"] <- "NS/NR"
df$P58_recod[!is.na(df$P58_recod_numeric)] <- as.character(as.numeric(df$P58_recod_numeric[!is.na(df$P58_recod_numeric)]))
df$P58_recod <- factor(df$P58_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P58_recod)


#ok todas estão no formato original, até aqui, no entanto, é necessário:
#padronizar
#padronizar? Deixar todas com o mesmo sinal
#a escolha foi deixar sinal positivo para posição antidemocrática


table(df$P50_antidemoc_positivo)# sinal ok

#P51 10 é totalmente justificável fechar o congresso
table(df$P51_recod_numeric) # ok


#P52  formato original: 10 é “concordo fortemente que a democracia eleitoral é o melhor”
#essa é uma que tem inverter
table(df$P52_recod)#tem q arrumar!
table(df$P52_recod_numeric)#tem q arrumar!

df$P52_recod <- memisc::recode(as.character(df$P52_recod ), "0" <- c("10"),
                               "1" <-c("9"),
                               "2"<-c("8"),
                               "3"<-c("7"),
                               "4"<-c("6"),
                               "5"<-c("5"),
                               "6"<-c("4"),
                               "7"<-c("3"),
                               "8"<-c("2"),
                               "9"<-c("1"),
                               "10"<-c("0"),
                               "NS/NR"<-c("NS/NR"))
df$P52_recod_numeric <- memisc::recode(as.numeric(df$P52_recod_numeric), 0 <- c(10),
                                       1 <- c(9),
                                       2<-c(8),
                                       3<-c(7),
                                       4<-c(6),
                                       5<-c(5),
                                       6<-c(4),
                                       7<-c(3),
                                       8<-c(2),
                                       9<-c(1),
                                       10<-c(0))
table(df$P52_recod)#Conferir se deu certo?
table(df$P52_recod_numeric)#Conferir se deu certo?
#Sim


#próx
#P53 10 é apoia fortemente participação de pessoas em manifestações permitidas por lei
#essa é uma que tem inverter
table(df$P53_recod)#tem q arrumar!
table(df$P53_recod_numeric)#tem q arrumar!

df$P53_recod <- memisc::recode(as.character(df$P53_recod ), "0" <- c("10"),
                               "1" <-c("9"),
                               "2"<-c("8"),
                               "3"<-c("7"),
                               "4"<-c("6"),
                               "5"<-c("5"),
                               "6"<-c("4"),
                               "7"<-c("3"),
                               "8"<-c("2"),
                               "9"<-c("1"),
                               "10"<-c("0"),
                               "NS/NR"<-c("NS/NR"))
df$P53_recod_numeric <- memisc::recode(as.numeric(df$P53_recod_numeric), 0 <- c(10),
                                       1 <- c(9),
                                       2<-c(8),
                                       3<-c(7),
                                       4<-c(6),
                                       5<-c(5),
                                       6<-c(4),
                                       7<-c(3),
                                       8<-c(2),
                                       9<-c(1),
                                       10<-c(0))
table(df$P53_recod)#Conferir se deu certo?
table(df$P53_recod_numeric)#Conferir se deu certo?
#Sim


#próx
#essa é uma que tem inverter
#P54  10 “concordo totalmente que, para poder prender criminosos, elas devem sempre respeitar as leis”
table(df$P54_recod)#tem q arrumar!
table(df$P54_recod_numeric)#tem q arrumar!

df$P54_recod <- memisc::recode(as.character(df$P54_recod ), "0" <- c("10"),
                               "1" <-c("9"),
                               "2"<-c("8"),
                               "3"<-c("7"),
                               "4"<-c("6"),
                               "5"<-c("5"),
                               "6"<-c("4"),
                               "7"<-c("3"),
                               "8"<-c("2"),
                               "9"<-c("1"),
                               "10"<-c("0"),
                               "NS/NR"<-c("NS/NR"))
df$P54_recod_numeric <- memisc::recode(as.numeric(df$P54_recod_numeric), 0 <- c(10),
                                       1 <- c(9),
                                       2<-c(8),
                                       3<-c(7),
                                       4<-c(6),
                                       5<-c(5),
                                       6<-c(4),
                                       7<-c(3),
                                       8<-c(2),
                                       9<-c(1),
                                       10<-c(0))
table(df$P54_recod)#Conferir se deu certo?
table(df$P54_recod_numeric)#Conferir se deu certo?
#Sim


#próx

#P55 10 é “concorda totalmente”, até que ponto o(a) Sr(a) concorda que a vontade da maioria deveria sempre prevalecer, mesmo que prejudique os direitos das minorias

# nao precisa inverter
table(df$P55_recod)
table(df$P55_recod_numeric)


#próxima
# apoio a 8 de janeiro
# nao precisa inverter
table(df$P56_recod_8jan)
table(df$P56_recod_numeric_apoio_positivo)

# P58 10 é se justificaria o interv milit/golpe militar
# nao precisa inverter
table(df$P58_recod)
table(df$P58_recod_numeric)



#colocar entre 0 e 1 

table(df$P50_antidemoc_positivo)#ok
df$P51_recod_numeric <- scales::rescale(df$P51_recod_numeric, to = c(0, 1))
table(df$P51_recod_numeric)#OK
df$P52_recod_numeric <- scales::rescale(df$P52_recod_numeric, to = c(0, 1))
table(df$P52_recod_numeric)#OK
df$P53_recod_numeric <- scales::rescale(df$P53_recod_numeric, to = c(0, 1))
table(df$P53_recod_numeric)#OK
df$P54_recod_numeric <- scales::rescale(df$P54_recod_numeric, to = c(0, 1))
table(df$P54_recod_numeric)#OK
df$P55_recod_numeric <- scales::rescale(df$P55_recod_numeric, to = c(0, 1))
table(df$P55_recod_numeric)#OK
table(df$P56_recod_numeric_apoio_positivo)#ok, não precisa
df$P58_recod_numeric <- scales::rescale(df$P58_recod_numeric, to = c(0, 1))
table(df$P58_recod_numeric)#OK


df$Amostra


semP50 <- subset(df, select=c(P51_recod_numeric,
                              P52_recod_numeric,
                              P53_recod_numeric,
                              P54_recod_numeric,
                              P55_recod_numeric,
                              P56_recod_numeric_apoio_positivo,
                              P58_recod_numeric, Amostra))
table(semP50$Amostra)
casosAmostra1 <- semP50[semP50$Amostra == 1, ]
casosAmostra2 <- semP50[semP50$Amostra == 2, ]
library(lavaan)
library(semTools)
#Amostra1
model_Amostra1 <- 'f =~ P51_recod_numeric + P52_recod_numeric + P53_recod_numeric + P54_recod_numeric + P55_recod_numeric + P56_recod_numeric_apoio_positivo + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_Amostra1 <- cfa(model_Amostra1, data = casosAmostra1)
summary(cfa_model_Amostra1, standardized = TRUE)
semTools::fitmeasures(cfa_model_Amostra1, c("tli", "cfi", "rmsea", "srmr"))

#Amostra2
model_Amostra2 <- 'f =~ P51_recod_numeric + P52_recod_numeric + P53_recod_numeric + P54_recod_numeric + P55_recod_numeric + P56_recod_numeric_apoio_positivo + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_Amostra2 <- cfa(model_Amostra1, data = casosAmostra2)
summary(cfa_model_Amostra2, standardized = TRUE)
semTools::fitmeasures(cfa_model_Amostra2, c("tli", "cfi", "rmsea", "srmr"))
