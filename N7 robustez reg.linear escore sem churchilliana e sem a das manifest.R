# N7 robustez regressao sem P50 e sem P53 (sem churchilliana e sem a das manifest)

#! nao rodar nada antes 



library(tidyverse)
rm(list=ls())
library(sjPlot)
library(wesanderson)
library(memisc)
library(huxtable)
df <- read_csv("df.csv")

table(df$P31)#direito do apoiador do PT_dir_discurso discursar
table(df$P32)#direito do apoiador do Bolsonaro discursar

# Crie um vetor com os rótulos personalizados para cada nível
labels <- c("Sim", "Não","NS", "NR")
# Transforme a variável P31 em um fator e atribua os rótulos personalizados aos níveis
df$PT_dir_discurso <- factor(df$P31, levels = 1:4, labels = labels)
table(df$PT_dir_discurso)

# Transforme a variável P32 em um fator e atribua os rótulos personalizados aos níveis
df$Bolsonarista_dir_discurso <- factor(df$P32, levels = 1:4, labels = labels)
table(df$Bolsonarista_dir_discurso)


#questões P22 e P24
#10 aprova direito de concorrrer totalmente/0-> não aprova /11 e 12 nao sabe/nao respo
table(df$P22)#PT
table(df$P24)#PL do Bolsonaro
#recodificar em três categorias para gráficos iniciais-> Não Conhece/NS/NR ,0-3, 4-6,7-10
df$dirPT <- memisc::recode(as.factor(df$P22), "Não Conhece/NS/NR" <- c(11,12),
                           "0-3" <- c(0,1,2,3),
                           "4-6"<-c(4,5,6),
                           "7-10"<-c(7,8,9,10))
prop.table(table(df$dirPT))*100#pra verificar se bate com o relatório (PDF file)
df$dirPLBolsonaro <- memisc::recode(as.factor(df$P24), "Não Conhece/NS/NR" <- c(11,12),
                                    "0-3" <- c(0,1,2,3),
                                    "4-6"<-c(4,5,6),
                                    "7-10"<-c(7,8,9,10))
prop.table(table(df$dirPLBolsonaro))*100#pra verificar se bate com o relatório (PDF file)


#usando lógica booleana

# ver arquivo 'nova formatação.txt' para entender a lógica com maior detalhamento



df$x <- ifelse(df$dirPLBolsonaro == "0-3" & df$Bolsonarista_dir_discurso == "Não" & df$dirPT == "0-3" & df$PT_dir_discurso == "Não", NA,
               ifelse(df$dirPT == "0-3" & df$PT_dir_discurso == "Não", -2,
                      ifelse(df$dirPLBolsonaro == "0-3" & df$Bolsonarista_dir_discurso == "Não", 2,
                             ifelse(df$dirPT == "0-3" | df$PT_dir_discurso == "Não", -1,
                                    ifelse(df$dirPLBolsonaro == "0-3" | df$Bolsonarista_dir_discurso == "Não", 1, 0)))))


table(df$x, useNA="always")
table(df$dirPLBolsonaro == "0-3" & df$Bolsonarista_dir_discurso == "Não" & df$dirPT == "0-3" & df$PT_dir_discurso == "Não")#os NA - só para verificar

prop.table(table(df$x))

table(df$x, df$dirPLBolsonaro)
table(df$x, df$Bolsonarista_dir_discurso)
table(df$x, df$dirPT)
table(df$x, df$PT_dir_discurso)
#


#agora escore


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

#
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

table(df$P50_antidemoc_positivo, useNA = "always")


semP53esemP50 <- subset(df, select=c(P51_recod_numeric,
                           P52_recod_numeric,
                           P54_recod_numeric,
                           P55_recod_numeric,
                           P56_recod_numeric_apoio_positivo,
                           P58_recod_numeric, x))

library(lavaan)
library(semTools)
library(psych)
library(ltm)

#semP53esemP50
# Especificação do modelo
model_semP53esemP50 <- 'f =~ P51_recod_numeric + P52_recod_numeric + P54_recod_numeric + P55_recod_numeric + P56_recod_numeric_apoio_positivo + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_semP53esemP50 <- cfa(model_semP53esemP50, data = semP53esemP50)
semP53esemP50_na <- na.omit(semP53esemP50[,1:6])
scores_semP53esemP50 <- lavPredict(cfa_model_semP53esemP50)
scores_semP53esemP50 -> semP53esemP50_na$scores
summary(scores_semP53esemP50)
# Lista de objetos a manter
objetos_a_manter <- c("df","semP53esemP50_na")#manter o que for que usar

#OBSSSS: obs mantive somente com retirada de P50, se decidirmos diferente é só mudar o código

# Remove todos os objetos, exceto os objetos que você deseja manter
rm(list = setdiff(ls(), objetos_a_manter))
# recod as de controle

table(df$P12, useNA = "always")#escolarid ok
df$Escolaridade <- df$P12
table(df$P6,useNA = "always")#ok
df$Mulher <- df$P6 == 2
summary(df$P8)
table(df$P8, useNA = "always")# transformar em faixa
# Recodificar a variável P8 em faixas etárias
df$faixa_etaria <- cut(df$P8, breaks = c(16, 25, 34, 59, 92), labels = c(1, 2, 3, 4), include.lowest = TRUE)
# Converter a nova variável faixa_etaria para numeric
df$faixa_etaria <- as.numeric(df$faixa_etaria)
table(df$faixa_etaria,useNA = "always" )



table(df$P60,useNA = "always")
df$Renda <- df$P60
table(df$P7,useNA = "always")
df$Raca_Branca <- df$P7 == 1




# complete cases
df <- subset(df, select=c(Renda,Raca_Branca,faixa_etaria,Mulher,Escolaridade,
                          P51_recod_numeric,P52_recod_numeric,
                          P54_recod_numeric,P55_recod_numeric, P58_recod_numeric,
                          P56_recod_numeric_apoio_positivo,x))
summary(df)#deu certo


df -> dffull # para manter intacto algum completão
df$x <- ifelse(is.na(df$x), -66, df$x)



table(df$x)
table(df$x, useNA = "always")

df <- na.omit(df)

df$antiDemoc <- semP53esemP50_na$scores
df <- subset(df, select=c(Renda,Raca_Branca,
                          faixa_etaria,Mulher,
                          Escolaridade,antiDemoc,x))
rm(dffull)
rm(semP53esemP50_na)
df$antiDemoc <- as.numeric(df$antiDemoc)
df$antiDemoc  <- scales::rescale(df$antiDemoc, to = c(0, 1))

df$x_factor <- factor(df$x, levels = c(-2, -1, 0, 1, 2),
                      labels = c("AntiPetista Forte", "AntiPetista Moderado", "Neutro", "Antibolsonarista Moderado", "Antibolsonarista Forte"))
table(df$x_factor, useNA = "always")


summary(df)
#modelos com AntiPetista forte como referência


df$x_factor <- relevel(df$x_factor, ref = "AntiPetista Forte")

modelo1_N7 <- lm(antiDemoc~x_factor+
                   Renda+faixa_etaria+Mulher+Escolaridade+
                   Raca_Branca,data=df)
tab_model(modelo1_N7, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
