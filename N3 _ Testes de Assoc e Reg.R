#N3 vem do script N1 e N2 dois objetos df e aquela combinação do que preferir (atual: semP50)

# escolha = Sem P50

# Lista de objetos a manter
objetos_a_manter <- c("df","semP50_na")#manter o que for que usar

#OBSSSS: obs mantive somente com retirada de P50, se decidirmos diferente é só mudar o código

# Remove todos os objetos, exceto os objetos que você deseja manter
rm(list = setdiff(ls(), objetos_a_manter))




# variáveis de controle em df selecionar, ver se tem muito NA

#lógica:
#variáveis a serem testadas em bivariadas (indepedentes) e  regressão (indep + controles)

#Independente = Os 3 tipos de desengaj
#Controle = Idade (faixa:  16-25; 26-34; 35-59;  60+)
#Controle = Gênero
#Controle = Renda (faixas, 6 já existentes no banco)
#Controle = Escolaridade (faixas,6 já existentes no banco)
#Controle =  Raça (binário - Branco e Não Branco)

#nas regressão dois tipos de modelos
#dep (linear) antiDemoc
#dep (tercil superior de antidemoc, justiticado dado o histograma)

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


#obs nennuma na nessas de controle


# complete cases

str(semP50_na)#aqui nos interessa o score e as variáveis presentes para rodar o complete cases
nrow(semP50_na)
nrow(df)#base cheia
1500-1237 # 263 casos perdidos - OK

# df deve ter os de cima + essas selecionadas , antes de ir pro complete cases

df <- subset(df, select=c(Renda,Raca_Branca,faixa_etaria,Mulher,Escolaridade,
                          P51_recod_numeric,P52_recod_numeric, P53_recod_numeric,
                          P54_recod_numeric,P55_recod_numeric, P58_recod_numeric,
                          P56_recod_numeric_apoio_positivo,x))
summary(df)#deu certo
#temporariamente trazer NAS de volta
df$x <- ifelse(is.na(df$x), -66, df$x)



table(df$x)
table(df$x, useNA = "always")

df -> dffull # para manter intacto algum completão
df <- na.omit(df)

df$antiDemoc <- semP50_na$scores
df <- subset(df, select=c(Renda,Raca_Branca,
                          faixa_etaria,Mulher,
                          Escolaridade,antiDemoc,x))
rm(dffull)
rm(semP50_na)


df$x[df$x==-66]<-NA

summary(df)
df <- na.omit(df)


# depois criamos um de tipo categorial

df$x_factor <- factor(df$x, levels = c(-2, -1, 0, 1, 2),
                      labels = c("AntiPetista Forte", "AntiPetista Moderado", "Neutro", "Antibolsonarista Moderado", "Antibolsonarista Forte"))
table(df$x_factor, useNA = "always")


#df$x para trabalhar como uma variável numérica que vai do antipetismo ao antibolsonarismo
#df$x_factor para trabalhar como uma variável em 5 categorias




#bivariada dep x indep

#
# da distribuição como um todo

library(tidyquant)
library(ggdist)
df %>%
  ggplot(aes(x = x_factor, y = antiDemoc, fill = x_factor)) +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = -.2, outlier.colour = "red", outlier.shape = 1, alpha = 0.5, coef = 1.5) +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") +
  scale_fill_tq() +
  scale_colour_tq() +
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
  theme_tq() +
  labs(title = "",
       subtitle = "",
       x = "Perfil de desengajamento moral",
       y = "Escore Antidemocrático",
       fill = "",
       colour = "Outliers"
  ) +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  labs(caption="OBS: O círculo branco aponta a média")+
  theme(plot.caption = element_text(size = 13))

#
#regressões


modelo1.1 <- lm(antiDemoc~x+
                Renda+faixa_etaria+Mulher+Escolaridade+
                Raca_Branca,data=df)

summary(modelo1.1)


hist(df$antiDemoc, breaks=50)
df$tercil <- ntile(df$antiDemoc, 3)
by(df$antiDemoc,df$tercil, mean)#so pra ver
ggplot(df, aes(tercil, antiDemoc, group=tercil)) + geom_boxplot()#so pra ver

df$TercilSuperiorAntiDemoc <- df$tercil==3

modelo2.1 <- glm(TercilSuperiorAntiDemoc ~ x+
                 Renda+faixa_etaria+Mulher+Escolaridade+
                 Raca_Branca, data = df, family=binomial(link=logit))
tab_model(modelo1.1, modelo2.1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


#cada elevação em x....
#mas não diz muita coisa

library(marginaleffects)
plot_models(modelo2.1,
show.p = T, p.shape = TRUE, digits=4,
std.est=TRUE,p.threshold = c(0.05, 0.01, 0.001),
vline.color = "black",dot.size = 3, spacing=0.7,
ci.lvl=0.9, grid=F,colors = "Set2")

library(marginaleffects)
plot_cap(modelo2.1,condition="x",draw=T)# tratando x como contínua


#agora usando x_factor

#modelos com AntiPetista forte como referência


df$x_factor <- relevel(df$x_factor, ref = "AntiPetista Forte")


modelo1.2 <- lm(antiDemoc~x_factor+
                  Renda+faixa_etaria+Mulher+Escolaridade+
                  Raca_Branca,data=df)

modelo2.2 <- glm(TercilSuperiorAntiDemoc ~ x_factor+
                   Renda+faixa_etaria+Mulher+Escolaridade+
                   Raca_Branca, data = df, family=binomial(link=logit))
tab_model(modelo1.2, modelo2.2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


plot_cap(modelo2.2, condition="x_factor", draw=T,
         conf_level = .9)
plot_cap(modelo2.2, condition="x_factor", draw=F,
         conf_level = .9)

plot_cap(modelo1.2, condition="x_factor", draw=T,
         conf_level = .9)

plot_model(modelo2.2)#se quiser usar


# Realizar o teste ANOVA
anova_result <- aov(antiDemoc ~ x_factor, data = df)

# Resumo dos resultados
summary(anova_result)

# Realizar o teste de Kruskal-Wallis
kruskal_result <- kruskal.test(antiDemoc ~ x_factor, data = df)

# Exibir os resultados
print(kruskal_result)

options(scipen = 999)
# Carregar a biblioteca para realizar o teste de Tukey
library(multcomp)

# Realizar o teste de Tukey
tukey_result <- glht(anova_result, linfct = mcp(x_factor = "Tukey"))

# Exibir os resultados
summary(tukey_result)




# Obtenha as comparações múltiplas do teste de Tukey
tukey_comparisons <- TukeyHSD(anova_result, "x_factor")

# Converta os resultados em um data frame
tukey_df <- as.data.frame(tukey_comparisons$`x_factor`)

# Criação do gráfico de barras
library(ggplot2)
head(tukey_df)
tukey_df$`p adj`

tukey_df <- tukey_df %>%
  rownames_to_column(var = "Comparação")

# Agora 'Comparação' é uma variável factor
tukey_df$Comparação<- as.factor(tukey_df$Comparação)
levels(tukey_df$Comparação)
library(forcats)

ggplot(tukey_df, aes(x = Comparação, y = diff, ymin = lwr, ymax = upr, fill = `p adj` < 0.05)) +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.2) +
  geom_point(aes(x = Comparação, y = (lwr + upr) / 2), position = position_dodge(width = 0.9), size = 3, color = "black") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Linha vermelha no zero
  labs(x = "Comparação", y = "Diferença nas Médias", title = "Comparação de Médias por Grupo",
       caption = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "red")) + coord_flip()
