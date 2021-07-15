# Instalando e carregando pacotes -----------------------------------------
pacotes <- c("plotly","tidyverse","knitr","kableExtra","PerformanceAnalytics",
             "factoextra","reshape2","psych","ggrepel")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando a base de dados
dbCountry <- read.table("Country-data.csv", sep = ",", header = T)

# Padronizando a base de dados
dbCountryStd <- dbCountry %>% 
  column_to_rownames("country") %>% 
  scale() %>% 
  data.frame()

# Matriz de correlação
rho <- cor(dbCountryStd)

# Teste de esfericidade de Barlett para verificar se pode utilizar PCA
cortest.bartlett(R = rho) 

# Rodando a PCA:
afpc <- prcomp(dbCountryStd)
summary(afpc)

#Visualizando os pesos de cada variável nos
data.frame(afpc$rotation) %>%
  mutate(var = names(dbCountry[2:10])) %>% 
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

# Extraindo as Cargas Fatoriais
k <- sum((afpc$sdev ^ 2) > 1)
cargas_fatoriais <- afpc$rotation[, 1:k] %*% diag(afpc$sdev[1:k])

# Relatório das cargas fatoriais e das comunalidades
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Scores Fatoriais
scores_fatoriais <- t(afpc$rotation)/afpc$sdev 
colnames(scores_fatoriais) <- colnames(dbCountryStd)

scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2,
         PC3 = 3) %>%
  select(PC1, PC2,PC3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

score_D1 <- scores_fatoriais[1,]
score_D2 <- scores_fatoriais[2,]
score_D3 <- scores_fatoriais[3,]

F1 <- t(apply(dbCountryStd, 1, function(x) x * score_D1))
F2 <- t(apply(dbCountryStd, 1, function(x) x * score_D2))
F3 <- t(apply(dbCountryStd, 1, function(x) x * score_D3))


#Ponderando invertendo sinal ou não dependendo dos pesos de cada
# fator para cada variável
F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * -1)

F2 <- data.frame(F2) %>%
  mutate(fator2 = rowSums(.) * -1)

F3 <- data.frame(F3) %>%
  mutate(fator3 = rowSums(.) * -1)


# Importando as colunas de fatores F1 e F2
dbCountry["Fator1"] <- F1$fator1
dbCountry["Fator2"] <- F2$fator2
dbCountry["Fator3"] <- F3$fator3

#Calculando a variância compartilhada
var_compartilhada <- (afpc$sdev ^ 2/sum(afpc$sdev ^ 2))
var_compartilhada

dbCountry %>%
  mutate(pontuacao = Fator1 * var_compartilhada[1] +
                     Fator2 * var_compartilhada[2] +  
                     Fator3 * var_compartilhada[3]) -> dbCountry

# Ranking final
dbCountry %>%
  arrange(desc(pontuacao)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)