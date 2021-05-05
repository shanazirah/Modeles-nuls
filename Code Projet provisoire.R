rm(list = ls())

# ---------- Installation des packages ----------
#install.packages(c("readr", "dplyr"))
library(dplyr)
library(readr)
library(tidyverse)

# ---------- Importation des données ----------

data <- read_csv(file = "persons.csv", 
         col_types = cols(
           'gender' = col_character(),
           'degrees' = col_integer()
           ) 
        )

# ---------- Visualisation des données ----------
head(data)
summary(data)

# Boxplot des données 
ggplot(data, aes(x = gender, y = degrees, fill = gender)) +
  geom_boxplot(alpha = .3, outlier.shape = NA) +
  geom_jitter(width = .1, size = 2) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick", "dodgerblue"))

# Visualisation des données par classe
W <- data$degrees[which (data$gender == "Female")]
M <- data$degrees[which (data$gender == "Male")]

mean(W)
mean(M)

# ---------- Visualisation des rangs des données ----------
data = data %>%
mutate(rangs = rank(data$degrees))
data

# Boxplot des rangs des données
ggplot(data, aes(x = gender, y = rangs, fill = gender)) +
  geom_boxplot(alpha = .3, outlier.shape = NA) +
  geom_jitter(width = .1, size = 2) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick", "dodgerblue"))

# Résumé des valeurs des statistiques 
data_summary = data %>% group_by(gender)%>% summarise(n = n(),
                                                      rank_mean = mean(rangs),
                                                      rank_sum_0 = n * (n + 1)/2,
                                                      rank_sum = sum(rangs),
                                                      M_U = rank_sum - rank_sum_0,
                                                      .groups = "drop")
data_summary

# ---------- Test de Mann-Whitney ----------
montest = wilcox.test(degrees ~ gender, data = data)
montest

# ---------- Test par les modèles nuls complexes ----------
n <- 1000
diff_moy <- numeric(n)

for (i in 1:n){
  Sexe <- sample(data$gender, 100, replace = FALSE)
  moy_f = mean(data$degrees[which (Sexe == "Female")])
  moy_h = mean(data$degrees[which (Sexe == "Male")])
  diff_moy[i] <- moy_f - moy_h
}

# ---------- Histogramme des différences de moyennes ----------
dm <- data.frame(diff_mean = diff_moy)

ggplot(dm, aes(x = diff_mean)) +
  geom_histogram(color = "black", fill = "blue", alpha = .4) +
  geom_vline(color = "red", lwd = 1, lty = 2, xintercept = 1.033654) +
  theme_classic() +
  ggtitle("Différences de moyennes sur 1000 échantillons simulés")

# ---------- Calcul de la p-valeur ----------
p_val <- 100 * (sum(diff_moy > (mean(W) - mean(M))) / n)
p_val


