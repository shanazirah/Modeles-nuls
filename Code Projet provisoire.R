rm(list = ls())

# ---------- Installation des packages ----------
#install.packages(c("readr", "dplyr"))
library(dplyr)
library(readr)
library(tidyverse)

simu_graph <- function(Gender, Degree){
  obs <- cbind(Gender, Degree)
  obs_f <- obs[,2][which (Gender == 1)]
  obs_h <- obs[,2][which (Gender == 0)]
  
  return(list(new_graph = obs, moy_f = mean(obs_f), moy_h = mean(obs_h)))
}

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

ggplot(data, aes(x = gender, y = degrees, fill = gender)) +
  geom_boxplot(alpha=.3, outlier.shape = NA) +
  geom_jitter(width=.1, size=2) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick", "dodgerblue"))

# 1 = femme ; 0 = homme
Gender <- as.matrix(sapply(data$gender[1:100], function(x) {return (1 * (x == "Female"))}))

Degree <- as.matrix(sapply(data$degrees[1:100], function(x) as.numeric(x)))

W <- data$degrees[which (Gender == 1)]
M <- data$degrees[which (Gender == 0)]

mean(W)
mean(M)

#tib = tibble(x = as.character(Gender), y = Degree)

# ---------- Visualisation des rangs des données ----------
data = data %>%
  mutate(rangs = rank(Degree))
data

ggplot(data, aes(x = gender, y = rangs, fill = gender)) +
  geom_boxplot(alpha=.3, outlier.shape = NA) +
  geom_jitter(width=.1, size=2) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick", "dodgerblue"))

data_summary = data %>% 
  group_by(gender)%>% 
  summarise(n = n(),
            rank_mean = mean(rangs),
            rank_sum_0 = n * (n + 1)/2,
            rank_sum = sum(rangs),
            M_U = rank_sum - rank_sum_0,
            .groups="drop")
data_summary

# ---------- Test de Mann-Whitney ----------
montest = wilcox.test(degrees ~ gender, data = data)
montest


# ---------- Test par les modèles nuls ----------
n <- 1000
diff_moy <- vector('list', length = n)

for (i in 1:n){
  Sexe <- sample(Gender, 100, replace = FALSE)
  result <- simu_graph(Sexe, Degree)
  diff_moy[[i]] <- result$moy_f - result$moy_h
}

dm <- data.frame(diff_mean = unlist(diff_moy))

ggplot(dm, aes(x = diff_mean)) +
  geom_histogram(color = "black", fill = "blue", alpha = .4) +
  geom_vline(color = "red",lwd = 1,lty = 2, xintercept = 1.033654) +
  theme_classic()+
  ggtitle("Différences de moyennes sur 1000 échantillons simulés")

pval <- 100 * sum(unlist(diff_moy) > (mean(W) - mean(M))) / 1000
pval


