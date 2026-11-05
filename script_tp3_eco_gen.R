###########################################################################
# Auteur : Anthony Carmichael
# ECL 1003 – ÉCOLOGIE GÉNÉRALE
# TP3 : EFFET DE L’INTENSITÉ LUMINEUSE SUR LA CROISSANCE DES VÉGÉTAUX
# Github: 
# https://github.com/AnthonyCarmichael/ECL-1003-ECOLOGIE-GENERALE-TP3      
##########################################################################

library(readxl)
library(tidyverse)

# Set working directory
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

# import data
don <- read_xlsx("./data/Données allométrie érable.xlsx", na = c("NA"))

# clean na
sum(is.na(don))
clean_don <- don |> drop_na()

# show cols name
colnames(don)

# Question 1 :  Relation entre hauteur et exposition à la lumières
figure1 <- clean_don |> ggplot(aes(x=LOGexposition_percent,y=LOGhauteur_max_cm)) +
  geom_point() +
  labs(
    x = "Logarithme de l'intensité lumineuse (%)",
    y = "Logarithme de la hauteur maximale de l'arbre (cm)"
  ) + 
  theme_bw()
figure1
#ggsave('./output/figure1.png',width = 6, height=5)
ggsave('./output/figure1.pdf')

# Linear Models donne p-value: 0.3478 donc aucune preuve de relation significative
modele <- lm(LOGhauteur_max_cm ~ LOGexposition_percent, data = clean_don)
summary(modele)

# Question 2 
# Est-ce que la longueur totale de l’arbre (LHM) est une co-variable importante
# des autres variables morphométriques ? 
figure2 <- clean_don |> ggplot(aes(x=LOGhauteur_max_cm,y=LOGnb_feuille)) +
  geom_point() +
  theme_bw() + 
  geom_smooth(method = "lm", se=FALSE) +
  labs(
    x = "Logarithme de la hauteur maximale de l'arbre (cm)",
    y = "Logarithme du nombre de feuilles"
  )
figure2
modele2 <- lm(LOGhauteur_max_cm ~ LOGexposition_percent, data = clean_don)
summary(modele)
ggsave('./output/figure2.png',width = 6, height=5)

# Linear Models donne p-value: 1.956e-06 donc preuve de relation très forte significative
modele2 <- lm(LOGnb_feuille ~ LOGhauteur_max_cm, data = clean_don)
summary(modele2)


figure3 <- clean_don |> ggplot(aes(x=LOGhauteur_max_cm,y=LOGsurface_foliaire_totale_cm2)) +
  geom_point() +
  theme_bw() + geom_smooth(method = "lm", se=FALSE) +
  labs(
    x = "Logarithme de la hauteur maximale de l'arbre (cm)",
    y = "Logarithme de la surface foliaire totale (cm²)"
  )
figure3
ggsave('./output/figure3.png',width = 6, height=5)

# Linear Models donne p-value: 0.001095 donc preuve de forte relation significative
modele3 <- lm(LOGsurface_foliaire_totale_cm2 ~ LOGhauteur_max_cm, data = clean_don)
summary(modele3)

figure4 <- clean_don |> ggplot(aes(x=LOGhauteur_max_cm,y=LOGhauteur_1_branche_cm)) +
  geom_point() +
  theme_bw() + 
  geom_smooth(method = "lm", se=FALSE) +
  labs(
    x = "Logarithme de la hauteur maximale de l'arbre (cm)",
    y = "Logarithme de la hauteur de la première branche (cm)"
  )
figure4
ggsave('./output/figure4.png',width = 6, height=5)

# Linear Models donne p-value: 0.2673 donc absence de preuve de relation significative
modele4 <- lm(LOGhauteur_1_branche_cm ~ LOGhauteur_max_cm, data = clean_don)
summary(modele4)

figure5 <- clean_don |> ggplot(aes(x=LOGhauteur_max_cm,y=LOGnb_branche_primaire)) +
  geom_point() +
  theme_bw() + geom_smooth(method = "lm", se=FALSE) +
  labs(
    x = "Logarithme de la hauteur maximale de l'arbre (cm)",
    y = "Logarithme du nombre de branches primaires"
  )
figure5
ggsave('./output/figure5.png',width = 6, height=5)

# Linear Models donne p-value: 0.0228 donc preuve de relation moyenne significative
modele5 <- lm(LOGnb_branche_primaire ~ LOGhauteur_max_cm, data = clean_don)
summary(modele5)

figure6 <- clean_don |> ggplot(aes(x=LOGhauteur_max_cm,y=LOGdiametre_tronc_cm)) +
  geom_point() +
  theme_bw() + 
  geom_smooth(method = "lm", se=FALSE) +
  labs(
    x = "Logarithme de la hauteur maximale de l'arbre (cm)",
    y = "Logarithme du diamètre du tronc (cm)"
  )
figure6
ggsave('./output/figure6.png',width = 6, height=5)

# Linear Models donne p-value: 0.03403 donc preuve de relation moyennement significative
modele6 <- lm(LOGdiametre_tronc_cm ~ LOGhauteur_max_cm, data = clean_don)
summary(modele6)

# Condenser toutes les figueres en une
figure_all <- ggplot(clean_don, aes(x = LOGhauteur_max_cm)) +
  geom_point(aes(y = LOGnb_feuille, color = "Nombre de feuilles")) +
  geom_smooth(aes(y = LOGnb_feuille, color = "Nombre de feuilles"), method = "lm", se = FALSE) +
  geom_point(aes(y = LOGsurface_foliaire_totale_cm2, color = "Surface foliaire totale (cm²)")) +
  geom_smooth(aes(y = LOGsurface_foliaire_totale_cm2, color = "Surface foliaire totale (cm²)"), method = "lm", se = FALSE) +
  geom_point(aes(y = LOGhauteur_1_branche_cm, color = "Hauteur de la première branche (cm)")) +
  geom_smooth(aes(y = LOGhauteur_1_branche_cm, color = "Hauteur de la première branche (cm)"),method = "lm", se = FALSE) +
  geom_point(aes(y = LOGnb_branche_primaire, color = "Nombre de branches primaires")) +
  geom_smooth(aes(y = LOGnb_branche_primaire, color = "Nombre de branches primaires"),method = "lm", se = FALSE) +
  geom_point(aes(y = LOGdiametre_tronc_cm, color = "Diamètre du tronc (cm)"))+
  geom_smooth(aes(y = LOGdiametre_tronc_cm, color = "Diamètre du tronc (cm)"),method = "lm", se = FALSE)+
  labs(
    x = "Logarithme de la hauteur maximale de l'arbre (cm)",
    y = "Logarithme de la variable morphométrique",
    color = "Variable"
  ) +
  theme_bw()

figure_all
#ggsave('./output/figure_all.png',width = 6, height=5)
ggsave('./output/figure_all.pdf')
