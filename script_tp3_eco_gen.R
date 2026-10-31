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
    y = "Logarithme de la hauteur de la première branche (cm)",
  ) + theme_bw() 

modele <- lm(LOGhauteur_max_cm ~ LOGexposition_percent, data = clean_don)
summary(modele)

# ANCOVA : Vérifier LHM co-variable (À FAIRE)

ggsave('./output/figure1.png')
#ggsave('./output/figure2.png')