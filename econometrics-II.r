# Importation des librairies nécessaires
library(readxl)

# Téléchargement des données dans un dataframe nommé celec
celec <- read_excel("celec_menages.xlsx", col_names = TRUE)

# IPC   (INSEE)	  Indice annuel des prix à la consommation - Base 2015 - Ensemble des ménages - France - Ensemble #nolint
# PIB   (INSEE)	  Produit intérieur brut (PIB), en Milliards d'euros 2020
# Pop1  (INSEE)   Population France métropolitaine
# Pelec (IEA)	  Prix de l'électricité des ménages (euro/MWh)
# DJU   (MTES)    Indice de rigueur du climat
# Celec (MTES)    Cons. Elec GWh (observée) des ménages


# Structure des données
str(celec)

# Synthèse descriptive des données
summary(celec)