#===================================#
#      M2 EEET - Econometrics 2     #
#            2024-2025              #
#         DA COSTA & GUILLAUT       #
#                                   #
#===================================#

#%============================%=
# LIBRAIRIES ####
#%============================%=
# Importation des librairies nécessaires
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


#%============================%=
# DATA LOADING ####
#%============================%=
# Téléchargement des données dans un dataframe nommé celec
celec <- read_excel("celec_menages.xlsx", col_names = TRUE)

# IPC   (INSEE)	  Indice annuel des prix à la consommation - Base 2015 - Ensemble des ménages - France - Ensemble #nolint
# PIB   (INSEE)	  Produit intérieur brut (PIB), en Milliards d'euros 2020
# Pop1  (INSEE)   Population France métropolitaine
# Pelec (IEA)	  Prix de l'électricité des ménages (euro/MWh)
# DJU   (MTES)    Indice de rigueur du climat
# Celec (MTES)    Cons. Elec GWh (observée) des ménages


#%=================================================%=
# PREPARING SET FOR EXPLORATION AND REGRESSION ####
#%=================================================%=

# Renommer les colonnes pour une meilleure clarté
celec <- celec %>%
# IRC = Indice de Rigueur Climatique.
    rename(
        IRC = DJU,
        Population = Pop1,
        elec_cons = Celec_menages,
        IPC = `IPC(base100=2015)`,
    )

# Structure des données
str(celec)
# Synthèse descriptive des données
summary(celec)

#%===========================================
# Going base 100 for all variables in 2015

base_2015 <- celec %>% filter(Date == 2015)

celec <- celec %>%
    mutate(
        PIB2020_base100_2015 = PIB2020 / base_2015$PIB2020 * 100,
        Pelec_base100_2015 = Pelec / base_2015$Pelec * 100,
        Population_base100_2015 = Population / base_2015$Population * 100,
        IRC_base100_2015 = IRC / base_2015$IRC * 100,
        elec_cons_base100_2015 = elec_cons / base_2015$elec_cons * 100, 
        IPC_base100_2015 = IPC  # L'IPC est déjà en base 100
    )

# Tracer toutes les colonnes qui se terminent par base100_2015 en fonction de Date
# Convertir les données en format long pour ggplot
celec_long <- celec %>%
    select(Date, ends_with("base100_2015")) %>%
    pivot_longer(cols = -Date, names_to = "variable", values_to = "value") %>%
    mutate(variable = gsub("_base100_2015", "", variable))

# Tracer les données
ggplot(celec_long, aes(x = Date, y = value, color = variable)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1) +
    scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "#00c3ff")) +
    labs(title = "Évolution temporelle des variables (normalisées en base 100)",
       x = "Date",
       y = "Valeur (base 100 en 2015)",
       color = "Variable") +
    theme_minimal()

# Enregistrer le plot
ggsave("econometrics-II-report/Images/data_base100_2015.jpeg", width = 10, height = 6)


#%===========================================
# Adding new variables for the regression

# Le PIB est en euro constant 2020 mais l'inflation en base 2015 : on doit ajuster le PIB en 2015

celec <- celec %>%
    mutate(
        elec_cons_pc = elec_cons / Population, # pc = per capita
        PIB2015 = PIB2020 * (IPC[Date == 2015] / IPC[Date == 2020]), #en milliards d'euros 2015
        PIB2015_pc = PIB2015 / Population, #in 2015 10^9 euros per capita, 
        Pelec_euro2015 = Pelec * (IPC[Date == 2015] / IPC), # Prix de l'électricité en euro constant 2015
    )

summary(celec)