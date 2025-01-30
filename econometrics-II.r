# ===================================#
#      M2 EEET - Econometrics 2     #
#            2024-2025              #
#         DA COSTA & GUILLAUT       #
#                                   #
# ===================================#

# %============================%=
# LIBRAIRIES ####
# %============================%=
# Importation des librairies nécessaires
# options(repos = c(CRAN = "https://cloud.r-project.org"))
# install.packages(c("caret", "skedastic", "nortest", "olsrr"))
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(car)
library(carData)
library(caret)
library(FactoMineR)
library(lmtest) # pour Breusch-Pagan
library(skedastic) # pour White
library(nortest) # pour Anderson-Darling
library(olsrr) # pour White


# %============================%=
# DATA LOADING ####
# %============================%=
# Téléchargement des données dans un dataframe nommé celec
celec <- read_excel("celec_menages.xlsx", col_names = TRUE)
# Charger les données de revenu net
revenu_net <- read.csv("revenu_net.csv", sep = ";")

# IPC   (INSEE)	         Indice annuel des prix à la consommation - Base 2015 - Ensemble des ménages - France - Ensemble #nolint
# PIB   (INSEE)	         Produit intérieur brut (PIB), en Milliards d'euros 2020
# Pop1  (INSEE)          Population France métropolitaine
# Pelec (IEA)	         Prix de l'électricité des ménages (euro/MWh)
# DJU   (MTES)           Indice de rigueur du climat
# Celec_menages (MTES)   Cons. Elec GWh (observée) des ménages
# netInc2020 (OCDE)     Revenu disponible net réel des ménages et des ISBLSM,
# déflaté par la consommation finale des ménages et des ISBLSM,
# en millions d'euros 2020


# %=================================================%=
# PREPARING SET FOR EXPLORATION AND REGRESSION ####
# %=================================================%=


# Renommer les colonnes de revenu_net pour la fusion
revenu_net <- revenu_net %>%
    rename(
        Date = `TIME_PERIOD`,
        netInc2020 = `OBS_VALUE`
    )

# Convert Date column to numeric in revenu_net
revenu_net$Date <- as.numeric(revenu_net$Date)

# Merge revenu_net with celec on the Date column
celec <- merge(celec, revenu_net, by = "Date", all = TRUE)

# Renommer les colonnes pour une meilleure clarté
celec <- celec %>%
    # IRC = Indice de Rigueur Climatique.
    rename(
        IRC = DJU,
        Population = Pop1,
        elec_cons = Celec_menages,
        IPC = `IPC(base100=2015)`,
    )

# %===========================================
# Adding new variables

# Le PIB est en euro constant 2020 mais l'inflation en base 2015 : on doit ajuster le PIB en 2015

celec <- celec %>%
    mutate(
        elec_cons_pc = elec_cons / Population, # pc = per capita
        PIB2015 = PIB2020 * (IPC[Date == 2015] / IPC[Date == 2020]), # en milliards d'euros 2015
        PIB2015_pc = PIB2015 / Population, # in 2015 10^9 euros per capita,
        netInc2015 = netInc2020 * (IPC[Date == 2015] / IPC[Date == 2020]), # en millions d'euros 2015
        netInc2015_pc = netInc2015 / Population, # in 2015 10^6 euros per capita,
        Pelec2015 = Pelec * (IPC[Date == 2015] / IPC), # Prix de l'électricité en euro constant 2015
    )

summary(celec)

# %===========================================
# Going base 100 for all variables in 2015

base_2015 <- celec %>% filter(Date == 2015)

celec <- celec %>%
    mutate(
        PIB2020_base100_2015 = PIB2020 / base_2015$PIB2020 * 100,
        Pelec_base100_2015 = Pelec / base_2015$Pelec * 100,
        Population_base100_2015 = Population / base_2015$Population * 100,
        IRC_base100_2015 = IRC / base_2015$IRC * 100,
        elec_cons_base100_2015 = elec_cons / base_2015$elec_cons * 100,
        IPC_base100_2015 = IPC, # L'IPC est déjà en base 100
        netInc2020_base100_2015 = netInc2020 / base_2015$netInc2020 * 100,
    )

# Tracer toutes les colonnes qui se terminent par base100_2015 en fonction de Date
# Convertir les données en format long pour ggplot
celec_long <- celec %>%
    select(Date, ends_with("base100_2015")) %>%
    pivot_longer(cols = -Date, names_to = "variable", values_to = "value") %>%
    mutate(variable = gsub("_base100_2015", "", variable))

    # Tracer les données avec les modifications demandées
    ggplot(celec_long, aes(x = Date, y = value, color = variable)) +
    geom_line(data = subset(celec_long, variable == "elec_cons"), size = 1.2) +  # Thick line
    geom_line(data = subset(celec_long, variable != "elec_cons"), size = 0.9) +  # Normal lines
        geom_point(size = 1) +
        scale_color_manual(values = c(
            "PIB2020" = "#20a7bb",
            "Pelec" = "purple",
            "Population" = "#1b8e30",
            "IRC" = "red",
            "elec_cons" = "#000001",
            "IPC" = "#ff79e9",
            "netInc2020" = "orange"
        )) +
        labs(
            # title = "Évolution temporelle des variables (normalisées en base 100)",
            x = "year",
            y = "Value (base 100 — 2015)",
            color = "Variable"
        ) +
        theme_minimal() +
        guides(linewidth = "none")
# Enregistrer le plot
ggsave("econometrics-II-report/Images/data_base100_2015.jpeg", width = 10, height = 6)


# %=================================================%=
# REGRESSION MODEL ####
# %=================================================%=
celec.lm <- lm(log(elec_cons_pc) ~ log(PIB2015_pc) + log(Pelec2015) + IRC, data = celec)
par(mfrow = c(2, 2))
plot(celec.lm)
summary(celec.lm)
