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
# install.packages("mbreaks")
# install.packages("strucchange")
# install.packages("stargazer")
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(car)
library(carData)
library(caret)
library(FactoMineR)
library(corrplot) # pour la matrice de corrélation
library(ggcorrplot)
library(lmtest) # pour Breusch-Pagan
library(skedastic) # pour White
library(nortest) # pour Anderson-Darling
library(olsrr) # pour White
library(mbreaks) # for Bai-Perron test
library(strucchange) # for Chow test

# Save regression results to LaTeX
library(stargazer)
library(xtable)


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

# %=================================================%=
# SOME DATA VISUALIZING ####
# %=================================================%=

# Tracer toutes les colonnes qui se terminent par base100_2015 en fonction de Date
# Convertir les données en format long pour ggplot
celec_long <- celec %>%
    select(Date, ends_with("base100_2015")) %>%
    pivot_longer(cols = -Date, names_to = "variable", values_to = "value") %>%
    mutate(variable = gsub("_base100_2015", "", variable))

# Tracer les données avec les modifications demandées
ggplot(celec_long, aes(x = Date, y = value, color = variable)) +
    geom_line(data = subset(celec_long, variable == "elec_cons"), size = 1.2) + # Thick line
    geom_line(data = subset(celec_long, variable != "elec_cons"), size = 0.9) + # Normal lines
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
# CORRELATION MATRIX ####
# %=================================================%=

# Sélectionner les colonnes numériques pour la matrice de corrélation
numeric_vars <- celec %>%
    select(elec_cons_pc, netInc2015_pc, Pelec2015, IRC, PIB2015_pc)

# Calculer la matrice de corrélation
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Visualiser la matrice de corrélation
jpeg("econometrics-II-report/Images/correlation_matrix.jpeg", width = 800, height = 600, quality = 100)
corrplot(cor_matrix, method = "circle", type = "upper", diag = FALSE, tl.cex = 2, tl.col = "black", p.mat = cor_pmat(numeric_vars), sig.level = 0.05)
dev.off()

# %=================================================%=
# REGRESSION MODEL ####
# %=================================================%=

celec.lm2 <- lm(log(elec_cons_pc) ~ log(netInc2015_pc) + log(Pelec2015) + IRC, data = celec)
par(mfrow = c(2, 2))
plot(celec.lm2)
summary(celec.lm2)

### In the previous one, the price effect is not significant when we take into account the disposable income per capita

celec.lm <- lm(log(elec_cons_pc) ~ log(netInc2015_pc)*log(Pelec2015) + IRC, data = celec)
par(mfrow = c(2, 2))
plot(celec.lm)
summary(celec.lm)

# Save the bad regression model
stargazer(celec.lm2, type = "latex", out = "econometrics-II-report/Results/celec_lm2_results.tex")

# Save the good regression model
stargazer(celec.lm, type = "latex", out = "econometrics-II-report/Results/celec_lm_results.tex")


# %=================================================%=
# Tests on the regression ####
# %=================================================%=

# Multicollinearity test with Variance Inflation Factor (VIF)
vif(celec.lm)

## STRONG MULTICOLLINEARITY

## Need for centering the variables
celec <- celec %>%
    mutate(
        ln_Inc_pc_MC = log(netInc2015_pc) - mean(log(netInc2015_pc), na.rm = TRUE),
        ln_price2015_MC = log(Pelec2015) - mean(log(Pelec2015), na.rm = TRUE)
    )

# Run regression again with centered variables
celec.lm <- lm(log(elec_cons_pc) ~ ln_Inc_pc_MC * ln_price2015_MC + IRC, data = celec)
par(mfrow = c(2, 2))
plot(celec.lm)
summary(celec.lm)

# Save the good regression model -corrected
stargazer(celec.lm, type = "latex", out = "econometrics-II-report/Results/celec_lm_results.tex")

# Check VIF again
vif(celec.lm)

# %=================================================%
# t-test for zero mean
t.test(residuals(celec.lm), mu = 0)

# %=================================================%
# Anderson-Darling test for normality of residuals
ad.test(residuals(celec.lm))

# %=================================================%
# Breusch-Pagan test for heteroskedasticity
bptest(celec.lm)

# %=================================================%
# Goldfeld-Quandt test for heteroskedasticity
gqtest(celec.lm)

# %=================================================%
# Durbin-Watson test for autocorrelation
dw_test <- dwtest(celec.lm)

# %=================================================%
# Ljung-Box test for autocorrelation
ljung_box_test <- Box.test(celec.lm$residuals, lag = 10, type = "Ljung-Box")

# %=================================================%
# Breusch-Godfrey test for autocorrelation
bg_test <- bgtest(celec.lm)

# Export the results to LaTeX

# Export to LaTeX
sink("econometrics-II-report/Results/autocorrelation_tests.tex")
cat("\\begin{table}[h]
\\centering
\\caption{Autocorrelation Test Results}
\\begin{tabular}{lccc}
\\hline
Test & Statistic & DF & p-value \\\\
\\hline
Durbin-Watson &", round(dw_test$statistic, 4), " & NA &", round(dw_test$p.value, 4), "\\\\
Ljung-Box (lag=10) &", round(ljung_box_test$statistic, 4), " &", ljung_box_test$parameter, "&", round(ljung_box_test$p.value, 4), "\\\\
Breusch-Godfrey &", round(bg_test$statistic, 4), " &", bg_test$parameter, "&", round(bg_test$p.value, 4), "\\\\
\\hline
\\end{tabular}
\\end{table}")
sink()

# %=================================================%=
# PLOT INTERACTION TERMS ####
# %=================================================%=

celec <- celec %>%
    mutate(Interaction_ln_price_Inc = ln_Inc_pc_MC * ln_price2015_MC)

# Convert data to long format for ggplot
interaction_long <- celec %>%
    select(Date, ln_Inc_pc_MC, ln_price2015_MC, Interaction_ln_price_Inc) %>%
    pivot_longer(cols = -Date, names_to = "variable", values_to = "value")

# Plot the interaction terms
ggplot(interaction_long, aes(x = Date, y = value, color = variable)) +
    geom_line(size = 1) +
    labs(
        x = "Year",
        y = "Value",
        color = "Variable",
        title = "Interaction Terms Over Time"
    ) +
    theme_minimal()

# Save the plot
ggsave("econometrics-II-report/Images/interaction_terms.jpeg", width = 10, height = 6)

# %=================================================%=
# WHERE IS THE STRUCTURAL BREAK ? ####
# %=================================================%=

# %===========================================
# Estimate structural breaks using Bai-Perron method

## How many structural test are there?
seq_test_results <- doseqtests(
    y_name = "elec_cons_pc", # Change this to your dependent variable
    z_name = c("ln_price2015_MC", "ln_Inc_pc_MC"), # Independent variables with possible breaks
    x_name = c("IRC"), # If independent variables have constant coefficients
    data = celec, # Change to your dataset name
    m = 3, # Maximum number of breaks to test,
    eps1 = 0.2, # Increase trimming (minimum segment size)
    prewhit = 1, # Prewhitening for autocorrelation robustness, filter out autocorrelation in the error terms
    robust = 1, # Allow heteroskedasticity/autocorrelation
    hetdat = 1, # Allow different moment matrices across segments
    hetvar = 1, # Allow residual variance to differ across segments
    hetq = 1,
    hetomega = 1
)

# Print test results
print(seq_test_results)

## Where are the breaks?
# Estimate breakpoints with 2 breaks
break_model <- dofix(
    y_name = "elec_cons_pc",
    z_name = c("ln_price2015_MC", "ln_Inc_pc_MC"),
    x_name = c("IRC"),
    data = celec,
    fixn = 2, # Set number of breaks to 2
    eps1 = 0.25 # Increase trimming (minimum segment size)
)


# View estimated break dates
print(break_model)
plot_model(break_model, title = "Structural Breaks in Electricity Consumption") +
    scale_x_continuous(breaks = seq(1, 32, by = 5), labels = celec$Date[seq(1, 32, by = 5)])
    
# Save the plot for structural breaks
ggsave("econometrics-II-report/Images/structural_breaks.jpeg", width = 10, height = 6)


# %===========================================
# Estimate structural breaks using Chow test
index_2006 <- which(celec$Date == 2006)
index_2011 <- which(celec$Date == 2011)

years <- index_2006:index_2011

chow_results <- data.frame(Break_Year = integer(), F_Statistic = numeric(), P_Value = numeric())

n <- length(celec$elec_cons_pc)
X <- matrix(c(celec$ln_Inc_pc_MC, celec$ln_price2015_MC, celec$Interaction_ln_price_Inc, celec$IRC), ncol = 4)
Y <- matrix(celec$elec_cons_pc, n, 1)
#
# transformation des variables en logarithme
y <- log(Y)
# x <- log(X)
# x <- cbind(x, c(celec$IRC, log(celec$netInc2015_pc) * log(celec$Pelec2015)))

for (i in years) {
    chow_test <- sctest(y ~ X, type = "Chow", point = i)
    chow_results <- rbind(chow_results, data.frame(
        Break_Year = celec$Date[i],
        F_Statistic = chow_test$statistic,
        P_Value = chow_test$p.value
    ))
}

best_break <- chow_results[which.min(chow_results$P_Value), ]
print(best_break)

ggplot(chow_results, aes(x = Break_Year, y = F_Statistic)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = qf(0.95, df1 = 3, df2 = nrow(celec) - 6), linetype = "dashed", color = "red") +
    labs(
        title = "Chow Test for Structural Breaks (2006-2011)",
        x = "Potential Break Year",
        y = "F-Statistic"
    ) +
    theme_minimal()

# Save the plot
ggsave("econometrics-II-report/Images/chow_test_results.jpeg", width = 10, height = 6)


# # Apply the CUSUM-square test
# cusum_sq_test <- efp(celec.lm$residuals ~ 1, type = "Rec-CUSUM")

# # Plot without x-axis labels
# plot(cusum_sq_test, xaxt = "n")

# # Manually set the x-axis labels to display years
# axis(1, at = seq(0, 1, length.out = length(celec$Date)), labels = celec$Date)

# %=================================================%=
# REGRESSION ON A SUBSET ####
# %=================================================%=
# Create a subset of celec for data after 2007
celec_before_2007 <- celec %>% filter(Date <= 2007)
celec_after_2007 <- celec %>% filter(Date > 2007)

# Perform regression on the subset
celec_before_2007.lm <- lm(log(elec_cons_pc) ~ log(ln_Inc_pc_MC)*log(ln_price2015_MC) + IRC, data = celec_before_2007)
par(mfrow = c(2, 2))
plot(celec_before_2007.lm)
summary(celec_before_2007.lm)

celec_after_2007.lm <- lm(log(elec_cons_pc) ~ log(ln_Inc_pc_MC)*log(ln_price2015_MC) + IRC, data = celec_after_2007)
par(mfrow = c(2, 2))
plot(celec_after_2007.lm)
summary(celec_after_2007.lm)

# # Apply the CUSUM-square test
# cusum_sq_test_2007 <- efp(celec_after_2007.lm$residuals ~ 1, type = "Rec-CUSUM")

# # Plot without x-axis labels
# plot(cusum_sq_test_2007, xaxt = "n")
# # Manually set the x-axis labels to display years
# axis(1, at = seq(0, 1, length.out = length(celec_after_2007$Date)), labels = celec_after_2009$Date)
