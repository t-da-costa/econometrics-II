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
# install.packages(c("forecast", "tseries"))
# install.packages("urca")
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

library(forecast)
library(tseries)
library(urca) # For ERS and KPSS tests

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
    mutate(
        Interaction_ln_price_Inc = ln_Inc_pc_MC * ln_price2015_MC, 
        ln_elec_cons_MC = log(elec_cons_pc) - mean(log(elec_cons_pc)))

# Convert data to long format for ggplot
interaction_long <- celec %>%
    select(Date, ln_Inc_pc_MC, ln_price2015_MC, Interaction_ln_price_Inc, ln_elec_cons_MC) %>%
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

# Create two dummies

celec <- celec %>%
    mutate(break_dummy = ifelse(Date > 2007, 1, 0),
    crisis_dummy = ifelse(celec$Date %in% c(2001, 2008, 2020), 1, 0))

# Create a subset of celec for data after 2007
celec_before_2007 <- celec %>% filter(Date <= 2007)
celec_after_2007 <- celec %>% filter(Date > 2007)

## Does the break causes a shift in consumption?
celecbreak.lm <- lm(log(elec_cons_pc) ~ ln_Inc_pc_MC*ln_price2015_MC + IRC + break_dummy, data = celec)
plot(celecbreak.lm)
summary(celecbreak.lm)
stargazer(celecbreak.lm, type = "latex", out = "econometrics-II-report/Results/celecbreak_dummy_lm_results.tex")

vif(celecbreak.lm)
# t-test for zero mean
t.test(residuals(celecbreak.lm), mu = 0)
# Anderson-Darling test for normality of residuals
ad.test(residuals(celecbreak.lm))
# Breusch-Pagan test for heteroskedasticity
bptest(celecbreak.lm)
# Goldfeld-Quandt test for heteroskedasticity
gqtest(celecbreak.lm)
# Durbin-Watson test for autocorrelation
dwtest(celecbreak.lm)
# Ljung-Box test for autocorrelation
Box.test(celecbreak.lm$residuals, lag = 10, type = "Ljung-Box")
# Breusch-Godfrey test for autocorrelation
bgtest(celecbreak.lm)


# %=================================================%=
#### What about subsets? ####

# # Perform regression on the subset pre-2007
# celec_before_2007.lm <- lm(log(elec_cons_pc) ~ (ln_Inc_pc_MC)*(ln_price2015_MC) + IRC, data = celec_before_2007)
# par(mfrow = c(2, 2))
# plot(celec_before_2007.lm)
# summary(celec_before_2007.lm)

# vif(celec_before_2007.lm)

# # Perform regression on the subset post-2007 with interactions
# celec_after_2007.lm <- lm(log(elec_cons_pc) ~ (ln_Inc_pc_MC)*(ln_price2015_MC) + IRC, data = celec_after_2007)
# par(mfrow = c(2, 2))
# plot(celec_after_2007.lm)
# summary(celec_after_2007.lm)

# # Check VIF
# vif(celec_after_2007.lm)

# # Perform regression on the subset post-2007 without interactions
# celec_after_2007.lm <- lm(log(elec_cons_pc) ~ (ln_Inc_pc_MC) + (ln_price2015_MC) + IRC, data = celec_after_2007)
# par(mfrow = c(2, 2))
# plot(celec_after_2007.lm)
# summary(celec_after_2007.lm)

# vif(celec_after_2007.lm)

# Use BIC criteria to select the best variables for the regression model
BIC_before_2007 <- lm(log(elec_cons_pc) ~ ln_Inc_pc_MC*(ln_price2015_MC)*IRC*crisis_dummy, data = celec_before_2007)
best_model_before <- step(BIC_before_2007, direction = "both", k = log(nrow(celec_before_2007)))

# Removing ln_price2015_MC from the BIC selection because it was not statistically significant
best_model_before <- lm(log(elec_cons_pc) ~ ln_Inc_pc_MC  +  
                          IRC + ln_Inc_pc_MC:ln_price2015_MC, data = celec_before_2007)

# Print the summary of the best model
summary(best_model_before)

# Plot diagnostics for the best model
par(mfrow = c(2, 2))
plot(best_model_before)

vif(best_model_before)
# t-test for zero mean
t.test(residuals(best_model_before), mu = 0)
# Anderson-Darling test for normality of residuals
ad.test(residuals(best_model_before))
# Breusch-Pagan test for heteroskedasticity
bptest(best_model_before)
# Goldfeld-Quandt test for heteroskedasticity
gqtest(best_model_before)
# Durbin-Watson test for autocorrelation
dwtest(best_model_before)
# Ljung-Box test for autocorrelation
Box.test(best_model_before$residuals, lag = 10, type = "Ljung-Box")
# Breusch-Godfrey test for autocorrelation
bgtest(best_model_before)

stargazer(best_model_before, type = "latex", out = "econometrics-II-report/Results/celecbefore_results.tex")


BIC_after_2007 <- lm(log(elec_cons_pc) ~ ln_Inc_pc_MC*(ln_price2015_MC)*IRC*crisis_dummy, data = celec_after_2007)
best_model_after <- step(BIC_after_2007, direction = "both", k = log(nrow(celec_after_2007)))

# Print the summary of the best model
summary(best_model_after)

# Plot diagnostics for the best model
par(mfrow = c(2, 2))
plot(best_model_after)

vif(best_model_after)
# t-test for zero mean
t.test(residuals(best_model_after), mu = 0)
# Anderson-Darling test for normality of residuals
ad.test(residuals(best_model_after))
# Breusch-Pagan test for heteroskedasticity
bptest(best_model_after)
# Goldfeld-Quandt test for heteroskedasticity
gqtest(best_model_after)
# Durbin-Watson test for autocorrelation
dwtest(best_model_after)
# Ljung-Box test for autocorrelation
Box.test(best_model_after$residuals, lag = 10, type = "Ljung-Box")
# Breusch-Godfrey test for autocorrelation
bgtest(best_model_after)

stargazer(best_model_after, type = "latex", out = "econometrics-II-report/Results/celecafter_results.tex")

# %=================================================%=
# PLOT ELECTRICITY CONSUMPTION WITH REGRESSION LINE ####
# %=================================================%=

# Add predictions to dataset
celec$predicted_lm <- predict(celecbreak.lm, newdata = celec)

predicted_best_before <- data.frame(Date = celec_before_2007$Date, predicted_best_before = predict(best_model_before, newdata = celec_before_2007))
celec <- merge(celec, predicted_best_before, by = "Date", all.x = TRUE)

predicted_best_after <- data.frame(Date = celec_after_2007$Date, predicted_best_after = predict(best_model_after, newdata = celec_after_2007))
celec <- merge(celec, predicted_best_after, by = "Date", all.x = TRUE)

ggplot(celec, aes(x = Date, y = log(elec_cons_pc))) +
    geom_point(color = "black", size = 3) +  # Scatter points
    geom_line(aes(y = predicted_lm), color = "red", size = 1.5) +  # Custom regression line
    geom_line(aes(y = predicted_best_before), color = "green", size = 1) +  # Best model before 2007
    geom_line(aes(y = predicted_best_after), color = "#228eed", size = 1) +  # Best model after 2007
    labs(
        title = "Electricity Consumption Over Time with Regression Lines",
        x = "Year",
        y = "Log of Electricity Consumption per Capita"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("red", "green", "#228eed"), labels = c("Model with dummy", "Model Before 2007", "Model After 2007")) +
    theme(legend.position = "right")  # Ensure legend appears on the right side
# Save the plot
ggsave("econometrics-II-report/Images/electricity_consumption_regression.jpeg", width = 10, height = 10)


# # %=================================================%=
# # WHAT IS BELOW IS UNFINISHED AND BOTCHED
# # %=================================================%=

# # %=================================================%=
# # OLS FORECAST with bootstrap FULL ####
# # %=================================================%=
# # Extract regression residuals
# residuals_full_boot <- residuals(celecbreak.lm)

# # Generate projections for 2022-2030 using simple trends
# future_years <- data.frame(Date = seq(max(celec$Date) + 1, 2030, by = 1))
# future_values <- future_years %>%
#     mutate(
#         # netInc2020 = predict(lm(netInc2020 ~ Date, data = celec), newdata = future_years),
#         # Pelec = predict(lm(Pelec ~ Date, data = celec), newdata = future_years),
#         # IPC = predict(lm(IPC ~ Date, data = celec), newdata = future_years),
#         # Population = predict(lm(Population ~ Date, data = celec), newdata = future_years),
#         # IRC = predict(lm(IRC ~ Date, data = celec), newdata = future_years), 
#         # break_dummy = ifelse(Date > 2007, 1, 0),
#         break_dummy = ifelse(Date > 2007, 1, 0),
#         crisis_dummy = ifelse(Date %in% c(2001, 2008, 2020), 1, 0),
#         netInc2020 = head(forecast(auto.arima(celec$netInc2020), h = 10)$mean, nrow(future_years)), 
#         Pelec = head(forecast(auto.arima(celec$Pelec), h = 10)$mean, nrow(future_years)),
#         IPC = head(forecast(auto.arima(celec$IPC), h = 10)$mean, nrow(future_years)),
#         Population = head(forecast(auto.arima(celec$Population), h = 10)$mean, nrow(future_years)),
#         IRC = head(forecast(auto.arima(celec$IRC), h = 10)$mean, nrow(future_years))
#     )

# # Extract single values for IPC in 2015 and 2020
# IPC_2015 <- celec %>% filter(Date == 2015) %>% pull(IPC)
# IPC_2020 <- celec %>% filter(Date == 2020) %>% pull(IPC)

# # Now apply the transformation
# future_values <- future_values %>%
#     mutate(
#         netInc2015_pc = (netInc2020 / Population) * (IPC_2015 / IPC_2020),
#         Pelec2015 = Pelec * (IPC_2015 / IPC),
#         ln_Inc_pc_MC = log(netInc2015_pc) - mean(log(netInc2015_pc), na.rm = TRUE),
#         ln_price2015_MC = log(Pelec2015) - mean(log(Pelec2015), na.rm = TRUE)
#     )


# # %============================%=
# # Bootstrap forecast for full ####

# B <- 1000  # Number of bootstrap replications
# bootstrap_forecasts_full <- matrix(NA, nrow = B, ncol = nrow(future_values))
# set.seed(123)  # For reproducibility

# for (b in 1:B) {
#     # Resample residuals with replacement
#     resampled_residuals_full <- sample(residuals_full_boot, size = nrow(future_values), replace = TRUE)
    
#     # Compute forecasted log consumption per capita
#     log_forecasted_elec_cons_pc_full <- predict(celecbreak.lm, newdata = future_values) + resampled_residuals_full
    
#     # Convert back to absolute values
#     bootstrap_forecasts_full[b, ] <- exp(log_forecasted_elec_cons_pc_full)
# }

# # %=================================================%=
# # OLS FORECAST with bootstrap post-2007 ####
# # %=================================================%=
# # Extract regression residuals after 2007
# residuals_after_boot <- residuals(best_model_after)
# # Create future_values_after based on celec_after_2007
    
# future_values_after <- future_years %>%
#     mutate(
#         # netInc2020 = predict(lm(netInc2020 ~ Date, data = celec_after_2007), newdata = future_years),
#         # Pelec = predict(lm(Pelec ~ Date, data = celec_after_2007), newdata = future_years),
#         # IPC = predict(lm(IPC ~ Date, data = celec_after_2007), newdata = future_years),
#         # Population = predict(lm(Population ~ Date, data = celec_after_2007), newdata = future_years),
#         # IRC = predict(lm(IRC ~ Date, data = celec_after_2007), newdata = future_years), 
#         break_dummy = ifelse(Date > 2007, 1, 0),
#         crisis_dummy = ifelse(Date %in% c(2001, 2008, 2020), 1, 0),
#         netInc2020 = head(forecast(auto.arima(celec_after_2007$netInc2020), h = 10)$mean, nrow(future_years)), 
#         Pelec = head(forecast(auto.arima(celec_after_2007$Pelec), h = 10)$mean, nrow(future_years)),
#         IPC = head(forecast(auto.arima(celec_after_2007$IPC), h = 10)$mean, nrow(future_years)),
#         Population = head(forecast(auto.arima(celec_after_2007$Population), h = 10)$mean, nrow(future_years)),
#         IRC = head(forecast(auto.arima(celec_after_2007$IRC), h = 10)$mean, nrow(future_years))
#     )

# # Now apply the transformation for future_values_after
# future_values_after <- future_values_after %>%
#     mutate(
#         netInc2015_pc = (netInc2020 / Population) * (IPC_2015 / IPC_2020),
#         Pelec2015 = Pelec * (IPC_2015 / IPC),
#         ln_Inc_pc_MC = log(netInc2015_pc) - mean(log(netInc2015_pc), na.rm = TRUE),
#         ln_price2015_MC = log(Pelec2015) - mean(log(Pelec2015), na.rm = TRUE)
#     )

# bootstrap_forecasts_after <- matrix(NA, nrow = B, ncol = nrow(future_values_after))
# B <- 1000  # Number of bootstrap replications
# set.seed(123)  # For reproducibility

# for (b in 1:B) {
#     # Resample residuals with replacement
#     resampled_residuals_after <- sample(residuals_after_boot, size = nrow(future_values_after), replace = TRUE)
    
#     # Compute forecasted log consumption per capita
#     log_forecasted_elec_cons_pc_after <- predict(best_model_after, newdata = future_values_after) + resampled_residuals_after
    
#     # Convert back to absolute values
#     bootstrap_forecasts_after[b, ] <- exp(log_forecasted_elec_cons_pc_after)

# }

# # %============================%=
# # Confidence Intervals ####

# forecast_mean_after <- colMeans(bootstrap_forecasts_after)
# forecast_lower_after <- apply(bootstrap_forecasts_after, 2, function(x) quantile(x, 0.05))  # 5th percentile
# forecast_upper_after <- apply(bootstrap_forecasts_after, 2, function(x) quantile(x, 0.95))  # 95th percentile

# forecast_results_after <- future_values %>%
#     mutate(
#         elec_forecast_mean_after = forecast_mean_after,
#         elec_forecast_lower_after = forecast_lower_after,
#         elec_forecast_upper_after = forecast_upper_after
#     )

# forecast_mean_full <- colMeans(bootstrap_forecasts_full)
# forecast_lower_full <- apply(bootstrap_forecasts_full, 2, function(x) quantile(x, 0.05))  # 5th percentile
# forecast_upper_full <- apply(bootstrap_forecasts_full, 2, function(x) quantile(x, 0.95))  # 95th percentile

# forecast_results_full <- future_values %>%
#     mutate(
#         elec_forecast_mean_full = forecast_mean_full,
#         elec_forecast_lower_full = forecast_lower_full,
#         elec_forecast_upper_full = forecast_upper_full
#     )

# # %============================%=
# # Plot results ####

# # Plot results for after 2007
# ggplot(forecast_results_after, aes(x = Date)) +
#     geom_ribbon(aes(ymin = elec_forecast_lower_after, ymax = elec_forecast_upper_after), fill = "lightblue", alpha = 0.4) +
#     geom_line(aes(y = elec_forecast_mean_after), color = "blue", size = 1.2) +
#     geom_point(data = celec, aes(x = Date, y = elec_cons_pc), color = "black", size = 1) +
#     geom_line(data = celec %>% filter(Date <= 2020), aes(x = Date, y = elec_cons_pc), color = "black", size = 0.5) +  # Thin black line for actual data
#     geom_line(data = celec_after_2007 %>% filter(Date <= 2020), aes(x = Date, y = exp(predict(best_model_after, newdata = celec_after_2007 %>% filter(Date <= 2020)))), color = "red", size = 1.2) +
#     labs(
#         title = "Electricity Consumption Forecast",
#         x = "Year",
#         y = "Electricity Consumption (GWh)"
#     ) +
#     theme_minimal()

# # Save the plot
# ggsave("econometrics-II-report/Images/bootstrap_forecast_after.jpeg", width = 10, height = 6)

# # Plot results for full model
# ggplot(forecast_results_full, aes(x = Date)) +
#     geom_ribbon(aes(ymin = elec_forecast_lower_full, ymax = elec_forecast_upper_full), fill = "lightblue", alpha = 0.4) +
#     geom_line(aes(y = elec_forecast_mean_full), color = "blue", size = 1.2) +
#     geom_point(data = celec, aes(x = Date, y = elec_cons_pc), color = "black", size = 1) +
#     labs(
#         title = "Electricity Consumption Forecast with Bootstrap (95% CI) - Full Model",
#         x = "Year",
#         y = "Electricity Consumption (GWh)"
#     ) +
#     theme_minimal()

# # Save the plot
# ggsave("econometrics-II-report/Images/bootstrap_forecast_full.jpeg", width = 10, height = 6)




# %=================================================%=
# ECONOMETRIC FORECAST FROM ARIMA ####
# %=================================================%=
## Be sure that there is weak exogeneity between the dependent variable and the independent variables
# # Test if electricity consumption Granger-causes income per capita
# grangertest(ln_Inc_pc_MC ~ log(elec_cons_pc), order = 2, data = celec)

# # Test for electricity price
# grangertest(ln_price2015_MC ~ log(elec_cons_pc), order = 2, data = celec)

# # Test for climate index
# grangertest(IRC ~ log(elec_cons_pc), order = 2, data = celec)

# # Convert to long format for ggplot
# celec_after_2007_long <- celec_after_2007 %>%
#     select(Date, ln_Inc_pc_MC, ln_price2015_MC, IRC) %>%
#     pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value")

# # Plot each time series
# ggplot(celec_after_2007_long, aes(x = Date, y = Value, color = Variable)) +
#     geom_line() +
#     facet_wrap(~Variable, scales = "free_y") +
#     theme_minimal() +
#     labs(title = "Time Series of Explanatory Variables", x = "Year", y = "Value")

# # Convert Date column to a time series format
# log_elec_2007_ts <- ts(log(celec_after_2007$elec_cons_pc), start = min(celec_after_2007$Date), frequency = 1)

# log_Inc_pc_2007_ts <- ts(celec_after_2007$ln_Inc_pc_MC, start = min(celec_after_2007$Date), frequency = 1)

# log_price_2007_ts <- ts(celec_after_2007$ln_price2015_MC, start = min(celec_after_2007$Date), frequency = 1)

# IRC_2007_ts <- ts(celec_after_2007$IRC, start = min(celec_after_2007$Date), frequency = 1)


# # Perform Elliott-Rothenberg-Stock (ERS) test
# ers_test_price <- ur.ers(log_price_2007_ts, type = "DF-GLS", model = "constant", lag.max = 3)
# print(summary(ers_test_price))

# # Perform KPSS test
# kpss_test_price <- kpss.test(log_price_2007_ts)
# print(kpss_test_price)

# diff_price_ts <- diff(log_price_2007_ts, differences = 1)
# plot(diff_price_ts, main="Differenced Price", ylab="Differenced Consumption", xlab="Year")

# ## Non-stationary, apply differencing
# # Determine optimal differencing order (d)
# d <- 0
# max_d <- 3  # Set a reasonable maximum differencing order

# while (d < max_d) {
#   diff_price_ts <- diff(log_price_2007_ts, differences = d + 1)
  
#   # Check if the differenced series is long enough
#   if (length(diff_price_ts) < 10) {
#     cat("Insufficient data after differencing, stopping at d =", d, "\n")
#     break
#   }
  
#   ers_test_result <- ur.ers(diff_price_ts, type = "DF-GLS", model = "constant", lag.max = 3)
#   kpss_test_result <- kpss.test(diff_price_ts)
  
#   # Properly extract test statistics
#   ers_stat <- ers_test_result@teststat
#   ers_crit_5pct <- ers_test_result@cval["5pct"]
  
#   if (!is.na(ers_stat) && !is.na(ers_crit_5pct) && ers_stat <= -ers_crit_5pct && kpss_test_result$p.value >= 0.05) {
#     d <- d + 1
#     break
#   }
  
#   d <- d + 1
# }

# cat("Optimal differencing order (d) selected:", d, "\n")

# plot(diff_price_ts, main="Differenced Price", ylab="Differenced Consumption", xlab="Year")


# # ACF and PACF plots to determine p and q
# acf(diff_elec_ts, main="ACF of Differenced Data")
# pacf(diff_elec_ts, main="PACF of Differenced Data")

# # Fit ARIMA model
# fit <- auto.arima(elec_ts, seasonal = FALSE)
# summary(fit)

# # Forecast for the next 10 years
# forecasted <- forecast(fit, h=10)
# plot(forecasted, main="Electricity Consumption Forecast")

# cat("Optimal differencing order (d) selected:", d, "\n")
