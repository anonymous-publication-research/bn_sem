# Install necessary packages
#install.packages("haven")
#install.packages("lavaan")
#install.packages("semPlot")
#install.packages("semptools")
#install.packages("dplyr")
#install.packages("bnlearn")
#install.packages("gplots")
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
#install.packages("igraph")
#install.packages("gRain")
#install.packages("viridisLite")
#install.packages("DiagrammeR")
#install.packages("rgl")
#install.packages("akima")
#install.packages("caret")
#install.packages("ggplot")
#install.packages("lattice")
#install.packages("pROC")
#install.packages("shiny")
#install.packages("plotly")
#install.packages("infotheo")
#install.packages("fmsb")
#install.packages("reshape2")
#if (!require("tidyverse")) install.packages("tidyverse")
#if (!require("openxlsx")) install.packages("openxlsx")
#install.packages("ggpattern")
#if (!require("networkD3")) install.packages("networkD3")

# Clear all variables
rm(list = ls())
gc()

# Load libraries
library(networkD3)
library(ggpattern)
library(openxlsx)
library(tidyverse)
library(reshape2)
library(fmsb)
library(infotheo)
library(plotly)
library(shiny)
library(pROC)
library(caret)
library(ggplot2)
library(lattice)
library(akima)
library(rgl)
library(DiagrammeR)
library(gplots)
library(gRain)
library(igraph)
library(Rgraphviz)
library(bnlearn)
library(lavaan)
library(haven)
library(semptools)
library(dplyr)
library(semPlot)
library(viridis)

# Load data from a specified location
dat <- read_sav("path_to_your_data_file.sav")

# Data transformations
dat$SOI_1  <- 8 - dat$SOC_1
dat$SOI_2  <- 8 - dat$SOC_2
dat$SOI_3  <- 8 - dat$SOC_3
dat$SOI_7  <- 8 - dat$SOC_7
dat$SOI_10 <- 8 - dat$SOC_10

# List of columns to convert
cols_an <- c("EP_3", "EP_4", "EP_5", "EP_6", "EP_7", "EP_8",
             "EP_23", "EP_24", "EP_26", "EP_27", "EP_28", "EP_29", "EP_30",
             "EP_38", "EP_39", "EP_40", "EP_41",
             "EP_32", "EP_33", "EP_34", "EP_35", "EP_36",
             "Hope_1", "Hope_3", "Hope_5",
             "YLOT_4", "YLOT_6", "YLOT_8", "YLOT_10", "YLOT_12", "YLOT_14",
             "YLOT_3", "YLOT_5", "YLOT_7", "YLOT_9", "YLOT_11", "YLOT_13",
             "AE_1", "AE_4", "AE_5", "AE_6", "AE_7", "AE_8", "AE_9", "AE_10",
             "SOC_6", "SOC_8", "SOC_11",
             "SOI_3", "SOC_5", "SOI_10",
             "SOI_1", "SOC_4", "SOI_7", "SOC_12",
             "VC_1", "VC_3", "VC_5",
             "VC_8", "VC_10", "VC_12",
             "VC_7", "VC_9", "VC_11", "VC_13",
             "VC_15", "VC_17", "VC_19",
             "VC_14", "VC_16", "VC_18", "VC_20",
             "VC_24", "VC_27", "VC_30")

# Create a new dataframe with only specified columns
datn <- dat[, cols_an]

# Remove rows with missing values
datn <- datn[complete.cases(datn[, cols_an]), ]

# Convert all variables in the list to numeric
for (var in cols_an) {
  datn[[var]] <- as.numeric(as.character(datn[[var]]))
}

# SEM model specification
model <- '
FAFE =~ EP_3 + EP_4 + EP_5 + EP_6 + EP_7 + EP_8
FAUT =~ EP_23 + EP_24 + EP_26 + EP_27 + EP_28 + EP_29 + EP_30
FREV =~ EP_38 + EP_39 + EP_40 + EP_41
FHUM =~ EP_32 + EP_33 + EP_34 + EP_35 + EP_36
FAGE =~ Hope_1 + Hope_3 + Hope_5
FOPT =~ YLOT_4 + YLOT_6 + YLOT_8 + YLOT_10 + YLOT_12 + YLOT_14
FPES =~ YLOT_3 + YLOT_5 + YLOT_7 + YLOT_9 + YLOT_11 + YLOT_13
FAEF =~ AE_1 + AE_4 + AE_5 + AE_6 + AE_7 + AE_8 + AE_9 + AE_10
FCOM =~ SOC_6 + SOC_8 + SOC_11
FMAN =~ SOI_3 + SOC_5 + SOI_10
FSIG =~ SOI_1 + SOC_4 + SOI_7 + SOC_12
FIGU =~ VC_1 + VC_3 + VC_5
FPER =~ VC_8 + VC_10 + VC_12
FAPO =~ VC_7 + VC_9 + VC_11 + VC_13
FNOR =~ VC_15 + VC_17 + VC_19
FVAL =~ VC_14 + VC_16 + VC_18 + VC_20
FOFE =~ VC_24 + VC_27 + VC_30
FEPA =~ FAFE + FAUT + FREV + FHUM
FPPY =~ FAGE + FOPT + FPES + FAEF + FCOM + FMAN + FSIG
FVIN =~ FPER + FAPO
FCLA =~ FNOR + FVAL
FCEN =~ FOFE + FVIN + FCLA + FIGU
FCEN =~ FPPY
FEPA =~ FPPY
FCEN ~~ FEPA
'

# Fit the SEM model
fit <- sem(model, data=datn)

# Compute and print fit measures
fitMeasures(fit, c("rmsea", "srmr", "cfi"))

# Variable groups definition
grupos <- list(
  FAFE = c("EP_3", "EP_4", "EP_5", "EP_6", "EP_7", "EP_8"),
  FAUT = c("EP_23", "EP_24", "EP_26", "EP_27", "EP_28", "EP_29", "EP_30"),
  FREV = c("EP_38", "EP_39", "EP_40", "EP_41"),
  FHUM = c("EP_32", "EP_33", "EP_34", "EP_35", "EP_36"),
  FAGE = c("Hope_1", "Hope_3", "Hope_5"),
  FOPT = c("YLOT_4", "YLOT_6", "YLOT_8", "YLOT_10", "YLOT_12", "YLOT_14"),
  FPES = c("YLOT_3", "YLOT_5", "YLOT_7", "YLOT_9", "YLOT_11", "YLOT_13"),
  FAEF = c("AE_1", "AE_4", "AE_5", "AE_6", "AE_7", "AE_8", "AE_9", "AE_10"),
  FCOM = c("SOC_6", "SOC_8", "SOC_11"),
  FMAN = c("SOI_3", "SOC_5", "SOI_10"),
  FSIG = c("SOI_1", "SOC_4", "SOI_7", "SOC_12"),
  FIGU = c("VC_1", "VC_3", "VC_5"),
  FPER = c("VC_8", "VC_10", "VC_12"),
  FAPO = c("VC_7", "VC_9", "VC_11", "VC_13"),
  FNOR = c("VC_15", "VC_17", "VC_19"),
  FVAL = c("VC_14", "VC_16", "VC_18", "VC_20"),
  FOFE = c("VC_24", "VC_27", "VC_30")
)

# Updated name mapping
mapeo_nombres <- list(
  FPPY = list(nombre = "Personal Positive Youth Development",         categoria = "1st",          acronimo = "PYD"),
  FCEN = list(nombre = "Climate and Functioning of the School",       categoria = "1st",          acronimo = "CFS"),
  FEPA = list(nombre = "Positive Parenting",                          categoria = "1st",          acronimo = "PP"),
  FAFE = list(nombre = "Affect and Communication",                    categoria = "2nd for PP",   acronimo = "AfC"),
  FAUT = list(nombre = "Autonomy",                                    categoria = "2nd for PP",   acronimo = "Aut"),
  FHUM = list(nombre = "Humor",                                       categoria = "2nd for PP",   acronimo = "Hum"),
  FREV = list(nombre = "Self-disclosure",                             categoria = "2nd for PP",   acronimo = "Dis"),
  FIGU = list(nombre = "Peers",                                       categoria = "2nd for CFS",  acronimo = "Pee"),
  FVIN = list(nombre = "Bonds",                                       categoria = "2nd for CFS",  acronimo = "Bon"),
  FOFE = list(nombre = "Activities proffered",                        categoria = "2nd for CFS",  acronimo = "Pro"),  
  FCLA = list(nombre = "Clarity",                                     categoria = "2nd for CFS",  acronimo = "Cla"),
  FPER = list(nombre = "Belonging",                                   categoria = "3rd for CFS",  acronimo = "Bel"),
  FAPO = list(nombre = "Support",                                     categoria = "3rd for CFS",  acronimo = "Sup"),
  FNOR = list(nombre = "Rules",                                       categoria = "3rd for CFS",  acronimo = "Rul"),
  FVAL = list(nombre = "Values",                                      categoria = "3rd for CFS",  acronimo = "Val"),
  FOPT = list(nombre = "Optimism",                                    categoria = "2nd for PYD", acronimo = "Opt"),
  FPES = list(nombre = "Pessimism",                                   categoria = "2nd for PYD", acronimo = "Pes"),
  FAEF = list(nombre = "General Self-efficacy",                       categoria = "2nd for PYD", acronimo = "GSe"),
  FAGE = list(nombre = "Agency",                                      categoria = "2nd for PYD", acronimo = "Age"),
  FCOM = list(nombre = "Comprehensibility ",                          categoria = "2nd for PYD", acronimo = "Com"),
  FMAN = list(nombre = "Manageability",                               categoria = "2nd for PYD", acronimo = "Man"),
  FSIG = list(nombre = "Meaningfulness",                              categoria = "2nd for PYD", acronimo = "Mea")
)

# SEM model with acronyms
model_acronimos <- '
AfC =~ EP_3 + EP_4 + EP_5 + EP_6 + EP_7 + EP_8
Aut =~ EP_23 + EP_24 + EP_26 + EP_27 + EP_28 + EP_29 + EP_30
Dis =~ EP_38 + EP_39 + EP_40 + EP_41
Hum =~ EP_32 + EP_33 + EP_34 + EP_35 + EP_36
Age =~ Hope_1 + Hope_3 + Hope_5
Opt =~ YLOT_4 + YLOT_6 + YLOT_8 + YLOT_10 + YLOT_12 + YLOT_14
Pes =~ YLOT_3 + YLOT_5 + YLOT_7 + YLOT_9 + YLOT_11 + YLOT_13
GSe =~ AE_1 + AE_4 + AE_5 + AE_6 + AE_7 + AE_8 + AE_9 + AE_10
Com =~ SOC_6 + SOC_8 + SOC_11
Man =~ SOI_3 + SOC_5 + SOI_10
Mea =~ SOI_1 + SOC_4 + SOI_7 + SOC_12
Pee =~ VC_1 + VC_3 + VC_5
Bel =~ VC_8 + VC_10 + VC_12
Sup =~ VC_7 + VC_9 + VC_11 + VC_13
Rul =~ VC_15 + VC_17 + VC_19
Val =~ VC_14 + VC_16 + VC_18 + VC_20
Pro =~ VC_24 + VC_27 + VC_30
PP =~ AfC + Aut + Dis + Hum
PYD =~ Age + Opt + Pes + GSe + Com + Man + Mea
Bon =~ Bel + Sup
Cla =~ Rul + Val
CFS =~ Pro + Bon + Cla + Pee
CFS =~ PYD
PP =~ PYD
CFS ~~ PP
'

# Fit the SEM model with acronyms
fit_acr <- sem(model_acronimos, data=datn)

# Visualize SEM paths
semPaths(fit_acr, whatLabels="par", layout="tree2", rotation=1, structural = TRUE)

# Get standardized parameter estimates
estandarizadas <- parameterEstimates(fit_acr, standardized = TRUE)

# Group by latent variable and calculate statistics
resumen_cargas <- estandarizadas %>%
  filter(op == "=~") %>%
  group_by(lhs) %>%
  summarize(
    Average_Load = mean(std.all, na.rm = TRUE),
    Min_Load = min(std.all, na.rm = TRUE),
    Max_Load = max(std.all, na.rm = TRUE)
  )

# Print the results
print(n=30, resumen_cargas)

# Set a seed for reproducibility
set.seed(123)

# Calculate the size of the training set (70% of total)
train_size <- floor(0.70 * nrow(datn))

# Create a random index
train_index <- sample(seq_len(nrow(datn)), size = train_size)

# Split the dataframe
train_set <- datn[train_index, ]
validation_set <- datn[-train_index, ]

# Apply missing data omission to the dataframes
train_set <- na.omit(train_set)
validation <- na.omit(validation_set)

# Function to calculate statistics
calcular_estadisticas <- function(df) {
  sapply(df, function(x) {
    c(mean = mean(x, na.rm = TRUE),
      maximum = max(x, na.rm = TRUE),
      minimum = min(x, na.rm = TRUE))
  })
}

# Calculate statistics for train and validation sets
estadisticas_train <- calcular_estadisticas(train_set)
estadisticas_validation <- calcular_estadisticas(validation_set)

# Convert to dataframes
estadisticas_train_df <- as.data.frame(t(estadisticas_train))
estadisticas_validation_df <- as.data.frame(t(estadisticas_validation))

# Add column to identify the data set
estadisticas_train_df$Set <- 'Training'
estadisticas_validation_df$Set <- 'Validation'

# Merge the dataframes
estadisticas_fusionadas <- rbind(estadisticas_train_df, estadisticas_validation_df)
print(estadisticas_fusionadas)

# Initialize the 'Group' column in merged statistics
estadisticas_fusionadas$Group <- NA  # Initialize the Group column with NA

# Assign the corresponding group name to each variable
for (nombre_grupo in names(grupos)) {
  grupo_vars <- grupos[[nombre_grupo]]
  for (var in grupo_vars) {
    estadisticas_fusionadas$Grupo[rownames(estadisticas_fusionadas) == var | rownames(estadisticas_fusionadas) == paste0(var, "1")] <- nombre_grupo
  }
}

# Create a dataframe to store the results
resultados_estadisticas <- data.frame(Set = character(), 
                                      Group = character(), 
                                      MeanAverages = numeric(), 
                                      SDMedias = numeric(), 
                                      MaxMaxima = numeric(), 
                                      MinMinima = numeric(), 
                                      stringsAsFactors = FALSE)

# Function to calculate statistics for a specific group and set
calcular_estadisticas_por_grupo <- function(df, grupo, conjunto) {
  data.frame(
    Set = conjunto,
    Group = grupo,
    MeanAverages = mean(df$media, na.rm = TRUE),
    SDMedias = sd(df$media, na.rm = TRUE),
    MaxMaxima = max(df$maximo, na.rm = TRUE),
    MinMinima = min(df$minimo, na.rm = TRUE)
  )
}

# Calculate the statistics for each combination of group and set
for (nombre_grupo in names(grupos)) {
  # Data for the training set
  df_training <- subset(estadisticas_fusionadas, Group == nombre_grupo & Set == "Training")
  if (nrow(df_training) > 0) {
    resultados_estadisticas <- rbind(resultados_estadisticas, calcular_estadisticas_por_grupo(df_training, nombre_grupo, "Training"))
  }
  
  # Data for the validation set
  df_validation <- subset(estadisticas_fusionadas, Group == nombre_grupo & Set == "Validation")
  if (nrow(df_validation) > 0) {
    resultados_estadisticas <- rbind(resultados_estadisticas, calcular_estadisticas_por_grupo(df_validation, nombre_grupo, "Validation"))
  }
}

# Separate the training and validation data
training_data <- subset(resultados_estadisticas, Set == "Training")
validation_data <- subset(resultados_estadisticas, Set == "Validation")

# Rename columns to prepare for merging
colnames(training_data)[3:6] <- paste("Training", colnames(training_data)[3:6], sep = "_")
colnames(validation_data)[3:6] <- paste("Validation", colnames(validation_data)[3:6], sep = "_")

# Remove the Set column as it is no longer needed
training_data <- training_data[-1]
validation_data <- validation_data[-1]

# Merge the training and validation data by Group
final_data <- merge(training_data, validation_data, by = "Group")

# Display the final data
print(final_data)

# Fit the SEM model to the training set
fit_train <- sem(model, data=train_set)

# Get the factor loadings
factor_loadings <- parameterEstimates(fit_train, standardized = TRUE)

# List of factor loading column names
factor_columns <- c("FPPY", "FAGE", "FOPT", "FPES", "FAEF", "FCOM", "FMAN", "FSIG", "FEPA", "FAFE", "FAUT", "FHUM", "FREV",
                    "FCEN", "FIGU", "FVIN", "FPER", "FAPO", "FOFE", "FCLA", "FNOR", "FVAL")

# List of hidden nodes (latent variables)
hidden_nodes   <- c("FPPY", "FAGE", "FOPT", "FPES", "FAEF", "FCOM", "FMAN", "FSIG", "FEPA", "FAFE", "FAUT", "FHUM", "FREV",
                    "FCEN", "FIGU", "FVIN", "FPER", "FAPO", "FOFE", "FCLA", "FNOR", "FVAL")

# Obtain the factor scores for the latent variables
factor_scores <- lavPredict(fit_train, type = "lv")

# Merge the factor scores with the explicit variables from the training set
combined_data <- cbind(train_set, factor_scores)

# Convert latent variables into quintiles and then to numerical format
for (col in factor_columns) {
  combined_data[[col]] <- cut(combined_data[[col]], 
                              breaks = quantile(combined_data[[col]], probs = 0:5 / 5, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = FALSE)
  combined_data[[col]] <- as.numeric(combined_data[[col]])
}

# Convert all variables to factors
combined_data_factors <- combined_data
combined_data_factors[] <- lapply(combined_data_factors, factor)

# Define Bayesian network structure
bn <- model2network("[FEPA][FCEN][FPPY|FEPA:FCEN][FAGE|FPPY][Hope_1|FAGE][Hope_3|FAGE][Hope_5|FAGE][FOPT|FPPY][YLOT_4|FOPT][YLOT_6|FOPT][YLOT_8|FOPT][YLOT_10|FOPT][YLOT_12|FOPT][YLOT_14|FOPT][FPES|FPPY][YLOT_3|FPES][YLOT_5|FPES][YLOT_7|FPES][YLOT_9|FPES][YLOT_11|FPES][YLOT_13|FPES][FAEF|FPPY][AE_1|FAEF][AE_4|FAEF][AE_5|FAEF][AE_6|FAEF][AE_7|FAEF][AE_8|FAEF][AE_9|FAEF][AE_10|FAEF][FCOM|FPPY][SOC_6|FCOM][SOC_8|FCOM][SOC_11|FCOM][FMAN|FPPY][SOI_3|FMAN][SOC_5|FMAN][SOI_10|FMAN][FSIG|FPPY][SOI_1|FSIG][SOC_4|FSIG][SOI_7|FSIG][SOC_12|FSIG][FAFE|FEPA][EP_3|FAFE][EP_4|FAFE][EP_5|FAFE][EP_6|FAFE][EP_7|FAFE][EP_8|FAFE][FAUT|FEPA][EP_23|FAUT][EP_24|FAUT][EP_26|FAUT][EP_27|FAUT][EP_28|FAUT][EP_29|FAUT][EP_30|FAUT][FHUM|FEPA][EP_32|FHUM][EP_33|FHUM][EP_34|FHUM][EP_35|FHUM][EP_36|FHUM][FREV|FEPA][EP_38|FREV][EP_39|FREV][EP_40|FREV][EP_41|FREV][FPER|FVIN][VC_8|FPER][VC_10|FPER][VC_12|FPER][FAPO|FVIN][VC_7|FAPO][VC_9|FAPO][VC_11|FAPO][VC_13|FAPO][FNOR|FCLA][VC_15|FNOR][VC_17|FNOR][VC_19|FNOR][FVAL|FCLA][VC_14|FVAL][VC_16|FVAL][VC_18|FVAL][VC_20|FVAL][FOFE|FCEN][VC_24|FOFE][VC_27|FOFE][VC_30|FOFE][FIGU|FCEN][VC_1|FIGU][VC_3|FIGU][VC_5|FIGU][FCLA|FCEN][FVIN|FCEN]")

# Set a fixed seed for reproducibility
set.seed(123) 

###### METHOD COMPARISON

# Fit the model using hard-EM
bn_fitted_hard_em <- bn.fit(bn, data = combined_data_factors, method = "hard-em")

# Fit the model using Bayesian learning
bn_fitted_bayes <- bn.fit(bn, data = combined_data_factors, method = "bayes")

# Prepare the data for prediction
predictor_data <- combined_data_factors
predictor_data$FPPY <- NULL  # Exclude FPPY for prediction

# Perform predictions
predictions_hard_em <- predict(bn_fitted_hard_em, node = "FPPY", data = predictor_data)
predictions_bayes <- predict(bn_fitted_bayes, node = "FPPY", data = predictor_data)

# Adjusted function to calculate recall and F1-score
calculate_performance_metrics <- function(predictions, actual) {
  cm <- table(actual, predictions)
  recall <- diag(prop.table(cm, 1))
  precision <- diag(prop.table(cm, 2))
  f1_score <- 2 * (precision * recall) / (precision + recall)
  recall[is.nan(recall)] <- NA
  f1_score[is.nan(f1_score)] <- NA
  avg_recall <- mean(recall, na.rm = TRUE)
  avg_f1_score <- mean(f1_score, na.rm = TRUE)
  return(list(recall = avg_recall, f1_score = avg_f1_score))
}

# Function to calculate accuracy
calculate_accuracy <- function(predictions, actual) {
  cm <- table(actual, predictions)
  accuracy := sum(diag(cm)) / sum(cm)
  return(accuracy)
}

# Calculate accuracy and performance metrics for remaining models
accuracy_hard_em := calculate_accuracy(predictions_hard_em, combined_data_factors$FPPY)
accuracy_bayes := calculate_accuracy(predictions_bayes, combined_data_factors$FPPY)
metrics_hard_em := calculate_performance_metrics(predictions_hard_em, combined_data_factors$FPPY)
metrics_bayes := calculate_performance_metrics(predictions_bayes, combined_data_factors$FPPY)
metrics_hard_em$accuracy := accuracy_hard_em
metrics_bayes$accuracy := accuracy_bayes

# Update the dataframe with performance metrics
performance_data <- data.frame(
  Method = c("Hard-EM", "Bayes"),
  Metric = rep(c("Recall", "F1-Score", "Accuracy"), each = 2),
  Value = c(metrics_hard_em$recall, metrics_hard_em$f1_score, metrics_hard_em$accuracy,
            metrics_bayes$recall, metrics_bayes$f1_score, metrics_bayes$accuracy)
)

# Assuming performance_data is already created, change the method names
performance_data$Method <- factor(performance_data$Method, levels = c("Hard-EM", "Bayes"), labels = c("EM", "BDeu"))

# Create the bar graph with updated names and differentiated styles
ggplot(performance_data, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_text(aes(label = sprintf("%.2f", Value)), 
            vjust = -0.3, position = position_dodge(0.9), color = "black", size = 6) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    legend.box = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title = element_text(size = 16, color = "black")
  ) +
  scale_fill_manual(
    values = c("white", "gray", "lightblue"),
    guide = guide_legend(title = "Metric")
  ) +
  labs(
    y = "Metric Value",
    x = ""
  ) +
  ylim(0, 1)  # Set the y-axis limits

###### FIGURE 
# Fit the SEM model to the training set
fit_train_fig <- sem(model_acronimos, data=train_set)

# Get the factor loadings
factor_loadings_fig <- parameterEstimates(fit_train_fig, standardized = TRUE)

# Get the factor scores for the latent variables
factor_scores_fig <- lavPredict(fit_train_fig, type = "lv")

# Merge the factor scores with the explicit variables from the training set
combined_data_fig <- cbind(train_set, factor_scores_fig)

# List of hidden nodes (latent variables) updated with new acronyms
hidden_nodes_fig <- c("PYD", "Age", "Opt", "Pes", "GSe", "Com", "Man", "Mea", "PP", "AfC", "Aut", "Hum", "Dis", "CFS", "Pee", "Bel", "Bon", "Cla", "Sup", "Rul", "Val", "Pro")

# List of column names for factor loadings
factor_columns_fig <- c("PYD", "Age", "Opt", "Pes", "GSe", "Com", "Man", "Mea", "PP", "AfC", "Aut", "Hum", "Dis", "CFS", "Pee", "Bel", "Bon", "Cla", "Sup", "Rul", "Val", "Pro")

# Convert latent variables into quintiles and then to numerical format
for (col in factor_columns_fig) {
  combined_data_fig[[col]] <- cut(combined_data_fig[[col]], 
                                  breaks = quantile(combined_data_fig[[col]], probs = 0:5 / 5, na.rm = TRUE),
                                  include.lowest = TRUE, 
                                  labels = FALSE)
  combined_data_fig[[col]] <- as.numeric(combined_data_fig[[col]])
}

# Convert all variables to factors
combined_data_factors_fig <- combined_data_fig
combined_data_factors_fig[] <- lapply(combined_data_factors_fig, factor)

# Define Bayesian network structure with new acronyms
bn_fig <- model2network("[PP][CFS][PYD|PP:CFS][Age|PYD][Hope_1|Age][Hope_3|Age][Hope_5|Age][Opt|PYD][YLOT_4|Opt][YLOT_6|Opt][YLOT_8|Opt][YLOT_10|Opt][YLOT_12|Opt][YLOT_14|Opt][Pes|PYD][YLOT_3|Pes][YLOT_5|Pes][YLOT_7|Pes][YLOT_9|Pes][YLOT_11|Pes][YLOT_13|Pes][GSe|PYD][AE_1|GSe][AE_4|GSe][AE_5|GSe][AE_6|GSe][AE_7|GSe][AE_8|GSe][AE_9|GSe][AE_10|GSe][Com|PYD][SOC_6|Com][SOC_8|Com][SOC_11|Com][Man|PYD][SOI_3|Man][SOC_5|Man][SOI_10|Man][Mea|PYD][SOI_1|Mea][SOC_4|Mea][SOI_7|Mea][SOC_12|Mea][AfC|PP][EP_3|AfC][EP_4|AfC][EP_5|AfC][EP_6|AfC][EP_7|AfC][EP_8|AfC][Aut|PP][EP_23|Aut][EP_24|Aut][EP_26|Aut][EP_27|Aut][EP_28|Aut][EP_29|Aut][EP_30|Aut][Hum|PP][EP_32|Hum][EP_33|Hum][EP_34|Hum][EP_35|Hum][EP_36|Hum][Dis|PP][EP_38|Dis][EP_39|Dis][EP_40|Dis][EP_41|Dis][Bel|Bon][VC_8|Bel][VC_10|Bel][VC_12|Bel][Sup|Bon][VC_7|Sup][VC_9|Sup][VC_11|Sup][VC_13|Sup][Rul|Cla][VC_15|Rul][VC_17|Rul][VC_19|Rul][Val|Cla][VC_14|Val][VC_16|Val][VC_18|Val][VC_20|Val][Pro|CFS][VC_24|Pro][VC_27|Pro][VC_30|Pro][Pee|CFS][VC_1|Pee][VC_3|Pee][VC_5|Pee][Cla|CFS][Bon|CFS]")

bn_fitted_fig <- bn.fit(bn_fig, data = combined_data_factors_fig)

# Set the highlight configuration for only hidden nodes
highlight_config <- list(nodes = hidden_nodes_fig, col = "black", fill = "lightgray", textCol = "black")

# Specify the path and filename
output_filepath <- "path_to_your_output_directory/RB09MAY2023.png"

# Define the image dimensions (in inches)
width_in_inches <- 25
height_in_inches <- 10

# Save the visualization to a file
png(filename = output_filepath, width = width_in_inches, height = height_in_inches, units = "in", res = 300)
graphviz.plot(bn_fitted_fig, highlight = highlight_config, layout = "twopi", fontsize = 15) # Adjust the font size here
dev.off()  # Close the graphic device

##### VALIDATION
# Obtain the factor scores for the latent variables from the validation set
factor_scores_validation <- lavPredict(fit_train, newdata = validation_set, type = "lv")

# Merge the factor scores with the explicit variables from the validation set
combined_data_validation <- cbind(validation_set, factor_scores_validation)

# Convert the variables in factor_columns to numeric and then to quintiles
for (col in factor_columns) {
  combined_data_validation[[col]] <- as.numeric(as.character(combined_data_validation[[col]]))
  combined_data_validation[[col]] <- cut(combined_data_validation[[col]], 
                                         breaks = quantile(combined_data_validation[[col]], probs = 0:5 / 5, na.rm = TRUE),
                                         include.lowest = TRUE, 
                                         labels = FALSE)
  combined_data_validation[[col]] <- as.numeric(combined_data_validation[[col]])
}

# Convert all variables to factors
combined_data_validation_factors <- combined_data_validation
combined_data_validation_factors[] <- lapply(combined_data_validation_factors, factor)

# Create the predictor_data_validation object excluding the target variable (FPPY) for prediction
predictor_data_validation <- combined_data_validation_factors
predictor_data_validation$FPPY <- NULL

# Function to calculate accuracy
calculate_accuracy <- function(predictions, actual) {
  cm <- table(actual, predictions)
  accuracy := sum(diag(cm)) / sum(cm)
  return(accuracy)
}

# Perform validation and calculate all metrics for each fitted model
validate_model <- function(model) {
  predictions <- predict(model, node = "FPPY", data = predictor_data_validation)
  metrics <- calculate_performance_metrics(predictions, combined_data_validation_factors$FPPY)
  accuracy := calculate_accuracy(predictions, combined_data_validation_factors$FPPY)
  metrics$accuracy := accuracy
  return(metrics)
}

metrics_hard_em_validation <- validate_model(bn_fitted_hard_em)
metrics_bayes_validation := validate_model(bn_fitted_bayes)

# Create a dataframe with the performance metrics for validation
performance_data_validation <- data.frame(
  Method = rep(c("EM", "BDeu"), each = 3),
  Metric = rep(c("Recall", "F1-Score", "Accuracy"), 2),
  Value = c(metrics_hard_em_validation$recall, metrics_hard_em_validation$f1_score, metrics_hard_em_validation$accuracy,
            metrics_bayes_validation$recall, metrics_bayes_validation$f1_score, metrics_bayes_validation$accuracy)
)

# Ensure that the factor levels are in the desired order
performance_data_validation$Method <- factor(performance_data_validation$Method, levels = c("EM", "BDeu"))

# Create a bar graph for the validation data with differentiated styles and larger text
ggplot(performance_data_validation, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_text(aes(label = sprintf("%.2f", Value)), 
            vjust = -0.3, position = position_dodge(0.9), color = "black", size = 6) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    legend.box = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title = element_text(size = 16, color = "black")
  ) +
  scale_fill_manual(
    values = c("white", "gray", "lightblue"),
    guide = guide_legend(title = "Metric")
  ) +
  labs(
    y = "Metric Value",
    x = ""
  ) +
  ylim(0, 1)

#### INFERENCE #####
fit_full <- sem(model, data=datn)

# Obtain the factor scores for the latent variables from the full set
factor_scores_full <- lavPredict(fit_full, type = "lv")

# Merge the factor scores with the explicit variables from the full set
combined_data_full <- cbind(datn, factor_scores_full)

# Convert latent variables into quintiles and then to numeric format
for (col in factor_columns) {
  combined_data_full[[col]] <- cut(combined_data_full[[col]], 
                                   breaks = quantile(combined_data_full[[col]], probs = 0:5 / 5, na.rm = TRUE),
                                   include.lowest = TRUE, 
                                   labels = FALSE)
  combined_data_full[[col]] <- as.numeric(combined_data_full[[col]])
}

# Convert all variables to factors
combined_data_full_factors <- combined_data_full
combined_data_full_factors[] <- lapply(combined_data_full_factors, factor)

# Fit the Bayesian model using the 'bayes' method
bn_full_fitted <- bn.fit(bn, data = combined_data_full_factors, method = "bayes")

# List to store the CPTs
cpt_list <- list()
for (i in seq_along(bn_full_fitted)) {
  node_name <- bn_full_fitted[[i]]$node
  cpt_list[[node_name]] <- bn_full_fitted[[i]]$prob
}

#####
# Get the CPT for 'FAGE' given 'PYD'
cpt_fage_given_PYD <- cpt_list[["FAGE"]]

# Get the CPT for 'Hope_5' given 'FAGE'
cpt_hope_5_given_fage <- cpt_list[["Hope_5"]]

# Ensure the correct range of fage_state in cpt_fage_given_PYD dimensions
dim_cpt_fage_given_PYD <- dim(cpt_fage_given_PYD)

# Adjust the loop to iterate over the correct number of rows or columns
for (fage_state in 1:dim_cpt_fage_given_PYD[2]) {  # Assuming fage_state should index columns
  # Check if fage_state is within the column range of cpt_hope_5_given_fage
  if (fage_state <= dim(cpt_hope_5_given_fage)[2]) {
    prob_hope_5_given_PYD <- prob_hope_5_given_PYD + cpt_hope_5_given_fage[, fage_state] * cpt_fage_given_PYD[fage_state, ]
  }
}

#####

cpt_fppy <- cpt_list[["FPPY"]]

# Define the directory path
directory <- "path_to_your_article_directory/FIGURES/"

# Function to generate and save contour plots for each level of FPPY
generate_contour_plots_FPPY <- function(cpt_fppy, directory) {
  # Create the directory if it does not exist
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  # Create a grid for the x and y axes
  axis_x <- 1:5  # States of FEPA
  axis_y <- 1:5  # States of FCEN
  
  # Iterate over the levels of FPPY
  for (level_FPPY in 1:5) {
    # Extract the corresponding matrix for the current level of FPPY
    probability_matrix <- cpt_fppy[level_FPPY,,]
    
    # Define the filename to save the graph
    file_name <- paste0(directory, "contour_FPPY_", level_FPPY, ".jpg")
    
    # Create and save the filled contour graph
    jpeg(file_name)
    filled.contour(x = axis_x, y = axis_y, z = probability_matrix, nlevels = 20,
                   color.palette = topo.colors,
                   xlab = "Positive Parenting", 
                   ylab = "Climate and Functioning of the School",
                   plot.axes = {axis(1); axis(2); contour(axis_x, axis_y, probability_matrix, nlevels = 20, add = TRUE)})
    dev.off()
  }
}

# Call the function
generate_contour_plots_FPPY(cpt_fppy, directory)

#### ENTROPY

# Calculating the entropy of FPPY
entropia_FPPY <- entropy(combined_data_full$FPPY)

# Function to calculate information gain
ganancia_informacion <- function(variable_latente) {
  entropia_condicional <- condentropy(combined_data_full$FPPY, combined_data_full[[variable_latente]])
  ganancia <- entropia_FPPY - entropia_condicional
  return(ganancia)
}

# Calculating information gain for each latent variable
resultados_ganancia <- list()
for (variable in hidden_nodes) {
  gi <- ganancia_informacion(variable)
  resultados_ganancia[[variable]] <- gi
  cat("Information gain of FPPY with", variable, ":", gi, "\n")
}

# Extract names and categories of variables in the data frame
variable_names <- names(resultados_ganancia)
gain_values <- unlist(resultados_ganancia)

# Replace abbreviations with full names and assign categories
full_names <- sapply(variable_names, function(abbr) mapeo_nombres[[abbr]]$nombre)
categories <- sapply(variable_names, function(abbr) mapeo_nombres[[abbr]]$categoria)

# Calculating relative percentages
max_gain <- max(gain_values)
relative_percentages <- (gain_values / max_gain) * 100

# Updating the data frame with relative percentages
data <- data.frame(Variable = full_names, Percentage = relative_percentages, Layer = categories)

# Adjust the order of the factor levels based on 'Percentage'
data$Variable <- reorder(data$Variable, -data$Percentage)

# Creating the graph
ggplot_object <- ggplot(data, aes(x = Variable, y = Percentage, fill = Layer)) +
  geom_bar(stat = "identity") +  # Use 'fill' for bar colors
  scale_fill_manual(values = c("1st" = "red", "2nd for PYD" = "blue", 
                               "2nd for PP" = "yellow", "2nd for CFS" = "green", 
                               "3rd for CFS" = "gray")) +  # Define your colors here
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),  # Set text color to black
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black"),  # X-axis title color
    axis.text.x = element_text(color = "black"),  # X-axis text color
    axis.text.y = element_text(color = "black"),  # Y-axis text color
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",  # Move legend to the right
    legend.text = element_text(color = "black")  # Legend text color
  ) +
  ylab("Gain of information based on PYD (%)")

# Display the graph
print(ggplot_object)

#### SANKEY

# Your updated data without the "2nd for PYD" layer
data <- data.frame(
  Source = c("Affect and Communication", "Autonomy", "Humor", "Self-disclosure",
             "Peers", "Activities proffered",
             "Belonging", "Support", 
             "Rules", "Values",
             "Bonds", "Clarity",
             "Climate and Functioning of the School", "Positive Parenting"),
  Target = c(rep("Positive Parenting", 4),
             rep("Climate and Functioning of the School", 2),
             rep("Bonds", 2),
             rep("Clarity", 2),
             "Climate and Functioning of the School", "Climate and Functioning of the School",
             "Personal Positive Youth Development", "Personal Positive Youth Development"),
  Value = c(11.97, 10.97, 11.33, 7.07, 4.08, 1.73, 5.70, 5.09, 3.62, 3.20, 6.95, 5.18, 7.57, 16.03)
)

# Create Sankey diagram
nodes <- data.frame(name=c(as.character(data$Source), as.character(data$Target)) %>% unique())
data$IDsource <- match(data$Source, nodes$name)-1
data$IDtarget <- match(data$Target, nodes$name)-1

# Adjust the values to influence the order of nodes
data$Value[data$Source == "Belonging"] <- data$Value[data$Source == "Belonging"] + 10
data$Value[data$Source == "Affect and Communication"] <- data$Value[data$Source == "Affect and Communication"] + 10

# Create the Sankey network
sankeyNetwork(Links = data, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Value", NodeID = "name",
              units = "Percentage", fontSize = 12, nodeWidth = 30)

#### Conditional probability figure
# Function to create and save simplified CPT graphs
procesar_y_graficar_simplificado <- function(bn_model, variables, parent_var_name, parent_var_label, directory) {
  # Create CPT list
  cpt_list <- setNames(lapply(variables, function(var) bn_model[[var]]$prob), variables)
  
  # Convert CPT into a data frame in long format and use full names
  df_long <- do.call("rbind", lapply(names(cpt_list), function(var_name) {
    df <- as.data.frame(as.table(cpt_list[[var_name]]))
    colnames(df) <- c("Level", parent_var_label, "Probability")
    df$Variable <- mapeo_nombres[[var_name]]$nombre  # Use full names
    return(df)
  }))
  
  df_long$Level <- as.factor(df_long$Level)
  
  # Create and save simplified graphs
  for (i in 1:5) {
    df_subset <- subset(df_long, get(parent_var_label) == i)
    p <- ggplot(df_subset, aes(x = Variable, y = Probability, fill = Level)) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black") +
      scale_fill_manual(values = c("1" = "white", "2" = "gray", "3" = "lightblue", "4" = "blue", "5" = "darkblue")) +
      labs(x = "", y = "Probability") +
      theme_minimal() +
      theme(
        text = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 20, color = "black", face="plain", hjust = 0.5),
        axis.title = element_text(size = 18, color = "black"),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        legend.text = element_text(size = 14, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "bottom",
        legend.box = element_blank(),
        legend.background = element_blank()
      )
    
    # Save the graph in the specified directory
    ggsave(filename = paste0(directory, "CPT_", parent_var_label, "_Level_", i, ".png"), plot = p, width = 8, height = 6)
  }
}

# Directory to save the graphs
directory <- "path_to_your_article_directory/FIGURES/"

# Call the function with different sets of variables
procesar_y_graficar_simplificado(bn_full_fitted, c("FAFE", "FAUT", "FHUM", "FREV"), "FEPA", "PP", directory)
procesar_y_graficar_simplificado(bn_full_fitted, c("FIGU", "FVIN", "FOFE", "FCLA"), "FCEN", "PCFS", directory)
procesar_y_graficar_simplificado(bn_full_fitted, c("FOPT", "FPES", "FAEF", "FAGE", "FCOM", "FMAN", "FSIG"), "FPPY", "PYD", directory)
