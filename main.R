###############################################################################
# Topic: Microeconomia do Desenvolvimento - Trabalho 1
# Instructor: Amanda Arabage
# Course: Microeconomia do Desenvolvimento
# Autor: Daniel Colli, Diogo Leite, Michel Finger, Nícolas de Moura
# Goal: Criar Curvas de Lorenz, coeficiente de Gini e índice de pobreza para analisar a desigualdade e pobreza 
###############################################################################
# Organize the working environment
###############################################################################

# Clean the working environment
rm(list = ls())
load.lib <- c("dplyr", "ipumsr", "ggplot2", "splines", "stargazer", "Hmisc", "AER","readxl", "tidyverse", "data.table", "stargazer", "lubridate", "fixest", "ggplot2", "pracma", "dplyr", "remotes", "tidyr", "mvProbit", "ipw", "MASS", "xtable", "quantreg", "nprobust", "chron")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)
remove(install.lib, lib, load.lib)

# Set the seed
set.seed(20250323)

# Set the maximum number of rows 
max_rows <- 100000

###############################################################################
# Creating the dataset
###############################################################################

data_list <- read_ipums_ddi("data/ipumsi_00014.xml")
raw_data <- read_ipums_micro(data_list)

###############################################################################
# Process the data
###############################################################################

# Select random sample of 10% of the data by serial 
temp_data <- raw_data %>% filter(SERIAL %in% sample(unique(SERIAL), max_rows))

# Input NA data for missing values
temp_data$INCTOT <- ifelse(temp_data$INCTOT == 9999999 | temp_data$INCTOT == 9999998, NA, temp_data$INCTOT)
temp_data$INCWEL <- ifelse(temp_data$INCWEL == 999999 | temp_data$INCWEL == 999998, NA, temp_data$INCWEL)
temp_data <- as.data.frame(temp_data)

# Create the dataset
ds <- temp_data %>% 
    group_by(YEAR, SERIAL) %>% 
    summarise(HHWT = first(HHWT),
                PERNUM = n(),
                INCTOT = sum(INCTOT, na.rm = TRUE),
                INCWEL = sum(INCWEL, na.rm = TRUE),
                INCPC = INCTOT / PERNUM,
                INCPCWEL = INCWEL / PERNUM)

# TODO: Inflation?

# Calculate the Lorenz curve
ds <- ds %>%
    group_by(YEAR) %>%
    arrange(INCPC) %>% 
    mutate(share = cumsum(HHWT) / sum(HHWT),
            lorenz = cumsum(INCPC * HHWT) / sum(INCPC * HHWT))

# Plot the Lorenz curve
ggplot(ds, aes(x = share, y = lorenz, color = factor(YEAR))) +
    geom_line(size = 1.5) +
    labs(title = "Curva de Lorenz",
         x = "Fração da População",
         y = "Fração da Renda Acumulada",
         color = "Ano") +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm"),
         legend.position = "bottom",
         legend.text = element_text(size = 15),
         legend.title = element_text(size = 16),
         legend.key.size = unit(0.7, "cm"),
         legend.background = element_rect(color = "black", size = 0.5)) +
    scale_color_manual(values = c("1970" = "#1E88E5", "2000" = "#D81B60"))


View(ds[ds$YEAR == 1970,])

temp_data[temp_data$SERIAL == 75417000,]
