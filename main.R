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
load.lib <- c("dplyr", "ipumsr", "ggplot2", "splines", "stargazer", "Hmisc", "AER","readxl", "tidyverse", "data.table", "stargazer", "lubridate", "fixest", "ggplot2", "pracma", "dplyr", "remotes", "tidyr", "mvProbit", "ipw", "MASS", "xtable", "quantreg", "nprobust", "chron", "WDI")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)
remove(install.lib, lib, load.lib)

# Set the seed
set.seed(20250325)

# Set the maximum number of rows 
#max_rows <- 10000

# Import functions
source("functions.R")

###############################################################################
# Creating the dataset
###############################################################################

# Import the data
data_list <- read_ipums_ddi("data/ipumsi_00017.xml")
raw_data <- read_ipums_micro(data_list)

raw_data <- raw_data %>% filter(YEAR == 1990 | YEAR == 2020)

YEARS <- unique(raw_data$YEAR)
YEARS <- sort(YEARS)

# Fetch inflation data for Mexico
inflation_data <- WDI(country = "MX", indicator = "FP.CPI.TOTL", start = min(YEARS), end = max(YEARS))
inflation_rate <- rep(0, length(YEARS))
for(i in 1:(length(YEARS))) {
    inflation_rate[i] <- inflation_data[inflation_data$year==max(YEARS), ]$FP.CPI.TOTL / inflation_data[inflation_data$year==(YEARS[i]), ]$FP.CPI.TOTL - 1
}

print(inflation_rate)

###############################################################################
# Process the data
###############################################################################

# Sampled data
temp_data <- raw_data # %>% group_by(YEAR) %>% filter(SERIAL %in% sample(unique(SERIAL), max_rows, replace = FALSE))

# Input NA data for missing values
temp_data$INCEARN <- ifelse(temp_data$INCEARN == 99999999 | temp_data$INCEARN == 99999998 , NA, temp_data$INCEARN)
temp_data$LIT <- ifelse(temp_data$LIT == 9 | temp_data$LIT == 0, NA, temp_data$LIT)
temp_data$LIT <- ifelse(temp_data$LIT == 2, 1, 0)
temp_data$YRSCHOOL <- ifelse(temp_data$YRSCHOOL >= 90, NA, temp_data$YRSCHOOL)
temp_data$EMPSTAT <- ifelse(temp_data$EMPSTAT == 0 | temp_data$EMPSTAT == 3 | temp_data$EMPSTAT == 9, NA, temp_data$EMPSTAT)
temp_data$EMPSTAT <- ifelse(temp_data$EMPSTAT == 1, 1, 0)

# Create the dataset
temp_data <- as.data.frame(temp_data)
# INCTOT: Total earned income
# LIT: Literacy status (1 = literate, 0 = illiterate)
# YRSCHOOL: Years of schooling 
# EMPSTAT: Employment status (1 = employed, 0 = unemployed)
# HHWT: Household weight
# PERNUM: Number of people in the household
# SERIAL: Household identifier

# Inflate the income from and consider the currency change from 1000 pesos to 1 new peso
for(i in 1:(length(YEARS))) {
    temp_data <- temp_data %>% 
        mutate(INCEARN = ifelse(YEAR == YEARS[i], INCEARN * (1+inflation_rate[i])/ifelse(YEARS[i]<=1993,1000,1), INCEARN))
}

# Create the dataset
ds <- temp_data %>% 
    group_by(YEAR, SERIAL) %>% 
    summarise(HHWT = first(HHWT),
                PERNUM = n(),
                INCEARN = sum(INCEARN, na.rm = TRUE),
                INCPC = INCEARN / PERNUM,
                LIT = mean(LIT, na.rm = TRUE),
                YRSCHOOL = mean(YRSCHOOL, na.rm = TRUE),
                EMPSTAT = mean(EMPSTAT, na.rm = TRUE))

# Calculate the Lorenz curve
ds <- ds %>%
    group_by(YEAR) %>%
    arrange(INCPC) %>% 
    mutate(SHARE = cumsum(HHWT) / sum(HHWT),
            LORENZ = cumsum(INCPC * HHWT) / sum(INCPC * HHWT))

###############################################################################
# Descriptive Statistics
###############################################################################

# Calculate the mean and standard deviation of the income per capita and welfare benefits income per capita
descriptive_stat <- ds %>% 
    group_by(YEAR) %>% 
    summarise(INCPC = weighted.mean(INCPC, HHWT, na.rm = TRUE),
                YRSCHOOL = weighted.mean(YRSCHOOL, HHWT, na.rm = TRUE),
                LIT = weighted.mean(LIT, HHWT, na.rm = TRUE),
                EMPSTAT = 1-weighted.mean(EMPSTAT, HHWT, na.rm = TRUE),
                PERNUM = weighted.mean(PERNUM, HHWT, na.rm = TRUE),
                HHNUM = n())
                
# Create a dataframe with household income per capita, household welfare income per capita, number of households, average size of households 
table_stat <- as.data.frame(matrix(nrow = 6, ncol = length(YEARS)+1))
colnames(table_stat) <- c("Variável", YEARS)
table_stat$Variável <- c("Renda domiciliar per capita", "Anos de Escolaridade", "Taxa de Alfabetização", "Taxa de Desemprego", "Número de Pessoas", "Número de Domicílios")

for(i in 1:6) {
    for(j in 1:length(YEARS)) {
        table_stat[i, j+1] <- descriptive_stat[j, i+1]
    }
}

# Export the table using stargazer as latex code
stargazer(table_stat, type = "latex", title = "Estatísticas Descritivas", summary = FALSE, header = FALSE, digits = 3, out = "tables/descriptive_statistics.tex", rownames = FALSE, align = TRUE)

###############################################################################
# Poverty and Inequality Measures
###############################################################################

# Calculate the Gini coefficient and the 75-25 ratio
measures_inequality <- ds %>% 
    group_by(YEAR) %>% 
    summarise(gini = gini(INCPC, HHWT),
               ratio = ratio(INCPC, HHWT, 0.75))

# Calculate the headcount ratio, poverty gap, poverty severity for different poverty lines 

poverty_lines <- c(100, 500, 1000)
measures_poverty <- ds %>% 
    group_by(YEAR) %>% 
    summarise(headcount_ratio_1 = P_alpha(INCPC, HHWT, 0, poverty_lines[1]),
               headcount_ratio_2 = P_alpha(INCPC, HHWT, 0, poverty_lines[2]),
               headcount_ratio_3 = P_alpha(INCPC, HHWT, 0, poverty_lines[3]),
               poverty_gap_1 = P_alpha(INCPC, HHWT, 1, poverty_lines[1]),
               poverty_gap_2 = P_alpha(INCPC, HHWT, 1, poverty_lines[2]),
               poverty_gap_3 = P_alpha(INCPC, HHWT, 1, poverty_lines[3]),
               poverty_severity_1 = P_alpha(INCPC, HHWT, 2, poverty_lines[1]),
               poverty_severity_2 = P_alpha(INCPC, HHWT, 2, poverty_lines[2]),
               poverty_severity_3 = P_alpha(INCPC, HHWT, 2, poverty_lines[3]))

# Create a dataframe with the inequality and poverty measures
table_measures <- as.data.frame(matrix(nrow = 16, ncol = length(YEARS)+1))
colnames(table_measures) <- c("Variável", YEARS)

table_measures[1,] <- c("Desigualdade", rep("", length(YEARS)))
table_measures[2,] <- c("Gini", as.numeric(measures_inequality$gini))
table_measures[3,] <- c("Razão 75-25", as.numeric(measures_inequality$ratio))

table_measures[4,] <- c("Pobreza", rep("", length(YEARS)))
table_measures[5,] <- c("Taxa de Pobreza", rep("", length(YEARS)))
table_measures[6,] <- c(poverty_lines[1], as.numeric(measures_poverty$headcount_ratio_1))
table_measures[7,] <- c(poverty_lines[2], as.numeric(measures_poverty$headcount_ratio_2))
table_measures[8,] <- c(poverty_lines[3], as.numeric(measures_poverty$headcount_ratio_3))
table_measures[9,] <- c("Hiato de Pobreza", rep("", length(YEARS)))
table_measures[10,] <- c(poverty_lines[1], as.numeric(measures_poverty$poverty_gap_1))
table_measures[11,] <- c(poverty_lines[2], as.numeric(measures_poverty$poverty_gap_2))
table_measures[12,] <- c(poverty_lines[3], as.numeric(measures_poverty$poverty_gap_3))
table_measures[13,] <- c("Severidade da Pobreza", rep("", length(YEARS)))
table_measures[14,] <- c(poverty_lines[1], as.numeric(measures_poverty$poverty_severity_1))
table_measures[15,] <- c(poverty_lines[2], as.numeric(measures_poverty$poverty_severity_2))
table_measures[16,] <- c(poverty_lines[3], as.numeric(measures_poverty$poverty_severity_3))

# Export the table using stargazer as latex code
stargazer(table_measures, type = "latex", title = "Medidas de Desigualdade e Pobreza", summary = FALSE, header = FALSE, digits = 2, out = "tables/measures_poverty_inequality.tex", rownames = FALSE, align = TRUE)

###############################################################################
# Plotting the curves
###############################################################################

# Plot the Lorenz curve
ggplot(ds, aes(x = SHARE, y = LORENZ, color = factor(YEAR))) +
    geom_line(size = 1.5) +
    scale_color_manual(values = c("1990" = "#1E88E5", "2020" = "#D81B60", "Igualdade Perfeita" = "black")) +
    labs(x = "Fração da População",
         y = "Fração da Renda Acumulada",
         color = "Ano") +
    theme_bw(base_size = 25) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm"),
         legend.position = "bottom",
         legend.text = element_text(size = 15),
         legend.title = element_text(size = 16),
         legend.key.size = unit(1, "cm"),
         legend.background = element_rect(color = "black", size = 0.5)) +
    coord_cartesian(xlim = c(0.04, 0.96), ylim = c(0.04, 0.96))
ggsave("figures/lorenz_curve.png", width = 10, height = 10)

# Plot the income distribution 
ggplot(ds, aes(x = SHARE, y = INCPC, color = factor(YEAR))) +
    geom_line(size = 1.5) +
    scale_color_manual(values = c("1990" = "#1E88E5", "2020" = "#D81B60")) +
    labs(x = "Fração da População",
         y = "Renda",
         color = "Ano") +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm"),
         legend.position = "bottom",
         legend.text = element_text(size = 15),
         legend.title = element_text(size = 16),
         legend.key.size = unit(1, "cm"),
         legend.background = element_rect(color = "black", size = 0.5)) +
    coord_cartesian(xlim = c(0.04, 0.96))
ggsave("figures/income_distribution.png", width = 10, height = 10)

# Plot the log income distribution, accounting for the 0 values
 
ggplot(ds, aes(x = SHARE, y = log(INCPC + 1, base = 10), color = factor(YEAR))) +
    geom_line(size = 1.5) +
    scale_color_manual(values = c("1990" = "#1E88E5", "2020" = "#D81B60")) +
    labs(x = "Fração da População",
         y = "Renda (log10)",
         color = "Ano") +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm"),
         legend.position = "bottom",
         legend.text = element_text(size = 15),
         legend.title = element_text(size = 16),
         legend.key.size = unit(1, "cm"),
         legend.background = element_rect(color = "black", size = 0.5)) +
    coord_cartesian(xlim = c(0.04, 0.96))
ggsave("figures/income_distribution_log.png", width = 10, height = 10)
