library(tidyverse)
library(dplyr)

# Read the data
gem <- read.table("twins.txt", header = TRUE, sep = ",")

# Define columns to clean
filters <- c("DLHRWAGE", "AGE", "DEDUC1", "AGESQ", "HRWAGEH", "WHITEH", 
             "MALEH", "EDUCH", "HRWAGEL", "WHITEL", "MALEL", "EDUCL", 
             "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")

# Limpieza de datos
#Eliminar los datos que contengan valores vacíos
for (col in names(gem)) {
  gem <- gem %>% filter(!is.na(!!sym(col)) & (nchar(!!sym(col)) > 1 | !!sym(col) != "."))
}


