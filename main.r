library(tidyverse)
library(dplyr)

#== Pregunta  1
# Lectura de datos
gem <- read.table("twins.txt", header = T, sep = ",", dec=".", na.strings = ".")

# Deifnir la filatración por variables
filters <- c("DLHRWAGE", "AGE", "DEDUC1", "AGESQ", "HRWAGEH", "WHITEH", 
             "MALEH", "EDUCH", "HRWAGEL", "WHITEL", "MALEL", "EDUCL", 
             "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")

# Limpieza de datos
#Eliminar los datos que contengan valores vacíos
filtered_data <- gem %>%
  filter(across(all_of(filters), ~(!is.na(.) & (nchar(.) > 1 | . != "."))))

# Save filtered data to a CSV file
write.csv(filtered_data, file = "filtered_data.csv", row.names = FALSE)











