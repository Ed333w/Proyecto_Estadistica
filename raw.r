#== Pregunta 1 ===#

raw <- read.table("twins.txt", header = TRUE, sep = ",")

cat("Numero de registros (rows):", nrow(gem), "\n")
cat("Numero de variables (columns):", ncol(gem), "\n")

# Number of registros con al menos un  dato faltante
records_with_missing <- gem %>% filter(rowSums(is.na(.)) > 0) %>% nrow()

# Number of registros completos
complete_records <- gem %>% filter(rowSums(is.na(.)) == 0) %>% nrow()

# Display 
#Registros con al menos un dato faltante = 187- 147 = 40
cat("Number of records with complete information:", complete_records, "\n")
