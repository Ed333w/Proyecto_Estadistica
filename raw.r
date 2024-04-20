gem <- read.table("twins.txt", header = TRUE, sep = ",")

cat("Number of records (rows):", nrow(gem), "\n")
cat("Number of variables (columns):", ncol(gem), "\n")