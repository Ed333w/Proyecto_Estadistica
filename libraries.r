
#== Instalación de paquetes para la lecutra de datos en R usando "tidyverse" como el paquete principal
install.packages("tidyverse")
install.packages("dplyr")
install.packages("modeest")
install.packages("psych")
install.packages("https://cran.r-project.org/src/contrib/Archive/fdth/fdth_1.2-6.tar.gz", repo=NULL, type="source")
install.packages('rsconnect')
library(rsconnect)
library(tidyverse)
library(dplyr)
library(modeest)
library(psych)

#=== Deployment
library(rsconnect)

# Path to the directory containing your app.R
app_dir <- "C:/Users/POWER/Documents/Proyecto_Estadistica"

# Deploy the app
rsconnect::deployApp(app_dir)

rsconnect::setAccountInfo(name='hzjjqj-juan0david-serna0murcia',
                          token='7383198E899765720374478D4ED99B8E',
                          secret='pNjoJN2a9RjtJNhBsBwndaweRlTGqQ2dz61lM/iQ')


