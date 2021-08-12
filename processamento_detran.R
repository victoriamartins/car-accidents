library(data.table)
library(magrittr)
library(dplyr)

base <- fread("datatran2020.csv") 

base$km <- round(base$km, 0)

rodovia <- base %>% subset(base$uf == 'PR' & base$br == 376) 
contagem <- rodovia %>% count(km)

conta_km <- function(lista, pos, limite, km_acidentado, n_acidentes) {
  if (pos > limite) print(km_acidentado)
  
  else{
    km <- lista$km[pos]
    n <- lista$n[pos]

    if (n > n_acidentes) conta_km(lista, pos+1, limite, km, n)
    else conta_km(lista, pos+1, limite, km_acidentado, n_acidentes)
  }
}

conta_km(contagem, 1, length(contagem$n), 0, 0)