### Script om data van de pilot (DemoDat) te verwerken tot 1 .rdata bestand
### Gebruik in terminal: > Rscript pilot_process.R 
library(data.table)
source("process_functions.R")
klas_id = "pilot"

dat_1_sec <- pilot_1_sec()
dat_1_sec[, ll_id := as.numeric(as.factor(ll_id))]
dat_risk <- pilot_risk()
dat_risk[, ll_id := as.numeric(as.factor(ll_id))]
dat_context <- pilot_context()
dat_context[, ll_id := as.numeric(as.factor(ll_id))]

### Sla tabellen op als rdata in een folder van de klas
cat("\nSaving to ", paste0("pilot_dat/alldat_", klas_id, ".rdata\n"))
save(klas_id, dat_1_sec, dat_risk, dat_context, file = paste0("pilot_dat/alldat_", klas_id, ".rdata"))

### Laad landelijke data, maak lege database als er nog niks is
if(!file.exists("sim_dat.rdata")){
  cat("\nCreating new database\n")
  create_empty_database("sim_dat.rdata")
} else {
  cat("\nLoading existing database\n")
}
load("sim_dat.rdata")

### Voeg samengevatte klasdata toe aan geladen database
cat("\nAdding klas data to database\n")
sumdat_1_sec <- rbind(sumdat_1_sec, summ_1_sec(klas_id, dat_1_sec))
sumdat_risk <- rbind(sumdat_risk, summ_risk(klas_id, dat_risk))
sumdat_context <- rbind(sumdat_context, summ_context(klas_id, dat_context))

### Overschrijf land_data
cat("\nSaving database to file\n")
save(sumdat_1_sec, sumdat_risk, sumdat_context, file = "pilot_dat.rdata")

