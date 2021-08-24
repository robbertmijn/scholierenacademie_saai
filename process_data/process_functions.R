# Helper functions

### Defineer functies voor inladen van ruwe data (JATOS  NOG NIET GEIMPLEMENTEERD)
process_1_sec <- function(klas_folder){
  dat_1_sec <- NULL
  for(f in Sys.glob(paste0(klas_folder, "1_sec/*.csv"))){
    # laad csv naar tabel (hier moet dus iets komen dat JSON kan inlezen)
    lldat <- fread(f)
    # gooi practice trials weg, behoud 2 kolommen: id en response time
    lldat <- lldat[practice == "no", .(ll_id = subject_nr, # haal leerling id uit filename
                                       rt = response_time_keyboard_resp)]
    #voeg rijen van leerling toe aan klastabel
    dat_1_sec <- rbind(dat_1_sec, lldat)
  }
  cat("\n1 s taak: ", length(unique(dat_1_sec$ll_id)), " leerlingen verwerkt")
  return(dat_1_sec)
}
process_risk <- function(klas_folder){
  dat_risk <- NULL
  for(f in Sys.glob(paste0(klas_folder, "risk/*.csv"))){
    
    # laad csv naar tabel (hier moet dus iets komen dat JSON kan inlezen)
    lldat <- fread(f)
    # gooi practice trials weg, behoud 2 kolommen: id en response time
    lldat <- lldat[practice == "no", .(ll_id, # haal leerling id uit filename
                                       rt = response_time_reproduction,
                                       punish,
                                       total_points)]
    
    #voeg rijen van leerling toe aan klastabel
    dat_risk <- rbind(dat_risk, lldat)
  }
  cat("\nrisk taak: ", length(unique(dat_risk$ll_id)), " leerlingen verwerkt")
  return(dat_risk)
}
process_context <- function(klas_folder){
  dat_context <- NULL
  
  # loop over alle files in de klas_folder
  for(f in Sys.glob(paste0(klas_folder, "context/*.csv"))){
    
    # laad csv naar tabel (hier moet dus iets komen dat JSON kan inlezen)
    lldat <- fread(f)
    
    # gooi practice trials weg, behoud 2 kolommen: id en response time
    lldat <- lldat[practice == "no", .(ll_id, # haal leerling id uit filename
                                       rt = response_time_reproduction,
                                       context,
                                       exp_dur)]
    
    #voeg rijen van leerling toe aan klastabel
    dat_context <- rbind(dat_context, lldat)
  }
  cat("\ncontext taak: ", length(unique(dat_context$ll_id)), " leerlingen verwerkt")
  return(dat_context)
}

sim_1_sec <- function(n_sub, n_trial){
  # Simuleer 1 klas. Elke leerling heeft een normaal verveeld gemiddelde en onafhankelijke normaal verdeeld sd
  dat_1_sec <- NULL
  for(ll in 1:n_sub){
    ll_mean <- round(abs(rnorm(1, 1000, 200)))
    ll_sd <- round(abs(rnorm(1, 100, 100)))
    lldat <- data.table(ll_id = as.character(ll),
                        rt = round(abs(rnorm(n_trial, ll_mean, ll_sd))))
    dat_1_sec <- rbind(dat_1_sec, lldat)
  }
  return(dat_1_sec)
}
sim_risk <- function(n_sub, n_trial){
  dat_risk <- NULL
  for(ll in 1:n_sub){
    ll_mean <- round(abs(rnorm(1, 1000, 200)))
    ll_sd <- round(abs(rnorm(1, 100, 100)))
    lldat <- data.table(ll_id = as.character(ll),
                        punish = c(rep(0, n_trial), rep(5, n_trial), rep(30, n_trial)), 
                        rt = c(round(abs(rnorm(n_trial, ll_mean, ll_sd))), 
                               round(abs(rnorm(n_trial, ll_mean, ll_sd))) + 50,
                               round(abs(rnorm(n_trial, ll_mean, ll_sd))) + 200))
    lldat[, total_points := cumsum(ifelse(rt < 850, -punish, 
                                          ifelse(rt > 1150, 0, 5)))]
    dat_risk <- rbind(dat_risk, lldat)
  }
  return(dat_risk)
}
sim_context <- function(n_sub, n_trial){
  dat_context <- NULL
  for(ll in 1:n_sub){
    ll_sd <- round(abs(rnorm(1, 100, 100)))
    lldat <- data.table(ll_id = as.character(ll),
                        context = c(rep("lang", n_trial*2), rep("kort", n_trial*2)), 
                        exp_dur = c(sample(c(rep(1300, n_trial), rep(900, n_trial))), 
                                    sample(c(rep(625, n_trial), rep(900, n_trial))))
    )
    lldat[exp_dur == 1300, rt := round(abs(rnorm(.N, exp_dur - 50, ll_sd)))]
    lldat[exp_dur == 625, rt := round(abs(rnorm(.N, exp_dur + 50, ll_sd)))]
    lldat[context == "lang" & exp_dur == 900, rt := round(abs(rnorm(.N, exp_dur + 50, ll_sd)))]
    lldat[context == "kort" & exp_dur == 900, rt := round(abs(rnorm(.N, exp_dur - 50, ll_sd)))]
    dat_context <- rbind(dat_context, lldat)
  }
  return(dat_context)
}

pilot_1_sec <- function(){
  dat_1_sec <- fread("DemoData/1sec.csv")
  # gooi practice trials weg, behoud 2 kolommen: id en response time
  dat_1_sec <- dat_1_sec[, .(ll_id = subject_nr, # haal leerling id uit filename
                             rt = response_time_keyboard_resp, 
                             outlier = ifelse(response_time_keyboard_resp > 2000, 1, 0))]
  
  cat("1 s taak: ", length(unique(dat_1_sec$ll_id)), " leerlingen verwerkt\n")
  return(dat_1_sec)
}
pilot_risk <- function(){
  dat_risk <- fread("DemoData/tur.csv")
  dat_risk[, ll_id := subject_nr]
  dat_risk[, trial := 1:.N, by = subject_nr]
  
  dat_risk <- dat_risk[trial > 10, .(ll_id, # haal leerling id uit filename
                                     rt = response_time_reproduction,
                                     punish,
                                     total_points,
                                     outlier = ifelse(response_time_reproduction > 2000, 1, 0))]
  return(dat_risk)
}
pilot_context <- function(){
  dat_context <- fread("DemoData/context.csv")
  # gooi practice trials weg, behoud 2 kolommen: id en response time
  dat_context <- dat_context[context != "0", .(ll_id = subject_nr, # haal leerling id uit filename
                                               rt = response_time_reproduction,
                                               context = ifelse(context == "short", "kort", "lang"),
                                               exp_dur = ifelse(context == "short" & exp_dur == "[short_dur]", 625,
                                                                ifelse(context == "long" & exp_dur == "[long_dur]", 1300,
                                                                       900)),
                                               outlier = ifelse(response_time_reproduction > 2000, 1, 0))]
  
  cat("\ncontext taak: ", length(unique(dat_context$ll_id)), " leerlingen verwerkt")
  return(dat_context)
}

summ_1_sec <- function(klas_id, dat_1_sec){
  # remove rt's that are over 10 sec
  # return table with one row per subject that holds all stats (eg. mean, sd, etc.)
  return(
    dat_1_sec[rt < 10000, 
              .(rt = mean(rt, na.rm = T),
                sd = sd(rt, na.rm = T),
                k_id = klas_id), by = ll_id]
  )
}
summ_risk <- function(klas_id, dat_risk){
  # remove rt's that are over 10 sec
  # return table with one row per subject that holds all stats (eg. mean, sd, etc.)
  final_score <- tail(dat_risk$total_points, 1)
  return(
    dat_risk[rt < 10000,
             .(rt = mean(rt, na.rm = T),
               sd = sd(rt, na.rm = T),
               k_id = klas_id, 
               final_score = final_score), by = list(punish, ll_id)]
  )
}
summ_context <- function(klas_id, dat_context){
  # remove rt's that are over 10 sec
  # return table with one row per subject that holds all stats (eg. mean, sd, etc.)
  return(
    dat_context[rt < 10000,
                .(rt = mean(rt, na.rm = T),
                  sd = sd(rt, na.rm = T),
                  k_id = klas_id), by = list(context, exp_dur, ll_id)]
  )
}

create_empty_database <- function(filename){
  sumdat_1_sec <- data.table(ll_id = as.integer(), 
                             rt = as.numeric(), 
                             sd = as.numeric(), 
                             k_id = as.character())
  sumdat_risk <- data.table(ll_id = as.integer(), 
                            rt = as.numeric(), 
                            sd = as.numeric(), 
                            punish = as.numeric(),
                            final_score = as.numeric(),
                            k_id = as.character())
  sumdat_context <- data.table(ll_id = as.integer(), 
                               rt = as.numeric(), 
                               sd = as.numeric(), 
                               exp_dur = as.numeric(),
                               context = as.character(),
                               k_id = as.character())
  save(sumdat_1_sec, sumdat_risk, sumdat_context, file = filename)
}
