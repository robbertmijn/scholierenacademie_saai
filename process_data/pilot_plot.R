library(data.table)
library(ggplot2)
library(plotly)
#https://pandoc.org/installing.html

klas_id <- "pilot"

### Laad databade, laad data van de klas
load("pilot_dat.rdata")
load(paste0("pilot_dat/alldat_", klas_id, ".rdata"))

### Definieer functies om per taak de benodigde plots te maken
### 1 s taak krijgt 1 plot met 3 violins (3 niveaus)
make_plots_1_sec <- function(dat_1_sec, sumdat_1_sec){
  
  plots_1_sec <- list()
  ### Vat alle klassen samen tot plotbare tabel
  landdat <- sumdat_1_sec[, .(rt ,
                              sd ,
                              niveau = "Heel Nederland", 
                              id = paste0(k_id, "_", ll_id))]
  ### Vat alle leerlingen samen tot plotbare tabel
  klasdat <- dat_1_sec[, .(rt = mean(rt, na.rm = T),
                           sd = sd(rt, na.rm = T),
                           niveau = "Jouw Klas"), by = ll_id]
  setnames(klasdat, "ll_id", "id")

  ### Maak voor elke leerling een drie-delige plot
  for(ll in unique(dat_1_sec$ll_id)){
    ### isoleer data van de leerling
    lldat <- dat_1_sec[ll_id == ll, .(rt, sd = 0, niveau = "Jouw Data", id = ll_id, outlier)]
    captionText <- ifelse(lldat[, sum(outlier)] == 0, "", 
                          paste0("Je was ", lldat[, sum(outlier)], " keer later dan 2 seconden."))
    
    plotdat <- rbind(landdat, klasdat, lldat, fill = T)
    plotdat$niveau <- factor(plotdat$niveau, levels = c("Heel Nederland", "Jouw Klas", "Jouw Data"))
    
    plots_1_sec[[as.character(ll)]] <-
      ggplot(plotdat) +
      geom_violin(aes(x = 1, y = rt), alpha = .5, draw_quantiles = c(.5), scale = "count") +
      geom_jitter(data = plotdat[niveau == "Jouw Data"], 
                  aes(x = 1,y = rt), width = .1, size = .6) +
      scale_y_continuous(limits = c(0,2000)) +
      labs(x = NULL, y = "Reactietijd (milliseconden)", title = paste0("Jouw ID: ", ll, ", Jouw Klas ID: ", klas_id), caption = captionText) +
      geom_hline(yintercept = 1000, linetype = 2) +
      theme_classic() +
      facet_wrap(vars(niveau)) +
      theme(text = element_text(size = 25),
            legend.position = "none", 
            axis.text.x = element_blank(),
            axis.line = element_blank(),
            axis.ticks.x = element_blank())
  }
  return(plots_1_sec)
}
make_plots_risk <- function(dat_risk, sumdat_risk){
  
  plots_risk <- list()
  
  ### Vat alle klassen samen tot plotbare tabel
  landdat <- sumdat_risk[, .(rt = mean(rt, na.rm = T),
                             sd = sd(rt, na.rm = T),
                             niveau = "Heel Nederland"), by = list(punish, ll_id)]
  setnames(landdat, "ll_id", "id")
  
  ### Maak voor elke leerling een drie-delige plot
  for(ll in unique(dat_risk$ll_id)){
    ### isoleer data van de leerling
    lldat <- dat_risk[ll_id == ll, .(rt, sd = 0, punish = as.factor(punish), niveau = "Jouw Data", id = ll_id, outlier)]
    captionText <- ifelse(lldat[, sum(outlier)] == 0, "", 
                          paste0("Je was ", lldat[, sum(outlier)], " keer later dan 2 seconden."))
    plotdat <- rbind(landdat, lldat, fill = T)
    plots_risk[[as.character(ll)]] <-
      ggplot(plotdat) +
      geom_violin(aes(x = punish, y = rt, fill = punish), alpha = .5, draw_quantiles = c(.5), scale = "count") +
      geom_jitter(data = plotdat[niveau == "Jouw Data"], aes(x = punish, y = rt, color = punish), size = .6, width = .1) +
      scale_y_continuous(limits = c(0,2000)) +
      scale_color_manual(values = c("darkgreen", "orange", "red")) +
      scale_fill_manual(values = c("darkgreen", "orange", "red")) +
      labs(x = "Strafpunten voor te vroeg zijn", y = "Reactietijd (milliseconden)", 
           title = paste0("Jouw ID: ", ll, ", Jouw Klas ID: ", klas_id), caption = captionText) +
      geom_hline(yintercept = 850, linetype = 2, color = "red") +
      geom_hline(yintercept = 1150, linetype = 2, color = "gray") +
      facet_wrap(vars(niveau)) +
      theme_classic() +
      theme(text = element_text(size = 25),
            axis.line = element_blank(),
            legend.position = "none")
    
  }
  return(plots_risk)
}
make_plots_context <- function(dat_context, sumdat_context){
  
  plots_context <- list()
  
  ### Vat alle klassen samen tot plotbare tabel
  landdat <- sumdat_context[exp_dur == 900, .(rt = mean(rt, na.rm = T),
                                niveau = "Heel Nederland"), by = list(k_id, context)]
  setnames(landdat, "k_id", "id")
  
  ### Maak voor elke leerling een twee-delige plot
  for(ll in unique(dat_context$ll_id)){
    captionText <- ifelse(dat_context[ll_id == ll, sum(outlier)] == 0, "", 
                          paste0("Je was ", dat_context[ll_id == ll, sum(outlier)], " keer later dan 2 seconden."))
    
    ### isoleer data van de leerling
    lldat <- dat_context[ll_id == ll & exp_dur == 900, .(id = ll_id, context, rt, niveau = "Jouw Data", outlier)]
    
    plotdat <- rbind(landdat, lldat, fill = T)
    
    plots_context[[as.character(ll)]] <-
      ggplot(plotdat) +
      geom_violin(aes(x = context, y = rt), alpha = .5, draw_quantiles = c(.5), scale = "count") +
      geom_jitter(aes(x = context, y = rt), width = .1, size = .6) +
      scale_y_continuous(limits = c(0,2000)) +
      labs(x = "Context", y = "Reactietijd (milliseconden)", title = paste0("Jouw ID: ", ll, ", Jouw Klas ID: ", klas_id)) +
      geom_hline(yintercept = 1000, linetype = 2) +
      theme_classic() +
      facet_wrap(vars(niveau)) +
      theme(text = element_text(size = 25),
            legend.position = "none", 
            axis.line = element_blank(),
            axis.ticks.x = element_blank())
    
  }
  return(plots_context)
}

### Return list met ggplot objecten per leerling
plots_1_sec <- make_plots_1_sec(dat_1_sec, sumdat_1_sec)
plots_risk <- make_plots_risk(dat_risk, sumdat_risk)
plots_context <- make_plots_context(dat_context, sumdat_context)

### per leerling plots naar pdf schrijven
dir.create(paste0("output/klas_", klas_id))
for(ll in unique(dat_risk$ll_id)){
  
  rmarkdown::render("knit_html.Rmd", output_file =  paste0("output/klas_", klas_id, "/plot_", ll, ".html"),
                    params = list(sec = plots_1_sec[[as.character(ll)]], 
                                  risk = plots_risk[[as.character(ll)]],
                                  context = plots_context[[as.character(ll)]]
                                  )
                    )
} 
