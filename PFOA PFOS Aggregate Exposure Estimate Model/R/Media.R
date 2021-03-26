# Non-Food Concentration to Intake Estimates for EPA's Lorber-Egeghy Model
# AE, ORAU, 2020.

# ______________________________ Import ______________________________ #

data <- read_excel(input, sheet = mysheet, guess_max = 17000)

data$Media_Type<- tolower(data$Media_Type)
data<- data[!str_detect(data$Media_Type, "effluent|groundwater|ground|sediment"),]

# ______________________________ Units ______________________________ #

data<- data %>%mutate(UNITFACTOR = case_when(
  (Units %in% c("ng/m³","ng/L","ng/g","µg/kg","ug/kg","pg/mL","pg/ml")) ~ 1,
  (Units %in% c("pg/m³","pg/g")) ~ 0.001,
  (Units %in% c("ng/mL","ug/l","µg/L","ug/m³","µg/m³")) ~ 1000)) %>%
  mutate_at(c("Min","Max","Median","Mean","SD","GM","GSD","P10","P25","P75","P90","P95","P99"),~.*UNITFACTOR) %>%
  mutate(Units = case_when(
    (Units %in% c("ug/m3","µg/m³","pg/m³","ng/m³")) ~ "ng/m³",
    (Units %in% c("ng/mL","ug/l","ug/L","µg/l","µg/L","pg/ml","pg/mL","ng/L")) ~ "ng/L",
    (Units %in% c("pg/g","µg/kg","ug/kg","ng/g")) ~ "ng/g")) %>%
  select(-UNITFACTOR)

# ______________________________ Estimate GM/GSD ______________________________ #

numerics <- data[c("Sample_Size","Min","Max","Median","Mean","SD","GM","GSD","P10","P25","P75","P90","P95","P99")]

# A. Estimate GM using Pleil 1.
numerics <- numerics %>% mutate(GM = if_else(!is.na(GM),GM , Median))

# B. Estimate GM using Pleil 2.
numerics <- numerics %>% mutate(GM = if_else(!is.na(GM),GM , Mean/(1+0.5 *(SD/Mean)^2)))

# C. Estimate GSD using Pleil 1.
numerics <- numerics %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P10/GM)/qnorm(.10)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P25/GM)/qnorm(.25)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P75/GM)/qnorm(.75)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P90/GM)/qnorm(.90)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P95/GM)/qnorm(.95)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P99/GM)/qnorm(.99))))

# D. Estimate GSD using Pleil 2.
numerics <- numerics %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(sqrt(2 * log(Mean/GM)))))
# E. Estimate GM using Pleil 3.
numerics <- numerics %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Max/GM)/qnorm(1-1/Sample_Size))))
numerics <- numerics %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Min/GM)/qnorm(1/Sample_Size))))

# F. Estimate Mean and SD using "Estimating.xlsx" Methods (5) and (16). Mean calculated from min, median, maximum,
# and SD from minimum, median, maximum, and range.

numerics <- numerics %>% mutate(Mean = if_else(!is.na(Mean),Mean, (Min+2*Median+Max)/4))
numerics <- numerics %>% mutate(SD = if_else(!is.na(SD),SD, sqrt ((1/12) * ((Min-2*Median+Max)^2)/4 + (Max-Min)^2)))


# G. Estimate SD using Ramirez & Cox Method and range rule. Applied only if sample size  > 10.
numerics <- numerics %>% mutate(SD = if_else((!is.na(SD) & Sample_Size > 10),SD, (Max-Min)/ (3*sqrt(log(Sample_Size))-1.5)))
numerics <- numerics %>% mutate(SD = if_else((!is.na(SD) & Sample_Size > 10),SD, (Max-Min)/4))

# ______________________________ Repeat A - E. ______________________________ #


# H. Estimate GM using Pleil 1.
numerics <- numerics %>% mutate(GM = if_else(!is.na(GM),GM , Median))

# I. Estimate GM using Pleil 2.
numerics <- numerics %>% mutate(GM = if_else(!is.na(GM),GM , Mean/(1+0.5 *(SD/Mean)^2)))

# J. Estimate GSD using Pleil 1.
numerics <- numerics %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P10/GM)/qnorm(.10)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P25/GM)/qnorm(.25)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P75/GM)/qnorm(.75)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P90/GM)/qnorm(.90)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P95/GM)/qnorm(.95)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P99/GM)/qnorm(.99))))

# K. Estimate GSD using Pleil 2.
numerics <- numerics %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(sqrt(2 * log(Mean/GM)))))

# L. Estimate GM using Pleil 3.
numerics <- numerics %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Max/GM)/qnorm(1-1/Sample_Size))))
numerics <- numerics %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Min/GM)/qnorm(1/Sample_Size))))


# ______________________________ Tidy Data ______________________________ #


data[c("Sample_Size","Min","Max","Median","Mean","SD","GM","GSD","P10","P25","P75","P90","P95","P99")]<- numerics
data$Chemical[str_detect(data$Chemical,"PFOA")]<-"PFOA"
data$Chemical[str_detect(data$Chemical,"PFOS")]<-"PFOS"

data <- data %>% filter(complete.cases(Sample_Size,GM,GSD) &
                          Chemical %in% c("PFOS", "PFOA") &
                          Uncontaminated %in% 0:1) %>%
  dlply(.variables = c("Route","Chemical","Uncontaminated")) 

names(data)<- str_replace_all(names(data),c(".1" =" Typical", ".0" = " Contaminated",
                                            ".PFOA" = " PFOA", ".PFOS" = " PFOS"))
rm(numerics)


# ______________________________ Concentration to Exposure ______________________________ #

# 1. Import People 

source('./R/Common.R')
options(scipen = 1000)
individuals <- get.people()

# 2. Calculate Per Route/Media/Scenario WM/WSD

weights <- function(x){

if (nrow(x) == 1){

GM_WM<-x$GM
GM_WSD<-x$GSD

} else {
GM_WM  <- WM(x$GM,x$Sample_Size)
GM_WSD <- WM(x$GSD,x$Sample_Size)
}

y<- data.frame(GM_WM,GM_WSD)

return(y)
}

routes<-lapply(data,weights)


# 3. Create Exposure Factors 
# 3a. Create unique Exposure distributions from Concentration

individual.exposures <- function(z){
  
  n<-z$n
  
  # Dust Ingestion Factor
  factor_di <- (z$`Dust Ingestion Rate (g/day)`*
                  z$`Dust Ingestion AF`)
  
  # Dermal Dust Factor
  factor_dd <- (z$`Dermal Dust Load (g/m3)`*
                  z$`Dermal Dust Transfer Coefficient (m2/h)`*
                  z$`Dermal Dust Time (hr)`*
                  z$`Dermal Dust AF`)
  
  # Indoor Air Factor
  factor_ia <- (z$`Inhalation Rate (m3/day)`*
                  z$`Fraction Time Indoors (h/day)`*
                  z$`Inhalation AF`)
  
  # Outdoor Air Factor
  factor_oa <-   (z$`Inhalation Rate (m3/day)`*
                    z$`Fraction Time Outdoors (h/day)`*
                    z$`Inhalation AF`)
  
  # Water Factor
  factor_w <- (z$`Water Intake (L/day)`*
                 z$`Water AF`)
  
  # Soil Factor
  factor_s <- (z$`Soil Ingestion (g/day)`*
                 z$`Soil AF`)
  
  
  
  get.distributions <- function(x,n){
    
    set.seed(as.numeric(read_excel(input, sheet = 'Seed')))
    
    concentration <- data.frame(rlnorm(n, log(x$GM_WM), abs(log(x$GM_WSD))))
    names(concentration)<- x[,1]
    
    return(concentration)
  }
  
  
  concentrations<- data.frame(lapply(routes,get.distributions,n))
  names <- names(concentrations)<- names(routes)
  
  
  # Inhal_inside
  Inhal_Indoor <- cbind(concentrations[str_detect(names,"Indoor")]*factor_ia)
  
  # Inhal_outside
  Inhal_Outdoor <- cbind(concentrations[str_detect(names,"Outdoor")]*factor_oa)
  
  # Dust Ingest
  Dust_Ingest <- cbind(concentrations[str_detect(names,"Dust")]*factor_di)
  names(Dust_Ingest)<- str_c("Ingestion ", names(Dust_Ingest))
  
  # Dust Absorb
  Dust_Dermal <- cbind(concentrations[str_detect(names,"Dust")]*factor_dd)
  names(Dust_Dermal)<- str_c("Dermal ", names(Dust_Dermal))
  
  # Soil Ingest
  Soil_Ingest <- cbind(concentrations[str_detect(names,"Soil")]*factor_s)
  
  # Drinking
  Drinking_Water <- cbind(concentrations[str_detect(names,"Water")]*factor_w)
  
  
  Exposures<- cbind(Inhal_Indoor,Inhal_Outdoor,Dust_Ingest,Dust_Dermal, Drinking_Water, Soil_Ingest)
  names(Exposures)<- str_c("Exposure to ", names(Exposures))
  
  
  
  return(Exposures)
}
exposures<-lapply(individuals,individual.exposures)


# ______________________________ # of Datasets Used ______________________________ #

numberofstudies<- sapply(data, nrow)
routes<-rbind.fill(routes)
routes<-cbind(numberofstudies,routes)
colnames(routes)<- c("Number of Datasets Used","WM","WSD")
routes$Scenario<- rownames(routes)
routes<-routes[c("Scenario","Number of Datasets Used","WM","WSD")]
data<- rbind.fill(data)
rm(numberofstudies)

routes <- routes %>% mutate(Units = case_when(
  str_detect(Scenario, "Water") ~"ng/L",
  str_detect(Scenario, "Dust|Soil" ) ~"ng/g",
  str_detect(Scenario,"Air") ~ "ng/m3"
  ))

routes<-routes[c(1,5,2,3,4)]

# ____________________________________________________________________________________________ #



