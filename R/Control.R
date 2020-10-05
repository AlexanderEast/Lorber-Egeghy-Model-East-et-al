# PFOA / PFOS Aggregate Route Exposure Model
# AE, ORAU, 2020.






LEM.run<-function(){
# ______________________________  Source, Name, & Tidy  ______________________________ #

input <<- "./input/Input 10052020.xlsx"
mysheet <<- "Data 10052020"
source('./R/Packages.R')
source('./R/Common.R')
source('./R/Food.R')
suppressWarnings(source('./R/Media.R'))

exposure.distributions<<- Map(c,exposures,foodexposures)


# ______________________________  Number of Datasets Used  ______________________________ #


names(foodroutes)<-names(routes)
routes<-rbind(routes,foodroutes)
rownames(routes)<- NULL


# ______________________________  Summary Statistics ______________________________ #

for.summary <- function(x){

x<-data.frame(x)
colnames(x)<- str_replace_all(colnames(x),"[[:punct:]]"," ")

sumstats <- t(sapply(x,quantile,c(0,.10,.5,.75,.95,1)))
mymean   <- sapply(x,mean)
sumstats <- cbind(mymean,sumstats)
colnames(sumstats)<-c("Mean","Min","10th%","Median","75th%","95th","Max")

sumstats<- signif(sumstats,5)
rn <- rownames(sumstats)

y<-sumstats[str_detect(rownames(sumstats),c("PFOA Typical")),]
z<-sumstats[str_detect(rownames(sumstats),c("PFOS Typical")),]
a<-sumstats[str_detect(rownames(sumstats),c("PFOA Contaminated")),]
b<-sumstats[str_detect(rownames(sumstats),c("PFOS Contaminated")),]
result<- data.frame(rbind(y,z,a,b))
colnames(result)<-c("Mean","Min","10th %","Median","75th %","95th %","Max")


# Chemical
result$Chemical[str_detect(rownames(result),"PFOA")]<- "PFOA"
result$Chemical[str_detect(rownames(result),"PFOS")]<- "PFOS"

# Scenario
result$Scenario[str_detect(rownames(result),"Typical")]<- "Typical"
result$Scenario[str_detect(rownames(result),"Contaminated")]<- "Contaminated"

# Route
result$Route[str_detect(rownames(result),"Indoor Air")]     <- "Indoor Inhalation"
result$Route[str_detect(rownames(result),"Outdoor Air")]    <- "Outdoor Inhalation"
result$Route[str_detect(rownames(result),"Water")]          <- "Water Ingestion"
result$Route[str_detect(rownames(result),"Soil")]           <- "Soil Ingestion"
result$Route[str_detect(rownames(result),"Dermal Dust")]    <- "Dust Dermal"
result$Route[str_detect(rownames(result),"Ingestion Dust")] <- "Dust Ingestion"
result$Route[str_detect(rownames(result),"Dietary")]        <- "Dietary Ingestion"


result<- result[,c("Chemical","Route","Scenario","Mean","Min","10th %","Median","75th %","95th %","Max")]
rownames(result) <- NULL

return(result)
}

summary<- bind_rows(lapply(exposure.distributions,for.summary), .id = "Group")


# ______________________________  Tidy Generated Data  ______________________________ #

for.boxplot<-function(x){

x<-data.frame(x)
colnames(x)<- str_replace_all(colnames(x),"[[:punct:]]"," ")
x<- data.frame(do.call(c,x))

# Chemical
x$Chemical[str_detect(rownames(x),"PFOA")]<- "PFOA"
x$Chemical[str_detect(rownames(x),"PFOS")]<- "PFOS"

# Scenario
x$Scenario[str_detect(rownames(x),"Typical")]<- "Typical"
x$Scenario[str_detect(rownames(x),"Contaminated")]<- "Contaminated"

# Route
x$Route[str_detect(rownames(x),"Indoor Air")]     <- "Indoor Inhalation"
x$Route[str_detect(rownames(x),"Outdoor Air")]    <- "Outdoor Inhalation"
x$Route[str_detect(rownames(x),"Water")]          <- "Water Ingestion"
x$Route[str_detect(rownames(x),"Soil")]           <- "Soil Ingestion"
x$Route[str_detect(rownames(x),"Dermal Dust")]    <- "Dermal Dust"
x$Route[str_detect(rownames(x),"Ingestion Dust")] <- "Dust Ingestion"
x$Route[str_detect(rownames(x),"Dietary")]        <- "Dietary Ingestion"

names(x)[1] <- "Intake"
rownames(x) <- NULL
x<- x[,c("Chemical","Route","Scenario","Intake")]
x$Units<- "ng/day"
return(x)
}

distdata <-bind_rows(lapply(exposure.distributions,for.boxplot), .id = "Group")


# ______________________________  Sum Median PFOA/PFOS  ______________________________ #

for.medians<-function(x){

x<- data.frame(x)
colnames(x)<- str_replace_all(colnames(x),"[[:punct:]]"," ")

lapply(x[(str_detect(colnames(x),"PFOA Typical")),],median)

MedianPFOA <- sum(sapply(x[(str_detect(colnames(x),"PFOA Typical"))],median))
MedianPFOS <- sum(sapply(x[(str_detect(colnames(x),"PFOS Typical"))],median))

medians <-data.frame(c(MedianPFOA),c(MedianPFOS))
colnames(medians)<- c("Median PFOA ng/day"," Median PFOS ng/day")
return(medians)
}

medians<- bind_rows(lapply(exposure.distributions,for.medians),.id = "Group")


# ______________________________  PK Model  ______________________________ #

CP.PK <- function(x){


individual <- x$Individual
wt <- x$`Bodyweight (kg)`

agemedians <- medians[(str_detect(individual,medians$Group)),]

dosefactors <- read_excel(input, sheet = 'Dose Factors', guess_max = 17000)


dosefactors$SDF<- dosefactors$`Vd (Volume Distribution, ml/kg bw)`*
                  dosefactors$`kP (Elimination Rate, day -1)`

PFOA_CP <- agemedians$`Median PFOA ng/day`/ dosefactors$SDF[(dosefactors$Chemical == "PFOA")]/wt
PFOS_CP <- agemedians$`Median PFOA ng/day`/ dosefactors$SDF[(dosefactors$Chemical == "PFOS")]/wt

results<- data.frame(PFOA_CP,PFOS_CP)
names(results)<- c("PFOA Blood ng/mL", "PFOS Blood ng/mL" )

return(results)
}

PK<- bind_rows(lapply(individuals,CP.PK),.id="Group")

Medians.PK <- cbind(medians, PK[c(2,3)])
rm(medians,PK)


# ______________________________  Export Results ______________________________ #

results<-list(Medians.PK,summary,data,routes,distdata)
names(results)<- c("Daily Intake and Dose","Summary Statistics","Input Data Used","Input Summary","Raw Generated Data")


date<-Sys.Date()
date<- str_replace_all(date,"[[:punct:]]","")

time<- str_replace_all(Sys.time(),"[[:punct:]]","")
time<- word(time,-1)
time<- substr(time, 1, nchar(time)-2)

if (time > 1159){
  time <- as.numeric(time) - 1200
  time <- str_c(substr(time,1,2),"",substr(time,3,4),"PM")
} else {
  time <- str_c(substr(time,1,2),"",substr(time,3,4),"AM")
}

key<- str_c(date," ",time)


filename <- str_c("./output/PFOA PFOS Intake Results ",key,".xlsx")


export(results,filename)

return(results)
}



results<- LEM.run()
rm(list=setdiff(ls(), c("results")))