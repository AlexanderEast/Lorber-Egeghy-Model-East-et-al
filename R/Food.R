# EFSA Food Intake to Lorber-Egeghy Model Intake Estimation
# AE, ORAU, 2020.

dietary <- read_excel(input, sheet = 'EFSA Food', guess_max = 17000)

individuals<-get.people()

dietary<- dietary[complete.cases(dietary$`Number of subjects`,dietary$`Lower Bound Mean Exposure`,dietary$`Lower Bound 95th Exposure`),]
dietary$`Age class`<-tolower(dietary$`Age class`)


food <- function(x){

EF.Dietgroup<- tolower(x$`Dietary Group`)
EF.BW<-x$`Bodyweight (kg)`
EF.n<- x$n
agefood<- dietary[(EF.Dietgroup %in% dietary$`Age class`),]
agefood<-split(agefood,agefood$Chemical)

food.distribution <- function(y){
set.seed(as.numeric(read_excel(input, sheet = 'Seed')))
y$Min <- 0
y$SD  <- y$`Lower Bound 95th Exposure` - y$Min/4
y$GM  <- y$`Lower Bound Mean Exposure`/ (1+ .05 * (y$SD/y$`Lower Bound Mean Exposure`)^2)
y$GSD <- exp(log(y$`Lower Bound 95th Exposure`/y$GM)/qnorm(.95))

y_WM<-  WM(y$GM,y$`Number of subjects`)
y_WSD<- WM(y$GSD,y$`Number of subjects`)

dist <- (rlnorm(EF.n,log(y_WM),log(y_WSD))) * EF.BW

return(dist)
}

result<- data.frame(sapply(agefood,food.distribution))

colnames(result)<- str_c("Exposure to Dietary ", colnames(result)," Typical")

return(result)
}



# ______________________________ Pass to Results and Boxplots.R ______________________________ #

foodexposures<- lapply(individuals,food)

# ____________________________________________________________________________________________ #

# Make Routes Output to match other media 


food.routes<- function(x){

groups<-tolower(rbind.fill(x)$`Dietary Group`)
ages<- dietary$`Age class`

dietary <- dietary[(ages %in% groups),] 
dietary <- split(dietary, paste(dietary$Chemical,dietary$`Age class`))


get.route.info <- function(x){
  
x$Min <- 0
x$SD  <- x$`Lower Bound 95th Exposure` - x$Min/4
x$GM  <- x$`Lower Bound Mean Exposure`/ (1+ .05 * (x$SD/x$`Lower Bound Mean Exposure`)^2)
x$GSD <- exp(log(x$`Lower Bound 95th Exposure`/x$GM)/qnorm(.95))
WM<-  WM(x$GM,x$`Number of subjects`)
WSD<- WM(x$GSD,x$`Number of subjects`)
Total<- as.numeric(nrow(x))

info<- data.frame(cbind(Total,WM,WSD))

return(info)
}

result<- bind_rows(lapply(dietary,get.route.info),.id = "Group")
result$Units<-"ng/kg-bw/day"
result <- result[,c(1,5,2,3,4)]
result$Group <- str_c("Dietary ",result$Group)
return(result)
}

foodroutes<- food.routes(individuals)
