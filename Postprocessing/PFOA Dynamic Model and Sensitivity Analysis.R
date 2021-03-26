# Sensitivity Analysis and Steady State PK Comparison for PFOA #


# 1. Load libraries and clear wd
rm(list=ls())
library('ggplot2')
library('deSolve')
library('plyr')

# 2. Dynamic Model construction
model <- function(time, Y, parms){
  with(as.list(c(Y, parms)),
       {
         dCP <- (DP/Vd) - (kP * CP)
         list(dCP)
       })
}

dynamic <- function(x,y,z,a){
  
  
  Y0 <- c(CP=0)
  parms <- c(Vd=x, kP=y, DP=z/a)
  times <- seq(0, 8000, 1)
  out <- ode(Y0, times, model, parms)
  
  # Steady-state concentration
  ssCP = parms[["DP"]] / (parms[["kP"]] * parms[["Vd"]])
  
  ssCP<- as.numeric(ssCP)
  out <- data.frame(out)
  f<- list(ssCP,data.frame(out))
  return(f)
}

# 3. Run model three times with varying Vds

PFOA <- 40.4010281831809
one   <- dynamic(140,0.00083,PFOA,71.3)
two   <- dynamic(170,0.00083,PFOA,71.3)
three <- dynamic(185,0.00083,PFOA,71.3)

# 4. Subset Data and format for plot
lowss  <- as.numeric(one[1])
medss  <- as.numeric(two[1])
highss <- as.numeric(three[1])

lowdata<-data.frame(one[2])
meddata<-data.frame(two[2])
highdata<-data.frame(three[2])

lowdata$Vd<- "140"
meddata$Vd<- "170"
highdata$Vd <- "185"

# Change data to years
lowdata$time<-lowdata$time/365
meddata$time<-meddata$time/365
highdata$time<-highdata$time/365



data<-rbind(lowdata,meddata,highdata)
mysize <- 1

# 5. Plot Data

ggplot()+
  geom_line(size=mysize,highdata,mapping = aes(x=time,y=CP))
ggplot(data, aes(x=time, y=CP, color=Vd))+
  geom_line(size= mysize)+
  scale_color_manual(values=c("Blue","Black","Red"))+
  geom_hline(aes(yintercept = highss),color = "Red",size = mysize)+
  geom_hline(aes(yintercept = medss) ,color = "Black",size = mysize)+
  geom_hline(aes(yintercept = lowss) ,color = "Blue",size = mysize)+ 
  theme_bw()+
  labs(title = "Dynamic Model and Steady State Comparisons for Varying PFOS Volume Distributions Among Adults",
       x= "Time (years)",
       y= "Serum concentration (ng/ml)")+
  scale_x_continuous(expand = c(0, 0), breaks = 1:21, lim = c(0, 21))+
  scale_y_continuous(expand = c(0,0),lim=c(0,7),breaks=1:7)