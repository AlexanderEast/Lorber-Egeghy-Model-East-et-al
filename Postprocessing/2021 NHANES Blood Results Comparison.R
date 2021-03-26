# Comparison of aggregate route medians to NHANES data #

# 1. Load libraries and clear wd
rm(list=ls())
library('dplyr')
library('readxl')
library('ggplot2')

# 2. Import NHANES data

blood <- read_excel('20132014 NHANES PFAS Serum Data.xlsx', sheet = 'PFAS_All', guess_max = 17000)

# 3. Tidy NHANES Data, selecting 2013-2014 cycle

blood <- blood %>% select(Cycle, RIAGENDR, RIDAGEYR, SSNPFOA, SSBPFOA, SSNPFOS, SSMPFOS) %>%
  filter(Cycle == 20132014) 

blood<- data.frame(blood)
blood[is.na(blood)] <- 0
blood <- blood %>% mutate(PFOA = (SSNPFOA + SSBPFOA)*1000,
                          PFOS = (SSNPFOS + SSMPFOS)*1000)%>%
  select(-SSBPFOA,-SSNPFOA,-SSMPFOS,-SSNPFOS) %>%
  mutate( RIAGENDR = case_when( (RIAGENDR == "1") ~ "Male",
                                (RIAGENDR == "2") ~ "Female"))
names(blood)[1:3]<- c("Year","Gender","Age")
# units are ug/ L per Labels sheet in NHANES input file, hence*1000 above
# to switch to ng/L. 
blood <- blood %>% mutate(PFOA = PFOA/1000,
                          PFOS = PFOS/1000)
adults<- blood[(blood$Age >= 18),]
children<-blood[(blood$Age < 18),]

table(children$Age)
table(adults$Age)
# childrens ages begin at 3.

# 4. Manually subset data to create histograms (PFOA, PFOS, Adult, Child)

ADULTSPFOS<- adults[c("Gender","Age","PFOS")]
colnames(ADULTSPFOS)[3]<- "Concentration"
ADULTSPFOS$Chemical<- "PFOS"

CHILDRENPFOS <-children[c("Gender","Age","PFOS")]
colnames(CHILDRENPFOS)[3]<- "Concentration"
CHILDRENPFOS$Chemical<- "PFOS"

CHILDRENPFOA <-children[c("Gender","Age","PFOA")]
colnames(CHILDRENPFOA)[3]<- "Concentration"
CHILDRENPFOA$Chemical<- "PFOA"

ADULTSPFOA<- adults[c("Gender","Age","PFOA")]
colnames(ADULTSPFOA)[3]<- "Concentration"
ADULTSPFOS$Chemical<- "PFOA"

tail(sort(CHILDRENPFOS$Concentration),10)
tail(sort(CHILDRENPFOA$Concentration),10)
tail(sort(ADULTSPFOA$Concentration),10)
tail(sort(ADULTSPFOS$Concentration),10)

median(ADULTSPFOS$Concentration)


# 5. Plots of NHANES Data For Comparison. Results from Lorber-Egeghy Model added
#    as geom_vline().

# CHILDREN PFOS #

ggplot(aes(x = Concentration), data = CHILDRENPFOS) + 
  geom_histogram(color = 'black',binwidth = .05) + 
  geom_vline(aes(xintercept=11.7502565,
  color="Estimated Median"), linetype="dashed",size=1) +
  geom_vline(aes(xintercept=median(Concentration),
  color="NHANES Median"), linetype="dashed",size=1) +
  scale_color_manual(name = "Median Estimate Comparison", 
  values = c(`Estimated Median` = "blue",`NHANES Median` = "red"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 5), lim = c(0, 40))+
  scale_y_continuous(expand = c(0,0), lim = c(0,30))+
  labs(title = "NHANES 2013-2014 PFOS Serum Concentration (Children)",
  x= "Serum Concentration (ng/mL)",
  y= "Count")+ theme_bw()+
  theme(plot.caption = element_text(size=7.5, hjust=0, face="italic", color="black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))

# CHILDREN PFOA #


ggplot(aes(x = Concentration), data = CHILDRENPFOA) + 
  geom_histogram(color = 'black',binwidth = .05) + 
  geom_vline(aes(xintercept=7.4698654,
                 color="Estimated Median"), linetype="dashed",size=1) +
  geom_vline(aes(xintercept=median(Concentration),
                 color="NHANES Median"), linetype="dashed",size=1) +
  scale_color_manual(name = "Median Estimate Comparison", 
                     values = c(`Estimated Median` = "blue",`NHANES Median` = "red"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 5), lim = c(0, 40))+
  scale_y_continuous(expand = c(0,0), lim = c(0,30))+
  labs(title = "NHANES 2013-2014 PFOA Serum Concentration (Children)",
       x= "Serum Concentration (ng/mL)",
       y= "Count")+ theme_bw()+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))

# ADULTS PFOS #

ggplot(aes(x = Concentration), data = ADULTSPFOS) + 
  geom_histogram(color = 'black',binwidth = .05) + 
  geom_vline(aes(xintercept=6.316993716,
                 color="Estimated Median"), linetype="dashed",size=1) +
  geom_vline(aes(xintercept=median(Concentration),
                 color="NHANES Median"), linetype="dashed",size=1) +
  scale_color_manual(name = "Median Estimate Comparison", 
                     values = c(`Estimated Median` = "blue",`NHANES Median` = "red"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 5), lim = c(0, 70))+
  scale_y_continuous(expand = c(0,0), lim = c(0,35))+
  labs(title = "NHANES 2013-2014 PFOS Serum Concentration (Adults)",
       x= "Serum Concentration (ng/mL)",
       y= "Count",caption = "Five serum concentrations beyond X axis: 78.2, 83.6, 104.1, 161.0, 1403.0")+ 
  theme_bw()+
  theme(plot.caption = element_text(size=7.5, hjust=0, face="italic", color="black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))

# ADULTS PFOA #

ggplot(aes(x = Concentration), data = ADULTSPFOA) + 
  geom_histogram(color = 'black',binwidth = .05) + 
  geom_vline(aes(xintercept=4.015835127,
                 color="Estimated Median"), linetype="dashed",size=1) +
  geom_vline(aes(xintercept=median(Concentration),
                 color="NHANES Median"), linetype="dashed",size=1) +
  scale_color_manual(name = "Median Estimate Comparison", 
                     values = c(`Estimated Median` = "blue",`NHANES Median` = "red"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 5), lim = c(0, 70))+
  scale_y_continuous(expand = c(0,0), lim = c(0,35))+
  labs(title = "NHANES 2013-2014 PFOA Serum Concentration (Adults)",
       x= "Serum Concentration (ng/mL)",
       y= "Count",caption = "One serum concentrations beyond X axis: 85.27")+ 
  theme_bw()+
  theme(plot.caption = element_text(size=7.5, hjust=0, face="italic", color="black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))


# PERCENTILES OF EACH ESTIMATE 

# CHILDREN PFOS 
ecdf(CHILDRENPFOS$Concentration)(11.7502565)
# CHILDREN PFOA
ecdf(CHILDRENPFOA$Concentration)(7.4698654)

# ADULTS PFOS
ecdf(ADULTSPFOS$Concentration)(6.316993716)
# ADULTS PFOA 
ecdf(ADULTSPFOA$Concentration)(4.015835127)

# ADULTS PFOA WITHOUT NON-DETECTS
ecdf(ADULTSPFOA$Concentration[ADULTSPFOA$Concentration > 0])(4.015835127)
# CHILDREN PFOA WITHOUT NON-DETECTS 
ecdf(ADULTSPFOA$Concentration[CHILDRENPFOA$Concentration > 0])(7.4698654)
