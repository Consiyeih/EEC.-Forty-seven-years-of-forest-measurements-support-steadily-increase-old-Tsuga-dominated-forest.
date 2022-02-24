##miniproject evrything great inshallah
##Natalia Constanza de la Tijera Fernandez 22/02/22
#####################
##cleaning the work space and also the graphs
rm(list=ls())
dev.off()
#calling needed packages (hopefully you have them installed)
library("dplyr")
library(lme4)
library(ggplot2)

#set working directory
setwd("/Users/nataliadelatijera/Documents/Miniproject/growth rate plants/")
#open my CV. If you need the data let me know.
dat<-read.csv("rawdata (2).csv", header=TRUE)
glimpse(dat)

###create a column which has the ID values only because SAMPLE_DESC includes
#year in each value
dat$ID<-substr(dat$SAMPLE_DESC,1,nchar(dat$SAMPLE_DESC)-5)
##create a vector with the IDs without repetition of them for future for loops
nam<-unique(dat$ID)
dat<-dat[order(dat$SAMPLE_DESC),]

##Obtaining the values for the allometric biomass equations for each species
#for each tree in the data set

#Abies balsamea beta0=-2.3123  beta1= 2.3482
#Acer rubrum beta0=-2.0470 beta1=2.3852
# Acer saccharinum beta0=-1.8011 beta1=2.3852
#Betula alleghaniensis  beta0=-1.8096   beta1=2.3480
#Fraxinus nigra beta0=-2.0314    beta1=2.3524
#Ostrya virginiana beta0=-2.2652   beta1= 2.5349
#Pinus strobus beta0=-2.6177 beta1=2.4638
#Populus grandidentata beta0=-2.4441  beta1=2.4561
#Quercus rubra beta0=-2.0705  beta1=2.4410
#Tilia americana beta0=-2.4108  beta1=2.4177
#Tsuga canadensis beta0=-2.3480  beta1=2.3876
#Ulmus americana beta0=-2.2118  beta1=2.4133

#create the vector for each species allometric values
##order the data in the genus species for the calculation
dat <- dat[order(dat$GENUS_SPECIES),]
genus<-unique(dat$GENUS_SPECIES)
beta0<-c(-2.3123,-2.0470,-1.8011, -1.8096, -2.0314, -2.2652, -2.6177, -2.4441, -2.0705,
         -2.4108,-2.3480,-2.2118)
beta1<-c(2.3482,2.3852, 2.3852, 2.3480, 2.3524, 2.5349, 2.4638, 2.4561, 2.4410,
         2.4177,2.3876,2.4133)
Table1<-cbind(genus,beta0,beta1)

##calculate the biomass for eah ID with the equation and each specific value
realbiomass<-data.frame(GENUS_SPECIES=character(), SAMPLE_DESC=character(), biomasskg=numeric())
for(i in 1:length(genus)){
  #select each genus from the 12 present in the data to asses the different
  #Beta0 and beta1 for the biomass calculation
  genusonly<-dat %>% filter(GENUS_SPECIES%in%genus[i])
  #calculate the biomass for each individual given that the DBH (BIOMASS in the 
  #data frame)
  for(j in 1:nrow(genusonly)){
    parttoexp<-beta0[i]+beta1[i]*log(genusonly$BIOMASS[j])
    bm<-exp(parttoexp)
    datfr<- data.frame(GENUS_SPECIES=genus[i],
                       SAMPLE_DESC=genusonly$SAMPLE_DESC[j],biomasskg=bm)
    realbiomass<-rbind(realbiomass, datfr)
  }
}
##add the information created in the loop to my dataset
dat<-merge(dat, realbiomass, on="SAMPLE_DESC")

#just checking values are correct
dat$SAMPLE_DESC[88]
A.rubrum375_1967_a<-exp(-2.0470+2.3852*log(dat$BIOMASS[88]))
A.rubrum375_1967_b<-exp(beta0[2]+beta1[2]*log(dat$BIOMASS[88]))
dat$biomasskg[88]

##checking agian if the loop was correct (values shuld be same calculated by hand
#than the ones calculated by the loop)
dat$SAMPLE_DESC[1000]
dat$GENUS_SPECIES[1000]
Acer_saccharum561_2004_a<-exp(-1.8011+2.3852*log(dat$BIOMASS[1000]))
Acer_saccharum561_2004_b<-exp(beta0[3]+beta1[3]*log(dat$BIOMASS[1000]))
dat$biomasskg[1000]

dat$PLOT1 = substr(dat$PLOT, 1, 4)# Extract first four characters to be
#able to identify same plots since PLOT column has characters for iindividual ID
ko<-unique(dat$PLOT1)
length(ko) ###confirm we have 19 plots now in the data set

## Select the dbh which is bigger than 12.7cm because in the first censuses 
#1962 and 1967 only the trees over 12.7 were reccorded.
datamorethan12.7cm<-dat %>% filter(BIOMASS>12.6)

##I know that the burned plots have less total biomass in 1964 and I can also 
#confirm in the paper from Woods, 2000 
biomassperplotperyear<-datamorethan12.7cm%>% group_by(YEAR,PLOT1) %>% 
  summarise(sum(biomasskg))
biomassperplotperyear$biomasperhe<-biomassperplotperyear$`sum(biomasskg)`
## the ones with less biomass are "7106","7104","7094"
b<-(c("7106","7104","7094"))
datamorethan12.7cm$burned<-ifelse(datamorethan12.7cm$PLOT1%in%b, 1,0)
###lets select Tsuga dominated plots for unburned, the ones that has the highest
#biomass values for T. canadensis. Also, confirm thr plot for burned and unburned 
#in Woods, 2000
tsugadom<-datamorethan12.7cm %>% filter(burned==0) %>% filter(YEAR==1962)%>%
  group_by(PLOT1,GENUS_SPECIES) %>%
  summarise(sum(biomasskg)) #the ones that has more are 7093 and 7092
## select only the parcels in which T. canadensis is the dominant sp.
# create a vecto c with the plots values 7093 and 7092 plus burned 7106,7104,7094
c<-c("7093","7092", "7106", "7104","7094")
#filter the plots in c from  the data ober 12.7 cm in dbh
dattsugadom<-datamorethan12.7cm %>% filter(PLOT1%in%c)

#covert into per hectare and Megagram (1 kg= 1/1000 Mega grams)
eachplothe<-0.2*0.404686*1000
#create a vector with the same ammount of values to be able to operate.
eachplothe7<-rep(c(eachplothe),times=35)

biomassperplotperyear<-dattsugadom%>% group_by(YEAR,PLOT1) %>% 
  summarise(sum(biomasskg))
biomassperplotperyear$biomasperhe<-biomassperplotperyear$`sum(biomasskg)`/
  eachplothe7


biomassperplotperyear$burned<-ifelse(biomassperplotperyear$PLOT1%in%b, 1,0)
biomassperplotperyear$burned<-as.character(biomassperplotperyear$burned)


##Figure 1.Biomass accumulation rate in old-growth and second-growth (red) 
#Tsuga canadensis-dominated forest in Michigan, USA.
plotbiomasperhepery <-ggplot(data = biomassperplotperyear, 
 aes(x = YEAR, y = (biomasperhe), color=factor(burned))) + 
  geom_point()+stat_smooth(method=lm, se = FALSE)+ 
  xlab("Census Year")+ylab(expression(Biomass~Mg~ha^{"-1"}))+
  scale_color_manual(name ="", labels = c("Old Forest (unburned)", 
                                           "Secondary forest (burned)"),
                                           values=c("blue","red"))+theme_bw()+
  theme(legend.background = element_rect(fill = "white", size = 5, colour = "white"),
        legend.justification = c(0.1, 1),
        legend.position = c(0.1, 1),
        axis.title = element_text(size = 15))
  
plotbiomasperhepery

##make a linear model to compare both biomass growth rate and see if the slope of them are
##different (interaction)
eachplotbiomasschange<-lm((biomasperhe)~YEAR*burned, biomassperplotperyear)
summary(eachplotbiomasschange)
anova(eachplotbiomasschange)

#to have a quick check of the assumptioms easily with autoplot
library(ggfortify)
autoplot(eachplotbiomasschange, smooth.colour = NA)
#to have a quick check of the assumptioms easily with basic R
par(mfrow=c(2,2))
plot(eachplotbiomasschange)
## see the distribution of the residuals in an histogram.
hist(residuals(eachplotbiomasschange))
##residulas are not perfect but ok
biomassperplotperyear$burned<-as.character(biomassperplotperyear$burned)
##now PLOT1 as random factor because of the pseudoreplication in time
eachplotbiomasschangelmer<-lmer((biomasperhe)~YEAR*burned+(1|PLOT1), biomassperplotperyear)
summary(eachplotbiomasschangelmer)
##check distr of residuals, ok I would say
plot(eachplotbiomasschangelmer)

##pseudorepeatability value 
900.6/(900.6 +648.7)


##just to see if there is another better model with no interaction between year
#and burned
eachplotbiomasschangelmernoint<-lmer((biomasperhe)~YEAR+burned+(1|PLOT1), 
                                     biomassperplotperyear)
summary(eachplotbiomasschangelmernoint)
plot(eachplotbiomasschangelmernoint)
require(lmtest)
##No difference between interaction and no interaction
lrtest(eachplotbiomasschangelmernoint, eachplotbiomasschangelmer)
#If I compare to the simple linear model the LMM with interaction is better
#explaining variation
lrtest(eachplotbiomasschangelmer, eachplotbiomasschange)

##hypothesis 2 about the diffrence of structure in both forest types

##assign a value to dbh range for calculating the bomass of each part
datamorethan12.7cm$dbhgroup<-ifelse(datamorethan12.7cm$BIOMASS <= 17, "12-17", 
ifelse(17.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 22,"17.1-22",
ifelse(22.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 27, "22.1-27",
ifelse(27.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 32, "27.1-32",
ifelse(32.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 37, "32.1-37",
ifelse(37.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 42, "37.1-42",
ifelse(42.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 47, "42.1-47",
ifelse(47.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 52, "47.1-52",
ifelse(52.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 57, "52.1-57",
ifelse(57.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 62, "57.1-62",
ifelse(62.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 67, "62.1-67",
ifelse(67.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 72, "67.1-72",
ifelse(72.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 77, "72.1-77",
ifelse(77.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 82, "77.1-82",
ifelse(82.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 87, "82.1-87",
ifelse(87.1 <= datamorethan12.7cm$BIOMASS & datamorethan12.7cm$BIOMASS <= 92, "87.1-92",
"92.1-97"))))))))))))))))


##now that new column is added in the datamorethan12.7 is added,re-make the data
#of tsuga only with the added column
dattsugadom<-datamorethan12.7cm %>% filter(PLOT1%in%c)

##count the number of individuals in the whole study
length(unique(dattsugadom$ID))
onlyunburned<-dattsugadom %>% filter(burned=="0")
length(unique(onlyunburned$PLOT1))
onlyburned<-dattsugadom %>% filter(burned=="1")
length(unique(onlyburned$ID))

##########I change datamorethan12.7cm  to analyze only dattsugadom
bdh1962<-dattsugadom %>% filter(YEAR==1962) %>% group_by(burned,dbhgroup) %>% 
  summarise(sum(biomasskg))                    
bdh1962tot<-dattsugadom %>% filter(YEAR==1962) %>% group_by(burned) %>% 
  summarise(sum(biomasskg)) 
##for unburned

dbh1962totalunburned<-bdh1962tot[1, 2]
dbh1962totalunburned<-as.numeric(dbh1962totalunburned)
dbh1962totalunburnedrep<-rep(c(dbh1962totalunburned),times=13)
bdh1962unburned<-bdh1962 %>% filter(burned=="0")
bdh1962unburned$percentage<-(bdh1962unburned$`sum(biomasskg)`/dbh1962totalunburnedrep)*
  100

##burned  
dbh1962totalburned<-bdh1962tot[2, 2]
dbh1962totalburned<-as.numeric(dbh1962totalburned)
dbh1962totalburnedrep<-rep(c(dbh1962totalburned),times=8)
bdh1962burned<-bdh1962 %>% filter(burned=="1")
bdh1962burned$percentage<-(bdh1962burned$`sum(biomasskg)`/dbh1962totalburnedrep)*
  100
Totdbh1962<-rbind(bdh1962unburned, bdh1962burned)
structure(Totdbh1962)
Totdbh1962$burned<-as.character(Totdbh1962$burned)

##add 0 where there is no biomass in that dbh range

bur1962<-c("1","1","1","1","1","1","1","1","1", "0","0", "0","0")
dbh1962<-c("52.1-57","57.1-62","62.1-67","67.1-72","72.1-77", "77.1-82", "82.1-87",
           "87.1-92", "92.1-97","72.1-77", "77.1-82", "82.1-87", "92.1-97")
bio1962<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
per1962<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)

#install.packages("tidyverse")
library(tidyverse)
Totdbh1962<-Totdbh1962 %>% 
  rename(
    totalbiomasskg = `sum(biomasskg)`
  )
###correct `sum(biomasskg)` NAS ptm
added1962<-data.frame(burned=bur1962, dbhgroup=dbh1962, totalbiomasskg=bio1962,
                      percentage=per1962)

Totdbh1962<-rbind(Totdbh1962,added1962)

structure(added1962)
structure(Totdbh1962)

# make a graph with the percentage of biomass per hectare??? in different dbh
#groups to understand differences in the structure distribution of the biomass 
#between burned (1830) and unburned data in T. canadiensisdominated plots
p1962 <- ggplot(data=Totdbh1962, aes(x=dbhgroup, y=percentage, fill=burned)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("1962")+
  xlab("")+ylab("Percent of Total Biomass")+
  scale_fill_manual(name = "", labels = c("Old Forest (unburned)", 
                                           "Secondary forest (burned)"),
                     values=c("blue","red"))+theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 12), 
        axis.title = element_text(size = 15))

##see graph
p1962


###now 2009
bdh2009<-dattsugadom %>% filter(YEAR==2009) %>% group_by(burned,dbhgroup) %>% 
  summarise(sum(biomasskg))                    
bdh2009tot<-dattsugadom %>% filter(YEAR==2009) %>% group_by(burned) %>% 
  summarise(sum(biomasskg)) 
##for unburned

dbh2009totalunburned<-bdh2009tot[1, 2]
dbh2009totalunburned<-as.numeric(dbh2009totalunburned)
dbh2009totalunburnedrep<-rep(c(dbh2009totalunburned),times=15)
bdh2009unburned<-bdh2009 %>% filter(burned=="0")
bdh2009unburned$percentage<-(bdh2009unburned$`sum(biomasskg)`/dbh2009totalunburnedrep)*
  100

##burned  
dbh2009totalburned<-bdh2009tot[2, 2]
dbh2009totalburned<-as.numeric(dbh2009totalburned)
dbh2009totalburnedrep<-rep(c(dbh2009totalburned),times=11)
bdh2009burned<-bdh2009 %>% filter(burned=="1")
bdh2009burned$percentage<-(bdh2009burned$`sum(biomasskg)`/dbh2009totalburnedrep)*
  100
Totdbh2009<-rbind(bdh2009unburned, bdh2009burned)
structure(Totdbh2009)
Totdbh2009$burned<-as.character(Totdbh2009$burned)

###change
bur2009<-c("1","1","1","1","1","1")
dbh2009<-c("67.1-72","72.1-77","77.1-82","82.1-87","87.1-92","92.1-97")
bio2009<-c(0,0,0,0,0,0)
per2009<-c(0,0,0,0,0,0)

Totdbh2009<-Totdbh2009 %>% 
  rename(
    totalbiomasskg = `sum(biomasskg)`
  )

added2009<-data.frame(burned=bur2009, dbhgroup=dbh2009,totalbiomasskg=bio2009,
                      percentage=per2009)

Totdbh2009<-rbind(Totdbh2009,added2009)



# add the labels please!!!place the labels
p2009 <- ggplot(data=Totdbh2009, aes(x=dbhgroup, y=percentage, fill=burned)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("2009")+
  xlab("Diameter at breast height (dbh) Classes, cm")+
  ylab("Percent of Total Biomass")+
  scale_fill_manual(name = "", labels = c("Old Forest (unburned)", 
                                          "Secondary forest (burned)"),
                    values=c("blue","red"))+theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(size = 15))
##See the graph
p2009

#########Supplementary 2.
library(gridExtra)
Supplemental_2<-grid.arrange(p1962, p2009, ncol=1, nrow = 2)



###check if ditributions differ with kolmogorv smirnoff
# perform#The ks.boot function in the Matching package (Sekhon 2011) was used 
#because the function executes a bootstrapped version of the univariate 
#Kolmogorov-Smirnov test that corrects for distributions that are not perfectly
#continuous a two-sample Kolmogorov-Smirnov test for differences in 1962

install.packages("Matching")
library(Matching)

#differences in dbh in old forest
ks.boot(dat1962$BIOMASS[dat1962$burned == 0], dat1962$BIOMASS[dat1962$burned == 1], 
        nboots=1000, alternative = c("two.sided", "less", "greater"), print.level=0)

#differences in dbh in 2009
ks.boot(dat2009$BIOMASS[dat2009$burned == 0], dat2009$BIOMASS[dat2009$burned == 1], 
        nboots=1000, alternative = c("two.sided", "less", "greater"), print.level=0)

##percentage of biomass less than 52 cm burned in 1962
Totdbh1962$dbhgroup
dbhtocompare<-c("12-17","17.1-22","22.1-27","27.1-32","32.1-37","37.1-42",
                "42.1-47","47.1-52","52.1-57")
Totdbh1962less52<-Totdbh1962 %>% filter(dbhgroup%in%dbhtocompare)%>%
  summarise(sum(percentage))
##percentage of biomass less than 52 cm burned in 2009
Totdbh2009less52<-Totdbh2009 %>% filter(dbhgroup%in%dbhtocompare)%>%
  summarise(sum(percentage))


###see the rends of Tsuga to increase or decrease in 1962 and 2009 in both forest
#types
eachplotha<-0.2*0.404686*1000
eachplotha10times<-rep(c(eachplothe),times=10)
yearstocomp<-c("1962", "2009")
tsugatrens<-dattsugadom %>% filter(YEAR%in%yearstocomp) %>%group_by(YEAR,
                                                                    burned,PLOT1) %>% filter(GENUS_SPECIES=="Tsuga canadensis") %>% summarise(sum(biomasskg))
tsugatrens$biomasperhe<-tsugatrens$`sum(biomasskg)`/
  eachplotha10times

tsugatrens$YEAR<-as.character(tsugatrens$YEAR)
##compare with generalized linear mixed model unburned
tsugatrensunburned<-tsugatrens %>% filter(burned=="0")
tsugatrensunburnedlmer<-lmer(biomasperhe~YEAR+(1|PLOT1), tsugatrensunburned)
##Suplemental 3.1 Information for Table of linear mixed model in Old forest
summary(tsugatrensunburnedlmer)

##compare with generalized linear mixed model burned
tsugatrensburned<-tsugatrens %>% filter(burned=="1")
tsugatrensburnedlmer<-lmer(biomasperhe~YEAR+(1|PLOT1), tsugatrensburned)
##Suplemental 3.2 Information for Table of linear mixed model in Secondary forest
summary(tsugatrensburnedlmer)

### for lmer p calc but no use because the p values in LMM are in debate
library(car)
Anova(tsugatrensunburnedlmer)
plot(tsugatrensunburnedlmer)




tsugaburunbur<-rbind(tsugatrensunburned, tsugatrensburned)
averagetsu<-tsugaburunbur %>% group_by(YEAR,burned) %>% summarise(mean(biomasperhe),
                                                                  sd(biomasperhe))
averagetsu$YEAR<-as.character(averagetsu$YEAR)
averagetsu$burned<-as.character(averagetsu$burned)

##Suplemental 3.3  Biomass accumulated (Mg/ha) in T. canadensis between 1962 and 
#2009 in each forest type. 1962 in yellow and 2009 in orange.
plotunburnedtsuga<-ggplot(data=averagetsu, aes(x=burned, y=`mean(biomasperhe)`, 
                                               fill=YEAR)) +geom_bar(stat="identity", position=position_dodge())+
  xlab("Forest type")+ylab(expression(Biomass~Mg~ha^{"-1"}))+
  scale_x_discrete(labels=c("Old forest", "Secondary forest"))+
  scale_fill_manual(name = "",values=c("yellow","orange"))+theme_minimal()+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text=element_text(size = 13),
        legend.background = element_rect(fill = "white", size = 5, colour = "white"),
        legend.justification = c(1, 1),
        legend.position = c(1, 1))


### for lmer p calc
library(car)
Anova(tsugatrensburnedlmer)
plot(tsugatrensburnedlmer)

###Discussion

##calculate the above mass in secondary forest by the other paper
(47*2.2)/(245.4*(1-(exp(-0.017*132)))-(245.4*(1-(exp(-0.017*179)))))
##years of study
132-179


