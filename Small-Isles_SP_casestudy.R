
##################################
#Code for Swiftia pallida case study study as part of the Small Isles data report results
##Created by Rebecca Langton 06 May 2022
##Updated by Rachel Boschen-Rose 06 December 2022
##For sources of underlying data and abbreviations, see the report.
##################################


#setwd() #set working directory

set.seed(612)

###required libraries
library(openxlsx)
library(MASS)
library(DHARMa)
library(rgdal)
library(maptools)
library(sf)

###############################################
##Preparing the video data
###############################################

#read in the video data
HDVs<-read.csv("Small-Isles_2012-17_HDV-PMF-data.csv") #read in the file with the calculated densities

#reduce to only boxes S06 and S67, and the columns needed
#NOTE - SP_n = Swiftia pallida count; SP_n.m2 = Swiftia pallida density
HDVs<-HDVs[HDVs$Box_ID %in% c("S06","S67"),c("Year","Cruise_ID","Box_ID","Tow_ID","Viewed_Area_m2","SP_n","SP_n.m2")] 

#create a column of log viewed area to use as an offset later
HDVs$log_viewed_area<-log(HDVs$Viewed_Area_m2)

#make a new column with year as a factor
HDVs$Year_F<-as.factor(HDVs$Year)

################################################
#Table of number of videos
################################################
table(list(HDVs$Year,HDVs$Box_ID))
#      .2
#.1     S06 S67
#2015  10   2
#2016   8   7
#2017   4   7


#################################################
##Plotting the locations of DSIs by year
#################################################

#read in the box shapefile
boxes<-st_read("Small-Isles_2012-17_Boxes_WKT.csv")
boxes<-st_set_crs(boxes,4326)
boxes<-as(boxes,"Spatial")

##read in stills to look at where in the box Swiftia was observed in 2012
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities.csv") #read in the file with the calculated densities
DSIs<-DSIs[DSIs$Box_ID %in% c("S06","S67"),c("Year","Cruise_ID","Image_ID","DecLat","DecLong","Box_ID","Viewed_Area_m2","SP_n")]

#make spatial dataset of the DSIs
DSIs_sp<-SpatialPointsDataFrame(coords=DSIs[,c("DecLong","DecLat")],data=DSIs,proj4string = CRS(SRS_string = "EPSG:4326"))

##read in the line shapefiles for the three surveys

surveys<-st_read("Small-Isles_2012-17_Tows_WKT.csv")
surveys<-st_set_crs(surveys,4326)
surveys<-as(surveys,"Spatial")


survey1515a<-merge(surveys[surveys$Cruise_ID=="1515a",c("Tow_ID","Year","Cruise_ID")],HDVs[HDVs$Year==2015,c("Box_ID","Tow_ID","SP_n.m2")],by="Tow_ID")
survey1515a<-survey1515a[!(is.na(survey1515a$Box_ID)),]

survey1816a<-merge(surveys[surveys$Cruise_ID=="1816a",c("Tow_ID","Year","Cruise_ID")],HDVs[HDVs$Year==2016,c("Box_ID","Tow_ID","SP_n.m2")],by="Tow_ID")
survey1816a<-survey1816a[!(is.na(survey1816a$Box_ID)),]

survey1617a<-merge(surveys[surveys$Cruise_ID=="1617a",c("Tow_ID","Year","Cruise_ID")],HDVs[HDVs$Year==2017,c("Box_ID","Tow_ID","SP_n.m2")],by="Tow_ID")
survey1617a<-survey1617a[!(is.na(survey1617a$Box_ID)),]


##################################
###plot spatial dataset of tow lines
##################################


png("Boxes_S06.png",width=2500,height=2000,res=500)
par(mar=c(1.2,1.2,0,0),cex.axis=0.8,mgp=c(0.8,0.1,0),tcl=-0.05,cex.lab=0.8)
#plot the two boxes of interest
plot(boxes[boxes$Box_ID %in% c("S06"),],axes=T)
plot(DSIs_sp,cex=(0.2+DSIs$SP_n)/5,pch=1,add=T)

plot(survey1617a[survey1617a@data$SP_n.m2==0,],add=T,lwd=0.001,lty=2,col=3)
plot(survey1617a[survey1617a@data$SP_n.m2>0,],add=T,lwd=(5*survey1617a@data$SP_n.m2[survey1617a@data$SP_n.m2>0]),col=3)
plot(survey1515a[survey1515a@data$SP_n.m2==0,],add=T,lwd=0.001,lty=2,col=2)
plot(survey1515a[survey1515a@data$SP_n.m2>0,],add=T,lwd=(5*survey1515a@data$SP_n.m2[survey1515a@data$SP_n.m2>0]),col=2)
plot(survey1816a[survey1816a@data$SP_n.m2==0,],add=T,lwd=0.001,lty=2,col=4)
plot(survey1816a[survey1816a@data$SP_n.m2>0,],add=T,lwd=(5*survey1816a@data$SP_n.m2[survey1816a@data$SP_n.m2>0]),col=4)


legend(-6.5005,57.0389,legend="2012 & 2014 DSIs",pch=1,bty="n",cex=0.5)
legend(-6.5005,57.0385,legend=c(2015:2017),lwd=1,col=c(2,4,3),bty="n",cex=0.5)

#....

text(x=-6.49,y=57.0383,labels = "S06",cex=1)
dev.off()



png("Boxes_S67.png",width=2500,height=2000,res=500)
par(mar=c(1.2,1.2,0,0),cex.axis=0.8,mgp=c(0.8,0.1,0),tcl=-0.05,cex.lab=0.8)
#plot the two boxes of interest
plot(boxes[boxes$Box_ID %in% c("S67"),],axes=T)
plot(DSIs_sp,cex=(0.2+DSIs$SP_n)/5,pch=1,add=T)

plot(survey1617a[survey1617a@data$SP_n.m2==0,],add=T,lwd=0.001,lty=2,col=3)
plot(survey1617a[survey1617a@data$SP_n.m2>0,],add=T,lwd=(5*survey1617a@data$SP_n.m2[survey1617a@data$SP_n.m2>0]),col=3)
plot(survey1515a[survey1515a@data$SP_n.m2==0,],add=T,lwd=0.001,lty=2,col=2)
plot(survey1515a[survey1515a@data$SP_n.m2>0,],add=T,lwd=(5*survey1515a@data$SP_n.m2[survey1515a@data$SP_n.m2>0]),col=2)
plot(survey1816a[survey1816a@data$SP_n.m2==0,],add=T,lwd=0.001,lty=2,col=4)
plot(survey1816a[survey1816a@data$SP_n.m2>0,],add=T,lwd=(5*survey1816a@data$SP_n.m2[survey1816a@data$SP_n.m2>0]),col=4)


legend(-6.421,57.0617,legend="2012 & 2014 DSIs",pch=1,bty="n",cex=0.8)
legend(-6.421,57.0614,legend=c(2015:2017),lwd=1,col=c(2,4,3),bty="n",cex=0.8)

#....

text(x=-6.41,y=57.0605,labels = "S67",cex=1)
dev.off()


##################################################
##S06
##################################################

#glm with the count as the response and log viewed area as an offset (so are still modelling density) - using poisson distribution
#Year included as a factor
mod_s06_1<-glm(SP_n~Year_F+offset(log_viewed_area),data=HDVs[HDVs$Box_ID=="S06",],family=poisson)
summary(mod_s06_1)
#Call:
# glm(formula = SP_n ~ Year_F + offset(log_viewed_area), family = poisson, 
#     data = HDVs[HDVs$Box_ID == "S06", ])
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -14.243   -8.011   -3.587    3.842   17.616  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.06414    0.03221  -33.04   <2e-16 ***
#   Year_F2016  -1.02155    0.05444  -18.76   <2e-16 ***
#   Year_F2017  -1.18025    0.06988  -16.89   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 2323.9  on 21  degrees of freedom
# Residual deviance: 1824.0  on 19  degrees of freedom
# AIC: 1948.7
# 
# Number of Fisher Scoring iterations: 5

#Use DHARMa package to make the residual plots easier to interpret
simulationOutput_mod1<-simulateResiduals(mod_s06_1,plot=T)
plotResiduals(simulationOutput_mod1,form=HDVs$Year_F[HDVs$Box_ID=="S06"])  ##lots of problems with the residuals!

#try negative binomial
mod_s06_2<-glm.nb(SP_n~Year_F+offset(log_viewed_area),data=HDVs[HDVs$Box_ID=="S06",])
summary(mod_s06_2)
#Call:
# glm.nb(formula = SP_n ~ Year_F + offset(log_viewed_area), data = HDVs[HDVs$Box_ID == 
#                                                                         "S06", ], init.theta = 0.7406449933, link = log)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6890  -1.0321  -0.5529   0.4343   1.4393  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)  -1.1100     0.3690  -3.008  0.00263 **
#   Year_F2016   -0.7596     0.5537  -1.372  0.17014   
# Year_F2017   -1.0652     0.6911  -1.541  0.12324   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for Negative Binomial(0.7406) family taken to be 1)
# 
# Null deviance: 29.430  on 21  degrees of freedom
# Residual deviance: 26.364  on 19  degrees of freedom
# AIC: 244.36
# 
# Number of Fisher Scoring iterations: 1
# 
# 
# Theta:  0.741 
# Std. Err.:  0.204 
# 
# 2 x log-likelihood:  -236.363 

AIC(mod_s06_2)
#[1] 244.3628

#Use DHARMa package to make the residual plots easier to interpret
simulationOutput_mod2<-simulateResiduals(mod_s06_2,plot=T)
plotResiduals(simulationOutput_mod2,form=HDVs$Year_F[HDVs$Box_ID=="S06"])  ##better diagnostic plots -  small sample size probably making things difficult.

#create a null model just including the viewed area offset
null_mod<-glm.nb(SP_n~offset(log_viewed_area),data=HDVs[HDVs$Box_ID=="S06",])
AIC(null_mod)
#[1] 243.2657

anova(null_mod,mod_s06_2,test="Chi")
# Likelihood ratio tests of Negative Binomial Models
# 
# Response: SP_n
# Model     theta Resid. df    2 x log-lik.   Test    df LR stat.   Pr(Chi)
# 1          offset(log_viewed_area) 0.6649438        21       -239.2657                                
# 2 Year_F + offset(log_viewed_area) 0.7406450        19       -236.3628 1 vs 2     2 2.902908 0.2342295
# Warning message:
#   In anova.negbin(null_mod, mod_s06_2, test = "Chi") :
#   only Chi-squared LR tests are implemented

###Year effect is not significant

##################################################
##S67
##################################################

#glm with the count as the response and log viewed area as an offset (so are still modelling density) - using poisson distribution
#Year included as a factor
mod_s67_1<-glm(SP_n~Year_F+offset(log_viewed_area),data=HDVs[HDVs$Box_ID=="S67",],family=poisson)
summary(mod_s67_1)
#Call:
# glm(formula = SP_n ~ Year_F + offset(log_viewed_area), 
#     family = poisson, data = HDVs[HDVs$Box_ID == "S67", ])
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -16.9637   -6.9171   -5.0992   -0.3682   19.2509  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -0.73539    0.07125 -10.322   <2e-16 ***
#   Year_F2016  -1.59189    0.12278 -12.965   <2e-16 ***
#   Year_F2017  -0.01920    0.08591  -0.223    0.823    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 2271.8  on 15  degrees of freedom
# Residual deviance: 1969.1  on 13  degrees of freedom
# AIC: 2017.1
# 
# Number of Fisher Scoring iterations: 7

#Use DHARMa package to make the residual plots easier to interpret
simulationOutput_mod1<-simulateResiduals(mod_s67_1,plot=T)##too few data points for DHARMa package to work. 
#plotResiduals(simulationOutput_mod1,form=HDVs$Year_F[HDVs$Box_ID=="S67"])  

#try negative binomial
mod_s67_2<-glm.nb(SP_n~Year_F+offset(log_viewed_area),data=HDVs[HDVs$Box_ID=="S67",])
summary(mod_s67_2)
# 
#Call:
#  glm.nb(formula = SP_n ~ Year_F + offset(log_viewed_area), 
#         data = HDVs[HDVs$Box_ID == "S67", ], init.theta = 0.1212640397, 
#         link = log)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.3590  -1.1822  -0.8603  -0.2440   0.9776  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)  -1.0476     2.0325  -0.515    0.606
#Year_F2016   -0.6057     2.3061  -0.263    0.793
#Year_F2017    0.8282     2.3046   0.359    0.719

#(Dispersion parameter for Negative Binomial(0.1213) family taken to be 1)
# 
# Null deviance: 15.275  on 15  degrees of freedom
# Residual deviance: 14.448  on 13  degrees of freedom
# AIC: 115.57
# 
# Number of Fisher Scoring iterations: 1
# 
# 
# Theta:  0.1213 
# Std. Err.:  0.0487 
# 
# 2 x log-likelihood:  -107.5690 


#Use DHARMa package to make the residual plots easier to interpret
simulationOutput_mod2<-simulateResiduals(mod_s67_2,plot=T)
plotResiduals(simulationOutput_mod2,form=HDVs$Year_F[HDVs$Box_ID=="S67"])  ##residuals better - but sample size of very small.


#create a null model just including the viewed area offset
null_mod<-glm.nb(SP_n~offset(log_viewed_area),data=HDVs[HDVs$Box_ID=="S67",])
AIC(null_mod)
#[1] 112.376

anova(null_mod,mod_s67_2,test="Chi")
#Likelihood ratio tests of Negative Binomial Models
# 
# Response: SP_n
# Model     theta Resid. df    2 x log-lik.   Test    df  LR stat.  Pr(Chi)
# 1          offset(log_viewed_area) 0.1103079        15       -108.3760                                
# 2 Year_F + offset(log_viewed_area) 0.1212640        13       -107.5691 1 vs 2     2 0.8069011 0.668011
# Warning message:
#   In anova.negbin(null_mod, mod_s67_2, test = "Chi") :
#   only Chi-squared LR tests are implemented


########################################################
##Making Predictions
########################################################

#create dataframe to make prediction
NewData<-data.frame(Year_F=c("2015","2016","2017"),Viewed.Area.m2=rep(1,3))
NewData$log_viewed_area<-log(NewData$Viewed.Area.m2)

#create the predictions on the link scale including the standard error (to allow calculation of the confidence interval)
pred_s06<-predict(mod_s06_2,newdata = NewData,type="link",se.fit=T)
pred_s67<-predict(mod_s67_2,newdata = NewData,type="link",se.fit=T)

#create vector of predicted densities
NewData$S06_ExpectedDens<-exp(pred_s06$fit)
#calculate the upper and lower 95% confidence interval 
NewData$S06_L<-exp(pred_s06$fit-pred_s06$se.fit)
NewData$S06_U<-exp(pred_s06$fit+pred_s06$se.fit)

##same for S67
#create vector of predicted densities
NewData$S67_ExpectedDens<-exp(pred_s67$fit)
#calculate the upper and lower 95% confidence interval 
NewData$S67_L<-exp(pred_s67$fit-pred_s67$se.fit)
NewData$S67_U<-exp(pred_s67$fit+pred_s67$se.fit)

#plot the results
par(mar=rep(2,4))
plot((as.numeric(as.character(NewData$Year_F))-0.05),NewData$S06_ExpectedDens,col=2,ylim=c(0,max(c(NewData$S06_U,NewData$S67_U))),xlim=c(2014.9,2017.3),xlab="Year",ylab="Predicted Density",cex=1.5,pch=16,axes=FALSE)
axis(1,at=2015:2017,labels = c("2015","2016","2017"))
axis(2)
arrows(x0=(as.numeric(as.character(NewData$Year_F))-0.05),y0=NewData$S06_L,y1=NewData$S06_U,col=2,code=3,angle=90,length=0.05)

points((as.numeric(as.character(NewData$Year_F))+0.05),NewData$S67_ExpectedDens,col=4,pch=18,cex=1.5)
arrows(x0=(as.numeric(as.character(NewData$Year_F))+0.05),y0=NewData$S67_L,y1=NewData$S67_U,col=4,code=3,angle=90,length=0.05)
legend("topright",col=c(2,4),legend=c("S06","S67"),cex=1.5,pch=c(16,18),bty="n")

#