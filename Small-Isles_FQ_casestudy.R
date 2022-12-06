
##################################
#Code for Funiculina quadrangularis case study study as part of the Small Isles data report results
##Created by Rebecca Langton 06 May 2022
##Updated by Rachel Boschen-Rose 06 December 2022
##For sources of underlying data, see the report.
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
##Preparing the DSI data
###############################################

#read in the DSI data
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities.csv") #read in the file with the calculated densities

#reduce to only boxes S07 and S07, and the columns needed
#NOTE - FQ_n = Funiculina quadrangularis count; FQ_n.m2 = Funiculina quadrangularis density
DSIs<-DSIs[DSIs$Box_ID %in% c("S07","S08"),c("Year","Cruise_ID","Image_ID","DecLat","DecLong","Box_ID","Viewed_Area_m2","FQ_n","FQ_n.m2")] 

#create a column of log viewed area to use as an offset later
DSIs$log_viewed_area<-log(DSIs$Viewed_Area_m2)

#make a new column with year as a factor
DSIs$Year_F<-as.factor(DSIs$Year)
DSIs$Year_F<-relevel(DSIs$Year_F,ref="2014")


################################################
#Table of number of stills
################################################
table(list(DSIs$Year,DSIs$Box_ID))
#     S07 S08
#2012 251 193
#2014 139 146
#2015 252 255
#2016 250 249
#2017 124  58

#################################################
##Plotting the locations of DSIs by year
#################################################

#read in the box shapefile
#read in the box shapefile
boxes<-st_read("Small-Isles_2012-17_Boxes_WKT.csv")
boxes<-st_set_crs(boxes,4326)
boxes<-as(boxes,"Spatial")

#make spatial dataset of the DSIs
DSIs_sp<-SpatialPointsDataFrame(coords=DSIs[,c("DecLong","DecLat")],data=DSIs,proj4string = CRS(SRS_string = "EPSG:4326"))

png("Boxes_S07S08.png",width=2500,height=2000,res=500)
par(mar=c(1.2,1.2,0,0),cex.axis=0.8,mgp=c(0.8,0.1,0),tcl=-0.05,cex.lab=0.8)

#plot the two boxes of interest
#plot(boxes[boxes$Bx_Nmbr %in% c("S07","S08"),],axes=T)

plot(boxes[boxes$Box_ID %in% c("S07","S08"),],axes=T)
plot(DSIs_sp,pch=as.numeric(DSIs_sp$Year_F),col=as.numeric(DSIs_sp$Year_F),add=T, cex=(0.2+DSIs$FQ_n))

legend(-6.53,56.984,legend=unique(DSIs_sp$Year_F),pch=as.numeric(unique(DSIs_sp$Year_F)),col=as.numeric(unique(DSIs_sp$Year_F)),bty="n",cex=1)

text(x=c(-6.555,-6.53),y=c(56.98,56.973),labels = c("S07","S08"),cex=1)
dev.off()

#########################################
##S07
#########################################
########################

#glm with the count as the response and log viewed area as an offset (so are still modelling density) - using poisson distribution
mod_s07_2<-glm(FQ_n~Year_F+offset(log_viewed_area),data=DSIs[DSIs$Box_ID=="S07",],family=poisson)
summary(mod_s07_2)
# Call:
#   glm(formula = FQ_n ~ Year_F + offset(log_viewed_area), 
#       family = poisson, data = DSIs[DSIs$Box_ID == "S07", ])
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.4391  -0.3457  -0.0929  -0.0892   3.1877  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -2.6011     0.2774  -9.378  < 2e-16 ***
#   Year_F2012   -0.2468     0.3789  -0.651  0.51486    
#   Year_F2015   -3.1874     1.0377  -3.072  0.00213 ** 
#   Year_F2016   -3.3422     1.0377  -3.221  0.00128 ** 
#   Year_F2017   -2.4527     1.0377  -2.364  0.01810 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 228.28  on 1015  degrees of freedom
# Residual deviance: 183.36  on 1011  degrees of freedom
# AIC: 252.59

#Use DHARMa package to make the residual plots easier to interpret
simulationOutput_mod2<-simulateResiduals(mod_s07_2,plot=T)
plotResiduals(simulationOutput_mod2,form=DSIs$Year_F[DSIs$Box_ID=="S07"])
testSpatialAutocorrelation(simulationOutput_mod2,x=DSIs$DecLong[DSIs$Box_ID=="S07"],y=DSIs$DecLat[DSIs$Box_ID=="S07"])

#create a null model just including the viewed area offset
null_mod<-glm(FQ_n~offset(log_viewed_area),data=DSIs[DSIs$Box_ID=="S07",],family=poisson)
AIC(null_mod)
#[1] 289.5117

anova(null_mod,mod_s07_2,test="Chi")
#Analysis of Deviance Table

#Model 1: FQ_n ~ offset(log_viewed_area)
#Model 2: FQ_n ~ Year_F + offset(log_viewed_area)
#  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#1      1015     228.28                          
#2      1011     183.36  4   44.923 4.124e-09 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

############################
##S08
############################

########################

#glm with the count as the response and log viewed area as an offset (so are still modelling density) - using poisson distribution
mod_s08_2<-glm(FQ_n~Year_F+offset(log_viewed_area),data=DSIs[DSIs$Box_ID=="S08",],family=poisson)
summary(mod_s08_2)
# Call:
#   glm(formula = FQ_n ~ Year_F + offset(log_viewed_area), family = poisson, 
#       data = DSIs[DSIs$Box_ID == "S08", ])
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.4752  -0.3526  -0.1252   0.0000   3.1636  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -2.4430     0.2500  -9.772  < 2e-16 ***
#   Year_F2012    -0.3653     0.3819  -0.957 0.338799    
# Year_F2015    -2.6670     0.7500  -3.556 0.000377 ***
#   Year_F2016   -19.2450  1617.5642  -0.012 0.990507    
# Year_F2017   -19.1097  3360.4423  -0.006 0.995463    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 212.79  on 900  degrees of freedom
# Residual deviance: 159.61  on 896  degrees of freedom
# AIC: 228.22
# 
# Number of Fisher Scoring iterations: 19

simulationOutput_mod2<-simulateResiduals(mod_s08_2,plot=T)
plotResiduals(simulationOutput_mod2,form=DSIs$Year_F[DSIs$Box_ID=="S08"])
testSpatialAutocorrelation(simulationOutput_mod2,x=DSIs$DecLong[DSIs$Box_ID=="S08"],y=DSIs$DecLat[DSIs$Box_ID=="S08"])

#create a null model just including the viewed area offset
null_mod<-glm(FQ_n~offset(log_viewed_area),data=DSIs[DSIs$Box_ID=="S08",],family=poisson)
AIC(null_mod)
#[1] 273.4056

anova(null_mod,mod_s08_2,test="Chi")
#Analysis of Deviance Table
#
#Model 1: FQ_n ~ offset(log_viewed_area)
#Model 2: FQ_n ~ Year_F + offset(log_viewed_area)
#       Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#1       900     212.79                          
#2       896     159.61  4   53.182 7.806e-11 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


########################################################
##Making Predictions
########################################################

#create dataframe to make prediction
NewData<-data.frame(Year_F=c("2012","2014","2015","2016","2017"),Viewed_Area_m2=rep(1,5))
NewData$log_viewed_area<-log(NewData$Viewed_Area_m2)

#create the predictions on the link scale including the standard error (to allow calculation of the confidence interval)
pred_s07<-predict(mod_s07_2,newdata = NewData,type="link",se.fit=T)
pred_s08<-predict(mod_s08_2,newdata = NewData,type="link",se.fit=T)

#create vector of predicted densities
NewData$S07_ExpectedDens<-exp(pred_s07$fit)
#calculate the upper and lower 95% confidence interval 
NewData$S07_L95CI<-exp(pred_s07$fit-(1.96*pred_s07$se.fit))
NewData$S07_U95CI<-exp(pred_s07$fit+(1.96*pred_s07$se.fit))

NewData$S07_L<-exp(pred_s07$fit-pred_s07$se.fit)
NewData$S07_U<-exp(pred_s07$fit+pred_s07$se.fit)


#same for box S08
NewData$S08_ExpectedDens<-exp(pred_s08$fit)
NewData$S08_L95CI<-exp(pred_s08$fit-(1.96*pred_s08$se.fit))
NewData$S08_U95CI<-exp(pred_s08$fit+(1.96*pred_s08$se.fit))

NewData$S08_L<-exp(pred_s08$fit-pred_s08$se.fit)
NewData$S08_U<-exp(pred_s08$fit+pred_s08$se.fit)


#plot the results - 95% CIs
plot((as.numeric(as.character(NewData$Year_F))-0.05),NewData$S07_ExpectedDens,col=2,lty=2,ylim=c(0,max(NewData$S08_U95CI[1:3])),xlab="Year",ylab="Predicted Density",cex=1.5,pch=16)
arrows(x0=(as.numeric(as.character(NewData$Year_F))-0.05),y0=NewData$S07_L95CI,y1=NewData$S07_U95CI,col=2,code=3,angle=90,length=0.05)

points((as.numeric(as.character(NewData$Year_F))+0.05)[1:3],NewData$S08_ExpectedDens[1:3],col=4,pch=18,cex=1.5)
arrows(x0=(as.numeric(as.character(NewData$Year_F))+0.05)[1:3],y0=NewData$S08_L95CI[1:3],y1=NewData$S08_U95CI[1:3],col=4,code=3,angle=90,length=0.05)

legend("topright",col=c(2,4),legend=c("S07","S08"),pch=c(16,18),cex=1.5,bty="n")



######################################################
##Add-on for post-hoc analysis
##original model for S08 had 0 variance for some years, which made post-hoc analysis difficult.
##Instead, check that box isn't a significant variable and then combine to test year together.

#change box to a factor
DSIs$Box_ID<-as.factor(DSIs$Box_ID)

#four candidate models
#1, offset term only
offset<-glm(FQ_n~offset(log_viewed_area),data=DSIs,family=poisson)

#2. year term and offset only
year<-glm(FQ_n~Year_F+offset(log_viewed_area),data=DSIs,family=poisson)
# 
# Call:
# glm(formula = FQ_n ~ Year_F + offset(log_viewed_area), family = poisson, 
#     data = DSIs)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.4580  -0.3487  -0.1089  -0.0655   3.2177  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.5170     0.1857 -13.554  < 2e-16 ***
#   Year_F2012   -0.3135     0.2674  -1.172  0.24107    
# Year_F2015   -2.8734     0.6065  -4.738 2.16e-06 ***
#   Year_F2016   -4.1032     1.0171  -4.034 5.48e-05 ***
#   Year_F2017   -2.9267     1.0171  -2.878  0.00401 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 441.18  on 1916  degrees of freedom
# Residual deviance: 345.62  on 1912  degrees of freedom
# AIC: 473.46
# 
# Number of Fisher Scoring iterations: 8

#3. box term and offset only
box<-glm(FQ_n~Box_ID+offset(log_viewed_area),data=DSIs,family=poisson)

#4. box and year interaction with offset
yearxbox<-glm(FQ_n~Year_F*Box_ID+offset(log_viewed_area),data=DSIs,family=poisson)
summary(yearxbox)
#
# Call:
# glm(formula = FQ_n ~ Year_F * Box_ID + offset(log_viewed_area), 
#     family = poisson, data = DSIs)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.4752  -0.3457  -0.1252  -0.0892   3.1877  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -2.6011     0.2774  -9.378  < 2e-16 ***
# Year_F2012             -0.2468     0.3789  -0.651  0.51482    
# Year_F2015             -3.1874     1.0377  -3.071  0.00213 ** 
# Year_F2016             -3.3422     1.0377  -3.221  0.00128 ** 
# Year_F2017             -2.4527     1.0377  -2.364  0.01810 *  
# Box_IDS08               0.1581     0.3734   0.423  0.67202    
# Year_F2012:Box_IDS08   -0.1185     0.5380  -0.220  0.82571    
# Year_F2015:Box_IDS08    0.5204     1.2804   0.406  0.68440    
# Year_F2016:Box_IDS08  -14.9028   981.1028  -0.015  0.98788    
# Year_F2017:Box_IDS08  -15.6570  2038.2115  -0.008  0.99387    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 441.18  on 1916  degrees of freedom
# Residual deviance: 342.97  on 1907  degrees of freedom
# AIC: 480.81
# 
# Number of Fisher Scoring iterations: 18

#compare the AICs
AIC(offset)
#[1] 561.0187
AIC(year)
#[1] 473.4616
AIC(box)
#[1] 562.9174
AIC(yearxbox)
#[1] 480.8121


#The box with only year effect is the selected model as it has the lowest AIC

##check the residuals
simulationOutput_year<-simulateResiduals(year,plot=T)
plotResiduals(simulationOutput_year,form=DSIs$Year_F)
testSpatialAutocorrelation(simulationOutput_year,x=DSIs$DecLong,y=DSIs$DecLat) #suggests some deviation, however very large sample size so is a lot of power to detect small changes. Visually, it seems valid.


###post-hoc test to check pairwise comparisons
library(multcomp)
my.mod.mc=glht(year, mcp(Year_F="Tukey"))
summary(my.mod.mc)

#Simultaneous Tests for General Linear Hypotheses

#Multiple Comparisons of Means: Tukey Contrasts


#Fit: glm(formula = FQ_n ~ Year_F + #offset(log_viewed_area), 
#         family = poisson, data = DSIs)

# Fit: glm(formula = FQ_n ~ Year_F + offset(log_viewed_area), family = poisson, 
#          data = DSIs)
# 
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)    
# 2012 - 2014 == 0 -0.31352    0.26743  -1.172  0.73423    
# 2015 - 2014 == 0 -2.87342    0.60648  -4.738  < 0.001 ***
# 2016 - 2014 == 0 -4.10324    1.01709  -4.034  < 0.001 ***
# 2017 - 2014 == 0 -2.92670    1.01710  -2.878  0.02622 *  
# 2015 - 2012 == 0 -2.55990    0.60858  -4.206  < 0.001 ***
# 2016 - 2012 == 0 -3.78972    1.01834  -3.721  0.00146 ** 
# 2017 - 2012 == 0 -2.61318    1.01835  -2.566  0.06280 .  
# 2016 - 2015 == 0 -1.22981    1.15470  -1.065  0.79718    
# 2017 - 2015 == 0 -0.05328    1.15470  -0.046  1.00000    
# 2017 - 2016 == 0  1.17653    1.41421   0.832  0.90623    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# (Adjusted p values reported -- single-step method)



##################################################
##Consideration of fishing pressure
##################################################

#This code looks at the level of fishing activity in the boxes. It is based on https://doi.org/10.17895/ices.pub.4505 which is no longer publicly available. The latest version of this dataset is:
#ICES. 2021. OSPAR request on the production of spatial data layers of fishing intensity/pressure. In Report of the ICES Advisory Committee, 2021. ICES Advice 2021, sr.2021.12. https://doi.org/10.17895/ices.advice.8297

# 
# for (year in 2009:2017)
# {
#   shp<-readOGR(paste0("ICES.2018.Shapefiles-OSPAR-spatial-data-fishing-intensity\\",year),paste0("OSPAR_intensity_total_",year)) #read in the shapefile
#   shp<-st_as_sf(shp) #change to sf object
#   shp$Year<-rep(year,times=nrow(shp)) #add a column for the year
#   assign(paste("Y",as.character(year),sep="_"),shp) #assign an object name for the sf object
# }  
# #combine into one object
# fishing<-rbind(Y_2009,Y_2010,Y_2011,Y_2012,Y_2013,Y_2014,Y_2015,Y_2016,Y_2017) #combine into a single sf object
# 
# #change the boxes into an sf object
# boxes_sf<-st_as_sf(boxes[boxes$Bx_Nmbr %in% c("S07"),]) #both c-squares overlap with S07
# boxes_sf<-st_set_crs(boxes_sf,4326)
# 
# #intersect the fishing pressure layers and sf boxes, to get the c-squares in boxes S07 and S08
# fishing_box_int<-st_intersection(fishing,boxes_sf)
# 
# 
# #create line plots of surface and subsurface wept area ratios for all years in the two c-squarea that cover the survey boxes
# plot(fishing_box_int$SurfSAR[fishing_box_int$c_square=="7500:466:495:3"]~fishing_box_int$Year[fishing_box_int$c_square=="7500:466:495:3"],col="darkorchid",type="l",lwd=2,xlab="Year",ylab="Surface Swept Area Ratio",ylim=c(0,max(fishing_box_int$SurfSAR)))
# lines(fishing_box_int$SurfSAR[fishing_box_int$c_square=="7500:466:495:4"]~fishing_box_int$Year[fishing_box_int$c_square=="7500:466:495:4"],col="darkorange",lwd=2)
# legend("topleft",legend=c("7500:466:495:3","7500:466:495:4"),col=c("darkorchid","darkorange"),lwd=2,bty="n",title="c-square")
# 
# plot(fishing_box_int$SubsurfSAR[fishing_box_int$c_square=="7500:466:495:3"]~fishing_box_int$Year[fishing_box_int$c_square=="7500:466:495:3"],col="darkorchid",type="l",lwd=2,xlab="Year",ylab="Sub-surface Swept Area Ratio",ylim=c(0,max(fishing_box_int$SubsurfSAR)))
# lines(fishing_box_int$SubsurfSAR[fishing_box_int$c_square=="7500:466:495:4"]~fishing_box_int$Year[fishing_box_int$c_square=="7500:466:495:4"],col="darkorange",lwd=2)
# legend("topleft",legend=c("7500:466:495:3","7500:466:495:4"),col=c("darkorchid","darkorange"),lwd=2,bty="n",title="c-square")
