
##################################
#Code for Funiculina quadrangularis fishing effects study as part of the Small Isles data report results
##Created by Rebecca Langton 08 September 2022
##Adapted from code by Philip Boulcott
##For sources of underlying data and abbreviations, see the report
##################################


########################################################################
# Analysis of the  VMS FQ data
# 5 quadrats per station - approximately 30 stations (sometimes 25) per box
# The quadrat station design is used  to increase the quadrat area given the density of FQ in the test areas and the fixed photographic area


########################################################################
# Section 1. Reading in the data
######################################################################## 

#set directories
rm(list=ls())
getwd()
dir<-substr(getwd(), 1, nchar(getwd())-25)
dir

#load libraries
library(pbkrtest) # Estimate p-values computed using a Kenward-Roger correction is not available in glmer - need this for family =poisson

#reads in the dataset with variables centred using the median as a reference value.
dtaS= read.csv("FQ_2017_dtaStation.csv", header=T) #this is a dataset that has been 

## Now to produce a second simplified dataset called dtaSdrop
# Without the box which data exploration suggested was an outlier


dtaSdrop = dtaS[dtaS$fBox != c("VM21"),]

############################################################################################
#Section 2 - mean density
############################################################################################
(with(dtaS,tapply(Funiculina.quadrangularis, fBox, mean)))

# ordering it to help read through
sort(with(dtaS,tapply(Funiculina.quadrangularis, fBox, mean)))

names(dtaS)
# Writing out a csv that matches the densities figure in the manuscript - see WC_FQ_Survey map
FQDEN_TABLE_MANUSCRIPT = data.frame((with(dtaS,tapply(Funiculina.quadrangularis, fBox, mean))))
colnames(FQDEN_TABLE_MANUSCRIPT) = c("density")
FQDEN_TABLE_MANUSCRIPT$oldnames =row.names(FQDEN_TABLE_MANUSCRIPT)
FQDEN_TABLE_MANUSCRIPT$newnames = paste0("F",1:29)
FQDEN_TABLE_MANUSCRIPT$vms =(with(dtaS,tapply(SurfSAR, fBox, mean)))
FQDEN_TABLE_MANUSCRIPT
FQDEN_TABLE_MANUSCRIPT$Area=dtaS$fArea[match(FQDEN_TABLE_MANUSCRIPT$oldnames,dtaS$fBox)]
write.csv(FQDEN_TABLE_MANUSCRIPT, file= "FQ_2017_VMS_Manuscript_Densities.csv")

#This does produce the same table as that in the report

############################################################################################
# Section 3.  Fitting the model and model selection
############################################################################################

# dropping out terms - using backwards selection of AIC
#uses full dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  

md10<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel + cminsal +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) 
summary (md10)

md10.D<- glmer(Funiculina.quadrangularis ~ cvms + cmud + cgravel + cminsal +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md10.V<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cgravel + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md10.M<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cgravel + cminsal +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md10.G<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud  + cminsal +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md10.S = glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel  +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) 
md10.SL = glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel + cminsal +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) 
md10.C = glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel + cminsal+cslope  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) 
md10.A<- glmer(Funiculina.quadrangularis ~ cdepth +  cvms + cmud + cgravel + cminsal+cslope +ccurvature  + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  




AIC(md10,md10.D, md10.V, md10.M, md10.G, md10.S,  md10.SL,md10.C,md10.A) #Gravel drops first 

# df      AIC
# md10    12 1210.270
# md10.D  11 1212.753
# md10.V  11 1211.193
# md10.M  11 1209.170
# md10.G  11 1208.625 *lowest
# md10.S  11 1210.720
# md10.SL 11 1215.143
# md10.C  11 1223.541
# md10.A  10 1221.506

summary(md10) # this is supported by the output summary for the maximal model
anova(md10,md10.G)

# Data: dtaS
# Models:
#   md10.G: Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md10: Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel + cminsal + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# md10.G   11 1208.6 1262.0 -593.31   1186.6                     
# md10     12 1210.3 1268.5 -593.13   1186.3 0.3548  1     0.5514


md11<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md11.D<- glmer(Funiculina.quadrangularis ~  cvms + cmud + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))   
md11.V<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md11.M<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md11.S<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md11.SL<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal+ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md11.C<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal+cslope   + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md11.A<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal+cslope +ccurvature  + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


summary(md11) 
AIC(md11,md11.D, md11.V, md11.M, md11.S, md11.SL,md11.C,md11.A) # salinity drops out 
# 
# df      AIC
# md11    11 1208.625
# md11.D  10 1210.990
# md11.V  10 1209.368
# md11.M  10 1209.704
# md11.S  10 1209.693 *lowest
# md11.SL 10 1214.910
# md11.C  10 1221.541
# md11.A   9 1220.923
anova(md11,md11.S) # chisq= 3.068 df = 1, pr = 0.08 for Salinty
#Data: dtaS
# Models:
#   md11.S: Funiculina.quadrangularis ~ cdepth + cvms + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md11: Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# md11.S   10 1209.7 1258.2 -594.85   1189.7                       
# md11     11 1208.6 1262.0 -593.31   1186.6 3.0687  1    0.07981 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



md12<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md12.D<- glmer(Funiculina.quadrangularis ~ cvms + cmud +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md12.V<- glmer(Funiculina.quadrangularis ~ cdepth + cmud +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md12.M<- glmer(Funiculina.quadrangularis ~ cdepth + cvms+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md12.SL<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md12.C<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud +cslope   + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md12.A<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud +cslope +ccurvature  + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


summary (md12)
AIC(md12,md12.D, md12.V, md12.M, md12.SL,md12.C,md12.A) # VMS drops out 

#        df      AIC
# md12    10 1209.693
# md12.D   9 1211.521
# md12.V   9 1208.931 *lowest
# md12.M   9 1209.183
# md12.SL  9 1220.281
# md12.C   9 1221.743
# md12.A   8 1219.352

anova(md12,md12.V) # chisq= 1.238 df = 1, pr = 0.265 for VMS
# Data: dtaS
# Models:
#   md12.V: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md12: Funiculina.quadrangularis ~ cdepth + cvms + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
#         npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# md12.V    9 1208.9 1252.6 -595.47   1190.9                     
# md12     10 1209.7 1258.2 -594.85   1189.7 1.2381  1     0.2658




md13 <- glmer(Funiculina.quadrangularis ~ cdepth + cmud +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md13.D <- glmer(Funiculina.quadrangularis ~  cmud +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md13.M <- glmer(Funiculina.quadrangularis ~ cdepth +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md13.SL <- glmer(Funiculina.quadrangularis ~ cdepth + cmud +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md13.C <- glmer(Funiculina.quadrangularis ~ cdepth + cmud +cslope  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md13.A <- glmer(Funiculina.quadrangularis ~ cdepth + cmud +cslope +ccurvature  + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaS, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary (md13)
AIC(md13,md13.D, md13.M, md13.SL,md13.C,md13.A) #

#         df      AIC
# md13     9 1208.931
# md13.D   8 1210.537
# md13.M   8 1209.819
# md13.SL  8 1219.995
# md13.C   8 1221.289
# md13.A   7 1218.362

anova(md13,md13.D) # chisq= 3.606 df = 1, pr = 0.057 for VMS

# Data: dtaS
# Models:
#   md13.D: Funiculina.quadrangularis ~ cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md13: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
#         npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)  
# md13.D    8 1210.5 1249.3 -597.27   1194.5                      
# md13      9 1208.9 1252.6 -595.47   1190.9 3.606  1    0.05757 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#
# To find the P values and Chi sqrs of the above md13
summary (md13)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: poisson  ( log )
# Formula: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature +      fArea + (1 | fBox) + (1 | fStation.No:fBox)
# Data: dtaS
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
# 
# AIC      BIC   logLik deviance df.resid 
# 1208.9   1252.6   -595.5   1190.9      934 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.2803 -0.4641 -0.1696 -0.0801  6.3170 
# 
# Random effects:
#   Groups           Name        Variance Std.Dev.
# fStation.No:fBox (Intercept) 0.5419   0.7361  
# fBox             (Intercept) 1.7977   1.3408  
# Number of obs: 943, groups:  fStation.No:fBox, 943; fBox, 29
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -4.26616    0.66720  -6.394 1.61e-10 ***
#   cdepth      -0.26299    0.13992  -1.880 0.060172 .  
#   cmud         0.26400    0.15107   1.748 0.080549 .  
#   cslope      -0.48563    0.14481  -3.354 0.000798 ***
#   ccurvature  -0.16584    0.04456  -3.722 0.000198 ***
#   fAreaSK      3.08094    0.90936   3.388 0.000704 ***
#   fAreaWR      3.15672    0.93235   3.386 0.000710 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) cdepth cmud   cslope ccrvtr fAreSK
# cdepth      0.500                                   
# cmud       -0.325 -0.244                            
# cslope     -0.147 -0.098  0.110                     
# ccurvature  0.225  0.187 -0.122 -0.027              
# fAreaSK    -0.797 -0.622  0.055  0.099 -0.180       
# fAreaWR    -0.797 -0.524  0.422  0.130 -0.190  0.638

anova(md13, md13.D) # chisq= 3.606 df = 1, pr = 0.058 for Depth ; negative relationship

# Models:
# md13.D: Funiculina.quadrangularis ~ cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md13: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
#        npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)  
# md13.D    8 1210.5 1249.3 -597.27   1194.5                      
# md13      9 1208.9 1252.6 -595.47   1190.9 3.606  1    0.05757 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



anova(md13, md13.M) # chisq= 2.888 df = 1, pr = 0.08 for Mud ; positive relationship

# Data: dtaS
# Models:
#   md13.M: Funiculina.quadrangularis ~ cdepth + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md13: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)  
# md13.M    8 1209.8 1248.6 -596.91   1193.8                      
# md13      9 1208.9 1252.6 -595.47   1190.9 2.888  1    0.08924 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



anova(md13, md13.SL) # chisq= 4.371 df = 1, pr < 0.001 for Slope ; negative relationship

# Data: dtaS
# Models:
#   md13.SL: Funiculina.quadrangularis ~ cdepth + cmud + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md13: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# md13.SL    8 1220.0 1258.8 -602.00   1204.0                         
# md13       9 1208.9 1252.6 -595.47   1190.9 13.064  1   0.000301 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


anova(md13, md13.C) # chisq= 14.357 df = 1, pr < 0.001 for Curvature ; positive relationship

# Data: dtaS
# Models:
#   md13.C: Funiculina.quadrangularis ~ cdepth + cmud + cslope + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md13: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# md13.C    8 1221.3 1260.1 -602.64   1205.3                         
# md13      9 1208.9 1252.6 -595.47   1190.9 14.357  1  0.0001512 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova(md13, md13.A) # chisq= 13.430 df = 2, pr < 0.01 for Area ; SMI < Skye < WR

# Data: dtaS
# Models:
#   md13.A: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + (1 | fBox) + (1 | fStation.No:fBox)
# md13: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)   
# md13.A    7 1218.4 1252.3 -602.18   1204.4                       
# md13      9 1208.9 1252.6 -595.47   1190.9 13.43  2   0.001212 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


############################################################################################
# Section 3.  Fitting the model and model selection without outlier
############################################################################################



md14<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel + cminsal +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) 
summary (md14)

md14.D<- glmer(Funiculina.quadrangularis ~ cvms + cmud + cgravel + cminsal +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md14.V<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cgravel + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md14.M<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cgravel + cminsal +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md14.G<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud  + cminsal +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md14.S = glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel  +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) 
md14.SL = glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel + cminsal +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) 
md14.C = glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel + cminsal+cslope  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) 
md14.A<- glmer(Funiculina.quadrangularis ~ cdepth +  cvms + cmud + cgravel + cminsal+cslope +ccurvature  + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  




AIC(md14,md14.D, md14.V, md14.M, md14.G, md14.S, md14.SL,md14.C,md14.A) #Gravel drops first - to give a lower AIC :  

#  d14    12 1062.837
# md14.D  11 1066.103
# md14.V  11 1061.766
# md14.M  11 1061.821
# md14.G  11 1061.196 *lowest
# md14.S  11 1062.644
# md14.SL 11 1065.697
# md14.C  11 1072.694
# md14.A  10 1074.394

summary(md14) # this is supported by the output summary for the maximal model

anova(md14,md14.G) 
# Data: dtaSdrop
# Models:
#   md14.G: Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md14: Funiculina.quadrangularis ~ cdepth + cvms + cmud + cgravel + cminsal + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
#         npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# md14.G   11 1061.2 1114.2 -519.60   1039.2                     
# md14     12 1062.8 1120.6 -519.42   1038.8 0.3585  1     0.5493


# dropping Gravel
md15<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
md15.D<- glmer(Funiculina.quadrangularis ~  cvms + cmud + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))   
md15.V<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md15.M<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md15.S<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud +cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md15.SL<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal+ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md15.C<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal+cslope   + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md15.A<- glmer(Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal+cslope +ccurvature  + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


AIC(md15,md15.D, md15.V, md15.M, md15.S, md15.SL,md15.C,md15.A) #VMS drops first - to give a lower AIC :  
# df      AIC
# md15    11 1061.196
# md15.D  10 1064.470
# md15.V  10 1059.943 *lowest
# md15.M  10 1062.765
# md15.S  10 1061.542
# md15.SL 10 1065.207
# md15.C  10 1070.743
# md15.A   9 1073.458
summary(md15) # this is supported by the output summary for the maximal model
anova(md15,md15.V) # chisq= 0.747, df = 1, pr = 0.38 for VMS
# Data: dtaSdrop
# Models:
#   md15.V: Funiculina.quadrangularis ~ cdepth + cmud + cminsal + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md15: Funiculina.quadrangularis ~ cdepth + cvms + cmud + cminsal + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
#       npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# md15.V   10 1059.9 1108.1 -519.97   1039.9                     
# md15     11 1061.2 1114.2 -519.60   1039.2 0.7474  1     0.3873

#drop VMS
md16<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

md16.D<- glmer(Funiculina.quadrangularis ~ cmud + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md16.M<- glmer(Funiculina.quadrangularis ~ cdepth  + cminsal+cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md16.S<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md16.SL<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cminsal +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md16.C<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cminsal+cslope  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md16.A<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cminsal+cslope +ccurvature + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


AIC(md16,md16.D,md16.M,md16.S,md16.SL,md16.C,md16.A)
# df      AIC
# md16    10 1059.943
# md16.D   9 1062.998
# md16.M   9 1062.445
# md16.S   9 1059.650 *lowest
# md16.SL  9 1065.249
# md16.C   9 1069.623
# md16.A   8 1071.626

anova(md16,md16.S)
#drop salinity
# Data: dtaSdrop
# Models:
#   md16.S: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md16: Funiculina.quadrangularis ~ cdepth + cmud + cminsal + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
#         npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# md16.S    9 1059.7 1103.0 -520.82   1041.7                     
# md16     10 1059.9 1108.1 -519.97   1039.9 1.7067  1     0.1914

md17<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

md17.D<- glmer(Funiculina.quadrangularis ~ cmud + cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md17.M<- glmer(Funiculina.quadrangularis ~ cdepth  + cslope +ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md17.SL<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + ccurvature  + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md17.C<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cslope   + fArea + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
md17.A<- glmer(Funiculina.quadrangularis ~ cdepth  + cmud + cslope +ccurvature   + (1|fBox) + (1|fStation.No:fBox),family = poisson, data = dtaSdrop, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


AIC(md17,md17.D,md17.M,md17.SL,md17.C,md17.A)

#        df      AIC
# md17     9 1059.650 *lowest
# md17.D   8 1062.206
# md17.M   8 1060.569
# md17.SL  8 1067.530
# md17.C   8 1068.579
# md17.A   7 1069.808

anova(md17,md17.M)
# Data: dtaSdrop
# Models:
#   md17.M: Funiculina.quadrangularis ~ cdepth + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
# md17: Funiculina.quadrangularis ~ cdepth + cmud + cslope + ccurvature + fArea + (1 | fBox) + (1 | fStation.No:fBox)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# md17.M    8 1060.6 1099.1 -522.28   1044.6                       
# md17      9 1059.7 1103.0 -520.82   1041.7 2.9193  1    0.08752 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1