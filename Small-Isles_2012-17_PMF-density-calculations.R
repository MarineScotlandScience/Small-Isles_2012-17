
###############################
##Code for Small Isles data report results
##Created by Rachel Boschen-Rose 01 August 2022 
##Adapted from code by Rebecca Langton and Phil Boulcott
##Underlying data from Clare Greathead, updated by Rachel Boschen-Rose
##For sources of underlying data and abbreviations, see the report
################################

#set working directory
setwd()

##############################################################
#Swiftia pallida calculations at the box level#
##############################################################

##############################################################
# Swifitia pallida data
# for DSI footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the DSI data
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities.csv") #read in the sheet with the raw counts and calculated densities, >75% obscured removed

#reduce to only columns needed
DSIs<-DSIs[,c("Cruise_ID","Box_ID","SP_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(DSIs)

#change the column names
names(DSIs)<-c("Cruise_ID","Box_ID","Swiftia.pallida","Viewed.area","Year")

#Check column names changed properly
View(DSIs)

#checking all records imported
head(DSIs) #displays headers with some row values
names(DSIs) #displays row names
nrow(DSIs) #returns number of rows, should equal number of DSIs with < 25% obscured viewed area, n = 8137

#calculating the box level density for Swiftia pallida
#includes step to create a unique identifier for matching

stableva =aggregate(Viewed.area ~ Box_ID + Year , data = DSIs, FUN= "sum")
stablespc=aggregate(Swiftia.pallida ~ Box_ID + Year , data = DSIs, FUN= "sum")

stablespc$unique= paste0(stablespc[,1], stablespc[,2])

stableva$unique= paste0(stableva[,1], stableva[,2])

stableva$Count =stablespc$Swiftia.pallida[match(stableva$unique, stablespc$unique)]
stableva

stableva$Density = round(stableva$Count/ stableva$Viewed.area, dig=3)

stableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#stableva<-stableva[,c("Box_ID","Year","Density")]

#write out DSI results at box level#
write.csv(stableva,paste0("Swiftia.pallida_DSI_box-density.csv"),row.names=F)

##############################################################
# Swifitia pallida data
# for HDV footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the HDV data
HDVs<-read.csv("Small-Isles_2012-17_HDV-PMF-data.csv") #read in the sheet with the calculated densities

#reduce to only columns needed
HDVs<-HDVs[,c("Cruise_ID","Box_ID","SP_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(HDVs)

#change column names to make sure they match the DSI dataframe
names(HDVs)<-c("Cruise_ID","Box_ID","Swiftia.pallida","Viewed.area","Year")

#Check column names changed properly
View(HDVs)

#checking all records imported
head(HDVs) #displays headers with some row values
names(HDVs) #displays row names
nrow(HDVs) #returns number of rows, should equal number of HDV tows, n = 177

#calculating the box level density for Swiftia pallida
#includes step to create a unique identifier for matching

htableva =aggregate(Viewed.area ~ Box_ID + Year , data = HDVs, FUN= "sum")
htablespc=aggregate(Swiftia.pallida ~ Box_ID + Year , data = HDVs, FUN= "sum")

htablespc$unique= paste0(htablespc[,1], htablespc[,2])

htableva$unique= paste0(htableva[,1], htableva[,2])

htableva$Count =htablespc$Swiftia.pallida[match(htableva$unique, htablespc$unique)]
htableva

htableva$Density = round(htableva$Count/ htableva$Viewed.area, dig=3)

htableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#htableva<-htableva[,c("Box_ID","Year","Density")]

#write out HDV results at box level#
write.csv(htableva,paste0("Swiftia.pallida_HDV_box-density.csv"),row.names=F)

##############################################################
#Parazoanthus anguicomus calculations at the box level#
##############################################################

##############################################################
# Parazoanthus anguicomus data
# for DSI footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the DSI data
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities_28Jul22.csv") #read in the sheet with the raw counts and calculated densities, >75% obscured removed

#reduce to only columns needed
#Parazoanthus abundance should use 'occurrence' patch records (o) column.
DSIs<-DSIs[,c("Cruise_ID","Box_ID","PA_o","Viewed_Area_m2","Year")]

#Check data imported properly
View(DSIs)

#change the column names
names(DSIs)<-c("Cruise_ID","Box_ID","Parazoanthus.anguicomus","Viewed.area","Year")

#Check column names changed properly
View(DSIs)

#checking all records imported
head(DSIs) #displays headers with some row values
names(DSIs) #displays row names
nrow(DSIs) #returns number of rows, should equal number of DSIs with < 25% obscured viewed area, n = 8137

#calculating the box level density for Parazoanthus anguicomus
#includes step to create a unique identifier for matching

stableva =aggregate(Viewed.area ~ Box_ID + Year , data = DSIs, FUN= "sum")
stablespc=aggregate(Parazoanthus.anguicomus ~ Box_ID + Year , data = DSIs, FUN= "sum")

stablespc$unique= paste0(stablespc[,1], stablespc[,2])

stableva$unique= paste0(stableva[,1], stableva[,2])

stableva$Count =stablespc$Parazoanthus.anguicomus[match(stableva$unique, stablespc$unique)]
stableva

stableva$Density = round(stableva$Count/ stableva$Viewed.area, dig=3)

stableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#stableva<-stableva[,c("Box_ID","Year","Density")]

#write out DSI results at box level#
write.csv(stableva,paste0("Parazoanthus.anguicomus_DSI_box-density.csv"),row.names=F)

##############################################################
# Parazoanthus anguicomus data
# for HDV footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the HDV data
HDVs<-read.csv("Small-Isles_2012-17_HDV-PMF-data_28Jul22.csv") #read in the sheet with the calculated densities

#reduce to only columns needed
HDVs<-HDVs[,c("Cruise_ID","Box_ID","PA_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(HDVs)

#change column names to make sure they match the DSI dataframe
names(HDVs)<-c("Cruise_ID","Box_ID","Parazoanthus.anguicomus","Viewed.area","Year")

#Check column names changed properly
View(HDVs)

#checking all records imported
head(HDVs) #displays headers with some row values
names(HDVs) #displays row names
nrow(HDVs) #returns number of rows, should equal number of HDV tows, n = 177

#calculating the box level density for Parazoanthus anguicomus
#includes step to create a unique identifier for matching

htableva =aggregate(Viewed.area ~ Box_ID + Year , data = HDVs, FUN= "sum")
htablespc=aggregate(Parazoanthus.anguicomus ~ Box_ID + Year , data = HDVs, FUN= "sum")

htablespc$unique= paste0(htablespc[,1], htablespc[,2])

htableva$unique= paste0(htableva[,1], htableva[,2])

htableva$Count =htablespc$Parazoanthus.anguicomus[match(htableva$unique, htablespc$unique)]
htableva

htableva$Density = round(htableva$Count/ htableva$Viewed.area, dig=3)

htableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#htableva<-htableva[,c("Box_ID","Year","Density")]

#write out HDV results at box level#
write.csv(htableva,paste0("Parazoanthus.anguicomus_HDV_box-density.csv"),row.names=F)

###################################

##############################################################
#Atrina fragilis calculations at the box level#
##############################################################

##############################################################
# Atrina fragilis data
# for DSI footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the DSI data
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities_28Jul22.csv") #read in the sheet with the raw counts and calculated densities, >75% obscured removed

#reduce to only columns needed
DSIs<-DSIs[,c("Cruise_ID","Box_ID","AF_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(DSIs)

#change the column names
names(DSIs)<-c("Cruise_ID","Box_ID","Atrina.fragilis","Viewed.area","Year")

#Check column names changed properly
View(DSIs)

#checking all records imported
head(DSIs) #displays headers with some row values
names(DSIs) #displays row names
nrow(DSIs) #returns number of rows, should equal number of DSIs with < 25% obscured viewed area, n = 8137

#calculating the box level density for Atrina fragilis
#includes step to create a unique identifier for matching

stableva =aggregate(Viewed.area ~ Box_ID + Year , data = DSIs, FUN= "sum")
stablespc=aggregate(Atrina.fragilis ~ Box_ID + Year , data = DSIs, FUN= "sum")

stablespc$unique= paste0(stablespc[,1], stablespc[,2])

stableva$unique= paste0(stableva[,1], stableva[,2])

stableva$Count =stablespc$Atrina.fragilis[match(stableva$unique, stablespc$unique)]
stableva

stableva$Density = round(stableva$Count/ stableva$Viewed.area, dig=3)

stableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#stableva<-stableva[,c("Box_ID","Year","Density")]

#write out DSI results at box level#
write.csv(stableva,paste0("Atrina.fragilis_DSI_box-density.csv"),row.names=F)

##############################################################
# Atrina fragilis data
# for HDV footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the HDV data
HDVs<-read.csv("Small-Isles_2012-17_HDV-PMF-data_28Jul22.csv") #read in the sheet with the calculated densities

#reduce to only columns needed
HDVs<-HDVs[,c("Cruise_ID","Box_ID","AF_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(HDVs)

#change column names to make sure they match the DSI dataframe
names(HDVs)<-c("Cruise_ID","Box_ID","Atrina.fragilis","Viewed.area","Year")

#Check column names changed properly
View(HDVs)

#checking all records imported
head(HDVs) #displays headers with some row values
names(HDVs) #displays row names
nrow(HDVs) #returns number of rows, should equal number of HDV tows, n = 177

#calculating the box level density for Atrina fragilis
#includes step to create a unique identifier for matching

htableva =aggregate(Viewed.area ~ Box_ID + Year , data = HDVs, FUN= "sum")
htablespc=aggregate(Atrina.fragilis ~ Box_ID + Year , data = HDVs, FUN= "sum")

htablespc$unique= paste0(htablespc[,1], htablespc[,2])

htableva$unique= paste0(htableva[,1], htableva[,2])

htableva$Count =htablespc$Atrina.fragilis[match(htableva$unique, htablespc$unique)]
htableva

htableva$Density = round(htableva$Count/ htableva$Viewed.area, dig=3)

htableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#htableva<-htableva[,c("Box_ID","Year","Density")]

#write out HDV results at box level#
write.csv(htableva,paste0("Atrina.fragilis_HDV_box-density.csv"),row.names=F)

###################################

##############################################################
#Leptometra celtica calculations at the box level#
##############################################################

##############################################################
# Leptometra celtica data
# for DSI footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the DSI data
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities_28Jul22.csv") #read in the sheet with the raw counts and calculated densities, >75% obscured removed

#reduce to only columns needed
DSIs<-DSIs[,c("Cruise_ID","Box_ID","LC_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(DSIs)

#change the column names
names(DSIs)<-c("Cruise_ID","Box_ID","Leptometra.celtica","Viewed.area","Year")

#Check column names changed properly
View(DSIs)

#checking all records imported
head(DSIs) #displays headers with some row values
names(DSIs) #displays row names
nrow(DSIs) #returns number of rows, should equal number of DSIs with < 25% obscured viewed area, n = 8137

#calculating the box level density for Leptometra celtica
#includes step to create a unique identifier for matching

stableva =aggregate(Viewed.area ~ Box_ID + Year , data = DSIs, FUN= "sum")
stablespc=aggregate(Leptometra.celtica ~ Box_ID + Year , data = DSIs, FUN= "sum")

stablespc$unique= paste0(stablespc[,1], stablespc[,2])

stableva$unique= paste0(stableva[,1], stableva[,2])

stableva$Count =stablespc$Leptometra.celtica[match(stableva$unique, stablespc$unique)]
stableva

stableva$Density = round(stableva$Count/ stableva$Viewed.area, dig=3)

stableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#stableva<-stableva[,c("Box_ID","Year","Density")]

#write out DSI results at box level#
write.csv(stableva,paste0("Leptometra.celtica_DSI_box-density.csv"),row.names=F)

##############################################################
# Leptometra.celtica data
# for HDV footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the HDV data
HDVs<-read.csv("Small-Isles_2012-17_HDV-PMF-data_28Jul22.csv") #read in the sheet with the calculated densities

#reduce to only columns needed
HDVs<-HDVs[,c("Cruise_ID","Box_ID","LC_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(HDVs)

#change column names to make sure they match the DSI dataframe
names(HDVs)<-c("Cruise_ID","Box_ID","Leptometra.celtica","Viewed.area","Year")

#Check column names changed properly
View(HDVs)

#checking all records imported
head(HDVs) #displays headers with some row values
names(HDVs) #displays row names
nrow(HDVs) #returns number of rows, should equal number of HDV tows, n = 177

#calculating the box level density for Leptometra celtica
#includes step to create a unique identifier for matching

htableva =aggregate(Viewed.area ~ Box_ID + Year , data = HDVs, FUN= "sum")
htablespc=aggregate(Leptometra.celtica ~ Box_ID + Year , data = HDVs, FUN= "sum")

htablespc$unique= paste0(htablespc[,1], htablespc[,2])

htableva$unique= paste0(htableva[,1], htableva[,2])

htableva$Count =htablespc$Leptometra.celtica[match(htableva$unique, htablespc$unique)]
htableva

htableva$Density = round(htableva$Count/ htableva$Viewed.area, dig=3)

htableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#htableva<-htableva[,c("Box_ID","Year","Density")]

#write out HDV results at box level#
write.csv(htableva,paste0("Leptometra.celtica_HDV_box-density.csv"),row.names=F)

###################################

##############################################################
#Modiolus modiolus calculations at the box level#
##############################################################

##############################################################
# Modiolus modiolus data
# for DSI footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the DSI data
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities_28Jul22.csv") #read in the sheet with the raw counts and calculated densities, >75% obscured removed

#reduce to only columns needed
DSIs<-DSIs[,c("Cruise_ID","Box_ID","MM_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(DSIs)

#change the column names
names(DSIs)<-c("Cruise_ID","Box_ID","Modiolus.modiolus","Viewed.area","Year")

#Check column names changed properly
View(DSIs)

#checking all records imported
head(DSIs) #displays headers with some row values
names(DSIs) #displays row names
nrow(DSIs) #returns number of rows, should equal number of DSIs with < 25% obscured viewed area, n = 8137

#calculating the box level density for Modiolus modiolus
#includes step to create a unique identifier for matching

stableva =aggregate(Viewed.area ~ Box_ID + Year , data = DSIs, FUN= "sum")
stablespc=aggregate(Modiolus.modiolus ~ Box_ID + Year , data = DSIs, FUN= "sum")

stablespc$unique= paste0(stablespc[,1], stablespc[,2])

stableva$unique= paste0(stableva[,1], stableva[,2])

stableva$Count =stablespc$Modiolus.modiolus[match(stableva$unique, stablespc$unique)]
stableva

stableva$Density = round(stableva$Count/ stableva$Viewed.area, dig=3)

stableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#stableva<-stableva[,c("Box_ID","Year","Density")]

#write out DSI results at box level#
write.csv(stableva,paste0("Modiolus.modiolus_DSI_box-density.csv"),row.names=F)

##############################################################
# Modiolus.modiolus data
# for HDV footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the HDV data
HDVs<-read.csv("Small-Isles_2012-17_HDV-PMF-data_28Jul22.csv") #read in the sheet with the calculated densities

#reduce to only columns needed
HDVs<-HDVs[,c("Cruise_ID","Box_ID","MM_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(HDVs)

#change column names to make sure they match the DSI dataframe
names(HDVs)<-c("Cruise_ID","Box_ID","Modiolus.modiolus","Viewed.area","Year")

#Check column names changed properly
View(HDVs)

#checking all records imported
head(HDVs) #displays headers with some row values
names(HDVs) #displays row names
nrow(HDVs) #returns number of rows, should equal number of HDV tows, n = 177

#calculating the box level density for Modiolus modiolus
#includes step to create a unique identifier for matching

htableva =aggregate(Viewed.area ~ Box_ID + Year , data = HDVs, FUN= "sum")
htablespc=aggregate(Modiolus.modiolus ~ Box_ID + Year , data = HDVs, FUN= "sum")

htablespc$unique= paste0(htablespc[,1], htablespc[,2])

htableva$unique= paste0(htableva[,1], htableva[,2])

htableva$Count =htablespc$Modiolus.modiolus[match(htableva$unique, htablespc$unique)]
htableva

htableva$Density = round(htableva$Count/ htableva$Viewed.area, dig=3)

htableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#htableva<-htableva[,c("Box_ID","Year","Density")]

#write out HDV results at box level#
write.csv(htableva,paste0("Modiolus.modiolus_HDV_box-density.csv"),row.names=F)

###################################

##############################################################
#Arachnanthus sarsi calculations at the box level#
##############################################################

##############################################################
# Arachnanthus sarsi data
# for DSI footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the DSI data
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities_28Jul22.csv") #read in the sheet with the raw counts and calculated densities, >75% obscured removed

#reduce to only columns needed
DSIs<-DSIs[,c("Cruise_ID","Box_ID","AS_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(DSIs)

#change the column names
names(DSIs)<-c("Cruise_ID","Box_ID","Arachnanthus.sarsi","Viewed.area","Year")

#Check column names changed properly
View(DSIs)

#checking all records imported
head(DSIs) #displays headers with some row values
names(DSIs) #displays row names
nrow(DSIs) #returns number of rows, should equal number of DSIs with < 25% obscured viewed area, n = 8137

#calculating the box level density for Arachnanthus sarsi
#includes step to create a unique identifier for matching

stableva =aggregate(Viewed.area ~ Box_ID + Year , data = DSIs, FUN= "sum")
stablespc=aggregate(Arachnanthus.sarsi ~ Box_ID + Year , data = DSIs, FUN= "sum")

stablespc$unique= paste0(stablespc[,1], stablespc[,2])

stableva$unique= paste0(stableva[,1], stableva[,2])

stableva$Count =stablespc$Arachnanthus.sarsi[match(stableva$unique, stablespc$unique)]
stableva

stableva$Density = round(stableva$Count/ stableva$Viewed.area, dig=3)

stableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#stableva<-stableva[,c("Box_ID","Year","Density")]

#write out DSI results at box level#
write.csv(stableva,paste0("Arachnanthus.sarsi_DSI_box-density.csv"),row.names=F)

##############################################################
# Arachnanthus sarsi data
# for HDV footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the HDV data
HDVs<-read.csv("Small-Isles_2012-17_HDV-PMF-data_28Jul22.csv") #read in the sheet with the calculated densities

#reduce to only columns needed
HDVs<-HDVs[,c("Cruise_ID","Box_ID","AS_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(HDVs)

#change column names to make sure they match the DSI dataframe
names(HDVs)<-c("Cruise_ID","Box_ID","Arachnanthus.sarsi","Viewed.area","Year")

#Check column names changed properly
View(HDVs)

#checking all records imported
head(HDVs) #displays headers with some row values
names(HDVs) #displays row names
nrow(HDVs) #returns number of rows, should equal number of HDV tows, n = 177

#calculating the box level density for Arachnanthus sarsi
#includes step to create a unique identifier for matching

htableva =aggregate(Viewed.area ~ Box_ID + Year , data = HDVs, FUN= "sum")
htablespc=aggregate(Arachnanthus.sarsi ~ Box_ID + Year , data = HDVs, FUN= "sum")

htablespc$unique= paste0(htablespc[,1], htablespc[,2])

htableva$unique= paste0(htableva[,1], htableva[,2])

htableva$Count =htablespc$Arachnanthus.sarsi[match(htableva$unique, htablespc$unique)]
htableva

htableva$Density = round(htableva$Count/ htableva$Viewed.area, dig=3)

htableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#htableva<-htableva[,c("Box_ID","Year","Density")]

#write out HDV results at box level#
write.csv(htableva,paste0("Arachnanthus.sarsi_HDV_box-density.csv"),row.names=F)

###################################

##############################################################
#Pachycerianthus multiplicatus  calculations at the box level#
##############################################################

##############################################################
# Pachycerianthus multiplicatus  data
# for DSI footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the DSI data
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities_28Jul22.csv") #read in the sheet with the raw counts and calculated densities, >75% obscured removed

#reduce to only columns needed
DSIs<-DSIs[,c("Cruise_ID","Box_ID","PM_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(DSIs)

#change the column names
names(DSIs)<-c("Cruise_ID","Box_ID","Pachycerianthus.multiplicatus","Viewed.area","Year")

#Check column names changed properly
View(DSIs)

#checking all records imported
head(DSIs) #displays headers with some row values
names(DSIs) #displays row names
nrow(DSIs) #returns number of rows, should equal number of DSIs with < 25% obscured viewed area, n = 8137

#calculating the box level density for Pachycerianthus multiplicatus
#includes step to create a unique identifier for matching

stableva =aggregate(Viewed.area ~ Box_ID + Year , data = DSIs, FUN= "sum")
stablespc=aggregate(Pachycerianthus.multiplicatus ~ Box_ID + Year , data = DSIs, FUN= "sum")

stablespc$unique= paste0(stablespc[,1], stablespc[,2])

stableva$unique= paste0(stableva[,1], stableva[,2])

stableva$Count =stablespc$Pachycerianthus.multiplicatus[match(stableva$unique, stablespc$unique)]
stableva

stableva$Density = round(stableva$Count/ stableva$Viewed.area, dig=3)

stableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#stableva<-stableva[,c("Box_ID","Year","Density")

#write out DSI results at box level#
write.csv(stableva,paste0("Pachycerianthus.multiplicatus_DSI_box-density.csv"),row.names=F)

##############################################################
# Pachycerianthus.multiplicatus data
# for HDV footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the HDV data
HDVs<-read.csv("Small-Isles_2012-17_HDV-PMF-data_28Jul22.csv") #read in the sheet with the calculated densities

#reduce to only columns needed
HDVs<-HDVs[,c("Cruise_ID","Box_ID","PM_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(HDVs)

#change column names to make sure they match the DSI dataframe
names(HDVs)<-c("Cruise_ID","Box_ID","Pachycerianthus.multiplicatus","Viewed.area","Year")

#Check column names changed properly
View(HDVs)

#checking all records imported
head(HDVs) #displays headers with some row values
names(HDVs) #displays row names
nrow(HDVs) #returns number of rows, should equal number of HDV tows, n = 177

#calculating the box level density for Pachycerianthus multiplicatus
#includes step to create a unique identifier for matching

htableva =aggregate(Viewed.area ~ Box_ID + Year , data = HDVs, FUN= "sum")
htablespc=aggregate(Pachycerianthus.multiplicatus ~ Box_ID + Year , data = HDVs, FUN= "sum")

htablespc$unique= paste0(htablespc[,1], htablespc[,2])

htableva$unique= paste0(htableva[,1], htableva[,2])

htableva$Count =htablespc$Pachycerianthus.multiplicatus[match(htableva$unique, htablespc$unique)]
htableva

htableva$Density = round(htableva$Count/ htableva$Viewed.area, dig=3)

htableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#htableva<-htableva[,c("Box_ID","Year","Density")]

#write out HDV results at box level#
write.csv(htableva,paste0("Pachycerianthus.multiplicatus_HDV_box-density.csv"),row.names=F)

###################################

##############################################################
#Funiculina quadrangularis  calculations at the box level#
##############################################################

##############################################################
# Funiculina quadrangularis  data
# for DSI footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the DSI data
DSIs<-read.csv("Small-Isles_2012-17_PMF-DSI-densities_28Jul22.csv") #read in the sheet with the raw counts and calculated densities, >75% obscured removed

#reduce to only columns needed
DSIs<-DSIs[,c("Cruise_ID","Box_ID","FQ_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(DSIs)

#change the column names
names(DSIs)<-c("Cruise_ID","Box_ID","Funiculina.quadrangularis","Viewed.area","Year")

#Check column names changed properly
View(DSIs)

#checking all records imported
head(DSIs) #displays headers with some row values
names(DSIs) #displays row names
nrow(DSIs) #returns number of rows, should equal number of DSIs with < 25% obscured viewed area, n = 8137

#calculating the box level density for Funiculina quadrangularis
#includes step to create a unique identifier for matching

stableva =aggregate(Viewed.area ~ Box_ID + Year , data = DSIs, FUN= "sum")
stablespc=aggregate(Funiculina.quadrangularis ~ Box_ID + Year , data = DSIs, FUN= "sum")

stablespc$unique= paste0(stablespc[,1], stablespc[,2])

stableva$unique= paste0(stableva[,1], stableva[,2])

stableva$Count =stablespc$Funiculina.quadrangularis[match(stableva$unique, stablespc$unique)]
stableva

stableva$Density = round(stableva$Count/ stableva$Viewed.area, dig=3)

stableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#stableva<-stableva[,c("Box_ID","Year","Density")

#write out DSI results at box level#
write.csv(stableva,paste0("Funiculina.quadrangularis_DSI_box-density.csv"),row.names=F)

##############################################################
# Funiculina.quadrangularis data
# for HDV footage 
# calculating by box and year - i.e. averaging across boxes
##############################################################

#read in the HDV data
HDVs<-read.csv("Small-Isles_2012-17_HDV-PMF-data_28Jul22.csv") #read in the sheet with the calculated densities

#reduce to only columns needed
HDVs<-HDVs[,c("Cruise_ID","Box_ID","FQ_n","Viewed_Area_m2","Year")]

#Check data imported properly
View(HDVs)

#change column names to make sure they match the DSI dataframe
names(HDVs)<-c("Cruise_ID","Box_ID","Funiculina.quadrangularis","Viewed.area","Year")

#Check column names changed properly
View(HDVs)

#checking all records imported
head(HDVs) #displays headers with some row values
names(HDVs) #displays row names
nrow(HDVs) #returns number of rows, should equal number of HDV tows, n = 177

#calculating the box level density for Funiculina quadrangularis
#includes step to create a unique identifier for matching

htableva =aggregate(Viewed.area ~ Box_ID + Year , data = HDVs, FUN= "sum")
htablespc=aggregate(Funiculina.quadrangularis ~ Box_ID + Year , data = HDVs, FUN= "sum")

htablespc$unique= paste0(htablespc[,1], htablespc[,2])

htableva$unique= paste0(htableva[,1], htableva[,2])

htableva$Count =htablespc$Funiculina.quadrangularis[match(htableva$unique, htablespc$unique)]
htableva

htableva$Density = round(htableva$Count/ htableva$Viewed.area, dig=3)

htableva

#optional - reduce to columns needed if only want DSI data and not to merge with HDV
#htableva<-htableva[,c("Box_ID","Year","Density")]

#write out HDV results at box level#
write.csv(htableva,paste0("Funiculina.quadrangularis_HDV_box-density.csv"),row.names=F)

###################################