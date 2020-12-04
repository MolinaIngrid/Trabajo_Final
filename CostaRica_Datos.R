###Archivo para trabajar con los datos de costa rica de  Nespresso
install.packages("tidyverse")
install.packages("dplyr")
install.packages("rlang_0.4.7")
install.packages("tidyr")
install.packages("readr")
install.packages("lattice")
install.packages("parallel")
install.packages("Rcpp")
install.packages("reshape2")
install.packages("ggdag")

library(sf)
library(rnaturalearth)
library(lubridate)
library(tidyverse)
library(readr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)
library(rlang)
# library(wesanderson)
library(unmarked)
library(MuMIn)
library(AICcmodavg)
#
library(ggdag)


#Read in all of the CR data
CRdatos_all <- read.csv("~/Desktop/Nespresso/CR_Nespresso/MyEBirdData.csv",stringsAsFactors=FALSE)
head(CRdatos_all)
dim(CRdatos_all)
#total of 19,840 records
names(CRdatos_all)

#Clean up the scientific names

Data.CR.2020.BPI.rsp <- CRdatos_all[!grepl("sp,", CRdatos_all[["Scientific.Name"]]), ]
Data.CR.2020.BPI.rsp <- Data.CR.2020.BPI.rsp[!grepl("sp.", Data.CR.2020.BPI.rsp[["Scientific.Name"]]), ]
Data.CR.2020.BPI.rsp <- Data.CR.2020.BPI.rsp[!grepl("sp ", Data.CR.2020.BPI.rsp[["Scientific.Name"]]), ]

Data.CR.2020.BPI.rsp$Scientific.Name[which(Data.CR.2020.BPI.rsp$Scientific.Name == "Ramphocelus passerinii passerinii")] <- "Ramphocelus costaricensis"
Data.CR.2020.BPI.rsp$Scientific.Name[which(Data.CR.2020.BPI.rsp$Scientific.Name == "Ramphocelus passerinii costaricensis")] <- "Ramphocelus costaricensis"
Data.CR.2020.BPI.rsp$Scientific.Name[which(Data.CR.2020.BPI.rsp$Scientific.Name == "Empidonax alnorum/traillii")] <- "Empidonax traillii"

Lista.especies.all <- unique(Data.CR.2020.BPI.rsp$Scientific.Name)
length(Lista.especies.all)

#Subset the data to only the point counts that we got
Data.CR.2020.BPI <- Data.CR.2020.BPI.rsp[grepl("BPI-", Data.CR.2020.BPI.rsp[["Location"]]), ]
names(Data.CR.2020.BPI)
dim(Data.CR.2020.BPI)

Datos_fincas <- Data.CR.2020.BPI[,c("Location","Latitude","Longitude")]
Datos_BPI_CR <- distinct(Datos_fincas)
dim(Datos_BPI_CR)
head(Datos_fincas)
# write.csv(Datos_BPI_CR,"Datos_BPI_CR.csv", row.names = FALSE)
# Datos_BPI_CR$Location


Data.CR.2020.BPI <- Data.CR.2020.BPI %>% 
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date),
                rec_1 = 1)
unique(Data.CR.2020.BPI$year)

dim(Data.CR.2020.BPI)
head(Data.CR.2020.BPI)

# Data.CR.2020.BPI <- Data.CR.2020.BPI %>% drop_na(Scientific.Name)

Lista.especies.total <- unique(Data.CR.2020.BPI$Scientific.Name)
length(Lista.especies.total)

CR_bird_info <- read.delim("~/Desktop/Nespresso/CR_Nespresso/Lista_Costa_Rica.txt",stringsAsFactors=FALSE)
PIF_bird_info <- read.csv("~/Desktop/Nespresso/CR_Nespresso/CA_PIF_DataBase.csv",stringsAsFactors=FALSE)
PIF_bird_info.red <- PIF_bird_info[,c(3,5,6)]

#Match the order and family using common names
names(CR_bird_info)
names(PIF_bird_info.red) <- c("Sci_name","Oder","Family")

CR_bird_info2 <- CR_bird_info %>% left_join(PIF_bird_info.red)

dim(CR_bird_info2)
names(CR_bird_info2)

Aves_CR_sub <- subset(CR_bird_info2, Sci_name %in% Lista.especies.total)
names(Aves_CR_sub)
dim(Aves_CR_sub)

Lista.especies.info <- unique(CR_bird_info$Sci_name)

# missing_info <- setdiff(Lista.especies.total,Lista.especies.info)

#Fill in the missing families for species that did not match
which(is.na(Aves_CR_sub$Oder))
# 2  26  62 104 122 141 176 184 212
Aves_CR_sub[2,]
Aves_CR_sub$Oder[2] <- "Caprimulgiformes"
Aves_CR_sub$Family[2] <- "Throchilidae"

Aves_CR_sub[26,]
Aves_CR_sub$Oder[26] <- "Caprimulgiformes"
Aves_CR_sub$Family[26] <- "Throchilidae"

Aves_CR_sub[62,]
Aves_CR_sub$Oder[62] <- "Passeriformes"
Aves_CR_sub$Family[62] <- "Cardinalidae"

Aves_CR_sub[104,]
Aves_CR_sub$Oder[104] <- "Passeriformes"
Aves_CR_sub$Family[104] <- "Passerellidae"

Aves_CR_sub[122,]
Aves_CR_sub$Oder[122] <- "Passeriformes"
Aves_CR_sub$Family[122] <- "Parulidae"

Aves_CR_sub[141,]
Aves_CR_sub$Oder[141] <- "Piciformes"
Aves_CR_sub$Family[141] <- "Dryobates"

Aves_CR_sub[176,]
Aves_CR_sub$Oder[176] <- "Passeriformes"
Aves_CR_sub$Family[176] <- "Sporophila"

Aves_CR_sub[184,]
Aves_CR_sub$Oder[184] <- "Passeriformes"
Aves_CR_sub$Family[184] <- "Thraupidae"

Aves_CR_sub[212,]
Aves_CR_sub$Oder[212] <- "Passeriformes"
Aves_CR_sub$Family[212] <- "Tyrannidae"


names(Aves_CR_sub) 

CR_BIP_Hab

##How many IUCN species
IUCN_counts <- Aves_CR_sub %>%
  group_by(Red_list_16) %>%
  tally

IUCN_VU_CR <- Aves_CR_sub$Sci_name[which(Aves_CR_sub$Red_list_16 == "VU")]
IUCN_NT_CR <- Aves_CR_sub$Sci_name[which(Aves_CR_sub$Red_list_16 == "NT")]


Aves_CR_sub[26,]

##How many Watchlist species
WatchList_counts <- Aves_CR_sub %>%
  group_by(Cont_concern) %>%
  tally

Family_counts <- Aves_CR_sub %>%
  group_by(Family) %>%
  tally

CR_Family_List <- as.data.frame(Family_counts)

ggplot(Family_counts,aes(x = Family, n, fill = Family)) +
  geom_bar(stat = "identity") +
  # coord_polar("y",start=0)
  labs(x = "Family", y = "Number of species") +
  scale_fill_hue(c=43, l=50) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

####################################
#Get a site covariate file for each site, but process them later

BIP_Hab_AAA <- read.csv("~/Desktop/Nespresso/CR_Nespresso/BIP_Hab_AAA.csv", stringsAsFactors=FALSE)

head(BIP_Hab_AAA) 


CR_BIP_Hab <- BIP_Hab_AAA %>% left_join(Datos_BPI_CR)

##Subsetting data only to points in Nespresso Farms
CR_BIP_Hab_AAA <- CR_BIP_Hab %>%
  filter(Type == "Cafe")
AAA_CR_BPI <- CR_BIP_Hab_AAA[,1]

Data.CR.2020.AAA <- Data.CR.2020.BPI %>%
  filter(Location %in% AAA_CR_BPI)

Bird_species_AAA <- unique(Data.CR.2020.AAA$Scientific.Name)

Aves_CR_AAA <- Aves_CR_sub %>%
  filter(Sci_name %in% Bird_species_AAA)

##How many Watchlist species
WatchList_countsAAA <- Aves_CR_AAA %>%
  group_by(Cont_concern) %>%
  tally

Family_countsAAA <- Aves_CR_AAA %>%
  group_by(Family) %>%
  tally

IUCN_countsAAA <- Aves_CR_AAA %>%
  group_by(Red_list_16) %>%
  tally

WLYR <- Aves_CR_sub$Sci_name[which(Aves_CR_sub$Cont_concern == "Watch List - Yel-r")]
WLYD <- Aves_CR_sub$Sci_name[which(Aves_CR_sub$Cont_concern == "Watch List - Yel-d")]
WLRED <- Aves_CR_sub$Sci_name[which(Aves_CR_sub$Cont_concern == "Watch List - Red")]


###Preliminary data analysis

#get a sense of who should be modeled that will be interesting
Count_sp_obs <- ddply(Data.CR.2020.BPI,~Scientific.Name,summarise,species_count=length(unique(Submission.ID)))

Count_sp_obs <- Count_sp_obs[order(Count_sp_obs$species_count),]

Count_sp_obs.red <- Count_sp_obs[Count_sp_obs$species_count >=100 & Count_sp_obs$species_count <= 430,]

#Create a data matrix
#VEr bien los meses que fureon muestrados por cada temporada
#figure out how many days each point was sampled
Count_BPI_samp2019 <- ddply(Data.CR.2020.BPI[Data.CR.2020.BPI$year==2019,],~Location,summarise,Sampled_days=length(unique(Date)))
#weird- says most sites were only sampled 2 times and some just once? 

Count_BPI_samp2020 <- ddply(Data.CR.2020.BPI[Data.CR.2020.BPI$year==2020,],~Location,summarise,Sampled_days=length(unique(Date)))
#Seems like most sites were sampled 4 days, should figure out why there are some with 5 and others with six, will collapse to 4 for now

#Sticking to data from 2020 for now, using just four days

#figure out what months points were sampled

Months2019 <- sort(unique(Data.CR.2020.BPI$month[Data.CR.2020.BPI$year==2019]))

Months2020 <- sort(unique(Data.CR.2020.BPI$month[Data.CR.2020.BPI$year==2020]))

#CReating the sampled data file

J= 4 #number of sampled days
n= nrow(Datos_BPI_CR)
Sites <- Datos_BPI_CR[,1]
CR_2020_Matrix <- matrix(0, nrow=n, ncol=J,dimnames=list(1:n, paste("survey", 1:J, sep="")))


#Add the covariates that we have with Memo so far

LandUseCR_30m <- read.csv("~/Desktop/Nespresso/CR_Nespresso/LandUseCR_30m.csv")
head(LandUseCR_30m)
LandUseCR_100m <- read.csv("~/Desktop/Nespresso/CR_Nespresso/LandUseCR_100m.csv")
head(LandUseCR_100m)
LandUseCR_100m_red <- LandUseCR_100m[,c(1,9)]
CRbuff30_aboveGbio <- read.csv("~/Desktop/Nespresso/CR_Nespresso/CRbuff30_aboveGbio.csv")
head(CRbuff30_aboveGbio)
names(CRbuff30_aboveGbio) <- c("ID","Biomass_30m","Location","Coordinates")
CRbuff100_aboveGbio <- read.csv("~/Desktop/Nespresso/CR_Nespresso/CRbuff100_aboveGbio.csv")
head(CRbuff100_aboveGbio)
names(CRbuff100_aboveGbio) <- c("ID","Biomass_100m","Location","Coordinates")

CR_Hab1 <- CRbuff30_aboveGbio %>% left_join(CRbuff100_aboveGbio)
names(LandUseCR_100m_red) <- c("ID","Per_Forest")
CR_Hab_BPI <- CR_Hab1 %>% left_join(LandUseCR_100m_red)
CR_Hab_BPI2 <- as.data.frame(CR_Hab_BPI[,c(3,2,5,6)])

#standardize covariates
CR_HAB_BPI <- as.data.frame(scale(CR_Hab_BPI2[,-c(1)]))
hist(CR_HAB_BPI$Biomass_30m)
hist(CR_HAB_BPI$Biomass_100m)
hist(log(CR_HAB_BPI$Per_Forest)) #doesn't look right, won't use for now

#get the list of sites in the same order as the covariates, as a character vector
Sites_BPI_Obvs <- as.character(CR_Hab_BPI2[,1])

##Fill in the data matrix with the individual species

#Lepidocolaptes souleyetii
#Chiroxiphia linearis
#Basileuterus rufifrons

# Species_CR <- c("Lepidocolaptes souleyetii","Chiroxiphia linearis","Basileuterus rufifrons","Catharus aurantiirostris",
#                 "Momotus lessonii","Icterus galbula","Catharus ustulatus","Sittasomus griseicapillus")

Species_CR <- Count_sp_obs.red[-1,1]

Species_Res <- Species_CR[c(8,9,11,24,14,2)]
Species_Res_names <- c("Streaked-headed Woodpecker","Yellow Warbler","Long-tailed Manakin","Blue-gray Tanager","Orange-billed Nightengale-thrush","Yellow-green Vireo")

# for (i in 1:length(Species_CR)) {
# for (i in 5:6) {
    # sp.name <- Species_CR[i]
    sp.name <- Species_CR[3] 
    temp.det.mat <- CR_2020_Matrix
    temp.data <- Data.CR.2020.BPI %>%
    filter(Scientific.Name == sp.name) %>%
    filter(year == 2020) %>%
    filter(month %in% Months2020[1:3])
    temp.sites <- unique(temp.data$Location)
    site.match <- match(Sites_BPI_Obvs,temp.sites)
    site.idx <- !is.na(match(Sites_BPI_Obvs,temp.sites))
    for(ii in 1:length(Sites_BPI_Obvs)){
      if(site.idx[ii] == TRUE){
      temp.site <- temp.data %>%
        filter(Location == temp.sites[site.match[ii]])
        n.obs <- nrow(temp.site)
        if(n.obs > 4){
          n.obs <- 4;
        }
        for(n in 1:n.obs){
          temp.det.mat[ii,n] <- 1;
        }
      }else if(site.idx[ii] == FALSE){ 
      temp.det.mat <- temp.det.mat
      }
    }


BasRufUMF <- unmarkedFrameOccu(y = temp.det.mat,siteCovs = CR_HAB_BPI)
summary(BasRufUMF)
str(BasRufUMF)
BasRufUMF[1:5,]    

# Null model (ie, no covariates)
(null <- occu(~1 ~1, BasRufUMF))
## Backtransform psi and p to orignial scale
# NOTE: this function only works on the null model
backTransform(null, "state")
backTransform(null, "det")

# Global model (ie, all covariates)
full <- occu(~Biomass_100m +Biomass_30m ~Biomass_100m + Biomass_30m + Per_Forest, BasRufUMF)

occ_dredge <- dredge(full)

#model comparison to explore the results for occupancy
occ_dredge_table <- occ_dredge%>%
  mutate_all(~ round(., 3))

Occ_res <- as.data.frame(occ_dredge_table)

# take a quick peak at the model selection table
Occ_res
#here you can see which models have the most support for this species
#Anna Lello-Smith does have code for getting a lot of this automatically!
#Explore this for single species at first

# select models with the most suport for model averaging (less than 2.5 Delta AICc)
occ_dredge_Delta <- get.models(occ_dredge, subset = delta <= 2.5)

# average models based on model weights 
occ_avg <- model.avg(occ_dredge_Delta, fit = TRUE)

# model averaged coefficients for occupancy and detection probbility, fr the models with the most support from the data
coef(occ_avg)


#create a new data frame
newDatHab <- data.frame(Biomass_100m = mean(CR_HAB_BPI$Biomass_100m),
                        Biomass_30m = seq(min(CR_HAB_BPI$Biomass_30m),max(CR_HAB_BPI$Biomass_30m), length=100),
                        Per_Forest = mean(CR_HAB_BPI$Per_Forest))


occ_pred <- predict(occ_avg,
                    newdata = newDatHab,
                    type = "state")

Biomass_30m <- seq(min(CR_Hab_BPI2$Biomass_30m),max(CR_Hab_BPI2$Biomass_30m), length=100)

# par(mfrow=c(2,3))
plot(Biomass_30m, occ_pred$fit, xlab="Above Ground Biomass at 30m",
       ylab="Occupancy Probability", pch=16, ylim=0:1, type="l",
       # lwd=2, main = Species_CR[i])
     lwd=3, main = Species_Res_names[i],col=3)
lines(Biomass_30m, occ_pred$fit-occ_pred$se.fit, lty=3,col=3)
lines(Biomass_30m, occ_pred$fit+occ_pred$se.fit, lty=3,col=3)

#create a new data frame
# newDatHab2 <- data.frame(Biomass_100m = seq(min(CR_HAB_BPI$Biomass_100m),max(CR_HAB_BPI$Biomass_100m), length=100),
#                         Biomass_30m = mean(CR_HAB_BPI$Biomass_30m),
#                         Per_Forest = mean(CR_HAB_BPI$Per_Forest))
# 
# 
# occ_pred2 <- predict(occ_avg,
#                     newdata = newDatHab2,
#                     type = "state")
# 
# Biomass_100m <- seq(min(CR_Hab_BPI2$Biomass_100m),max(CR_Hab_BPI2$Biomass_100m), length=100)
# plot(Biomass_100m, occ_pred2$fit, xlab="Above Ground Biomass at 100m",
#      ylab="Occupancy Probability", pch=16, ylim=0:1, type="l",
#      # lwd=2, main = Species_CR[i])
#      lwd=3, main = Species_Res_names[i],col=3)
# lines(Biomass_100m, occ_pred2$fit-occ_pred2$se.fit, lty=3,col=3)
# lines(Biomass_100m, occ_pred2$fit+occ_pred2$se.fit, lty=3,col=3)

# #create a new data frame
newDatHab3 <- data.frame(Per_Forest = seq(min(CR_HAB_BPI$Per_Forest),max(CR_HAB_BPI$Per_Forest), length=100),
                         Biomass_30m = mean(CR_HAB_BPI$Biomass_30m),
                         Biomass_100m = mean(CR_HAB_BPI$Biomass_100m))


occ_pred3 <- predict(occ_avg,
                     newdata = newDatHab3,
                     type = "state")

Per_Forest <- seq(min(CR_Hab_BPI2$Per_Forest),max(CR_Hab_BPI2$Per_Forest), length=100)
plot(Per_Forest, occ_pred3$fit, xlab="Percent Forest 100m",
     ylab="Occupancy Probability", pch=16, ylim=0:1, type="l",
     # lwd=2, main = Species_CR[i])
     lwd=3, main = Species_Res_names[i],col=3)
lines(Per_Forest, occ_pred3$fit-occ_pred3$se.fit, lty=3,col=3)
lines(Per_Forest, occ_pred3$fit+occ_pred3$se.fit, lty=3,col=3)

}

# save(data1, data2, file = "data.RData")

sessionInfo()
