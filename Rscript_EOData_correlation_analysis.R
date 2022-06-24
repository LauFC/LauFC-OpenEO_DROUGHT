#Load libraries
library(corrplot)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(patchwork)

#Load the *.RDATa files processed previously 
load("C:/Rprojects/RStudio_EOdata_GitHub/OpenEO_DROUGHT/df_atm._10days.RData")
load("C:/Rprojects/RStudio_EOdata_GitHub/OpenEO_DROUGHT/class_mapEO.RData")
load("C:/Rprojects/RStudio_EOdata_GitHub/OpenEO_DROUGHT/faparEO.RData")
load("C:/Rprojects/RStudio_EOdata_GitHub/OpenEO_DROUGHT/dem_EOData.RData")

#Define common time interval for the calculation
#Start date: limmited by the start of the atmospheric data series starting at the statistics of the first 10 days of January 2015, coinciding with the fapar 2015/01/10 data collection (unclass date: 16445).
#end date: limited by the end of the period of available fapar data 2020/06/30.

#Data filtering
fapar.filtered <- fapar[c(fapar$undate >=16445),c(1:6)]
fapar.arranged <-arrange(fapar.arranged,tile) 
head (fapar.arranged)
tail (fapar.arranged)
#filtering atmospheric data ending 2020/06/30
atm.filtered <- df.atm.10

atm.filtered.id <- unite(atm.filtered, variables,c(2,3),  sep = "_", remove = TRUE)
atm.filtered$id <- atm.filtered.id$variables
#first we remove the year 2021 and 2022
#then, we remove from all tiles the values greater than 2020_06 in an iterative way.
atm.filtered <- atm.filtered [!c(atm.filtered$year == "2022"),]
atm.filtered <- atm.filtered [!c(atm.filtered$year == "2021"),]  
atm.filtered <- atm.filtered [!c(atm.filtered$id == "2020_7"),]  
atm.filtered <- atm.filtered [!c(atm.filtered$id == "2020_8"),]
atm.filtered <- atm.filtered [!c(atm.filtered$id == "2020_9"),]
atm.filtered <- atm.filtered [!c(atm.filtered$id == "2020_10"),]
atm.filtered <- atm.filtered [!c(atm.filtered$id == "2020_11"),]
atm.filtered <- atm.filtered [!c(atm.filtered$id == "2020_12"),]

head (atm.filtered)
tail (atm.filtered)

#now fapar.filtered and atm.filtered have the same number of records 343926 
#we can join them together because they are sorted in the same order

df.fapar.atm.EOData <- atm.filtered
df.fapar.atm.EOData$date <- fapar.arranged$date
df.fapar.atm.EOData$fapar <- fapar.arranged$value


#We link with land use data
head(class_mapEO)
df.landcover <- class_mapEO[,-c(1)]
df.landcover <-arrange(df.landcover, tile)
df.landcover$tile <- as.factor(df.landcover$tile)
head(df.landcover)
str(df.landcover)

df.fapar.atm.LC.EOData <-full_join(df.fapar.atm.EOData, df.landcover, by="tile") 

#We link with dem data
dem_EOData$tile <- as.factor(dem_EOData$tile)
df.fapar.atm.LC.dem.EOData <-full_join(df.fapar.atm.LC.EOData, dem_EOData, by="tile")
head(df.fapar.atm.LC.dem.EOData)
str(df.fapar.atm.LC.dem.EOData)

#select the data to be analysed and reorder the columns
df.EOData <- df.fapar.atm.LC.dem.EOData[,c(1,19,20,21,16,17,18,2:4,13,6:12)]
head(df.EOData)
tail(df.EOData)
save(df.EOData, file="df_EOData.RData")


#Correlation analysis in general terms-------------------------------------------------------------------------------------------------------
#correlation matrix of all land uses on all dates
matrixCor <- round(cor(df.EOData[,c(12:18,11,5)]),2) #round results to two decimals
matrixCor
#Graphic result
corrplot(matrixCor,
         method = "color",
         order = "original",
         #col = brewer.pal(n = 8, name = "PiYG"),
         cl.pos = "r",
         tl.col = "black",
         addCoef.col = "black",
         type="upper",
         diag = FALSE)

#To export the graph (not ggplot)
png("corrgraph.png", width = 15, height = 15, units = 'cm', res = 400, bg="transparent")
corrplot(matrixCor,
         method = "color",
         order = "original",
         #col = brewer.pal(n = 8, name = "PiYG"),
         cl.pos = "r",
         tl.col = "black",
         addCoef.col = "black",
         type="upper",
         diag = FALSE)
dev.off()

#Correlation analysis according to land use-------------------------------------------------------------------------------------------------------
#Divide the database into the different land uses
df.EOData.LC.Crop <- df.EOData[c(df.EOData$RCober=="Cropland"),]
df.EOData.LC.DBFc <- df.EOData[c(df.EOData$RCober=="DBFc"),]
df.EOData.LC.DBFo <- df.EOData[c(df.EOData$RCober=="DBFo"),]
df.EOData.LC.ENFc <- df.EOData[c(df.EOData$RCober=="ENFc"),]
df.EOData.LC.ENFo <- df.EOData[c(df.EOData$RCober=="ENFo"),]
df.EOData.LC.Forc <- df.EOData[c(df.EOData$RCober=="Fc"),]
df.EOData.LC.Foro <- df.EOData[c(df.EOData$RCober=="Fo"),]
df.EOData.LC.Herb <- df.EOData[c(df.EOData$RCober=="Herbaceous"),]
df.EOData.LC.HerW <- df.EOData[c(df.EOData$RCober=="HerbaceousW"),]
df.EOData.LC.Shrb <- df.EOData[c(df.EOData$RCober=="Shrubs"),]
df.EOData.LC.Urba <- df.EOData[c(df.EOData$RCober=="Urban"),]
df.EOData.LC.Wate <- df.EOData[c(df.EOData$RCober=="Water"),]

head(df.EOData.LC.Crop)

matrixCor.Crop <- round(cor(df.EOData.LC.Crop[,c(5,12:18,11)]),2)
matrixCor.DBFc <- round(cor(df.EOData.LC.DBFc[,c(5,12:18,11)]),2)
matrixCor.DBFo <- round(cor(df.EOData.LC.DBFo[,c(5,12:18,11)]),2)
matrixCor.ENFc <- round(cor(df.EOData.LC.ENFc[,c(5,12:18,11)]),2)
matrixCor.ENFo <- round(cor(df.EOData.LC.ENFo[,c(5,12:18,11)]),2)
matrixCor.Forc <- round(cor(df.EOData.LC.Forc[,c(5,12:18,11)]),2)
matrixCor.Foro <- round(cor(df.EOData.LC.Foro[,c(5,12:18,11)]),2)
matrixCor.Herb <- round(cor(df.EOData.LC.Herb[,c(5,12:18,11)]),2)
matrixCor.HerW <- round(cor(df.EOData.LC.HerW[,c(5,12:18,11)]),2)
matrixCor.Shrb <- round(cor(df.EOData.LC.Shrb[,c(5,12:18,11)]),2)
matrixCor.Urba <- round(cor(df.EOData.LC.Urba[,c(5,12:18,11)]),2)
matrixCor.Wate <- round(cor(df.EOData.LC.Wate[,c(5,12:18,11)]),2)


#Represent only the correlation data of each variable with the fapar index
matrixCor.row.array <- row.names(matrixCor.Crop)
matrix.cor.fapar <- cbind( matrixCor.Crop[,c(1)],
                           matrixCor.DBFc[,c(1)],
                           matrixCor.DBFo[,c(1)],
                           matrixCor.ENFc[,c(1)],
                           matrixCor.ENFo[,c(1)],
                           matrixCor.Forc[,c(1)],
                           matrixCor.Foro[,c(1)],
                           matrixCor.Herb[,c(1)],
                           matrixCor.HerW[,c(1)],
                           matrixCor.Shrb[,c(1)],
                           matrixCor.Urba[,c(1)],
                           matrixCor.Wate[,c(1)])

colnames(matrix.cor.fapar) <- c("Crop",
                                "DBFc",
                                "DBFo",
                                "ENFc",
                                "ENFo",
                                "Forc",
                                "Foro",
                                "Herb",
                                "HerW",
                                "Shrb",
                                "Urba",
                                "Wate")
matrix.cor.fapar.melted <- melt(matrix.cor.fapar[-(1),]) #exclude row 1 (fapar index)
colnames(matrix.cor.fapar.melted) <- c("Index", "Landcover", "Correlation")

#Graphic result
CoorGraph <- ggplot(matrix.cor.fapar.melted,
             aes(x=Landcover, y=Index, fill=Correlation))+
  geom_tile(color="white")+
  geom_text(aes(label = sprintf("%.2f",Correlation)),position = "identity", size = 4)+
  #scale_fill_gradient("RMSE \n(m)",low="mediumspringgreen", high="tomato3")+
  scale_fill_gradient2(low="red", high="blue", mid="#f5f5f5", midpoint = 0, limit=c(-1,1), name="")+
  theme_minimal()+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.x =  element_text(angle=90,vjust=0,
                                    #face = "bold",
                                    size = 12, hjust = 1, colour = "black"))+
  theme(axis.text.y =element_text(angle=0,vjust=0,colour = "black", #face = "bold", 
                                  size = 12, hjust = 1))+
  theme(axis.title=element_text(size=12))+ #texto título de los ejes
  theme(legend.title=element_text(size=12))+ #texto título leyenda
  theme(legend.text=element_text(size=12))+  #texto items leyenda
  theme(strip.text = element_text(size=12))+ #texto titulos facet wrap
  xlab("Land Cover")+
  ylab("Variable")+
  ggtitle("FAPAR Correlation values")+
  theme(legend.position = "right")+
  theme(legend.justification = "top")+
  coord_fixed(ratio=0.5)
CoorGraph

#Percentage of each land use within the study area
LC.percent.array <- c(0.225676454,	0.006332758,	0.005181347,	0.011514105,	0.00115141,	0.046632124,	0.322394934,	0.244099021,	0.001727116,	0.112838227,	0.014968336,	0.007484168)
LC.percent <- as.data.frame(LC.percent.array)
row.names(LC.percent) <- c("Crop",
                           "DBFc",
                           "DBFo",
                           "ENFc",
                           "ENFo",
                           "Forc",
                           "Foro",
                           "Herb",
                           "HerW",
                           "Shrb",
                           "Urba",
                           "Wate")

LC.percent$percent <- LC.percent$LC.percent.array*100

LC.percent$LC <- rownames(LC.percent)
LC.percent.melted <- melt(LC.percent[,c(2,3)])

#
LCgraph <- ggplot(LC.percent.melted) +
  aes(x = LC, weight = value) +
  geom_bar(fill = "#252525") +
  theme_minimal()+
  xlab("")+
  ylab("%")+
  ggtitle("Land cover in the study area")+
  theme(legend.position = "right")+
  theme(legend.justification = "top")+
  coord_fixed(ratio=0.05)
LCgraph
#To combine two ggplot graphics into the same graphic
library(patchwork)
(LCgraph/CoorGraph) 
ggsave("FAPAR_coor.png", width = 12, height =8, dpi = 400) 




