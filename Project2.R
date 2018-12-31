install.packages("gridExtra")

library(ggplot2)
library(gridExtra)
library(plyr)

setwd("~/coursera")

projectdata <- read.csv('repdata_data_StormData.csv')

summary(projectdata)


sum(projectdata$FATALITIES)

fdata <- ddply(projectdata,c("EVTYPE","FATALITIES","INJURIES","PROPDMGEXP","CROPDMGEXP"),summarise, F_sum=sum(FATALITIES),I_sum=sum(INJURIES),P_sum=sum(PROPDMG),C_sum=sum(CROPDMG))

fdata_Corr <- fdata
fdata_Corr$EVTYPE <- gsub("^WINTER STORM.*", "WINTER STORM",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("^(EXTREME|RECORD/EXCESSIVE|RECORD) HEAT$", "EXCESSIVE HEAT",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("^EXCESSIVE HEAT*", "HEAT",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("^HEAT WAVES?$|^UNSEASONABLY WARM$|^UNSEASONABLY WARM AND DRY$|^WARM WEATHER$", "HEAT",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("^TORNADO.*", "TORNADO",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("^FLOOD*|FLASH FLOOD", "FLOOD",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("TSTM|THUNDERSTORMS?|THUNDERSTORM WIND.*|THUNDERSTORM.*", "THUNDERSTORM",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("^ICE.*|BLIZZARD|HEAVY SNOW|HAIL", "WINTER STORM",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("EXTREME COLD/WIND CHILL", "EXTREME COLD",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("THUNDERSTORM WIND", "THUNDERSTORM",fdata_Corr$EVTYPE)
fdata_Corr$EVTYPE <- gsub("WILD/FOREST FIRE", "WILDFIRE",fdata_Corr$EVTYPE)

fdata_Corr$PROPDMGEXP <- gsub("B|b", "9",fdata_Corr$PROPDMGEXP)
fdata_Corr$CROPDMGEXP <- gsub("B|b", "9",fdata_Corr$CROPDMGEXP)
fdata_Corr$PROPDMGEXP <- gsub("M|m", "6",fdata_Corr$PROPDMGEXP)
fdata_Corr$CROPDMGEXP <- gsub("M|m", "6",fdata_Corr$CROPDMGEXP)
fdata_Corr$PROPDMGEXP <- gsub("K|k", "3",fdata_Corr$PROPDMGEXP)
fdata_Corr$CROPDMGEXP <- gsub("K|k", "3",fdata_Corr$CROPDMGEXP)
fdata_Corr$PROPDMGEXP <- gsub("H|h", "2",fdata_Corr$PROPDMGEXP)
fdata_Corr$CROPDMGEXP <- gsub("H|h", "2",fdata_Corr$CROPDMGEXP)
# fdata_Corr$PROPDMGEXP <- gsub("", "0",fdata_Corr$PROPDMGEXP)
# fdata_Corr$CROPDMGEXP <- gsub("", "0",fdata_Corr$CROPDMGEXP)

fdata2 <- ddply(fdata_Corr,c("EVTYPE"),summarise, F_sum=sum(FATALITIES))
fdata2 <- arrange(fdata2,fdata2[,2], decreasing=T)
fdata2 <- head(fdata2,n=15)

idata2 <- ddply(fdata_Corr,c("EVTYPE"),summarise, I_sum=sum(INJURIES))
idata2 <- arrange(idata2,idata2[,2], decreasing=T)
idata2 <- head(idata2,n=15)

edata2 <- ddply(fdata_Corr,c("EVTYPE","PROPDMGEXP","CROPDMGEXP"),summarise, P_sum=sum(P_sum),C_sum=sum(C_sum))

edata2$total <- edata2$P_sum*10^(as.numeric(edata2$PROPDMGEXP))+edata2$C_sum*10^(as.numeric(edata2$CROPDMGEXP))
edata2$ptotal <- edata2$P_sum*10^(as.numeric(edata2$PROPDMGEXP))
edata2$ctotal <- edata2$C_sum*10^(as.numeric(edata2$CROPDMGEXP))

edata3 <- ddply(edata2,c("EVTYPE"),summarise, T_sum=sum(total))

edata3 <- arrange(edata3,edata3[,2], decreasing=T)
edata3 <- head(edata3,n=15)

edata4 <- ddply(edata2,c("EVTYPE"),summarise, P_sum=sum(ptotal))

edata4 <- arrange(edata4,edata4[,2], decreasing=T)
edata4 <- head(edata4,n=15)

edata5 <- ddply(edata2,c("EVTYPE"),summarise, C_sum=sum(ctotal))

edata5 <- arrange(edata5,edata5[,2], decreasing=T)
edata5 <- head(edata5,n=15)

qplot(EVTYPE, F_sum, data=fdata2)

summary(fdata2$F_sum)

fatPlot <- qplot(EVTYPE, data = fdata2, weight = F_sum, geom = "bar") + 
    scale_y_continuous("Number of Fatalities") + 
    theme(axis.text.x = element_text(angle = 45, 
                                     hjust = 1)) + xlab("Severe Weather Type") + 
    ggtitle("Total Fatalities by Severe Weather\n Events in the U.S.\n from 1995 - 2011")
injPlot <- qplot(EVTYPE, data = idata2, weight = I_sum, geom = "bar") + 
    scale_y_continuous("Number of Injuries") + 
    theme(axis.text.x = element_text(angle = 45, 
                                     hjust = 1)) + xlab("Severe Weather Type") + 
    ggtitle("Total Injuries by Severe Weather\n Events in the U.S.\n from 1995 - 2011")
grid.arrange(fatPlot, injPlot, nrow = 2)

qplot(EVTYPE, data = edata3, weight = T_sum, geom = "bar") + 
    scale_y_continuous("Economic Impact") + 
    theme(axis.text.x = element_text(angle = 45, 
                                     hjust = 1)) + xlab("Severe Weather Type") + 
    ggtitle("Total Economic Impact by Severe Weather\n Events in the U.S.\n from 1995 - 2011")

PropPlot <- qplot(EVTYPE, data = edata4, weight = P_sum, geom = "bar") + 
    scale_y_continuous("Economic Impact") + 
    theme(axis.text.x = element_text(angle = 45, 
                                     hjust = 1)) + xlab("Severe Weather Type") + 
    ggtitle("Property Economic Impact by Severe Weather\n Events in the U.S.\n from 1995 - 2011")

CropPlot <- qplot(EVTYPE, data = edata5, weight = C_sum, geom = "bar") + 
    scale_y_continuous("Economic Impact") + 
    theme(axis.text.x = element_text(angle = 45, 
                                     hjust = 1)) + xlab("Severe Weather Type") + 
    ggtitle("Crop Economic Impact by Severe Weather\n Events in the U.S.\n from 1995 - 2011")
grid.arrange(PropPlot, CropPlot, nrow = 2)
