#scratch code for Weather event analysis work
#see .Rmd for final code
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
#uncomment this to load data first time
#data <- read.csv('repdata-data-StormData.csv')
datasub <- data[,c(2,8,23:28,37)]
datasub$EVTYPE <- toupper(datasub$EVTYPE)
datasub$CROPDMGEXP <- toupper(datasub$CROPDMGEXP)
datasub$PROPDMGEXP <- toupper(datasub$PROPDMGEXP)
datasub$BGN_DATE <- as.Date(datasub$BGN_DATE,"%m/%d/%Y")
datasub$YEAR <- year(datasub$BGN_DATE)
datasub$EVTYPEMAP <- as.character(NA)
EXP <- c("","-","?","+","0","1","2","3","4","5","6","7","8","H","K","M","B")
VAL <- c(0,0,0,1,rep(10,9),100,1000,1000000,1000000000)
Lookup <- data.frame(cbind(EXP,VAL))
Lookup$EXP <- as.character(Lookup$EXP)
Lookup$VAL <- as.numeric(as.character(Lookup$VAL))
datasub$CROPFAC <- Lookup[match(datasub$CROPDMGEXP,Lookup$EXP),]$VAL
datasub$PROPFAC <- Lookup[match(datasub$PROPDMGEXP,Lookup$EXP),]$VAL
datasub$CROPAMOUNT <- (datasub$CROPDMG * datasub$CROPFAC)/1000000
datasub$PROPAMOUNT <- (datasub$PROPDMG * datasub$PROPFAC)/1000000
dataevt <- datasub[which((datasub$FATALITIES+datasub$INJURIES+datasub$CROPAMOUNT+datasub$PROPAMOUNT>0) & datasub$YEAR>=1996),]
evtypes <- dataevt %>% group_by(EVTYPE) %>% summarise(length(unique(REFNUM)),sum(FATALITIES),sum(INJURIES),sum(CROPAMOUNT+PROPAMOUNT))
colnames(evtypes) <- c('EVTYPE','EVENTS','FATALITIES','INJURIES','AMOUNT')

evtypemap <- read.csv('evtypemap.csv',comment.char="#")
evtypemap$MAPTO <- as.character(evtypemap$MAPTO)
evtypes$EVTYPEMAP <- as.character(NA)
for (i in 1:nrow(evtypemap)) {
	repl <- with(evtypes,grepl(evtypemap[i,]$SEARCH,EVTYPE) & is.na(EVTYPEMAP))
	print(paste0('Mapping: ',sum(repl),' rows ',evtypemap[i,]$SEARCH,' to ',evtypemap[i,]$MAPTO))
	if (sum(repl) > 0) {
		evtypes[which(repl),]$EVTYPEMAP <- evtypemap[i,]$MAPTO
	}
}
print(paste(sum(!is.na(evtypes$EVTYPEMAP)),' mapped'))
print(paste(sum(is.na(evtypes$EVTYPEMAP)),' unmapped'))
evsum <- as.data.frame(evtypes %>% group_by(EVTYPEMAP) %>% summarise(sum(EVENTS),sum(FATALITIES),sum(INJURIES),sum(AMOUNT)))
colnames(evsum) <- c('EVTYPE_MAPPED','EVENTS','FATALITIES','INJURIES','AMOUNT')

evsum_pct <- cbind(
        evsum[c(1,2)],
        round(prop.table(evsum[3])*100,2),
        round(prop.table(evsum[4])*100,2),
        round(prop.table(evsum[5])*100,2))

evsum$TOP_HEALTH <- FALSE
evsum$TOP_AMOUNT <- FALSE
evsum[order(-evsum$AMOUNT),][c(1:5),]$TOP_AMOUNT <- TRUE
evsum[order(-evsum$FATALITIES),][c(1:5),]$TOP_HEALTH <- TRUE
evsum[order(-evsum$INJURIES),][c(1:5),]$TOP_HEALTH <- TRUE

evtop_cnt<-evsum[which(evsum$TOP_HEALTH | evsum$TOP_AMOUNT),]
evsum_pct <- cbind(
	evsum[1],
	round(prop.table(evsum[3])*100,2),
	round(prop.table(evsum[4])*100,2),
	round(prop.table(evsum[5])*100,2),
	evsum[c(6,7)])

evtop<-evsum_pct[which(evsum_pct$TOP_HEALTH | evsum_pct$TOP_AMOUNT),]
evtoptable<-as.data.frame(evtop[c(1:4)] %>% gather(IMPACT,COUNT,-EVTYPE_MAPPED))

#evtoptable1<-as.data.frame(evtop[c(1,2,3)] %>% gather(HEALTH_IMPACT,COUNT,-EVTYPE_MAPPED))
#evtoptable2<-as.data.frame(evtop[c(1,1,5)])
#colnames(evtoptable2) <- colnames(evtoptable1)
#evtoptable2$HEALTH_IMPACT <- 'ECON IMPACT'
#evtoptable<-rbind(evtoptable1,evtoptable2)

#evtop_amt <- evsum[order(-evsum$AMOUNT),][c(1:10),]
#evtop_fat <- evsum[order(-evsum$FATALITIES),][c(1:10),]
#evtop_inj <- evsum[order(-evsum$INJURIES),][c(1:10),]

#g <- ggplot(evtoptable,aes(x=reorder(EVTYPE_MAPPED,-COUNT),y=COUNT,fill=HEALTH_IMPACT))
#g <- ggplot(evtoptable,aes(x=EVTYPE_MAPPED,y=COUNT,fill=IMPACT))
#g <- g + geom_bar(stat="identity",position="dodge") + coord_flip()

g <- ggplot(evtoptable,aes(x=EVTYPE_MAPPED,y=COUNT,fill=IMPACT))
g <- g + geom_bar(stat="identity")
g <- g + facet_grid(rows = vars(IMPACT))
g <- g + labs(x="Weather Event (Top 5 Injuries, Fatalities or Economic Impact)",y="% of Total")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g <- g + geom_hline(yintercept=10)
print(g)
