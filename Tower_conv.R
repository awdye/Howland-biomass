library(dplR)
library(sp)
library(foreign)
library(lattice)
library(ggplot2)
library(sciplot)
library(data.table)
        
        setwd("C:/Paleon/Howland/NPP")
        tower.data<-read.table("Howland Main Tower Flux.csv", header=T, sep=",", na.strings="NA")
        #flux tower reading are in micromoles/m2/sec, and readings are taken every 30 minutes
        #computes GPP and RE readings for each 30 minute period (1800 sec/30min)
        tower.data$GPPsec<-tower.data$GPP*1800
        tower.data$REsec<-tower.data$RE*1800
        #sums all readings aggregated by year
        tower.data.prod<-aggregate(tower.data$GPPsec, list(year_sum=tower.data$Year), sum)
        setnames(tower.data.prod, "x", "GPP")
        tower.data.RE<-aggregate(tower.data$REsec, list(year_sum=tower.data$Year), sum)
        setnames(tower.data.RE, "x", "RE")
        tower.data.prod$RE<-tower.data.RE$RE
        #NPP=GPP-RE
        tower.data.prod$NPP<-tower.data.prod$GPP-tower.data.prod$RE
        #convert to kilograms/ha
        tower.data.prod$GPPkgha<-tower.data.prod$GPP/1000000*0.0120107*10000
        tower.data.prod$REkgha<-tower.data.prod$RE/1000000*0.0120107*10000
        tower.data.prod$NPPkgha<-tower.data.prod$NPP/1000000*0.0120107*10000
        #removes 2005, which is missing data
        tower.data.prod=tower.data.prod[-10,]
        #start correlation analysis with flux data
        fluxyrs<-data.frame(ab.ha.site)
        ab.ha.data<-subset(fluxyrs, year == "2008" | year == "2007" | year == "2006" | year == "2004" | year == "2003" | year == "2002" | year == "2001" | year == "2000" | year == "1999" | year == "1998" | year == "1997" |year == "1996")
        plot(ab.ha.data$HOW1,tower.data.prod$NPPkgha)
        cor.test(ab.ha.data$HOW1,tower.data.prod$NPP, use="complete.obs", method="pearson")