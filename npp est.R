library(dplR)
library(sp)
library(foreign)
library(lattice)
library(ggplot2)
library(sciplot)
library(data.table)


setwd("C:/Paleon/Howland/NPP/Howland-biomass")
hw.field.data<-read.table("HWall_FieldData.csv", header=T,sep=",")

#AD:convert units to /ha. These lines are the same as Dario's original, except that I suggest using breaks
  #AD: by distance class instead of dbh class. 
#hw.field.data$distclass<-cut(hw.field.data$dist,breaks=c(0,9.999,12.999,19.999,99.999), right=T,
                             #labels=c(10,13,20,30),include.lowest=T)
#hw.field.data$convha<-cut(hw.field.data$dist,breaks=c(0.000,9.999,12.999,19.999,99.999), right=T,
                          #labels=10000/(pi*(c(10,13,20,30)^2)), include.lowest=T) #10000m2/ha, radii are subplot sizes
hw.field.data$convha<-10000/(pi*(20)^2)                  

hw.field.data$convha<-as.numeric(as.character(hw.field.data$convha))

#AD: These equations give biomass in kilograms, correct?
bm.eqs<-read.table("biomass_coeff.csv", header=T,sep=",")

dim(unique(subset(hw.field.data, select=trees, site=="HOW1")))
unique(subset(ab.hw.all, select=trees, site=="HOW1"))
subset(hw.field.data, site=="HOW1")

#total biomass of tree in 2013 based on dbh
hw.field.data<-merge(hw.field.data, bm.eqs.sp, by="species")
hw.field.data$totAB<-hw.field.data$a*(hw.field.data$dbh)^hw.field.data$b

# Function for current biomass
# al.eq is a data.frame with coefficients (a & b) for alometric equations of
# the form a*DBH^b
# dbh.data is a data.frame with dbh data

setwd("C:/Paleon/Howland/NPP/Howland-biomass/RawRW_Data_by_sp")
files_to_read<-list.files()
# files_to_read<-"PM_TulipPoplar.rw"

core.means.all<-NULL
for (i in files_to_read){
  file_read<-read.rwl(i)
  assign(i,file_read)
  
  cores<-data.frame("code"=names(file_read))
  cores$trees<- substr(as.character(cores$code),1,7)
  core.means<-data.frame("year"=row.names(file_read))
  core.means<-NULL
  n=1
  file_read$year<-row.names(file_read)
  for (j in unique(cores$trees)){
    print(j)
    print(n)
    tree1<-data.frame(file_read[,c(as.character(cores$code[cores$trees==j]),"year")])
    if(dim(tree1)[2]>1){
      tree2<-data.frame(rowMeans(subset(tree1, select=-c(year)),na.rm=T))
    } else {tree2=tree1}
    tree2<-na.omit(tree2)
    tree2$tree<-j
    tree2$year<-as.numeric(as.character(row.names(tree2)))
    tree2$rACUM<-0
    tree2$rACUM.inv<-0
    for(k in 1:dim(tree2)[1]){
      tree2$rACUM[k]<-sum(tree2[c(1:k),1])
      tree2$rACUM.inv[k-1]<-sum(tree2[c((k):(dim(tree2)[1])),1])
    }
    names(tree2)<-c("meanRW","trees","year","rACUM","rACUM.inv")
    if(n==1)(core.means<-tree2)else(core.means<-rbind(core.means, tree2))
    n=n+1}
  plot(core.means$rACUM.inv~core.means$year, type="p")
  title(i)
  
  core.means.all<-rbind(core.means.all,core.means)
}
core.means.all$site<-substr(as.character(core.means.all$trees),1,4)
dim(unique(subset(core.means.all, select=trees, site=="HOW1")))


core.means.all$trees<-as.numeric(as.character(substr(as.character(core.means.all$trees),5,7)))

ab.hw.all<-merge(core.means.all,hw.field.data, by=c("site","trees"))
bm.eqs.sp<-bm.eqs[bm.eqs$eq==1,]
ab.hw.all<-merge(ab.hw.all,bm.eqs.sp,by="species")

#dbhest1==> dbh at the BEGINNING of the growing season
ab.hw.all$dbhest1<-ab.hw.all$dbh-0.2*(ab.hw.all$rACUM.inv+ab.hw.all$meanRW)
warnings()
ab.hw.all$dbhest1[ab.hw.all$dbhest1<0]<-NA
#dbhest2==> dbh at the END of the growing season
ab.hw.all$dbhest2<-ab.hw.all$dbh-0.2*(ab.hw.all$rACUM.inv)
ab.hw.all$dbhest2[ab.hw.all$dbhest2<0]<-NA
ab.hw.all$AB<-ab.hw.all$a*ab.hw.all$dbhest2^ab.hw.all$b

#annAB==>biomass acculumated during the year.
# that is, biomass at the END of the growing season
# minus the biomass at the BEGINNING of the growing season
ab.hw.all$annAB<-ab.hw.all$a*(ab.hw.all$dbhest2^ab.hw.all$b-ab.hw.all$dbhest1^ab.hw.all$b)

#biomass per hectare assuming the plot
#represents the entire hectare
ab.hw.all$annAB.ha<-ab.hw.all$annAB*ab.hw.all$convha


#Calculates total biomass per ha and number of trees
#first we need to define two basic functions
sum.fn<-function(x) sum(x, na.rm=TRUE)
count.fn<-function(x) length(unique(x, na.rm=TRUE))


#AD: Original script sums the ab.hw.all$annAB column by year and site. I think this only gives us 
#total biomass increment/year, not the /ha we are interested in. Should we instead sum the ab.hw.all$annAB.ha column
# or wait until this step to use the convha?
ab.ha.site<-data.frame(tapply(X=ab.hw.all$annAB,INDEX=list(ab.hw.all$year,ab.hw.all$site),sum.fn))
ab.ha.site$year<-as.numeric(as.character(row.names(ab.ha.site)))
ab.ha.site[is.na(ab.ha.site)]=0
ab.ha.site$annAB.ha<-ab.ha.site$HOW1*7.9577

n.trees<-data.frame(tapply(X=ab.hw.all$trees,INDEX=list(ab.hw.all$year,ab.hw.all$site),count.fn))
n.trees$year<-as.numeric(as.character(row.names(n.trees)))
plot(n.trees[,5])
n.trees[is.na(n.trees)]=0

write.csv(hw.field.data, file = "Rfielddata.csv")
write.csv(ab.hw.all, file = "annABdata.csv")

#Calculations END HERE
#From here on it's JUST GRAPHS

p<-ggplot(data=ab.hw.all,aes(x=year, y=AB, color=species))
p+geom_point(size=1)+facet_wrap(~site)

p<-ggplot(data=ab.hw.all,aes(x=year, y=annAB,color=species))
p+geom_point(size=1)+facet_wrap(~site)

p<-ggplot(data=ab.hw.all,aes(x=year, y=annAB.ha,color=species))
p+geom_point(size=1)+facet_wrap(~site)

lineplot.CI(x.factor=year,response=annAB.ha,group=site,
            data=subset(ab.hw.all,site!="HOW1"&year>1960))

lineplot.CI(x.factor=year,response=meanRW,group=species,
            data=subset(ab.hw.all,site!="HOW1"&site!="HOW1"&year>1960))

lineplot.CI(x.factor=year,response=annAB,group=site,
            data=subset(ab.hw.all,site!="HOW1"&year>1960))


tot.ab.ha<-lineplot.CI(x.factor=year,response=annAB,group=site,
                       data=subset(ab.hw.all,site!="HOW1"&year>1960),fun=sum.fn,
                       ylab="",xlab="")
axis(1,at=seq(1,60,5),labels=F)
mtext(side=1,"Year",line=2)
mtext(side=2,"Annual above-ground biomass (kg/ha)",line=2)


ggplot(data=subset(ab.hw.all,site!="HOW1"),
       aes(y = annAB, x = year,color=site)) +
  stat_summary(fun.y = 'sum', fun.ymin = function(x) 0, geom = 'point',
               aes(fill =site), position = 'dodge',
               fun.ymin = min, fun.ymax = max) 

