dstemp <- ds
ds <- dstemp

library(Hmisc); library(ggplot2); library(caret);

# FUNCTIONS


#sum up detailled data per couple (campaign, App)
groupMeasuresPerCampPerApp <- function(data, subcriteria){
      groupingby <- list(cid = data$CampaignID, aid = data$AppOriginID, ctry = data$Country)
      subsetFrom <- length(groupingby) + 1;
      subsetTo <- subsetFrom;
      if("Date" %in% colnames(data))
      {
            groupingby$date = data$Date
            subsetFrom <- subsetFrom + 1;
            subsetTo <- subsetFrom + 1;
      }
      print(groupingby)
      if(missing(subcriteria)){
            #groupingby <- list(cid = data$CampaignID, aid = data$AppOriginID, ctry = data$Country)
      }
      else{
            if(!is.vector(subcriteria) || (is.vector(subcriteria) && length(subcriteria) == 1)){
                  groupingby$sub = data[[subcriteria]]#list(cid = data$CampaignID, aid = data$AppOriginID, ctry = data$Country, sub = data[[subcriteria]])
                  subsetFrom <- subsetFrom + 1;
                  subsetTo <- subsetFrom + 1;
            }
            else{
                  #groupingby <- list(cid = data$CampaignID, aid = data$AppOriginID, ctry = data$Country)
                  
                  print(length(subcriteria))
                  for(i in 1:length(subcriteria)){
                        print(subcriteria[i])
                        groupingby <- c(groupingby, sub = data[ subcriteria[i] ]) #subcriteria[i] =
                        names(groupingby)[i+subsetFrom-1] <- subcriteria[i]
                  }
                  print(names(groupingby))
                  #subsetTo = subsetFrom + length(subcriteria) + 1
                  subsetFrom = subsetFrom + length(subcriteria) #+ 1
                  susbsetTo = subsetFrom + 1
                  #print(groupingby)
            }
      }
      list <- list(
            earnings = aggregate(data$earningsGlob, groupingby, sum),
            displays = aggregate(data$displaysGlob, groupingby, sum),
            clicks = aggregate(data$clicksGlob, groupingby, sum),
            installs = aggregate(data$installsGlob, groupingby, sum),
            leads = aggregate(data$leadsGlob, groupingby, sum),
            sessions = aggregate(data$sessionsGlob, groupingby, sum),
            new_users = aggregate(data$newUsersGlob, groupingby, sum)
      )
      print("herte")
      #print(list)
      #as.data.frame(list)
      group_by <- c("cid", "aid", "ctry")
      naming <- c("CampaignID", "AppOriginID", "Country", "earnings", "displays", "clicks", "installs", "leads", "sessions", "new_users")
      insertPos <- 3;
      if("Date" %in% colnames(data))
      {
            group_by <- c(group_by, "date")
            naming <- c("CampaignID", "AppOriginID", "Country", "Date", "earnings", "displays", "clicks", "installs", "leads", "sessions", "new_users")
            subsetFrom <- subsetFrom+1; subsetTo <- subsetTo+1;
            insertPos <- insertPos + 1;
      }
      subsetSel <- c(subsetFrom, subsetTo);
      print(subsetSel)
      #print(group_by)
      
      merger <- function(group_by){
            merge(x=list$earnings, 
                  y=merge(x=list$displays[,-subsetSel], 
                          y=merge(x=list$clicks[,-subsetSel], 
                                  y=merge(x=list$installs[,-subsetSel], 
                                        y=merge(x=list$leads[,-subsetSel], 
                                                y=merge(x=list$sessions[,-subsetSel], y=list$new_users[,-subsetSel], by=group_by),
                                                by=group_by),
                                        by=group_by),
                                  by=group_by),
                          by=group_by),
                  by=group_by)
      }
      
      if(missing(subcriteria))
      {
            res <- merger(group_by);
            names(res) <- naming
      }
      else if(!is.vector(subcriteria) || (is.vector(subcriteria) && length(subcriteria) == 1)){
            print("not vector")
            group_by <- c(group_by, c("sub")) # c("cid", "aid", "ctry", "sub")
            print(group_by)
            res <- merger(group_by);
            print(res)
            ###???groupingby <- list(cid = data$CampaignID, aid = data$AppOriginID, ctry = data$Country, sub = data[[subcriteria]])
            names(res) <- append(naming, subcriteria, insertPos)#c("CampaignID", "AppOriginID", "Country", subcriteria, "earnings", "displays", "clicks", "installs", "leads")
      }
      else{
            print("vector")
            print(subcriteria)
            group_by <- c(group_by, subcriteria)#c(group_by, c("sub"))
            print(group_by)
            #print(row.names(list))
            res <- merger(group_by);
            names(res) <- append(naming, subcriteria, insertPos)
            #names(res) <- ins(
            #  c("CampaignID", "AppOriginID", "Country", "earnings", "displays", "clicks", "installs", "leads"), 
            #  list(subcriteria), pos=c(3)
            #)
            print(names(res))
      }
      cpb <- sapply(as.numeric(res$CampaignID), function(x){mean(data[as.numeric(data$CampaignID)==x,]$CostPerBid)})
      #print(cpb)
      res <- cbind(res, costPerBid = cpb)#data[res$CampaignID == data$CampaignID,]$CostPerBid[[1]])
      #print(res$costPerBid)
      res
}
groupKPIsPerCampPerApp <- function(data, subcriteria){
      msrs <- groupMeasuresPerCampPerApp(data, subcriteria);
      #print(msrs)
      ##print(head(data))
      #print(head(data[which(data$CampaignID == msrs$CampaignID),]$CampaignID[1]))
      msrs <- cbind(msrs, 
                    dispPerSess = ifelse(msrs$displays > 0, msrs$displays / msrs$sessions, NaN),
                    CPM = ifelse(msrs$displays > 0, msrs$earnings / msrs$displays * 1000, NaN),
                    CTR = ifelse(msrs$displays > 0, msrs$clicks / msrs$displays, NaN),
                    CVR = ifelse(msrs$clicks > 0, (msrs$installs + msrs$leads) / msrs$clicks, NaN),
                    CPMnorm = ifelse(msrs$displays > 0, (msrs$earnings / msrs$displays * 1000)/msrs$costPerBid, NaN) #(data[which(data$CampaignID == msrs$CampaignID)[1],]$CostPerBid)
      )
      unique(msrs)
}

baysianPredict <- function(){
      #library(e1071)
      #library(caret)
      dsSplitPerSubCriteriaCPMgrouped <- cbind(dsSplitPerSubCriteria, CPMnorm_ = cut(dsSplitPerSubCriteria$CPMnorm, breaks=-1:6), rm.na=T)#(max(dsSplitPerSubCriteria$CPMnorm)+1)
      #cut(dsSplitPerSubCriteria$CPMnorm, breaks=-1:(max(dsSplitPerSubCriteria$CPMnorm)+1))
      datasplit <- createDataPartition(y=dsSplitPerSubCriteriaCPMgrouped$CPMnorm, list=F, p=.7)
      trainset <- dsSplitPerSubCriteriaCPMgrouped[datasplit,]
      testset <- dsSplitPerSubCriteriaCPMgrouped[-datasplit,]
      modelLDA <- train(CPMnorm_ ~ displays, data=trainset, method='lda')
      plda <- predict(modelLDA, newdata=testset)
}

getAverageCPM <- function(data){
      mean <- 0
      for(i in 1:dim(data)[1]){
            mean <- mean + data[i,]$displays * data[i,]$CPMnorm;
      }
      mean <- mean / sum(data$displays)
      mean
}


prepareDS <- function(ds){
      nonFeaturesCol <- c("ConnectionTypeName", "AppOriginID", "CampaignID", "Country", "AppAdvertisedID", "requestsGlob", "displaysGlob", "clicksGlob", "installsGlob", "leadsGlob", "earningsGlob", "spendingsGlob", "CPM", "CTR", "CVR") #, "CostPerBid"
      
      # define the 'ni' operator
      `%ni%` <- Negate(`%in%`)
      dsPrepared <- subset(ds, select=names(ds) %ni% nonFeaturesCol)
      dsPrepared
}
setDS <- function(csvFile){
      if(is.character(csvFile)){
            ds <- read.csv(csvFile)#'StatsForLaunchingPeriodWithConnType.csv') #('CampaignsFirstStats2.csv')#
      }
      #ds <- subset(ds, select=-c(CountryCode))#AppAdvertisedType, AppOriginType,
      #ds <- na.omit(ds)
      
      # we restrict our analysis to the most active object of the platform
      visibility.top30AppOriginIDs <- as.factor(c(2241 ,2272 ,2459, 977, 2300, 2001 ,2334 ,2332, 2284, 2363, 2458, 2256, 2539, 2320, 2495, 2500 ,1341 ,2508, 2468, 2485, 2523, 2237, 2462 ,2497, 2402, 2257 ,2464, 2452, 2514, 2367))
      visibility.top30CampaignIDs<- as.factor(c(2, 2441, 2401,2443,2453,2129,2033,2114,2258,2091,2448, 2452, 2083, 2093,2249, 2260, 2427, 2084, 2388, 2433))
      #print(summary(ds))
      ds <- subset(ds, 
                   displaysGlob >= 0
                   ## & (leadsGlob + installsGlob) < (clicksGlob * 0.2)
                   ##& clicksGlob < (displaysGlob * 0.2)
                   #& AppOriginID %in% visibility.top30AppOriginIDs
                   #& CampaignID %in% visibility.top30CampaignIDs
      )
      
      # setting categorical factors #
      #transform(ds, CampaignID = as.factor(CampaignID))
      ds$CampaignID <- as.factor(ds$CampaignID)
      ds$AppOriginID <- as.factor(ds$AppOriginID)
      ds$CampaignCostTypeName <- factor(ds$CampaignCostTypeName, levels=c(
            "CPM",
            "CPC (multiple / campaign)",
            "CPI (unique / campaign)",
            "CPL (multiple / campaign / productID) - any kind of goals reached"
      ))
      if("Date" %in% colnames(ds))
      {
            ds$Date <- as.Date(as.POSIXct(ds$Date, 'GMT'))
      }
      ds$AppAdvertisedID <- as.factor(ds$AppAdvertisedID)
      ds$AppOriginOrientation <- as.factor(ds$AppOriginOrientation)
      ds$AppAdvertisedOrientation <- as.factor(ds$AppAdvertisedOrientation)
      #ds$AppAdvertisedType <- as.factor(ds$AppAdvertisedType)
      #normalise CPM per cost#
      ds <- cbind(ds, CPMnorm = ifelse(ds$CostPerBid > 0, (ds$CPM / ds$CostPerBid), NA))
      ds
}

simpleRegression<- function(data, outcome, criteria){
      LM<- lm(eval(as.symbol(outcome))~eval(as.symbol(criteria)), data)
      plot(data[[criteria]], data[[outcome]], ylim=c(quantile(data[[outcome]], .05,na.rm = T),quantile(data[[outcome]], .95,na.rm = T)))#,  geom="boxplot"), col=colorRamp(c('red', 'blue'))(0.5) 
      #plot(data[[criteria]], data[[outcome]], ylim=c(0,1))#,  geom="boxplot"), col=colorRamp(c('red', 'blue'))(0.5) 
      
      plot(data[[criteria]], LM$fitted, pch=18, cex=3, col=5, add=TRUE) #col=colorRamp(c('red', 'blue'))(0.5),
      
      for(i in 1:length(unique(data[[criteria]]))){
            currentMod <- unique(data[[criteria]])[i];
            sel <- data[[criteria]]
            text(sel[sel== currentMod], min(data[[outcome]]), paste('Tot:',length(sel[sel== currentMod]), sep=":"), col=i)
      }
      print(LM)
}
multipleRegression <- function(data, outcome, criteria){
      
      LM<- lm(eval(as.symbol(outcome))~., data=subset(data, select=names(data) %ni% c(nonFeaturesCol, names(selection))))
      #plot(data[[criteria]], LM$fitted, type="l", lwd=3, col="red")
      qplot(data[[criteria]], LM$fitted)
      
      # compare prediction and mean
      lapply(split(similarCampaigns, similarCampaigns[criteria]),
             function(x){
                   print(x[1, criteria])
                   print(list( pred=getAverageCPM(x), mean = mean(x$CPMnorm), med = median(x$CPMnorm[x$CPMnorm>0])))
             })
}

## END FUNCTIONS


#feature plot#
featurePlot(x=ds[,c('CampaignTargetTypeName', 'AppAdvertisedCategory', 'AppOriginCategory')], y=ds$CPMnorm, plot="pairs")

ds <- setDS('AppsStatsPerDaySinceFirstJuly.csv')#('StatsOverFullTimeWithConnType.csv')#('StatsForLaunchingPeriodWithConnType.csv')
# we focus on only 1 appOrigin
ds1app <- ds[ds$AppOriginID == '2300',]

# selection of campaigns being exactly the same
selection=list(
      CampaignTypeName= "Full screen interstitial (in-app)"
      ,CampaignSubTypeName = "[PAID] AdDeals user acquisition campaign [quality users]"
      ,PlatformName = "Windows [Phone/Tablets/PCs]"
      ,AdTypeName="AdDeals Interstitials Full Screen (Native)"
      ,AppAdvertisedCategory = "Games"
      ,AppAdvertisedOrientation = "1"
      ,AppOriginCategory = "Games"
      ,AppOriginOrientation = "2"
      #,CampaignCostTypeName = "CPI (unique / campaign)"
      #,CampaignTargetTypeName = "Web mobile"
      #,ConnectionTypeName = "Ethernet"
      #,WeekDay = "Mond./Tues."
      #,grepl("US", CountrySet) == T
)
selectionNames <- names(selection)

#similarCampaigns <- ds; lapply(selectionNames, function(nameindex){similarCampaigns <- subset(similarCampaigns, similarCampaigns[nameindex]==selection[[nameindex]])}) #ds[x.name]=
similarCampaigns <- subset(ds, 
                           CampaignTypeName== "Full screen interstitial (in-app)" 
                           & CampaignSubTypeName == "[PAID] AdDeals user acquisition campaign [quality users]" 
                           & PlatformName == "Windows [Phone/Tablets/PCs]" 
                           #& AppAdvertisedCategory == "Games" 
                           #& AppOriginCategory == "Games" 
                           & AdTypeName=="AdDeals Interstitials Full Screen (Native)"
                           #& AppAdvertisedOrientation == "1"
                           #& AppOriginOrientation == "2"  
                           #& CampaignCostTypeName == "CPI (unique / campaign)" 
                           #& CampaignTargetTypeName == "Web mobile" 
                           
                           #& ConnectionTypeName == "Ethernet" 
                           #& WeekDay == "Satu./Sund."#"Mond./Tues."
                           & (grepl("US", CountrySet) == T)# || grepl("US", CountrySet) == T)
)# settle a dynamic criteria of study

criteria = c("AppAdvertisedOrientation","CampaignTargetTypeName", "CampaignCostTypeName", "AppOriginCategory" )#"CampaignTargetTypeName"#"CostPerBid"#"WeekDay" #"CampaignTargetTypeName"
#criteria = "AppAdvertisedID"
##dsSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(similarCampaigns, criteria),displays>1000)

ds1appSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(ds1app, criteria),displays>1000)

table <- with(ds1appSplitPerSubCriteria, tapply(CTR, list(CampaignID, Date), FUN=mean))

      # seeking for CTR per Campaigns..
      ggplot(data = ds1appSplitPerSubCriteria, aes(CTR)) + geom_density(aes(color=Country)) + facet_grid(. ~ CampaignID)
      ggplot(data = ds1appSplitPerSubCriteria, aes(CTR, AppAdvertisedID)) + geom_point(aes(color=CampaignID)) 
      
      ggplot(data = ds1appSplitPerSubCriteria, aes(CTR)) +
       geom_histogram(aes(color=CampaignID)) +
       facet_grid(. ~ AppAdvertisedID) +
       ggtitle('Daily CTR per AppAdvertisedID per Campaign') +
       geom_smooth(aes(y=sessions/median(sessions)), colour='#ff9422', se=T) + #/10000
       geom_smooth(aes(y=new_users/median(new_users)), colour='#422ff9', linetype='dotted') #/100
      #+ scale_line_manual(labels = c("NewUsers", "Sessions"), values = c("#422ff9", "#ff9422")) 
      #ggsave(paste(gsub(' ', '_', 'Daily CTR per Country per Campaign'), 'jpg', sep="."))
lmPerCountry <- function(){
      usaData <- dsSplitPerSubCriteria[dsSplitPerSubCriteria$Country=='USA', ];
      partCountry <- createDataPartition(p=0.7,y=usaData$CPMnorm, list =F)
      trainsetCountry <- dsSplitPerSubCriteria[partCountry,];
      testsetCountry <- dsSplitPerSubCriteria[-partCountry,]
      lmCountry <- train(CPMnorm ~ AppAdvertisedOrientation + AppOriginCategory + CampaignCostTypeName, data = trainsetCountry, method='lm')
      predictCountry <- predict(lmCountry, newdata = testsetCountry)
      
      summary(lmCountry)
      lmCountry$finalModel$residuals
      
}
predictPerCountry <- lmPerCountry()
predict(lm, newdata = testset)
#reagregate per cai, aid having nbDiqsplays >  1000 
#dsSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(similarCampaigns, c(criteria, "AppOriginOrientation")),displays>1000)


ggplot(data=dsSplitPerSubCriteria, eval(as.symbol(criteria)), CPMnorm) + #aes(CPM, CPMnorm)) +
      geom_point(aes(color=AppOriginID, shape=eval(as.symbol(criteria)), size=displays), alpha = 0.6) +
      coord_cartesian(xlim=c(0,4), ylim=c(0,7.5)) +
      facet_wrap(~CampaignTargetTypeName) 
#+ ggtitle("CampaignTypeName: Full screen interstitial (in-app) / CampaignSubTypeName: [PAID] AdDeals user acquisition campaign [quality users] / PlatformName == Windows [Phone/Tablets/PCs] / AppAdvertisedCategory: Games/ AppOriginCategory: Games / AdTypeName: AdDeals Interstitials Full Screen (Native) / AppAdvertisedOrientation: 1 / AppOriginOrientation: 2 / CampaignCostTypeName: CPI (unique / campaign)") 
#+  facet_wrap(~CampaignID)

simpleRegression(dsSplitPerSubCriteria, 'CPMnorm', criteria) 

dsSplitPerCriteria <- subset(groupKPIsPerCampPerApp(similarCampaigns),displays>1000)
ggplot(data=dsSplitPerCriteria, aes(shape=eval(as.symbol(criteria)), CPMnorm)) #+ geom_point(aes(WeekDay,CPMnorm), pch=5, cex=5)
+ geom_point(aes(color=AppOriginID, size=displays), alpha = 0.6) 
+ geom_smooth(method = "lm")
#cut2(similarCampaigns$CPMnorm, g=5)
# returns the value of predicted CPMnorm with various criteria modalities in column
comparePred <- sapply(split(similarCampaigns, similarCampaigns[criteria]), getAverageCPM)
cor(as.data.frame(comparePred))


var(split(similarCampaigns, similarCampaigns[criteria])[1], split(similarCampaigns, similarCampaigns[criteria])[2])

dataHistpresentation <- function(data){
      par(mfrow=c(1,2))
      #attach(dsSplitPerSubCriteria)
      qplot(dsSplitPerSubCriteria$CPMnorm, ylim=c(0,20), xlim=c(0,25), binwidth=0.1, color=dsSplitPerSubCriteria$CampaignCostTypeName)
      qplot(dsSplitPerSubCriteria$CPM, ylim=c(0,30), xlim=c(0,10), binwidth=0.1, color=dsSplitPerSubCriteria$CampaignCostTypeName)
}


isWeb <- list('Web mobile')
#creating subset to split between WebMobile and others #
dsWeb <- subset(ds, ds$CampaignTargetType %in% isWeb)
dsApp <- subset(ds, !(ds$CampaignTargetType %in% isWeb))

#creating subset per campaignCostType#
dsCPI <- subset(dsApp, grepl('CPI',dsApp$CampaignCostType))
boxplot(data=dsCPI, dsCPI$CPMnorm)
dsCPC <- subset(ds, grepl('CPC',ds$CampaignCostType))
boxplot(data=dsCPC, dsCPC$CPMnorm)
dsCPM <- subset(ds, grepl('CPM',ds$CampaignCostType))
dsCPL <- subset(dsWeb, grepl('CPL',dsWeb$CampaignCostType))

#plot CPM along campaignTargetType#
ggplot(data=ds[with(ds,CPMnorm<10),], aes(CampaignTargetTypeName, CPMnorm)) + geom_point(aes(fill=CTR, shape=AppOriginTypeName), alpha=1/2, size=1.5, pch=1) + labs(title='CPM per (Campaign/AppOrigin) since February having nbDisplays > 3000')

# aggregate dipslays ans earnings for every combination#
displaysGlobPerCampPerApp = aggregate(displaysGlob ~ campaignID + appOriginID, FUN = sum, data=ds)
earningsGlobPerCampPerApp = aggregate(earningsGlob ~ campaignID + appOriginID, FUN = sum, data=ds)
calculCPMGlobPerCampPerApp <- function(rawdata){aggregate(earningsGlob ~ campaignID + appOriginID, FUN=function(x){ifelse(x$CostPerBid > 0, (x$CPM / x$CostPerBid), NA)}, data=rawdata)}
cpmsGlob <- calculCPMGlobPerCampPerApp(similarCampaigns)



#CPM per (Campaign/AppOrigin) since February having nbDisplays > 3000#
ggplot(data=ds[with(ds,CPMnorm<10 && ConnectionTypeName != 'Unknown'),], aes(abbreviate(CampaignCostTypeName, 12), CPMnorm)) + geom_point(aes(shape=AppAdvertisedypeName, color=CostPerBid, size=earningsGlob), alpha=1/2, pch=1) + 
      labs(x='CampaignCostType', title='CPM per (Campaign/AppOrigin) since February having nbDisplays > 3000') + geom_smooth(method='lm') + 
      facet_grid(ConnectionTypeName~CampaignTargetTypeName) + coord_cartesian(ylim=c(0,5))

# compute kmodes and assign each combinaison to cluster#
kmodes <- kmodes(data=na.omit(subset(ds, select = -c(apporiginID, campaignID, AppAdvertisedID, CTR, CVR))), modes=4, iter.max=20, weighted=F)
ds <- cbind(ds,kmodes$cluster)
names(ds)[length(ds)] <- 'cluster';
split(ds, ds$cluster)[1]


# display CPM per cluster over various dimensions #
qplot(x=cluster, y=CPM, data=ds, color=AppAdvertisedCategory, shape=CampaignTargetTypeName, ylim=c(0,10))

# tracing the plot of displays = f(AppAdvertisedCategory, AppOriginCategory)#
catVScat <-qplot(data=ds, x=abbreviate(AppAdvertisedCategory_, 7), y=AppOriginCategory_, size=installsGlob, color=displaysGlob, xlab='promotedApp');
trainer <- createDataPartition(y=t$CPM, p=0.75, list=F) #displaysGlob#
trainingset <- t[trainer,]
train <- train(CPM~., data=trainingset, method='glm'); # ~ AppOriginType + AppAdvertisedType#
predict(newdata=t[-trainer, ], train)

library('klaR');

qplot(geom='density', x=CPM, data=ds ,xlim=c(0,4), color=CampaignTargetTypeName);
qplot(x=cluster, y=CPM, data=ds, color=AppOriginCategory ,ylim=c(0,20), shape=CampaignTargetTypeName)