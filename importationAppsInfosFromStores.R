install.packages("RODBC")
library(RODBC)
cn <- odbcDriverConnect(connection="driver=SQL Server Native Client 11.0;server=badtmo64a8.database.windows.net;pwd=AheadDB2011!;uid=reader;database=addeals;")

odbcClose(cn)
dataSQLQueryEUR <- sqlQuery(cn, "select top 5 * from appDealsUsersCountriesView")
View(dataSQLQueryEUR)

install.packages('RCurl', 'XML','rjson')
library('RCurl')
library('XML')
library('rjson')
params = list(term='Dark+Summoner', country = 'US', entity='software')
campaignJSON <- getURL(url = paste("http://itunes.apple.com/search", paste(names(params), params, sep='=', collapse='&'), sep='?'))#term=" + paste(params$term, collapse = '+') + "Dark+Summoner&country=US&entity=software", access_token, "&api_key=", api_key, sep=""))

getCampaignDataframeFromJSON <- function(campaignJSON, appID = NULL) {
      infos <- c('trackId', 'fileSizeBytes', 'primaryGenreName', 'price', 'currency', 'releaseDate', 'formattedPrice', 'minimumOsVersion')#, 'averageUserRating', 'userRatingCount') #'genres', 'genreIds',
      df <- data.frame(matrix(0, ncol = length(infos), nrow = 1))
      colnames(df) <- infos
      namelist <- NULL
      urllist <- NULL
      statuslist <- NULL
      datelist <- NULL
      JSONList <- fromJSON(campaignJSON)
      results <- JSONList$results
      retrieveValuableInfos <- function (appsArray){
            appInfos<-c()#list()#vector('character',length=length(infos))
            #row[[x]]<-
            appInfos <- append(appInfos, lapply(infos, function(x){print(appsArray[[i]][[x]]);appsArray[[i]][[x]]}))#row<-append(row, results[[i]][[x]])
            print(appInfos)
            appInfos
      }
      if(is.null(appID)){
            for (i in 1:length(results)) {
                  row <- retrieveValuableInfos(results)
                  df <- rbind(df, row)
                  #namelist <- c(namelist, results[i]$name )
                  ##urllist <- c(urllist, paste("https://api.constantcontact.com/v2/emailmarketing/campaigns/", results[i][[1]]$id, sep="", collapse=NULL))
                  #statuslist <- c(statuslist, results[i][[1]]$status)
                  #datelist <- c(datelist, results[i][[1]]$modified_date)
            }
      }
      else{
            print(results[[1]]$trackId)
            print(class(results[1]))
            
            #print(as.vector(results[['trackId']]))#[,'trackId']
            exactMatch <- lapply(results, function(subres){print(subset);if(subres$trackId == as.integer(appID)){return(subset)}})#results[which(as.data.frame(results)$trackId==as.integer(appID)),]
            if(!is.na(exactMatch)){
                  row <- retrieveValuableInfos(exactMatch)
                  df <- rbind(df, row)
                  return(df)
            }
            else{
                  print(paste("No app with id:", appID, "has been found on itunes"))
            }
      }
      #names(df) <- infos
      df[-c(1),]
}
