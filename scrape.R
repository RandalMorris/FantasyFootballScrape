#### Weekly Data Generator ####


#### PACKAGE INSTALLER ####
library(devtools)
devtools::install_github(repo = "FantasyFootballAnalytics/ffanalytics", build_vignettes = TRUE, force=TRUE)

#LOAD Fantasy Football Analytics Package
print("Load Packages")
suppressMessages(library(FantasyFootball))
suppressMessages(library(data.table))
suppressMessages(library(sqldf))
suppressMessages(library(jsonlite))
suppressMessages(library(xlsx))


#Set Vars
data_update <- 0
apikey <- "" #Change to your api key from them sportsdata.io
base <- "https://api.sportsdata.io/v3/nfl"
varseason <- 2019
vartype <- "Weekly"
varweek   <- week(Sys.Date()) - 35
mainDir <- paste(getwd(),"/FantasyFootball", sep = "") #Sets my main directory i save files in
setwd(mainDir)

CalculatePTs <-  function(x){
  ifelse(Data_all$pass_yds >= 300, scoring_rules[["pass"]][["pass_300_yds"]], 0) +
    round(Data_all$pass_yds * scoring_rules[["pass"]][["pass_yds"]],2) +
    round(Data_all$pass_tds * scoring_rules[["pass"]][["pass_tds"]],0) +
    round(Data_all$pass_int * scoring_rules[["pass"]][["pass_int"]],0) +
    ifelse(Data_all$rush_yds >= 100, scoring_rules[["rush"]][["rush_100_yds"]], 0) +
    round(Data_all$rush_yds * scoring_rules[["rush"]][["rush_yds"]],2) +
    round(Data_all$rush_tds * scoring_rules[["rush"]][["rush_tds"]],0) +
    ifelse(Data_all$rec_yds >= 100, scoring_rules[["rec"]][["rec_100_yds"]], 0) +
    round(Data_all$rec * x,0) +
    round(Data_all$rec_yds * scoring_rules[["rec"]][["rec_yds"]],2) +
    round(Data_all$rec_tds * scoring_rules[["rec"]][["rec_tds"]],0) +
    round(Data_all$fumbles_lost * scoring_rules[["misc"]][["fumbles_lost"]],0)

}

if(data_update == 1){
  #SCAPE DATA
  scraped_data <- scrape_data(src = c("CBS","FantasyPros", "FantasySharks", "FFToday", "NumberFire", 
                                      "Yahoo", "FantasyFootballNerd", "NFL"), 
                              pos = c("QB","RB","WR","TE","DST","K"),
                              season = varseason, week = varweek)
  save(scraped_data, file=paste(getwd(),"/Data/Scraped_Week",varweek,".rda", sep = ""))
}
load(file=paste(getwd(),"/Data/Scraped_Week",varweek,".rda", sep = ""))

#Combine into Single dataset
DataQB <- rbind(scraped_data[["QB"]])
DataQB <- subset(DataQB, select=-c(5,7,9,11,14,20:85))
DataQB$player <- ""
DataRB <- rbind(scraped_data[["RB"]])
DataRB <- subset(DataRB, select=-c(5,7,13,19:90))
DataRB$player <- ""
DataWR <- rbind(scraped_data[["WR"]])
DataWR <- subset(DataWR, select=-c(5,7,11:12,19:88))
DataWR$player <- ""
DataTE <- rbind(scraped_data[["TE"]])
DataTE <- subset(DataTE, select=-c(5,7,11:12,15:40,44:87))
DataTE$player <- ""

suppressMessages(library(plyr))
Data_all <- rbind.fill(DataQB, DataRB, DataWR, DataTE)
detach("package:plyr", unload = TRUE)
rm("DataQB", "DataRB", "DataWR", "DataTE")

#Get SportsDataIO Data
Player_Data <- fromJSON(paste(base,"/scores/json/Players?key=",apikey,sep = ""))
Stadium_Data <- fromJSON(paste(base,"/scores/json/Stadiums?key=",apikey, sep = ""))
temp1 <- fromJSON(paste(base,"/scores/json/Schedules/2018?key=",apikey, sep = ""))
temp2 <- fromJSON(paste(base,"/scores/json/Schedules/2019?key=",apikey, sep = ""))


#PreProcess SportsDataIO Data

#Process Player Data
#Get MFL Player ID for Fantasy Projections cross id
Player_Data1 <<- httr::GET("https://www70.myfantasyleague.com/2019/export?TYPE=players&DETAILS=1&SINCE=&PLAYERS=&JSON=1") %>%
  httr::content() %>% `[[`("players") %>% `[[`("player") %>%
  purrr::map(tibble::as.tibble) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::select(id, name, sportsdata_id, rotowire_id ) %>%
  tidyr::extract(name, c("last_name", "first_name"), "(.+),\\s(.+)") %>%
  tidyr::unite('Name',c("first_name", "last_name"),sep = " ")

Player_Data <- sqldf("SELECT PlayerID, Team, Name, ShortName, Age, Position,CollegeDraftYear, 2019-CollegeDraftYear as Experience, 
                      ByeWeek, UpcomingGameOpponent, UpcomingGameWeek, DraftKingsName, 
                      DraftKingsPlayerID, RotoWirePlayerID, SportRadarPlayerID
                      FROM Player_Data WHERE 
                      Active = TRUE and 
                      PositionCategory ='OFF' and 
                      Team IS NOT NULL and
                      FantasyPosition IN ('QB','RB','WR','TE','K')")

Player_Data <- sqldf("select Player_Data.*, Player_Data1.id from Player_Data left outer join 
                      Player_Data1 on Player_Data.RotoWirePlayerID = Player_Data1.rotowire_id")
#Add Player name from ID
Data_all$player <- Player_Data$Name[match(Data_all$id, Player_Data$id)]
Data_all$team <- Player_Data$Team[match(Data_all$id, Player_Data$id)]
Data_all <- sqldf("select Data_all.* FROM Data_all WHERE player != ''")

Data_all$src_id <- Data_all$player
#Data_all$player <- Player_Data$Team[match(Data_all$id, Player_Data$id)]
#Data_all <- add_column(Data_all, Pos = "", .before = 4)
Data_all$player <- Player_Data$Position[match(Data_all$id, Player_Data$id)]
names(Data_all)[3:5] <- c("Player", "Pos", "Team")

Data_all <- Data_all %>% mutate_if(is.numeric, round, digits=0)
Data_all[is.na(Data_all)] <- 0

#Create Points for Each Format
print("Starting ESPN")
usecase <- "ESPN"
source(paste(getwd(),"/Scripts/Functions/League Settings ",usecase,".R", sep=""))
Data_all$ESPN_STD <- CalculatePTs(0)
Data_all$ESPN_HALF <- CalculatePTs(.5)
Data_all$ESPN_PPR <- CalculatePTs(1)

print("Starting Yahoo")
usecase <- "Yahoo"
source(paste(getwd(),"/Scripts/Functions/League Settings ",usecase,".R", sep=""))
Data_all$Yahoo_STD <- CalculatePTs(0)
Data_all$Yahoo_HALF <- CalculatePTs(.5)
Data_all$Yahoo_PPR <- CalculatePTs(1)

print("Starting DraftKings")
usecase <- "DraftKings"
source(paste(getwd(),"/Scripts/Functions/League Settings ",usecase,".R", sep=""))
Data_all$DraftKings <- CalculatePTs(1)

#Process Game Stadium Data
Stadium_Data <- sqldf("SELECT StadiumID as StadiumID1, City, State, PlayingSurface, GeoLat, GeoLong, Type
                      FROM Stadium_Data")

temp1 <- subset(temp1, select =  c("GameKey", "Season", "Week", "Date", "HomeTeam", "AwayTeam", "StadiumID", "ForecastTempLow",
                                   "ForecastTempHigh", "ForecastDescription", "ForecastWindChill", "ForecastWindSpeed", "GlobalGameID", 
                                   "ScoreID"))
temp2 <- subset(temp2, select =  c("GameKey", "Season", "Week", "Date", "HomeTeam", "AwayTeam", "StadiumID", "ForecastTempLow",
                                   "ForecastTempHigh", "ForecastDescription", "ForecastWindChill", "ForecastWindSpeed", "GlobalGameID", 
                                   "ScoreID"))
Weather_Data <- rbind(temp1, temp2)
Weather_Data <- sqldf("SELECT Weather_Data.* ,Stadium_Data.*
                      FROM Weather_Data left outer join 
                      Stadium_Data on Stadium_Data.StadiumID1 = Weather_Data.StadiumID")                                    
Weather_Data <- subset(Weather_Data, select = -c(StadiumID1))
Weather_Data_Current_Week <- sqldf("SELECT * FROM Weather_Data WHERE Season = 2019 and Week = 14")
Weather_Data_Current_Week <- subset(Weather_Data_Current_Week, select = c("HomeTeam", "AwayTeam",
                                                                          "ForecastTempLow", "ForecastTempHigh", "ForecastDescription",
                                                                          "ForecastWindChill", "ForecastWindSpeed", "PlayingSurface","Type"))


#Add Weather Data to Players and round all 
Data_all <- sqldf("SELECT Data_all.*, Weather_Data_Current_Week.* FROM Data_all left outer join 
                      Weather_Data_Current_Week on Weather_Data_Current_Week.HomeTeam = Data_all.Team or Weather_Data_Current_Week.AwayTeam = Data_all.Team")


#Remove Datasets
rm("player_table","Player_Data1", "Stadium_Data", "temp1", "temp2", "Weather_Data", "Weather_Data_Current_Week")

#Export

#change path for File
#change path for File
temp <- sqldf("SELECT * FROM Data_all WHERE pos = 'QB' ORDER BY Player, data_src")
write.xlsx(temp, file = paste(getwd(),"/Data/Weekly/Fantasy_Data_Raw_Week15.xlsx", sep=""), sheetName="QB Raw Data", row.names=FALSE)

temp <- sqldf("SELECT * FROM Data_all WHERE pos = 'RB' ORDER BY Player, data_src")
write.xlsx(temp, file = paste(getwd(),"/Data/Weekly/Fantasy_Data_Raw_Week15.xlsx", sep=""), sheetName="RB Raw Data",append=TRUE, row.names=FALSE)

temp <- sqldf("SELECT * FROM Data_all WHERE pos = 'WR' ORDER BY Player, data_src")
write.xlsx(temp, file = paste(getwd(),"/Data/Weekly/Fantasy_Data_Raw_Week15.xlsx", sep=""), sheetName="WR Raw Data",append=TRUE,  row.names=FALSE)

temp <- sqldf("SELECT * FROM Data_all WHERE pos = 'TE' ORDER BY Player, data_src")
write.xlsx(temp, file = paste(getwd(),"/Data/Weekly/Fantasy_Data_Raw_Week15.xlsx", sep=""), sheetName="TE Raw Data",append=TRUE, row.names=FALSE)

