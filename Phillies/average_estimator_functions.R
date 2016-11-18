library("dplyr")
library("ggplot2")

### Download files
### Move downloaded folder to directory you wish to run script in
### Open RStudio, R Application
### In Console: Set working directory to filepath leading to downloaded folder was placed
    ### setwd(filePath)
### In Console: Verify working directory points to downloaded folder
    ### getwd()
### In Console: Load Script holding functions to perform analysis of player performance data
    ### source("average_estimator_functions.R")
### In Console: Run Script to output analysis of player performance data
    ### main()

main <- function(){
    batting <- read.csv("batting.csv")
    fangraphsData <- read.csv("Fangraphs_Player_Performance_March_April.csv")
    playerData <- merge(batting, fangraphsData, by = "Name", all.x = TRUE)
    cleansed <- filter(playerData, MarApr_PA > 0)
    regressionData <- performRegression(cleansed)
    estimatedAverages <- estimateBAvg(regressionData)
    avgEstimatedBA <- mean(estimatedAverages$avgEstimate)
    noPA <- filter(playerData, MarApr_PA == 0)
    noPA$avgEstimate <- avgEstimatedBA
    noPA$percentError <- abs(100*(noPA$FullSeason_AVG - noPA$avgEstimate))/noPA$FullSeason_AVG

    estimatesPA <- select(estimatedAverages, Name, MarApr_AVG, FullSeason_AVG, avgEstimate, percentError)
    noPAEstimates <- select(noPA, Name, MarApr_AVG, FullSeason_AVG, avgEstimate, percentError)
    outputEstimates <- rbind(estimatesPA, noPAEstimates)
    outputEstimates

}

### Utilizes input data table from findEstimates function call and  a max/min for league BABIP, and 
### a player's batting average for games in March/April to estimate a player's batting average. 
### Input: regressionData - Table containing player performance data for March/April games and estimates for each player's season long strikeout rate and walk rate
### Output: battingAverages <- Data table containing estimates for a player's season long batting average under "estAverage" column and percent error from actual season average
estimateBAvg <- function(regressionData) {
    battingAverages <- select(regressionData, Name, MarApr_AVG, FullSeason_AVG)
    BABIPVals <- c(0.380, 0.360, 0.340, 0.270, 0.250, 0.230)
    avgContainer <- data.frame(matrix(NA, nrow = nrow(regressionData), ncol = length(BABIPVals)))
    colnames(avgContainer) <- BABIPVals
    for (index in 1: length(BABIPVals)){
        BABIPVal <- BABIPVals[index]
        strikeOutRate <- regressionData$K_percent_stable/100.
        BBRate_stable <- regressionData$BB_percent_stable/100.
        BBRate_orig <- regressionData$BB_percent/100.
        plateAppears <- regressionData$MarApr_PA
        homers <- regressionData$HR
        numerator <- (((plateAppears - (BBRate_stable*plateAppears) - (strikeOutRate*plateAppears) - (plateAppears/80))*BABIPVal) + homers)
        denominator <- (plateAppears - (BBRate_orig * plateAppears))
        estAvg <- numerator/denominator
        avgContainer[,index] <- estAvg
    }
    
    battingAverages <- cbind(battingAverages, avgContainer)
    ### MarApr_AVG yields an error rate of 11.819%
    ### MarApr_AVG, 0.34, 0.27 yields an error rate of 8.7%
    targetAverages <- select(battingAverages, MarApr_AVG, 6,7)
    battingAverages$avgEstimate <- rowMeans(targetAverages)
    battingAverages <- mutate(battingAverages, "percentError" = abs((FullSeason_AVG - avgEstimate)/FullSeason_AVG * 100))
    print(paste(cor(battingAverages$FullSeason_AVG, battingAverages$avgEstimate), median(battingAverages$percentError), mean(battingAverages$percentError)))
    battingAverages
}

### Finds estiamte for season long strikeout rate  and walk rate depending on input parameter for each player if player plate appearances < 100.
### Season long estimates depending on average league strikeout rate (18.73%) and walk rate averages (8.28%) from 2007-2014. 
### Data to find season long averages acquired from Fangraphs - https://goo.gl/Kmf1po
### Minimum plate appearances threshold and estimate methodology based on Jeff Zimmerman's 'Predicting Batting Average Article - https://goo.gl/9Aiw6d
### Example Estimating Seasong Long Strikeout Rate: K%_Est = (Player K% * Player PA + League K% * 100 PA) / (Player PA  + 100 PA)
### Input: playerData - Data table consisting player performance data from March/April games
### Input: param - either "BB_percent" or "K_percent" to mark whether function walk rate or strikeout rate should be estimated
### Input: stabilizeFactor - either 168 or 100 depending if walk rate or strikeout rate is being estimated respectively
### Output: playerData - original playerData table with added columns for estimated season long strikeout and walk rate
estimator <- function(playerData, param, stabilizeFactor) {
    if (param == "BB_percent"){
        leagueAvg <- 8.28
    }
    else if (param == "K_percent") {
        leagueAvg <- 18.73
    }
    leagueAvg <- mean(as.numeric(unlist(playerData[eval(param)])))
    stabilizedStat <- c()
    for (index in 1: nrow(playerData)) {
        curRow <- playerData[index,]
        targetStat <- curRow[eval(param)]
        plateAppear <- curRow$MarApr_PA
        if (plateAppear < stabilizeFactor) {
            stat_Star <- (targetStat * plateAppear + leagueAvg * stabilizeFactor) / (plateAppear + stabilizeFactor)
            stabilizedStat <- c(stabilizedStat, stat_Star)
        }
        else {
            stabilizedStat <- c(stabilizedStat, targetStat)
        }
    }
    stableStatColName <- paste(eval(param), "_stable", sep = "")
    playerData[eval(stableStatColName)] <- unlist(stabilizedStat)
    playerData
}

### Utilizes input player performance data to estimate a player's season long strikeout rate and walk rate
### Input: originalPlayerData - Data table containing various statistics critical to player performance for March-April games
### Output: estimateBB - Data table consiting of original statistics and estimated season long strikeout rate and walk rate
findEstimates <- function(originalPlayerData){
    cleansed <- convertPercents(originalPlayerData)
    estimateK <- estimator(cleansed, "K_percent", 100)
    estimateBB <- estimator(estimateK, "BB_percent", 168)
    estimateBB
}

### Convert formatting for input player data for strike out rates and walks rate to numeric type
### Input: playerData - Data table consisting player performance data from March/April games
### Output: playerData - Original data table, except strikeout percentages and walk percentages have been converted to numeric types
convertPercents <- function(playerData){
    percent_numericK <- as.numeric(sub(" %", "", playerData$K.))
    playerData <- mutate(playerData, "K_percent" = percent_numericK)
    playerData$K. <- NULL
    
    percent_numericBB <- as.numeric(sub(" %", "", playerData$BB.))
    playerData <- mutate(playerData, "BB_percent" = percent_numericBB)
    playerData$BB. <- NULL
    playerData
}


