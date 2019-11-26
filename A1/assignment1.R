library(ggplot2)
library(plyr)
library(reshape2)

## Question 1
{
  no_errors <- subset(keyPressDataWithLaneDeviation, typingErrorMadeOnTrial == 0)

  # A
  {
    total_time <- subset(no_errors, no_errors$phoneNrLengthAfterKeyPress == 11)
    steer_dial_mean <- aggregate(total_time$timeRelativeToTrialStart ,by=list(total_time$partOfExperiment),FUN=mean, na.rm=TRUE)
    steer_dial_sd <- aggregate(total_time$timeRelativeToTrialStart,by=list(total_time$partOfExperiment),FUN=sd, na.rm=TRUE)
    steer_dial_se <- steer_dial_sd[2] / sqrt(max(unique(total_time$pp)))
    #no_errors$timeRelativeToTrialStart[with(no_errors, no_errors$phoneNrLengthAfterKeyPress == 11)]
  }

  # B
  {
    lateral_deviation_mean <- aggregate(abs(no_errors$lanePosition), by=list(no_errors$partOfExperiment),FUN=mean, na.rm=TRUE)
    lateral_deviation_sd <- aggregate(abs(no_errors$lanePosition), by=list(no_errors$partOfExperiment),FUN=sd, na.rm=TRUE)
    lateral_deviation_se <- lateral_deviation_sd[2] / sqrt(max(unique(no_errors$pp)))
  }

  # C
  {
    two_conditions <- subset(no_errors, no_errors$partOfExperiment == "dualDialFocus" | no_errors$partOfExperiment == "dualSteerFocus")
    per_participant <- aggregate(abs(two_conditions$lanePosition), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress), FUN=mean, na.rm=TRUE)
    time_participant <- aggregate(abs(two_conditions$timeRelativeToTrialStart), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress), FUN=mean, na.rm=TRUE)
    per_participant <- cbind(per_participant, time_participant[,3]/1000)

    pp <- aggregate(abs(two_conditions$lanePosition), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress, two_conditions$pp), FUN=mean, na.rm=TRUE)
    tp <- aggregate(abs(two_conditions$timeRelativeToTrialStart), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress, two_conditions$pp), FUN=mean, na.rm=TRUE)
    ppt <- cbind(pp, tp[,3]/1000)

    g1 <- subset(ppt, ppt$Group.1 == "dualDialFocus")
    min_g1 <- c()
    max_g1 <- c()
    min_g2 <- c()
    max_g2 <- c()
    mean_g1 <- c()
    sd_g1 <- c()
    mean_g2 <- c()
    sd_g2 <- c()

    for(i in 0:12){
      min_g1[i+1] <- min(subset(g1, Group.2 == i)[,4])
      max_g1[i+1] <- max(subset(g1, Group.2 == i)[,4])
      mean_g1[i+1] <- mean(subset(g1, Group.2 == i)[,4])
      sd_g1[i+1] <- sd(subset(g1, Group.2 == i)[,4])
    }

    g2 <- subset(ppt, ppt$Group.1 == "dualSteerFocus")
    for(i in 0:12){
      min_g2[i+1] <- min(subset(g2, Group.2 == i)[,4])
      max_g2[i+1] <- max(subset(g2, Group.2 == i)[,4])
      mean_g2[i+1] <- mean(subset(g2, Group.2 == i)[,4])
      sd_g2[i+1] <- sd(subset(g2, Group.2 == i)[,4])
    }
    min_val <- c()
    max_val <- c()
    mean_val <- c()
    sd_val <- c()
    for(i in 0:12){
      min_val[2*i + 1] <- min_g1[i+1]
      min_val[2*i + 2] <- min_g2[i+1]
      max_val[2*i + 1] <- max_g1[i+1]
      max_val[2*i + 2] <- max_g2[i+1]
      mean_val[2*i + 1] <- mean_g1[i+1]
      mean_val[2*i + 2] <- mean_g2[i+1]
      sd_val[2*i + 1] <- sd_g1[i+1]
      sd_val[2*i + 2] <- sd_g2[i+1]
    }

    se_val <- c()
    se_val <- sd_val / sqrt(12)

    ggplot(per_participant, aes(per_participant$`time_participant[, 3]`, per_participant$x, group = (Group.1), colour = Group.1)) + geom_line() + geom_point() + geom_errorbar(aes(ymin = mean_val-se_val , ymax = mean_val+se_val))  + xlab("Dialing Time (sec)") + ylab("Lateral Deviation (m)")
  }
}
  
## Question 2
{
  # A
  {
  position_change <- subset(tableOfDriftValuesCalibration, trialTime >= 15000 & trialTime <= 18000)
  ggplot(position_change, aes(trialTime, posX, group = trial, color = trial)) + 
    geom_line() + xlab("Trial time (ms)") + ylab("Lateral Position (m)")
}

  # B
  {
    position <- list()
    for (i in 1:20){
      distribution <- rnorm(60, mean = 0, sd = 0.13)
      position[[i]] <- cumsum(tableOfDriftValuesCalibration$posX[with(tableOfDriftValuesCalibration, tableOfDriftValuesCalibration$trial == i)][1:60] + distribution)
    }

    time <- c()
    for (j in 1:60){
      time[j] <- c(50*j-50)
    }

    position_dataframe <- as.data.frame(time)
    #names(position_dataframe)[1] <- 'time' 
    for (i in 1:length(position)){
      position_dataframe <- cbind(position_dataframe, position[[i]])
      names(position_dataframe)[i+1] <- i
    }

    position_dataframe <- melt(position_dataframe ,  id.vars = 'time', variable.name = 'series')
    ggplot(position_dataframe, aes(time,value)) + geom_line(aes(colour = series)) + xlab("Trial time (ms)") + ylab("Lateral Position (m)")
  }

  # C
  {
    human_trial <- hist(position_change$posX, xlab = "Lateral position (m)", ylab = "Frequency", xlim = range(-2:3), 
                        ylim = range(0:350), col = "gray", main = "Distribution of car positions (human data)", breaks = seq(-2,3,0.2))
    simulated_trial <- hist(position_dataframe$value, xlab = "Lateral position (m)", ylab = "Frequency", xlim = range(-2:3), 
                            ylim = range(0:350), col = "gray", main = "Distribution of car positions (simulated data)", breaks = seq(-2,3,0.2))
  }
  
  # D
  {
    sd_human <- sd(position_change$posX)
    sd_simulated <- sd(position_dataframe$value)
  }
  
  # E
  {
    
  }

}


