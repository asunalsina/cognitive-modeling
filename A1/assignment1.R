library(ggplot2)
library(plyr)
library(reshape2)
library(miceadds)

load.Rdata("keyPressDataWithLaneDeviation.Rdata", "keyPressDataWithLaneDeviation")
load.Rdata("tableOfDriftValuesCalibration.Rdata", "tableOfDriftValuesCalibration")

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
    per_participant <- cbind.data.frame(per_participant, time_participant[,3]/1000)

    pp <- aggregate(abs(two_conditions$lanePosition), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress, two_conditions$pp), FUN=mean, na.rm=TRUE)
    tp <- aggregate(abs(two_conditions$timeRelativeToTrialStart), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress, two_conditions$pp), FUN=mean, na.rm=TRUE)
    ppt <- cbind.data.frame(pp, tp[,3]/1000)

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
    names(per_participant)[1] <- "Condition"
    ggplot(per_participant, aes(per_participant$`time_participant[, 3]`, per_participant$x, group = (Condition), colour = Condition)) + 
      geom_line() + geom_point(aes(shape = Condition)) + geom_errorbar(aes(ymin = mean_val-se_val , ymax = mean_val+se_val))  + 
      xlab("Dialing Time (sec)") + ylab("Lateral Deviation (m)")
  }
}
  
## Question 2
{
  # A
  {
  position_change <- subset(tableOfDriftValuesCalibration, trialTime >= 15000 & trialTime <= 18000)
  pc <- position_change[,c(2,16,17)]
  names(pc)[2] <- "Trial"
  
  ggplot(position_change, aes(trialTime, posX, group = trial, color = trial)) + 
    geom_line() + xlab("Trial time (ms)") + ylab("Lateral Position (m)")
  
  tn <- 1 
  clr <- c()
  clr <- rgb(runif(20),runif(20),runif(20)) 
  while(tn<=20){
    x <- pc$trialTime[pc$Trial==tn]
    y <- pc$posX[pc$Trial==tn]
    if(tn==1){
      plot(x,y, col = clr[tn], type="l",xlab="Trial time (ms)",ylab="Lateral Position (m)",ylim = c(-1,2), xlim = c(15000,18700))
      
    }
    else{
      li <- lines(x,y,col=clr[tn])
    }
    legend(18100, 2, legend=c("Trial 1", "Trial 2", "Trial 3", "Trial 4", "Trial 5", "Trial 6", "Trial 7", "Trial 8", "Trial 9", 
                              "Trial 10", "Trial 11", "Trial 12", "Trial 13", "Trial 14", "Trial 15", "Trial 16", "Trial 17", 
                              "Trial 18", "Trial 19", "Trial 20"),
           cex=0.8, col = clr, lty = 1)
    tn=tn+1
  }
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
      position_dataframe <- cbind.data.frame(position_dataframe, position[[i]])
      names(position_dataframe)[i+1] <- i
    }

    position_dataframe <- melt(position_dataframe ,  id.vars = 'time', variable.name = 'series')
    names(position_dataframe)[2] <- "Trial"
    ggplot(position_dataframe, aes(time,value)) + geom_line(aes(colour = Trial)) + xlab("Trial time (ms)") + ylab("Lateral Position (m)")
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
    position_simulated <- list()
    for (i in 1:20){
      distribution_simulated <- rnorm(60, mean = 0, sd = 0.05)
      position_simulated[[i]] <- cumsum(tableOfDriftValuesCalibration$posX[with(tableOfDriftValuesCalibration, tableOfDriftValuesCalibration$trial == i)][1:60] + distribution_simulated)
    }

    position_simulated_dataframe <- as.data.frame(time)
    for (i in 1:length(position_simulated)){
      position_simulated_dataframe <- cbind.data.frame(position_simulated_dataframe, position_simulated[[i]])
      names(position_simulated_dataframe)[i+1] <- i
    }
    
    position_simulated_dataframe <- melt(position_simulated_dataframe ,  id.vars = 'time', variable.name = 'series')
    names(position_simulated_dataframe)[2] <- "Trial"
    ggplot(position_simulated_dataframe, aes(time,value)) + geom_line(aes(colour = Trial)) + xlab("Trial time (ms)") + ylab("Lateral Position (m)")
    
    simulated_data_trial <- hist(position_simulated_dataframe$value, xlab = "Lateral position (m)", ylab = "Frequency", xlim = range(-2:3), 
                            ylim = range(0:400), col = "gray", main = "Distribution of car positions (simulated data)", breaks = seq(-2,3,0.2))
    
    sd_simulated_data <- sd(position_simulated_dataframe$value)
  }
}

## Question 3
{
  keypress_time <- subset(no_errors, partOfExperiment == "singleDialing2")
  keypress_intervals <- aggregate(keypress_time$timeRelativeToTrialStart, by=list(keypress_time$phoneNrLengthAfterKeyPress, keypress_time$pp), FUN=mean, na.rm=TRUE)
  names(keypress_intervals) <- c("Digit", "Participant", "Time")
  ki <- c()
  for(i in 1:(length(keypress_intervals$Time)-1)){
    ki[i] <- keypress_intervals$Time[i+1] - keypress_intervals$Time[i]
  }
  keypress_intervals <- cbind.data.frame(keypress_intervals, ki)
  intervals <- subset(keypress_intervals, Digit != 12)
  average_keypress_time <- aggregate(intervals$ki, by=list(intervals$Participant), FUN=mean, na.rm=TRUE)
  new_keypress_time <- mean(average_keypress_time$x)
}