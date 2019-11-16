no_errors <- subset(keyPressDataWithLaneDeviation, typingErrorMadeOnTrial == 0)

## Question 1
# A
total_time <- subset(no_errors, no_errors$phoneNrLengthAfterKeyPress == 11)
steer_dial_mean <- aggregate(total_time$timeRelativeToTrialStart ,by=list(total_time$partOfExperiment),FUN=mean, na.rm=TRUE)
steer_dial_sd <- aggregate(total_time$timeRelativeToTrialStart,by=list(total_time$partOfExperiment),FUN=sd, na.rm=TRUE)
steer_dial_se <- steer_dial_sd[2] / sqrt(max(unique(total_time$pp)))
#no_errors$timeRelativeToTrialStart[with(no_errors, no_errors$phoneNrLengthAfterKeyPress == 11)]

# B
lateral_deviation_mean <- aggregate(no_errors$lanePosition, by=list(no_errors$partOfExperiment),FUN=mean, na.rm=TRUE)
lateral_deviation_sd <- aggregate(no_errors$lanePosition, by=list(no_errors$partOfExperiment),FUN=sd, na.rm=TRUE)
lateral_deviation_se <- lateral_deviation_sd[2] / sqrt(max(unique(no_errors$pp)))

# C