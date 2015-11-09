
data <- read.csv("F:\\as_duration.csv")[c("as", "duration")]
# data <- read.csv("~/Desktop/as_duration.csv")[c("as", "duration")]

# Log10 is a lot easier to read and plot
durations <- log10(data$duration)
filtered <- durations[durations >= 0]

# number of filtered elements
zeros <- length(durations) - length(filtered)
# only 2.56 * 10^-7 of the data is lost due to filtering, insignificant amount
p_0 <- length(zeros) / length(durations)

# ----- Basic Stats ------
med <- median(filtered) # median duration (sec)
avg <- mean(filtered) # average duration (sec)
std <- sd(filtered) # standard deviation
quantiles <- quantile(filtered) # quantiles, 0%, 25%, 50%, 75%, 100%
var <- var(filtered) # variance

# ----- Plots -----
# Log Histogram Plot so that the data is better visible
# Freq = true -> Show the frequencies
hist(pps2, freq=TRUE, xlab="Log10 Duration (sec)")
# Freq = true -> Show the probability distribution
# Prob = width (0.2) * height
# hist(durations, freq=FALSE, xlab="Log10 Duration (sec)")
curve(dnorm(x, mean=avg, sd=std), col="darkblue", lwd=2, add=TRUE, yaxt="n")

# ----- Observations -----

# Peak at durations of 2.4-2.8
# (2.8-2.4) * 0.7 * 100 = 0.4 * 0.7 * 100 = 28%
# 10^2.4 = 4 minutes, 10^2.8 = 10 minutes
# 28% of the attacks last between 4 and 10 minutes
# 10 targets in this range
minute_targets <- data[data$duration>=240 & data$duration<=600,]

# Secondary Peak occurs at 1.6-2.2
# (2.2-1.6) * 0.5 * 100 = 0.6 * 0.5 * 100 = 30%
# 10^1.6 = 40 seconds, 10^2.2 = 150 seconds
# 30% of the attacks last between 40 and 150 seconds
second_targets <- data[data$duration>=40 & data$duration<=150,]

# ----- Research Question -----


minute_targets <- dset[dset$duration>=240 & dset$duration<=600,]
second_targets <- dset[dset$duration>=40 & dset$duration<=150,]

targets <- dset[(dset$duration>=240 & dset$duration<=600)|(dset$duration>=40 & dset$duration<=150),]
rest1 <- dset[(dset$duration<240 | dset$duration>600)&(dset$duration<40 | dset$duration>150),]

rest1 <- dset[(dset$duration<40)|(dset$duration > 150 & dset$duration < 240),]
rest2 <- dset[(dset$duration>600),]


(pt <- ggplot(data = targets, aes(duration, packets)) + 
      geom_point(color="red", alpha="0.01") )

pr1 <- ggplot(data = rest1, aes(duration, packets)) + geom_point(color="green", alpha="0.01")
	  
pr21 <- ggplot(data = rest21, aes(duration, packets)) + geom_point(color="green", alpha="0.01")
	  
pt <- pt + expand_limits(x = 2000, y = 1800000)
pr21 <- pr21 + expand_limits(x = 0, y = 1800000)
pr1 <- pr1 + expand_limits(x = 2000, y = 1800000)

(plot2 <- ggplot(NULL, aes(duration, packets)) + 
      geom_point(data = targets, color="red", alpha="0.01") +
      geom_step(data = rest21, color="green", alpha="0.01")
)














dset_r
df <- dset_r
df1 <- dset_r[dset_r$duration < 40,]
df2 <- dset_r[dset_r$duration <= 600 & dset_r$duration >= 40,]
df3 <- dset_r[dset_r$duration > 600,]
df1 <- transform(df1, label = 0 )
df2 <- transform(df2, label = 1 )
df3 <- transform(df3, label = 2 )
df <- df1+df2+df3