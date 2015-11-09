
#Load the file. Keep only date, duration and packets columns
#DATA <- read.csv("F:\\combined.csv", sep="|")[c("date", "duration", "packets")]
#write.csv(DATA, "F:\\step1.csv")

DATA <- read.csv("F:\\combined.csv", sep="|")[c("date", "target_ip", "country", "duration", "packets")]

DATA <- read.csv("F:\\step1.csv")[c("date", "target_ip", "country", "duration", "packets")]
#remove zero durations
DATA <- DATA[DATA$duration >= 1,]

#create a new row for the bits per second
DATA["pps"] <- NA

#calculate bps
# packet is 1024 bytes //NO
# bits per second is #(packet)*1024*8/duration
# 1 Mbit = 1048576 bits
DATA["pps"] <- round( (DATA$packets / DATA$duration), 2)


dset["pps"] <- round( (dset$packets / set$duration), 0)


# ----- Basic Stats ------
med <- median(pps2) # median duration (sec)
avg <- mean(pps2) # average duration (sec)
std <- sd(pps2) # standard deviation
quantiles <- quantile(pps2) # quantiles, 0%, 25%, 50%, 75%, 100%
vari <- var(pps2) # variance

# ----- Plots -----
# Log Histogram Plot so that the data is better visible
# Freq = true -> Show the frequencies
hist(pps, freq=TRUE, xlab="Packet rate (pps)")
# Freq = true -> Show the probability distribution
# Prob = width (0.2) * height
# hist(durations, freq=FALSE, xlab="Log10 Duration (sec)")
curve(dnorm(x, mean=avg, sd=std), col="darkblue", lwd=2, add=TRUE, yaxt="n")


write.csv(DATA, "F:\\step2.csv")


d <- ddply(DATA, .(target_ip), summarise, count = length(duration))


unique(D$country)
unique(D$target-ip)



qplot(label, pps, data=dset_rest, geom=c("boxplot", "jitter"), fill=label, main="Boxplot for rest/target", xlab="", ylab="Packets per second")


boxplot(pm3_, use.cols = TRUE, ylim=c(0,3), ylab="log10(pps)", xlab="600s - on", main="Boxplots for pps")




#       Pearson's product-moment correlation
#
#data:  duration and packets
#t = 702.72, df = 3478000, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.3516830 0.3535236
#sample estimates:
#      cor 
#0.3526036 