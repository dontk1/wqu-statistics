############################################################################################################################
#Tosin Komolafe
#Case Study 1
#Statistics- 0617-B
#Instructor: Professor Steven Stelk
############################################################################################################################

############################################################################################################################
#STEP 1:Download data for last 2 years for the NASDAQ Composite. 
#Downloaded from Yahoo Finance
#NASDAQ Composite Historical Data from September 16, 2015 - September 15, 2017
############################################################################################################################

nasdaq_composite_data = read.csv("IXIC.csv",header=TRUE, sep=",")
nasdaq_composite = nasdaq_composite_data[,c(1,6)] # 1 & 6 is the Date & Adj. Close Column respectively
nasdaq_composite

############################################################################################################################
#STEP 2: Calculate Daily returns of NASDAQ composite for the last 1 year
#NASDAQ one year will be from September 16, 2016 - September 15, 2017
############################################################################################################################

last_one_year_nasdaq = nasdaq_composite[c(which(nasdaq_composite$Date=='2016-09-16'): which(nasdaq_composite$Date=='2017-09-15')),]
count = length(last_one_year_nasdaq$Adj.Close)
#daily_returns = numeric(count)
for (i in 1:count-1){
	last_one_year_nasdaq$Returns[i] = (last_one_year_nasdaq$Adj.Close[i+1]/last_one_year_nasdaq$Adj.Close[i]-1)
}
last_one_year_nasdaq

############################################################################################################################
#STEP 3: Graphically represent the stock prices as a line plot
############################################################################################################################
plot(nasdaq_composite, xlab="Date", ylab="Adj Close($)", main="NASDAQ Composite Price During September 16, 2015 to September 15, 2017")
lines(nasdaq_composite,col="blue")

############################################################################################################################
#STEP 4: Bucket the daily return values into bins and plot a histogram
############################################################################################################################
returns = last_one_year_nasdaq$Returns
min=min(returns)
max=max(returns)
mean=mean(returns)
median=median(returns)
sd=sd(returns)
skewness=(3*(mean-median))/sd
par(oma=c(0,0,0,2))
hist(returns, right=FALSE, breaks=seq(min,max,length=15),xlab="Daily Returns", ylab="Frequency",main="Daily Returns Distribution of NASDAQ Composite during September 16, 2016 - September 15, 2017",col=c("blue"))
points(seq(min, max, length.out=500),dnorm(seq(min,max,length.out=500),mean, sd), type="l", col="red")

############################################################################################################################
#STEP 5: Calculate mean, median and standard deviation of Daily return values and plot them on the same graph mentioned in step IV
############################################################################################################################

text(0.02, 50, paste("Mean=", format(round(mean,8))))
text(0.02, 45, paste("Median=", format(round(median,8),nsmall=8)))
text(0.018, 40, paste("Standard Deviation=", format(round(sd,8),nsmall=8)))
text(0.019, 35, paste("Skewness=", format(round(skewness,8),nsmall=8)))

############################################################################################################################
# Based on your findings, evaluate the following:
############################################################################################################################
# Question 1: How is the daily returns of NASDAQ distributed? Does it follow a normal distribution? 
#
# Answer: The daily returns of NASDAQ follows an approximately symmertic normal distribution with a bell-curve shape. 
# The skewness is around -0.013 which is closer to 0, mean is around 0.083% and standard deviation is around 0.65%. 
############################################################################################################################
# Question 2: Are any obvious trends visible in movement of NASDAQ prices for the period under study?
#
# Answer: There is an uptrends. From early 2016 to the current date Sep 15, 2017
#         The trend begined with a big up-surge from the bottom of the valley and even though sometimes it goes down, it has still
#         remained an an upward momentum.
############################################################################################################################
# Question 3: Analyze the measures of central tendency calculated and offer opinion on the overall risks and possible rewards 
#             associated with investing in the NASDAQ index for the period under study.
#
# Answer: During the period of study, we can earn so much return of investment in the uptrend period that began since early 2016
#         Imagine, if we invest in 2016 with about $4,200, by current date, our return of investment will be above 40% 
#	    However, there are some risk involved in this portfolio that can cause significant risk and also losses. But in this NASDAQ 
#         composite, since it's a highly profitable portfolio, we can cut our loss when we reach a price surge less than 20-25% 
#         between our purchase price and current price.
###########################################################################################################################