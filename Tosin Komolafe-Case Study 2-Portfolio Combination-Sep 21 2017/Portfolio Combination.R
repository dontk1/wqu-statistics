############################################################################################################################
#Tosin Komolafe
#Case Study 2 - Portfolio Combination
#Statistics- 0617-B
#Instructor: Professor Steven Stelk
############################################################################################################################

############################################################################################################################
#STEP 1:Download data for last 1 years for a set of the any five stock tickers belonging to the same industry segment. 
#Downloaded from Yahoo Finance focused on the five technology industries (AAPL,CSCO,FB,GOOG,MSFT)
#NASDAQ Composite Historical Data from October 1, 2016 - September 20, 2017
############################################################################################################################

# 1 & 6 is the Date & Adj. Close Column respectively

#AAPL - Apple Inc.
aapl_data = read.csv("AAPL.csv",header=TRUE, sep=",")
aapl = aapl_data[,c(1,6)] 
aapl

#CSCO - Cisco Systems, Inc.
csco_data = read.csv("CSCO.csv",header=TRUE, sep=",")
csco = csco_data[,c(1,6)]
csco

#FB - Facebook, Inc.
fb_data = read.csv("FB.csv",header=TRUE, sep=",")
fb = fb_data[,c(1,6)]
fb

#GOOG - Alphabet Inc.
goog_data = read.csv("GOOG.csv",header=TRUE, sep=",")
goog = goog_data[,c(1,6)]
goog

#MSFT - Microsoft Corporation 
msft_data = read.csv("MSFT.csv",header=TRUE, sep=",")
msft = msft_data[,c(1,6)] 
msft

############################################################################################################################
#STEP 2: Calculate Monthly returns of downloaded stock over the period under study
############################################################################################################################

monthly_returns = function(stock_data){
	year_stock_data = stock_data[c(which(stock_data$Date=='2016-10-01'): which(stock_data$Date=='2017-09-01')),]
	count = length(year_stock_data$Adj.Close)
	for (i in 1:count-1){
		year_stock_data$Returns[i] = (year_stock_data$Adj.Close[i+1]/year_stock_data$Adj.Close[i]-1)
	}
	return(year_stock_data)
}

"AAPL Monthly Returns"
aapl_returns = monthly_returns(aapl)
aapl_returns

"CSCO Monthly Returns"
csco_returns = monthly_returns(csco)
csco_returns

"FB Monthly Returns"
fb_returns = monthly_returns(fb)
fb_returns

"GOOG Monthly Returns"
goog_returns = monthly_returns(goog)
goog_returns

"MSFT Monthly Returns"
msft_returns = monthly_returns(msft)
msft_returns


############################################################################################################################
#STEP 3:Using a combination function, calculate the monthly returns of an equally 
#       weighted portfolio consisting of any 3 of the five stocks(AAPL,CSCO,FB,GOOG,MSFT) in question 
############################################################################################################################
number_of_cases = 3
cases = combn(c(1,2,3,4,5),number_of_cases)
length = length(cases)/number_of_cases
variance_sum = 0
cummulative_average_returns = NULL
cummulative_variance = 0
num_points = length(aapl_returns$Returns) 
color = c('chartreuse4','coral4','blue4','darkgoldenrod','darkmagenta',
'dimgray','lightcoral','greenyellow','orangered4','black')
plot(0, 0, xlim=c(1,13), ylim=c(-0.05,0.35),xlab="Month", ylab="Cumulative Monthly Return", main="Equally Weighted Portfolio of Cummulative Returns from October 2016 - September 2017", type = "n")
  	
for (i in 1:length){
	average_returns = numeric(num_points)
	for (j in 1:num_points){
		average_returns[j]=0
		for (k in 1:number_of_cases){
			return = 0
			stock_quote = cases[k, i]
			if (stock_quote==1){
				return=aapl_returns$Returns[j]
			}else if (stock_quote==2){
				return=csco_returns$Returns[j]
			}else if (stock_quote==3){
				return=fb_returns$Returns[j]
			}else if (stock_quote==4){
				return=goog_returns$Returns[j]
			}else if (stock_quote==5){
				return=msft_returns$Returns[j]
			}
			average_returns[j] = average_returns[j]+return
		}
		average_returns[j] = average_returns[j]/number_of_cases
		cummulative_average_returns = append(cummulative_average_returns, average_returns[j])
	}
	mark = ''
	for (n in 1:number_of_cases){
		stock_quote = cases[n,i]
		if (stock_quote==1){
			mark = paste(mark,'AAPL',seq=' ')		
		}else if (stock_quote==2){
			mark = paste(mark,'CSCO',seq=' ')
		}else if (stock_quote==3){
			mark = paste(mark,'FB',seq=' ')
		}else if (stock_quote==4){
			mark = paste(mark,'GOOG',seq=' ')
		}else if (stock_quote==5){
			mark = paste(mark,'MSFT',seq=' ')
		}
   	
	}
	print(mark)
  print(average_returns)

############################################################################################################################
#STEP 4:Graphically represent the cumulative monthly returns of each of the possible portfolios through line plots in question 
############################################################################################################################
  	cummulative_monthly_returns = numeric(num_points)
  	cummulative_monthly_returns[1] = average_returns[1]
  	for (m in 2:num_points) {
    		cummulative_monthly_returns[m] = cummulative_monthly_returns[m-1]+average_returns[m]
  	}
	lines(cummulative_monthly_returns, col=color[i])

############################################################################################################################
#STEP 5:Calculate mean, median and standard deviation of monthly values for each of the portfolios in question 
#	  and plot them on the same graph mentioned in step 4. 
############################################################################################################################
  	mean = mean(average_returns)
  	median = median(average_returns)
  	std = sd(average_returns)
  	text(10, cummulative_monthly_returns[num_points-2],paste(mark, 'Portfolio Monthly Returns:','Mean = ', format(round(mean,8),nsmall=8),
	'Median = ', format(round(median,8), nsmall=8),'SD = ', format(round(std,8), nsmall=8)),cex=0.4)  
	cummulative_variance = cummulative_variance + (std^2)
}
############################################################################################################################
#STEP 6:Calculate the overall variance of all portfolio returns. 
############################################################################################################################
overall_variance = cummulative_variance/length
overall_variance


############################################################################################################################
#Based on your project analysis, answer the following questions:
############################################################################################################################

############################################################################################################################
# Question 1: How are the monthly returns of possible portfolios distributed?
#
# Answer: Seeing this clearly shows I have to plot an histogram to properly analysis this. We can do this with the R command
#         for plotting histogram and after analysis, the monthly returns of (AAPL, CSCO, FB, GOOG & MSFT) portfolio follows 
#	    an approximately symmertic normal distribution with a bell-curve shape. 
#	    The skewness is around -0.3627 which is closer to 0, mean is around 1.7069% and standard deviation is around 3.6224%.

min=min(cummulative_average_returns)
max=max(cummulative_average_returns)
mean=mean(cummulative_average_returns)
sd=sd(cummulative_average_returns)
median=median(cummulative_average_returns)
skewness=(3*(mean-median))/sd
par(oma=c(0,0,0,2))
hist(cummulative_average_returns, right=FALSE, breaks=seq(min,max,length=15),xlab="Monthly Returns", ylab="Frequency",
	main="Monthly Returns Distribution for (AAPL, CSCO, FB, GOOG & MSFT) during October 2016 - September, 2017",col=c("blue"))
points(seq(min, max, length.out=500),dnorm(seq(min,max,length.out=500),mean, sd), type="l", col="red")
text(0.07, 15, paste("Mean=", format(round(mean,8))),cex=0.8)
text(0.07, 14, paste("Median=", format(round(median,8),nsmall=8)),cex=0.8)
text(0.07, 13, paste("Standard Deviation=", format(round(sd,8),nsmall=8)),cex=0.8)
text(0.07, 12, paste("Skewness=", format(round(skewness,8),nsmall=8)),cex=0.8)	
###########################################################################################################################
# Question 2: Do you see a wide variance in the possible portfolio returns and its cumulative outcome?
# 
# Answer:  No. I do not see a wide variance in the possible portfolio returns and its cumulative
#          outcome. All the portfolio combinations, sees to be moving in similar directions. 
#	     Hence, the normal distribution as I already answered in Question 1. 
###########################################################################################################################
# Question 3: Given that you chose similar stocks from the same industry, what accounts for the variance of returns 
#	        among different portfolios (if any)?
# 
# Answer 3: When we evaluate each company's performance we see that their individual performances are different. 
#           Apple's stock (AAPL) has been on the increase through out the year and has performed very well. The second best 
#           performing is Facebook (FB) and the top 3 porfolio combination which are AAPL, FB & MSFT are all gradually growing in 
#           terms of their individual company performance. However, Cisco's stock hasn't grown so much is the last one year. 
#           It's performance has been relatively low and stable. But because all the other companies including GOOG have been 
#           performing well in his last year under study, it has reduced the variances of the stock portfolio. 
###########################################################################################################################


