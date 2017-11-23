###############################################################################################################
#Tosin Komolafe
#Case Study 4
#Statistics-0617-B
#Instructor: Professor Steven Stelk
###############################################################################################################

###############################################################################################################
#STEP 1: Download data for last 3 years for the DJIA (Dow Jones Industrial Average) and each of the 30 
#component stocks. Download data from an appropriate financial website such as Google Finance, Yahoo Finance, 
#Quandl, CityFALCON, or another similar source. If you are using the R language, then there are videos in the 
#"Supplemental Videos in R" located in the "Supplemental Materials" at the bottom of the course ware on how to 
#import CSV files into your program.
###############################################################################################################

#Stock Data from November 2014 to October 2017 (3 years)

extra_csv_data = function(symbol){
  stock_data = read.csv(paste("stock_data/",symbol,".csv", sep=""),header=TRUE, sep=",")
  stock = stock_data[,c(1,6)] # 1 & 6 is the Date & Adj. Close Column respectively
  return(stock)
}
#DJIA - Dow Jones Industrial Average
djia = extra_csv_data("DJI")
print("<<< DJIA Monthly Stock >>>")
djia

###############################################################################################################
#30 COMPOUND STOCKS

#DD	E. I. du Pont de Nemours and Company	
#XOM	Exxon Mobil Corporation
#UTX	United Technologies Corporation
#HD	The Home Depot, Inc.
#VZ	Verizon Communications Inc.
#INTC	Intel Corporation
#BA	The Boeing Company
#TRV	The Travelers Companies, Inc.	
#WMT	Wal-Mart Stores, Inc.
#CVX	Chevron Corporation
#IBM	International Business Machines Corporation
#MCD	McDonald's Corporation
#DIS	The Walt Disney Company
#V	Visa Inc.
#PG	The Procter & Gamble Company	
#MRK	Merck & Co., Inc.	
#CSCO	Cisco Systems, Inc.	
#AXP	American Express Company
#PFE	Pfizer Inc.	
#GS	The Goldman Sachs Group, Inc.	
#MSFT	Microsoft Corporation	
#JNJ	Johnson & Johnson	
#AAPL	Apple Inc.	
#KO	The Coca-Cola Company
#MMM	3M Company	
#UNH	UnitedHealth Group Incorporated	
#CAT	Caterpillar Inc.	
#JPM	JPMorgan Chase & Co.	
#NKE	NIKE, Inc.	
#GE	General Electric Company
###############################################################################################################
compounds = c("DD","XOM","UTX","HD","VZ","INTC","BA","TRV","WMT","CVX","IBM","MCD","DIS","V","PG",
              "MRK","CSCO","AXP","PFE","GS","MSFT","JNJ","AAPL","KO","MMM","UNH","CAT","JPM","NKE","GE")
number_of_compound_stocks = length(compounds)
compound_stocks = matrix(list(), nrow=number_of_compound_stocks, ncol=1)
print("<<< 30 Components Stock Monthly Data >>>")
for(i in 1:number_of_compound_stocks){
  print(paste("<<< ",compounds[i], "Monthly Stock >>>", seq=""))
  compound_stocks[[i,1]] = extra_csv_data(compounds[i])
  print(compound_stocks[[i,1]])
  print("")
}


###############################################################################################################
#STEP 2: Calculate Monthly returns of the DJIA index and the downloaded stocks over the period under study
###############################################################################################################
monthly_returns = function(stock_data){
  count = length(stock_data$Adj.Close)
  stock_data$Returns[1]= 0 
  for (i in 2:count){
    stock_data$Returns[i] = (stock_data$Adj.Close[i]/stock_data$Adj.Close[i-1]-1)
  }
  return(stock_data)
}

print("<<< DJIA Monthly Returns >>>")
djia_returns = monthly_returns(djia)
djia_returns

print("<<< 30 Components Stock Monthly Returns >>>")
compound_stock_returns = matrix(list(), nrow=number_of_compound_stocks, ncol=1)
for(i in 1:number_of_compound_stocks){
  print(paste("<<< ",compounds[i], "Monthly Returns >>>", seq=""))
  compound_stock_returns[[i,1]] = monthly_returns(compound_stocks[[i,1]])
  print(compound_stock_returns[[i,1]])
  print("")
}


###############################################################################################################
#STEP 3: Calculate mean and standard deviation of monthly returns for the DJIA index
###############################################################################################################
print("<<< Mean of DJIA >>>")
djia_mean = mean(djia_returns$Returns)
djia_mean
print("<<< Standard Deviation of DJIA >>>")
djia_sd = sd(djia_returns$Returns)
djia_sd

###############################################################################################################
#STEP 4: Choose an equal weighted portfolio consisting of any 5 random stocks from the DJIA, calculate the 
#mean monthly returns and its standard deviation. Do the same for portfolios of 10,15, 20 and 25 random stocks 
#from the DJIA universe
###############################################################################################################

cummulative_monthly_returns = function(number_of_random_stocks) {
  cases = sample(seq(1,number_of_compound_stocks), number_of_random_stocks)
  cummulative_returns = numeric(number_of_random_stocks)
  for (i in 1: number_of_random_stocks) {
    cummulative_returns[i] = sum(compound_stock_returns[[cases[i],1]]$Returns)/length(compound_stock_returns[[cases[i],1]]$Returns)
  }
  return(cummulative_returns)
}
  
random_options = 5
random_mean_portfolios = numeric(random_options)
random_sd_portfolios = numeric(random_options)
tracking_mean_errors = numeric(random_options)
tracking_standard_deviation_errors = numeric(random_options)
for(i in 1:random_options){
  print(paste("<<< Mean of", random_options * i, "random stock from DJIA Components  >>>"))
  random_mean_portfolios[i] = mean(cummulative_monthly_returns(random_options * i))
  print(random_mean_portfolios[i]) 
  print(paste("<<< Standard Deviation of", random_options * i, "random stock from DJIA Components  >>>"))
  random_sd_portfolios[i] = sd(cummulative_monthly_returns(random_options * i))
  print(random_sd_portfolios[i]) 

###############################################################################################################
#STEP 5: Calculate tracking errors for each of the portfolios i.e. the margin by which the mean and 
#standard deviation of the portfolio returns diverge from those of DJIA
###############################################################################################################
  
  print(paste("<<< Tracking Mean Error of", random_options * i, "random stock from DJIA Components  >>>"))
  tracking_mean_errors[i] = random_mean_portfolios[i] - djia_mean
  print(tracking_mean_errors[i])
  print(paste("<<< Tracking Standard Deviation Error of", random_options * i, "random stock from DJIA Components  >>>"))
  tracking_standard_deviation_errors[i] = random_sd_portfolios[i] - djia_sd
  print(tracking_standard_deviation_errors[i]) 
  print("")
}


###############################################################################################################
#STEP 6: Graphically represent the tracking error for returns and risk (standard deviation of returns used as a 
#proxy for risk) on y-axis against the sample size of portfolio on the x-axis
###############################################################################################################
plot(c("5", "10", "15", "20", "25"),tracking_standard_deviation_errors,xlab="Sample size",ylab="Average returns",
     main="Tracking Error for Returns and Risk")

###############################################################################################################
#Based on the results of your findings, complete the following analysis:
###############################################################################################################

###############################################################################################################
#Question 1: What all factors account for the tracking error of the constructed portfolios?

#Answer: 1) The sample size of the random stock portfolio, 2) The monthly return of DJIA, 
#3) The random mean and standard deviation of the portfolio
###############################################################################################################

###############################################################################################################
#Question 2: What is the relationship between tracking error and portfolio sample size?

#Answer: The tracking error decreases with higher portfolio sample sizes but interestingly, the tracking error 
#is mostly lowest when its a 5 portfolio sample
###############################################################################################################

###############################################################################################################
#Question 3: What might be the most optimal way to decrease tracking error without having to construct a 
#full portfolio matching the entire index

#Answer: The most optimal way might be to select stocks that have mean returns close to the returns of the 
#index
###############################################################################################################
