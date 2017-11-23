###############################################################################################################
#Tosin Komolafe
#Case Study 5
#Statistics-0617-B
#Instructor: Professor Steven Stelk
###############################################################################################################

###############################################################################################################
#STEP 1: Download data for last 1 year for the S&P500 and any 10 of its component stocks. Download data from 
#an appropriate financial website such as Google Finance, Yahoo Finance, Quandl, CityFALCON, or another similar
#source.
###############################################################################################################

extra_csv_data = function(symbol){
  stock_data = read.csv(paste("stock_data/",symbol,".csv", sep=""),header=TRUE, sep=",")
  stock = stock_data[,c(1,6)] # 1 & 6 is the Date & Adj. Close Column respectively
  return(stock)
}

daily_returns = function(stock_data){
  count = length(stock_data$Adj.Close)
  stock_data$Returns[1]= 0 
  for (i in 2:count){
    stock_data$Returns[i] = (stock_data$Adj.Close[i]/stock_data$Adj.Close[i-1]-1)
  }
  return(stock_data)
}

###############################################################################################################
#^GSPC - S&P 500
#Stock Data from October 10 2016 to October 10 2017 (1 year)

#10 COMPOUND STOCKS

#FDX - FedEx Corporation
#XOM - Exxon Mobil Corporation
#FB - Facebook, Inc.
#DPS - Dr Pepper Snapple Group, Inc.
#HSY - The Hershey Company
#K - Kellogg Company
#MSFT - Microsoft Corporation
#PEP - Pepsico, Inc.
#SBUX - Starbucks Corporation
#VRTX - Vertex Pharmaceuticals Incorporated
###############################################################################################################

stocks = c("^GSPC","FDX","XOM","FB","DPS","HSY","K","MSFT","PEP","SBUX","VRTX")

###############################################################################################################
#STEP 2: Calculate daily returns of the S&P500 index and the downloaded stocks over the period under study.
###############################################################################################################
number_of_test_passes = 0
number_of_stocks = length(stocks)
all_stocks = matrix(list(), nrow=number_of_stocks, ncol=1)
for(i in 1:number_of_stocks){
  print(paste("<<< ",stocks[i], "Daily Stock Returns >>>", seq=""))
  all_stocks[[i,1]] = daily_returns(extra_csv_data(stocks[i]))
  print(all_stocks[[i,1]])
  print("")
  
###############################################################################################################
#STEP 3: For each of the selected stocks and the index perform a Student’s T test, calculate the p-value and 
#t-value and test the Null Hypothesis that the mean daily stock return is zero.
###############################################################################################################
  
  number_of_daily_returns = length(all_stocks[[i,1]]$Returns)
  sample_mean = mean(all_stocks[[i,1]]$Returns)
  standard_deviation = sd(all_stocks[[i,1]]$Returns)
  mean_zero = 0
  
  print(paste("<<< ",stocks[i], "- Null Hypothesis that the mean daily stock return is zero", seq=""))
  
  t_value= (sample_mean - mean_zero)/(standard_deviation/sqrt(number_of_daily_returns))
  print("<<< t-value >>>")
  print(t_value)
  
  p_value = 2 * pt(-abs(t_value), df=number_of_daily_returns-1, lower=FALSE)
  print("<<< p-value >>>")
  print(p_value)
  print("")
  
  alpha = 0.05
  print("Student's T-Test")
  print("Null Hypothesis - H0: mean_zero = 0")
  print("Alternate Hypothesis - H1: mean_zero != 0")
  print("Test H0")
  
  if (p_value > alpha) {
    print("Do Not Reject H0")
    number_of_test_passes = number_of_test_passes + 1
  } else {
    print("Reject H0")
  }
  print("")
}


###############################################################################################################
#Based on the results of your findings, complete the following analysis:
###############################################################################################################

###############################################################################################################
#Question 1: For how many stocks was the null hypothesis accepted.

#Answer:

print("No of stocks that did not reject the Null Hypothesis")
print(number_of_test_passes)
###############################################################################################################

###############################################################################################################
#Question 2: Given that you drew the stocks randomly from the index constituents, is it possible to extrapolate 
#the behavior of the index (in terms of the null hypothesis) from the average results obtained from analyzing 
#the stocks?

#Answer: Yes, it is possible. 
###############################################################################################################

