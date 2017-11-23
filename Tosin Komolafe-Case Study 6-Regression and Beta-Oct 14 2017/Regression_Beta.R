###############################################################################################################
#Tosin Komolafe
#Case Study 6
#Statistics-0617-B
#Instructor: Professor Steven Stelk
###############################################################################################################

###############################################################################################################
#STEP 1: Download data for last 1 year for the DJIA (Dow Jones Industrial Average) and all its 30 constituent 
#stocks. Download data from an appropriate financial website such as Google Finance, Yahoo Finance, Quandl, 
#CityFALCON, or another similar source
###############################################################################################################

#Stock Data from October 14, 2016 to October 14, 2017 (one year)

extra_csv_data = function(symbol){
  stock_data = read.csv(paste("stock_data/",symbol,".csv", sep=""),header=TRUE, sep=",")
  stock = stock_data[,c(1,6)] # 1 & 6 is the Date & Adj. Close Column respectively
  return(stock)
}

#DJIA - Dow Jones Industrial Average
djia = extra_csv_data("^DJI")
print("<<< DJIA Daily Stock >>>")
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
print("<<< 30 Components Stock Daily Data >>>")
for(i in 1:number_of_compound_stocks){
  print(paste("<<< ",compounds[i], "Daily Stock >>>", seq=""))
  compound_stocks[[i,1]] = extra_csv_data(compounds[i])
  print(compound_stocks[[i,1]])
  print("")
}


###############################################################################################################
#STEP 2: Calculate daily returns of the DJIA index and the downloaded stocks over the period under study
###############################################################################################################
daily_returns = function(stock_data){
  count = length(stock_data$Adj.Close)
  stock_data$Returns[1]= 0 
  for (i in 2:count){
    stock_data$Returns[i] = (stock_data$Adj.Close[i]/stock_data$Adj.Close[i-1]-1)
  }
  return(stock_data)
}

alpha_vector = c()
beta_vector = c()

print("<<< DJIA Daily Returns >>>")
djia_returns = daily_returns(djia)
djia_returns


print("<<< 30 Components Daily Returns >>>")
compound_stock_returns = matrix(list(), nrow=number_of_compound_stocks, ncol=1)
for(i in 1:number_of_compound_stocks){
  print(paste("<<< ",compounds[i], "Daily Returns >>>", seq=""))
  compound_stock_returns[[i,1]] = daily_returns(compound_stocks[[i,1]])
  print(compound_stock_returns[[i,1]])
  print("")


###############################################################################################################
#STEP 3: Considering the equation form provided above and matching the Index returns vs. the returns of one of 
#its constituent stocks at a time, perform linear regression fits and calculate Alpha and Beta values 
#for each stock
###############################################################################################################
  matching_returns =data.frame(djia=djia_returns$Returns,compound_stocks=compound_stock_returns[[i,1]]$Returns)
  lm_result = lm(compound_stock_returns[[i,1]]$Returns ~ djia_returns$Returns, data=matching_returns)
  alpha = coef(lm_result)["(Intercept)"]
  beta = coef(lm_result)["djia_returns$Returns"]
  print(paste("<<< Alpha of ", compounds[i], " = ", alpha, " >>>", sep=""))
  print(paste("<<< Beta of ", compounds[i], " = ", beta, " >>>", sep=""))
  alpha_vector = c(alpha_vector, alpha)
  beta_vector = c(beta_vector, beta)
  
}

###############################################################################################################
#STEP 4: Graphically represent the distribution of (alpha) and beta values for the constituents of DJIA
###############################################################################################################
hist(alpha_vector, breaks=15, main="Distribution of Alpha")
hist(beta_vector, breaks=15, main="Distribution of Beta")