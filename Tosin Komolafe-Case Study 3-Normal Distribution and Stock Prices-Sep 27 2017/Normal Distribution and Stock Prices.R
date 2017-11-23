###############################################################################################################
#Tosin Komolafe
#Case Study 3
#Statistics-0617-B
#Instructor: Professor Steven Stelk
###############################################################################################################

###############################################################################################################
#STEP 1: Consider following values for the purpose of this project:
###############################################################################################################

price = 10   #USD ($)
rate = 0.15 #(15% expected return per year)
sigma = 0.20 #(20% annual volatility in prices)
time = 1    #one year
number_of_steps = 100  #number of steps involved in calculation
distribution_term = 0.15 # e the distribution term with a zero mean

next_stock_price = function(price, rate, sigma, time, number_of_steps, distribution_term) {
  change_of_time = time/number_of_steps #change of time is the size of the unit step size=T/n 
  return(price * exp(((rate -(1/2 * sigma^2)) * change_of_time) + (sigma * distribution_term * sqrt(change_of_time))))
}

###############################################################################################################
#STEP 2: Starting with the initial stock price St as specified, and considering 100 Steps, 
#calculate the expected value of the stock price at the end of every successive change of time interval of time
###############################################################################################################

expected_values = function(price, distribution_term, random_distribution_term=FALSE) {
  stock_prices = c()
  stock_prices[1] = price
  for (i in 1:number_of_steps){
    if (random_distribution_term){
      distribution_term = rnorm(1)
    }
    stock_prices[i+1] = next_stock_price(stock_prices[i], rate, sigma, time, number_of_steps, distribution_term)
  }
  return(stock_prices)
}

stock_prices = expected_values(price, distribution_term)

###############################################################################################################
#STEP 3: Plot the entire movement of prices over the T period under observation
###############################################################################################################

plot(stock_prices, xlim=c(0, number_of_steps), ylim=c(10, 17), main='The Price Movement Over One Year with Fixed Distribution Term', xlab='Steps', ylab='Stock Prices', type='l', col='blue')

###############################################################################################################
#STEP 4: Instead of considering a fixed ?? as in the previous steps, randomly assign values to e 
#from a standard normal distribution.
###############################################################################################################

stock_prices = expected_values(price, distribution_term, random_distribution_term=TRUE)
plot(stock_prices, xlim=c(0, number_of_steps), ylim=c(5, 17), main='The Price Movement Over One Year with Random Distribution Term', xlab='Steps', ylab='Stock Prices', type='l', col='blue')

###############################################################################################################
#STEP 5: Perform 5 trials of 100 steps each to plot the probable movement of stock prices over a 1 year period. 
#        Plot each trajectory of prices as a separate line.
###############################################################################################################

colors = c('chartreuse4','coral4','blue4','darkgoldenrod','darkmagenta','dimgray','lightcoral','greenyellow','orangered4','black')

plot(stock_prices, xlim=c(0, number_of_steps), ylim=c(5, 17), main='Five Trials of Price Movement Over One Year with Random Distribution Term', xlab='Steps', ylab='Stock Prices', type='n')

trial = 5
for (t in 1:trial){
  stock_prices = expected_values(price, distribution_term, random_distribution_term=TRUE)
  lines(stock_prices, col=colors[t])
  text(90, stock_prices[101], paste('Trial - ',t), cex=1.0, col=colors[t])
}


###############################################################################################################
#After completing the project, answer the following as part of your analysis:
###############################################################################################################

###############################################################################################################
#Question 1: How wide a variance is noticeable in the final year-end price of the stock for the 
#5 separate trials performed through steps IV and V? Analyze and draw your conclusions.


#Answer: The fact about analyzying this cannot be fully conclusive with just five trials but these five trials 
#helps us to see a low variance because the stock prices are normally distributed. Concluding, we expect that 
#the final year-end price of the stock for the 5 seperate trials should have a relatively low variance that is,
#the price will not likely drift far away from the intial stock price.
###############################################################################################################


###############################################################################################################
#Question 2: Would the variance been higher if e (distribution term) was assigned purely on a random basis 
#from an arbitrary distribution? 


#Answer: Yes, the variance will be higher if the distribution term was assigned from an arbitrary distribution.
#Because there will be a likely chance of steep decrease or increase in relation to the price changes 
#for each of the steps.
###############################################################################################################