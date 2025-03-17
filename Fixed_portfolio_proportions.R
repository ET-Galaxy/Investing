library(tidyverse)
library(xts)
initial_wealth=1
horizon=301

# Define stock tickers
stocks <- c(
  AAPL = 0.06, NVDA = 0.06, AVGO = 0.04, TSM = 0.04,
  GOOG = 0.06, META = 0.06, KO = 0.04, LIN = 0.08,
  LLY = 0.06, JPM = 0.08, BRKB = 0.08, UBER = 0.04,
  AXP = 0.06, SONY = 0.04, PANW = 0.04, NFLX = 0.04, 
  GE = 0.04, SPOT = 0.04, ORCL = 0.04
)

set.seed=1
palet <- sample(colors(), 19)
wealth_dist<- data.frame(Start= as.vector(stocks), row.names = names(stocks))

props=matrix(nrow=19, ncol=horizon)
props[,1]=as.vector(stocks)

portfolio<-numeric(horizon)
portfolio[1]<-1

# Read all CSV files and store them in a named list
data_list <- lapply(names(stocks), function(ticker) {
  df <- read.csv(paste0("dataset/",ticker, ".csv"))[,-7]  # Remove 7th column
  df$Date <- as.Date(df$Date, format="%d/%m/%Y")  # Convert Date column
  df<-arrange(df, Date)
  df$Price <- gsub(",", "", df$Price)  # Remove commas
  df$Price <- trimws(df$Price)  # Trim spaces
  df$Price <- as.numeric(df$Price)  # Convert to numeric
  df$Change <- c(diff(df$Price) / df$Price[-301],NA)  # Calculate daily price change
  df$Price<-df$Price/df$Price[1]
  return(df)
})

# Assign names to the list for easier reference
names(data_list) <- names(stocks)

#Percentage Change to the next day closing price
change_df<-data_list[[1]]$Date[-301]
for (i in 1:length(stocks)){
  change_df<-cbind(change_df,data_list[[i]]$Change[-301])
}

rebalance_days<-c()
for (i in 2:horizon){
  wealth_dist[,i]<-wealth_dist[,i-1]*(1+change_df[i-1,-1])
  portfolio[i]=sum(wealth_dist[,i])
  props[,i]<-wealth_dist[,i]/portfolio[i]
  if (any(abs(props[,i]-props[,1])>0.2*props[,1])){
    wealth_dist[,i]<-portfolio[i]*props[,1]
    rebalance_days<-c(rebalance_days,i)
  } 
}

#Max drawdown
portfolio_max<-numeric(301)
portfolio_max[1]<-portfolio[1]
for (i in 1:301){
  portfolio_max[i]<-max(portfolio_max,portfolio[i])
}
drawdown<-portfolio/portfolio_max
plot(as.xts(drawdown,data_list[[1]]$Date))

names(portfolio)<-names(wealth_dist)<-data_list[[1]]$Date
rebalance_days<-data_list[[1]]$Date[rebalance_days]

plot(as.xts(portfolio), type = "l", col = "turquoise", lwd = 2,  
     xlab = "Time", ylab = "Price", main = "Portfolio")



#Individual stocks performance
for (i in 1:length(stocks)){
  lines(data_list[[i]]$Price, col=palet[i],lwd=0.5)
}

# Add legend
legend("topleft", legend = c("Portfolio",names(stocks)), 
       col=c("turquoise",palet), lwd = 1,
       bty = "n",            # Remove border
       cex = 0.3,            # Increase text size
       xpd = TRUE)           # Allow drawing outside plot region)

#Allocation ratios
plot(props[,1], type = "l", col = palet[1], lwd = 2, ylim=c(0.15,0.25),
     xlab = "Time", ylab = "Price", main = "Portfolio allocation")
for (i in 2:length(stocks)){
  lines(props[,i], col=palet[i], lwd=2)
}

# Add legend
legend("topright", legend = c(stocks), 
       col=c(palet), lwd = 2,
       bty = "n",            # Remove border
       cex = 0.5,            # Increase text size
       xpd = TRUE)           # Allow drawing outside plot region)
