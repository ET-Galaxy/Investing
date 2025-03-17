library(tidyverse)
library(xts)
initial_wealth=1
horizon=301

# Define stock tickers
stocks <- c(NVDA = 1, CASH =0)

set.seed=1
palet <- sample(colors(), 1)
wealth_dist<- data.frame(Start= as.vector(stocks), row.names = names(stocks))

portfolio<-numeric(horizon)
portfolio[1]<-1

# Read all CSV files and store them in a named list
data_list <- lapply(names(stocks)[-2], function(ticker) {
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
names(data_list) <- names(stocks)[-2]

#Percentage Change to the next day closing price
change_df<-data_list[[1]]$Date[-301]
for (i in 1:(length(stocks)-1)){
  change_df<-cbind(change_df,data_list[[i]]$Change[-301])
}

rebalance_days<-c()
drawdown<-wealth_max<-numeric(301)
wealth_max[1]<-wealth_dist[,1]
for (i in 2:horizon){
  wealth_dist[1,i]<-wealth_dist[1,i-1]*(1+change_df[i-1,-1])
  wealth_dist[2,i]<-wealth_dist[2,i-1]  #Cash
  portfolio[i]=sum(wealth_dist[,i])
  wealth_max[i]<-max(wealth_max[i-1],wealth_dist[1,i])
  drawdown[i]<-wealth_dist[1,i]/wealth_max[i]
  if (drawdown[i]<0.95 & wealth_dist[1,i]!=0){
    wealth_dist[2,i]<-wealth_dist[1,i]
    wealth_dist[1,i]<-0
    rebalance_days<-c(rebalance_days,i)
  } else if (wealth_dist[1,i]==0 & data_list[[1]]$Price[i]>=wealth_dist[2,i]){
    wealth_dist[1,i]<-wealth_dist[2,i]
    wealth_dist[2,i]<-0
  }
}

names(portfolio)<-names(wealth_dist)<-data_list[[1]]$Date
rebalance_days<-data_list[[1]]$Date[rebalance_days]

plot(portfolio, type = "l", col = "turquoise", lwd = 2,  
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
