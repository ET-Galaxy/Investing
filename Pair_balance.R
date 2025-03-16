set.seed(1)
initial_wealth=1
allocation=c(0.5,0.5)
horizon=100

changeA<-exp(rnorm(horizon,mean=0.005,sd=0.02))
changeB<-exp(rnorm(horizon,mean=0.005,sd=0.02))

price_A<-cumprod(changeA)
price_B<-cumprod(changeB)

moneyA<-initial_wealth*allocation[1]
moneyB<-initial_wealth*allocation[2]
currA<-currB<-numeric(horizon)

for (i in 1:horizon){
  currA[i]<-moneyA*changeA[i]
  currB[i]<-moneyB*changeB[i]
  portfolio=currA[i]+currB[i]
  propA<-currA[i]/portfolio
  if (abs(propA-0.5)>0.02){
    moneyA<-moneyB<-portfolio/2
  } else{
    moneyA<-currA[i]
    moneyB<-currB[i]
  }
}

# Plot the first series
plot(price_A, type = "l", col = "blue", lwd = 2, ylim = range(c(price_A, price_B)), 
     xlab = "Time", ylab = "Price", main = "Simulated Price Paths")

# Add the second series
lines(price_B, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("A", "B"), col = c("blue", "red"), lwd = 2)


# Plot the first series
plot(currA, type = "l", col = "blue", lwd = 2, ylim = range(c(currA,currB,currA+currB)), 
     xlab = "Time", ylab = "Price", main = "Portfolio allocation")

# Add the second series
lines(currB, col = "red", lwd = 2)

# Add legend
legend("bottomright", legend = c("A", "B"), col = c("blue", "red"), lwd = 2)


# Compare with price and portfolio
plot(price_A, type = "l", col = "blue", lwd = 2, ylim = range(c(price_A, price_B)), 
     xlab = "Time", ylab = "Price", main = "Simulated Price Paths")

# Add the second series
lines(price_B, col = "red", lwd = 2)


lines((currA+currB)/100,col = "black", lwd = 2)

lines((price_A+price_B)/2,col="green", lwd=2)

# Add legend
legend("topleft", legend = c("A", "B", "portfolio", "half"), col = c("blue", "red", "black", "green"), lwd = 2)


propA_final<-currA/(currA+currB)
propB_final<-currB/(currA+currB)
# Plot the first series
plot(propA_final, type = "l", col = "blue", lwd = 2, ylim = range(c(0.47,0.53)), 
     xlab = "Time", ylab = "Price", main = "Portfolio allocation")

# Add the second series
lines(propB_final, col = "red", lwd = 2)
# Add legend
legend("topright", legend = c("A", "B"), col = c("blue", "red"), lwd = 2)





set.seed(42)

sim_price<-function(horizon,theta=0.05){
  # Parameters
  S0 <- 1              # Initial stock price
  mu_s <- 1            # Long-term mean (equilibrium level)
  
  # Initialize price series
  S <- numeric(horizon)
  S[1] <- S0
  
  # Generate stock price path
  for (t in 2:horizon) {
    noise <- rnorm(1, mean = 0, sd = 0.02)  # Random shock
    S[t] <- S[t-1] * exp(noise)  # GBM component
    S[t] <- S[t] + theta * (mu_s - S[t])  # Mean reversion adjustment
  }
  return(S)
}
S1<-sim_price(500)
S2<-sim_price(500)
changeA<-diff(S1)/S1[-500]
changeB<-diff(S2)/S2[-500]

moneyA<-initial_wealth*allocation[1]
moneyB<-initial_wealth*allocation[2]
currA<-currB<-numeric(horizon-1)

for (i in 1:(horizon-1)){
  currA[i]<-moneyA*(1+changeA[i])
  currB[i]<-moneyB*(1+changeB[i])
  portfolio=currA[i]+currB[i]
  propA<-currA[i]/portfolio
  if (abs(propA-0.5)>0.02){
    moneyA<-moneyB<-portfolio/2
  } else{
    moneyA<-currA[i]
    moneyB<-currB[i]
  }
}


# Compare with price and portfolio
plot(S1, type = "l", col = "blue", lwd = 2, ylim = range(c(S1, S2)), 
     xlab = "Time", ylab = "Price", main = "Simulated Price Paths")

# Add the second series
lines(S2, col = "red", lwd = 2)

lines(rep(mu_s,horizon), col = "red", lty = 2)  # Show equilibrium level

lines(currA+currB,col = "black", lwd = 2)

lines((S1+S2)/2,col="green", lwd=2)


