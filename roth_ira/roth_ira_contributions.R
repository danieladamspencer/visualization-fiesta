P <- 1000000
rate <- .1
time.steps <- 33
A <- vector('numeric', time.steps)
A[1] <- P*rate
for(tt in seq(2,time.steps)) {
  A[tt] <- A[tt - 1] * (1 + rate)
}
plot(A)
money <- c(P,A)
plot(0:time.steps,money, type = 'l')
compound <- function(principle,rate,time,num_times = 1) return(principle*(1 + rate / num_times)^(num_times*time))
compound_money <- compound(103559.15,0.077,seq(0,time.steps))
plot(0:time.steps,compound_money, type = 'l')
data.frame(years = 0:time.steps, money = compound_money)

my_roth <- function(current_age, current_bal, ann_rate, n_years, ann.contrib = "max", already_contrib = TRUE) {
  if(current_age < 50 & ann.contrib == "max") ann.contrib <- 6000
  if(current_age >= 50 & ann.contrib == "max") ann.contrib <- 7000
  balance <- vector('numeric',(n_years))
  if(already_contrib) balance[1] <- current_bal*(1 + ann_rate)
  if(!already_contrib) balance[1] <- (current_bal + ann.contrib)*(1 + ann_rate)
  start_at <- ifelse(already_contrib,1,0)
  for(year in seq(2,n_years)) {
    current_age <- current_age + 1
    if(current_age >= 50 & ann.contrib == "max") ann.contrib <- 7000
    balance[year] <- (balance[year - 1] + ann.contrib)*(1 + ann_rate)
  }
  return(balance)
}

roth_bal <- my_roth(32,103559.15,0.077,33)
plot(roth_bal, type = 'l', xlab = "Years from Now", ylab = "Roth IRA Balance", xlim = c(0,40))
lines(1:33,compound_money[-1], col = 'red')
text(33,tail(roth_bal,1), labels = prettyNum(tail(roth_bal,1),big.mark = ","),pos = 4)
text(33,tail(compound_money,1), labels = prettyNum(tail(compound_money,1),big.mark = ","),pos = 4, col = 'red')
