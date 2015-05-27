df <- read.csv('birth.csv')

total <- sum(df$Births)
df$week <- substr(df$Week, 1, 2)
df$day <- substr(df$Week, 5, 5)

df$p <- df$Births / total

# function for getting probability
how <- function(date = NULL,
                due_date = as.Date('2015-05-21')){
  
  if(is.null(date)){
    date <- Sys.Date()}
  
  # Get current week and day
  day <- as.numeric(date - due_date) 
  days <- 280 + day
  if(day < 0){
    day <- (700 + day) %% 7   } else {
      day <- day %% 7
    }
  week <- days %/% 7
  
  
  #which row are you on
  d <- which(df$week == week & df$day == day)
  # get only later rows
  new_df <- df[d:nrow(df),]
  # get total number of births
  new_total <- sum(new_df$Births)
  
  # get percentage for each day
  new_df$percentage <- new_df$Births / new_total * 100
  
  # get cumulative percentage
  new_df$cum_p <- cumsum(new_df$percentage)
  
  new_df$date <- 0:(nrow(new_df) - 1) + date
  
  # subset
  new_df <- new_df[,c('date', 'week', 'day', 'percentage', 'cum_p')]
  return(new_df)
}

temp <- how()

plot(temp$date, temp$cum_p,
     type = 'l',
     col = adjustcolor('black', alpha.f = 0.6),
     lwd = 3, 
     xaxt = 'n',
     xlab = NA,
     ylab = 'Cumulative probability')
axis(side = 1,
     at = temp$date,
     labels = temp$date,
     las = 3,
     cex.axis = 0.7)
abline(h = seq(0, 100, 20),
       col = adjustcolor('black', alpha.f = 0.4))
abline(v = temp$date, 
       col = adjustcolor('black', alpha.f = 0.6))

plot(temp$date, 
     temp$percentage,
     type = 'l', 
     col = adjustcolor('black', alpha.f = 0.6),
     lwd = 3,
     xaxt = 'n',
     xlab = NA,
     ylab = 'Daily probability')
axis(side = 1,
     at = temp$date,
     labels = temp$date,
     las = 3,
     cex.axis = 0.7)
abline(h = seq(0, 100, 2),
       col = adjustcolor('black', alpha.f = 0.4))
abline(v = temp$date, 
       col = adjustcolor('black', alpha.f = 0.6))

# Cascading probabilities
dates <- seq(Sys.Date() - 30, Sys.Date() + 15, 1)

plot(dates, seq(1, 100, length = length(dates)),
     type = 'n', xlab = 'Date',
     ylab = 'Cumulative probability of spontaneous labor',
     cex.axis = 0.7)
for (i in 1:length(dates)){
  temp_date <- dates[i]
  if(temp_date == Sys.Date()){
    col <- 'red'
    lwd <- 3
  } else{
    col <- 'black'
    lwd <- 1
  }
  col <- adjustcolor(col, alpha.f = 0.5)
  temp_df <- how(date = as.Date(temp_date))
  lines(temp_df$date, temp_df$cum_p, 
        col = col,
        lwd = lwd)
}
abline(v = as.Date('2015-05-21'), 
       col = adjustcolor('darkblue', alpha.f = 0.6))

legend('topleft',
       lty = 1,
       legend = c('Bottom of line = date you\'re at now',
                  'Rest of line = your trajectory'),
       col = 'white')
title(main = 'When will Coloma give birth?')
