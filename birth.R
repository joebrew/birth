df <- read.csv('birth.csv')

total <- sum(df$Births)
df$week <- substr(df$Week, 1, 2)
df$day <- substr(df$Week, 5, 5)

df$p <- df$Births / total

# function for getting probability
how <- function(week = 40, day = 3, date = Sys.Date()){
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
     ylab = 'Cumulative probability')
axis(side = 1,
     at = temp$date,
     labels = temp$date,
     las = 3,
     cex.axis = 0.7)
abline(h = seq(0, 100, 2),
       col = adjustcolor('black', alpha.f = 0.4))
abline(v = temp$date, 
       col = adjustcolor('black', alpha.f = 0.6))