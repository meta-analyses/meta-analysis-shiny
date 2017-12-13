
create_plot <- function (df){
  require(tidyverse)
  require(ggplot2)
  # df <- raw_data_tp_ltpa
  di <- seq(0, 78.75, by = 8.75)
  h <- hist(df$dose, breaks = di)
  c <- list()
  tpyrs <- sum(df$personyrs, na.rm = T)
  for (i in 1:length(di)){
    if (i < length(di))
      c[i] <- (round(sum(filter(df, dose >= di[i] & dose < di[i + 1]) %>% select(personyrs), na.rm = T) / tpyrs * 100, 2))
    else
      c[i] <- ( round(sum(filter(df, dose >= di[i] ) %>% select(personyrs), na.rm = T) / tpyrs * 100, 2))
    
  }
  
  # td <- data.frame(d = di[-1] , count = round(c(h$counts) / sum(h$counts) * 100, 2), 'Person Years (%)' = t(as.data.frame(c)), check.names = FALSE)
  td <- data.frame(d = di[-1] , 'MMET.h (%)' = round(c(h$counts) / sum(h$counts) * 100, 2), 'Person Years (%)' = t(as.data.frame(c)), check.names = FALSE)
  td <- reshape2::melt(td, id.vars = 'd')
  
  ggplot(td, aes(x = d, y = value, fill = variable)) +
    geom_bar(stat="identity", position = "dodge") +
    labs(title = "Histogram of Marginal MET hours (MMET.h) per week \n (height represents dose percentage, whereas colour represents person-years percentage)", x= "MMET.h per week", y="Percentage") +
    scale_x_continuous(labels = paste(seq(-8.75, 70, by = 8.75), "<", seq(0, 78.75, by = 8.75)),
                       breaks = seq(0, 78.75 , by = 8.75)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  c <- list()
  ttp <- sum(df$totalpersons, na.rm = T)
  for (i in 1:length(di)){
    if (i < length(di))
      c[i] <- (round(sum(filter(df, dose >= di[i] & dose < di[i + 1]) %>% select(totalpersons), na.rm = T) / ttp * 100, 2))
    else
      c[i] <- ( round(sum(filter(df, dose >= di[i] ) %>% select(totalpersons), na.rm = T) / ttp * 100, 2))
    

  }
  
  td <- data.frame(d = di[-1] , 'MMET.h (%)' = round(c(h$counts) / sum(h$counts) * 100, 2), 'Total Persons (%)' = t(as.data.frame(c)), check.names = FALSE)
  td <- reshape2::melt(td, id.vars = 'd')
  
  ggplot(td, aes(x = d, y = value, fill = variable)) +
    geom_bar(stat="identity", position = "dodge") +
    labs(title = "Histogram of Marginal MET hours (MMET.h) per week \n (height represents dose percentage, whereas colour represents total-person percentage)", x= "MMET.h per week", y="Percentage") +
    scale_x_continuous(labels = paste(seq(-8.75, 70, by = 8.75), "<", seq(0, 78.75, by = 8.75)),
                       breaks = seq(0, 78.75 , by = 8.75)) +
    theme(plot.title = element_text(hjust = 0.5))
  
}