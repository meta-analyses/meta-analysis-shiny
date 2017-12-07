
create_plot <- function (df){
  di <- seq(0, 78.75, by = 8.75)
  h <- hist(df$dose, breaks = di)
  c <- list()
  tpyrs <- sum(df$personyrs, na.rm = T)
  for (i in 1:length(di)){
    if (i == 1)
      c[i] <- (round(sum(filter(df, dose <= di[i]) %>% select(personyrs), na.rm = T) / tpyrs * 100, 2))
    else
      c[i] <- ( round(sum(filter(df, dose <= di[i] & dose >= di[i - 1]) %>% select(personyrs), na.rm = T) / tpyrs * 100, 2))
    
  }
  
  td <- data.frame(d = di, count = c(h$counts,0), 'Person Years (%)' = t(as.data.frame(c)), check.names = FALSE)
  
  ggplot(td, aes(di, count, fill = `Person Years (%)`)) +
      geom_bar(stat = "identity") +
      scale_fill_continuous(low = "#fef0d9", high = "#b30000") + 
      labs(title = "Histogram of Marginal MET hours (MMETh) per week", x= "MMETh", y="Count") +
      scale_x_continuous(labels = paste(seq(0, 78.75, by = 8.75), "<", seq(8.75, 87.5, by = 8.75)),
                         breaks = seq(0, 78.75, by = 8.75)) +
      theme(plot.title = element_text(hjust = 0.5))
  
  
  c <- list()
  ttp <- sum(df$totalpersons, na.rm = T)
  for (i in 1:length(di)){
    if (i == 1)
      c[i] <- (round(sum(filter(df, dose <= di[i]) %>% select(totalpersons), na.rm = T) / ttp * 100, 2))
    else
      c[i] <- ( round(sum(filter(df, dose <= di[i] & dose >= di[i - 1]) %>% select(totalpersons), na.rm = T) / ttp * 100, 2))
    
  }
  
  td <- data.frame(d = di, count = c(h$counts,0), 'Total Persons (%)' = t(as.data.frame(c)), check.names = FALSE)
  
  ggplot(td, aes(di, count, fill = `Total Persons (%)`)) +
    geom_bar(stat = "identity") +
    scale_fill_continuous(low = "#fef0d9", high = "#b30000") + 
    labs(title = "Histogram of Marginal MET hours (MMETh) per week", x= "MMETh", y="Count") +
    scale_x_continuous(labels = paste(seq(0, 78.75, by = 8.75), "<", seq(8.75, 87.5, by = 8.75)),
                       breaks = seq(0, 78.75, by = 8.75)) +
    theme(plot.title = element_text(hjust = 0.5))
}