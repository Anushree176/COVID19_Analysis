indi <- as.data.frame(read.csv("IndividualDetails.csv"))
indi <- subset(indi,select = -c(1,2,4,5,6,7,9,12))
indi$diagnosed_date <- as.Date(indi$diagnosed_date, format = "%d/%m/%Y")
indi$status_change_date <- as.Date(indi$status_change_date, format = "%d/%m/%Y")
duration <- round(abs(indi$status_change_date-indi$diagnosed_date), 0)
indi <- cbind(indi, duration)

indi = indi[!indi$duration < 6,]

indi <- indi[complete.cases(indi), ]
rownames(indi) <- c(1:149)
mean(indi$duration)
duration <- plot_ly(indi, y = ~duration, type = "scatter")
duration <- duration %>% add_segments(x = 0, y = mean(indi$duration), xend = 150, yend = mean(indi$duration))
duration