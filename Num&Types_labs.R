labs <- as.data.frame(read.csv("ICMRTestingLabs.csv", header = TRUE))
labs <- labs[,5:6]
typeof(labs)
labs <- labs[with(labs, order(labs$state)), ]
labs_present <- cbind(labs[-112,])
labs_present_plt <- plot_ly(labs_present, x = ~state, y = ~type)
labs_present_plt <- labs_present_plt %>% layout(title = "Number & types of Labs in each state")
labs_present_plt
