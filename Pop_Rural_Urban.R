pop <- as.data.frame(read.csv("population_india_census2011.csv"))
str(pop)
pop <- subset(pop, select = -c(1,6,8))
pop <- pop[complete.cases(pop), ]
pop <- pop[with(pop, order(pop$State...Union.Territory)), ]

pop <- pop[-c(18,19,25),]
pop_rural_urban <- cbind(pop$State...Union.Territory)
pop_rural_urban <- cbind(pop_rural_urban, pop$Rural.population)
pop_rural_urban <- cbind(pop_rural_urban, pop$Urban.population)

colnames(pop_rural_urban) <- c("State","rural", "urban")

pop_rural_urban <- as.data.frame(pop_rural_urban)
pop_rural_urban$State <- factor(pop_rural_urban$State)
population <- plot_ly(pop_rural_urban, x = ~State, y = ~rural, type = "bar", name = "Rural")
population <- population %>% add_trace(y = ~urban, name = "Urban")
population <- population %>% layout(title = "Population - Rural vs Urban",
                                    xaxis = list(title = 'States'),
                                    yaxis = list(title = 'Count'))
population