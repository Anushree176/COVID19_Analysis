covid <- as.data.frame(read.csv("covid_19_india.csv"))
covid <- subset(covid, select = -c(1,3,5,6))
str(covid)

covid = covid[!covid$State.UnionTerritory == "Cases being reassigned to states", ]
covid = covid[!covid$State.UnionTerritory == "Unassigned", ]
covid <- covid[complete.cases(covid), ]
covid$Date <- as.Date(covid$Date, format = "%d/%m/%y")
covid <- covid[with(covid, order(covid$Date)),]

covid$State.UnionTerritory <- as.factor(covid$State.UnionTerritory)

covid_plt1 <- plot_ly(covid, x = ~Date, y = ~Confirmed, color = ~State.UnionTerritory,
                      colors = "Set1", mode = "lines")
covid_plt1 <- covid_plt1 %>% layout(title = "Confirmed cases in each state",
                                    xaxis = list(title = 'States'), yaxis = list(title = 'Count'))
covid_plt1

covid_plt2 <- plot_ly(covid, x = ~Date, y = ~Cured, color = ~State.UnionTerritory,
                      colors = "Blues", mode = "lines")
covid_plt2 <- covid_plt2 %>% layout(title = "Cured cases in each state",
                                    xaxis = list(title = 'States'), yaxis = list(title = 'Count'))
covid_plt2

covid_plt3 <- plot_ly(covid, x = ~Date, y = ~Deaths, color = ~State.UnionTerritory,
                      colors = "Greens", mode = "lines")
covid_plt3 <- covid_plt3 %>% layout(title = "Deaths in each state",
                                    xaxis = list(title = 'States'), yaxis = list(title = 'Count'))
covid_plt3

covid <- covid[with(covid, order(covid$State.UnionTerritory)),]

cases <- cbind(tapply(covid$Cured, covid$State.UnionTerritory, sum))
cases <- cbind(cases, tapply(covid$Deaths, covid$State.UnionTerritory, sum))
cases <- cbind(cases, tapply(covid$Confirmed, covid$State.UnionTerritory, sum))
colnames(cases) <- c("Cured", "Deaths", "Confirmed")
rownames(cases) <- unique(covid$State.UnionTerritory)
cases <- as.data.frame(cases)
cases <- cbind(cases, cases$Confirmed -(cases$Deaths+cases$Cured))
colnames(cases) <- c("Cured", "Deaths", "Confirmed", "Active")
cases <- cases[, c(3,1,4,2)]

cases[9,-1] <- cases[8,-1]+cases[9,-1]+cases[10,-1]
cases[33,-1] <- cases[33,-1]+cases[34,-1]
cases <- cases[-c(8,10,20,26,34), ]

c_plt <- plot_ly(cases, x = rownames(cases), y = ~Confirmed, name = "Confirmed",type = "bar")
c_plt <- c_plt %>% add_trace(y = ~Cured, name = "Cured")
c_plt <- c_plt %>% add_trace(y = ~Active, name = "Active")
c_plt <- c_plt %>% add_trace(y = ~Deaths, name = "Deaths")
c_plt <- c_plt %>% layout(title = "Confirmed, Cured, Active, Deaths in each state",
                          xaxis = list(title = 'States'), yaxis = list(title = 'Count'))
c_plt
