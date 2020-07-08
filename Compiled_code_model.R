labs <- as.data.frame(read.csv("ICMRTestingLabs.csv", header = TRUE))
labs <- labs[,5:6]
typeof(labs)
labs <- labs[with(labs, order(labs$state)), ]
labs_present <- cbind(labs[-112,])
labs_present_plt <- plot_ly(labs_present, x = ~state, y = ~type)
labs_present_plt <- labs_present_plt %>% layout(title = "Number & types of Labs in each state")
labs_present_plt

num_labs <- cbind(unique(labs$state))
num_labs <- cbind(num_labs, tapply(labs$type, labs$state, length))
num_labs <- num_labs[,-18]
num_labs <- data.frame(num_labs)
colnames(num_labs) <- c("State", "No_of_centers")

###############################################################################################################

hospital_beds <- as.data.frame(read.csv("HospitalBedsIndia.csv", header = TRUE))
str(hospital_beds)
head(hospital_beds)
num_hospital <- subset(hospital_beds, select = c(2,7,8,9,10,11,12))
num_hospital <- num_hospital[-37,]
num_hospital <- num_hospital[with(num_hospital, order(num_hospital$State.UT)), ]
num_hospital[8, ] <- num_hospital[8,-1]+num_hospital[9,-1]
num_hospital[8,1] <- "Dadra & Nagar Haveli & Daman and Diu"
num_hospital <- num_hospital[c(-9,-19,-25),]
sum(is.na(num_hospital))

nh_plt1 <- plot_ly(num_hospital, x = ~State.UT, y = ~TotalPublicHealthFacilities_HMIS, type = "bar")
nh_plt1 <- nh_plt1 %>% layout(title = "Total Public Health Facilities", yaxis = list(title = 'Count'))
nh_plt1

nh_plt2 <- plot_ly(num_hospital, x = ~State.UT, y = ~TotalPublicHealthFacilities_HMIS,
                   color = ~NumPublicBeds_HMIS, type = "bar")
nh_plt2 <- nh_plt2 %>% layout(title = "Total Public Health Facilities & Number of Public Beds",
                              yaxis = list(title = 'Count'))
nh_plt2

nh_plt3 <- plot_ly(num_hospital, x = ~State.UT, y = ~NumRuralHospitals_NHP18,
                   type = "bar", name = "Rural Hospitals", color = ~NumRuralBeds_NHP18)
nh_plt3 <- nh_plt3 %>% add_trace(y = ~NumUrbanHospitals_NHP18, name = 'Urban Hospitals',
                                 color = ~NumUrbanBeds_NHP18)
nh_plt3 <- nh_plt3 %>% layout(title = "Number of Hospital Beds - Rural vs Urban", 
                              xaxis = list(title = 'States'), yaxis = list(title = 'Count'),
                              barmode = 'group')
nh_plt3

############################################################################################################

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

############################################################################################################

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
pop_rural_urban$rural <- as.integer(pop_rural_urban$rural)
pop_rural_urban$urban <- as.integer(pop_rural_urban$urban)
population <- plot_ly(pop_rural_urban, x = ~State, y = ~rural, type = "bar", name = "Rural")
population <- population %>% add_trace(y = ~urban, name = "Urban")
population <- population %>% layout(title = "Population - Rural vs Urban",
                                    xaxis = list(title = 'States'),
                                    yaxis = list(title = 'Count'))

population

##########################################################################################################

all_data <- cbind(num_labs[-18,])
all_data <- cbind(all_data, num_hospital[,2:3])
all_data <- cbind(all_data, cases)
all_data <- cbind(all_data, pop_rural_urban[,2:3])
all_data <- all_data[,-1]
all_data$No_of_centers <- as.integer(all_data$No_of_centers)
all_data$rural <- as.integer(all_data$rural)
all_data$urban <- as.integer(all_data$urban)
str(all_data)


p1 <- plot_ly(all_data, x = ~No_of_centers, y = ~Confirmed, type = "scatter", name = "No of centers")
p2 <- plot_ly(all_data, x = ~TotalPublicHealthFacilities_HMIS, y = ~Confirmed, type = "scatter", name = "Public health facilities")
p3 <- plot_ly(all_data, x = ~NumPublicBeds_HMIS, y = ~Confirmed, type = "scatter", name = "Public Hosp Beds")
p4 <- plot_ly(all_data, x = ~rural, y = ~Confirmed, type = "scatter", name = "Rural Pop")
p5 <- plot_ly(all_data, x = ~urban, y = ~Confirmed, type = "scatter", name = "Urban Pop")
p <- subplot(p1,p2,p3,p4,p5)
p


d1 <- plot_ly(all_data, x = ~No_of_centers, y = ~Deaths, type = "scatter", name = "No of centers")
d2 <- plot_ly(all_data, x = ~TotalPublicHealthFacilities_HMIS, y = ~Deaths, type = "scatter", name = "Public health facilities")
d3 <- plot_ly(all_data, x = ~NumPublicBeds_HMIS, y = ~Deaths, type = "scatter", name = "Public Hosp Beds")
d4 <- plot_ly(all_data, x = ~rural, y = ~Deaths, type = "scatter", name = "Rural Pop")
d5 <- plot_ly(all_data, x = ~urban, y = ~Deaths, type = "scatter", name = "Urban Pop")
d <- subplot(d1,d2,d3,d4,d5)
d

#########################################################################################################

model_c <- lm(all_data$Confirmed ~ all_data$No_of_centers)
summary(model_c)


ggplot(all_data, aes(Confirmed, No_of_centers)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm")+
  expand_limits(x = c(0, 6e+06)) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))

########################################################################################################
model_d <- lm(all_data$Deaths ~ all_data$urban)
summary(model_d)

ggplot(all_data, aes(urban, Deaths)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm")+
  expand_limits(x = c(0, 6e+06)) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))
