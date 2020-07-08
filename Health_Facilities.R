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