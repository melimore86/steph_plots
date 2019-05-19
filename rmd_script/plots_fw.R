#Stephanie Plots
#will be using facet wrap to merge and then display the locations. 


library("ggplot2")
library("lubridate")
library("marelac")
library("tidyverse")
library("scales")
library("dplyr")

cabinroad_inland<-read.csv("data/inland_cabinroad2.csv", header=T)
cabinroad_coastal<-read.csv("data/coastal_cabinroad2.csv", header=T)
cabinroad_coastshallow<-read.csv("data/coastal_shallow_shired2.csv", header=T)
horse_inland<-read.csv("data/inland_horseshoe2.csv", header=T)
inland_sandridge<-read.csv("data/inland_sandridge2.csv", header=T)
shired_coastal<-read.csv("data/inland_shired2.csv", header=T)


colnames(cabinroad_inland) <- c("oldDate", "Pressure", "Temperature", "Conductivity")
colnames(cabinroad_coastal) <- c("oldDate", "Pressure", "Temperature", "Conductivity")
colnames(cabinroad_coastshallow) <- c("oldDate", "Pressure", "Temperature", "Conductivity")
colnames(horse_inland) <- c("oldDate", "Pressure", "Temperature", "Conductivity")
colnames(inland_sandridge) <- c("oldDate", "Pressure", "Temperature", "Conductivity")
colnames(shired_coastal) <- c("oldDate", "Pressure", "Temperature", "Conductivity")


cabinroad_inland$Date <- as.POSIXct(as.Date(cabinroad_inland$oldDate,origin= "1899-12-30"))
cabinroad_coastal$Date <- as.POSIXct(as.Date(cabinroad_coastal$oldDate,origin= "1899-12-30"))
cabinroad_coastshallow$Date <- as.POSIXct(as.Date(cabinroad_coastshallow$oldDate,origin= "1899-12-30"))
horse_inland$Date <- as.POSIXct(as.Date(horse_inland$oldDate,origin= "1899-12-30"))
inland_sandridge$Date <- as.POSIXct(as.Date(inland_sandridge$oldDate,origin= "1899-12-30"))
shired_coastal$Date <- as.POSIXct(as.Date(shired_coastal$oldDate,origin= "1899-12-30"))



standard= 42.914

cabinroad_inland$Salinity <- convert_RtoS(cabinroad_inland$Conductivity/standard, 
                                          t= cabinroad_inland$Temperature, p= 0)


cabinroad_coastal$Salinity <- convert_RtoS(cabinroad_coastal$Conductivity/standard, 
                                           t= cabinroad_coastal$Temperature, p= 0)


cabinroad_coastshallow$Salinity <- convert_RtoS(cabinroad_coastshallow$Conductivity/standard, 
                                                t= cabinroad_coastshallow$Temperature, p= 0)

horse_inland$Salinity <- convert_RtoS(horse_inland$Conductivity/standard, 
                                      t= horse_inland$Temperature, p= 0)

inland_sandridge$Salinity <- convert_RtoS(inland_sandridge$Conductivity/standard, 
                                          t= inland_sandridge$Temperature, p= 0)

shired_coastal$Salinity <- convert_RtoS(shired_coastal$Conductivity/standard, 
                                        t= shired_coastal$Temperature, p= 0)



cabinroad_inland$Station<- (cabinroad_inland$Station ="Cabin Road Inland")
cabinroad_coastal$Station<- (cabinroad_coastal$Station ="Cabin Road Coastal")
cabinroad_coastshallow$Station<- (cabinroad_coastshallow$Station ="Cabin Road Coastal Shallow")
horse_inland$Station<- (horse_inland$Station ="Horse Shoe Inland")
inland_sandridge$Station<- (inland_sandridge$Station = "Sandridge Inland")
shired_coastal$Station<-  (shired_coastal$Station$Station = "Shired Inland")

data<- rbind(cabinroad_coastal,cabinroad_coastshallow,cabinroad_inland, horse_inland,inland_sandridge,shired_coastal)



data <- data %>% 
  filter(!(Conductivity< 1)) %>% 
  filter(!(Salinity > 40 )) %>% 
  filter(!(Temperature > 40))

ggplot(data=data, aes(x=Date, y=Salinity))+
  labs(y= "Salinity (ppt)") +
  geom_point() +
  scale_x_datetime(labels = date_format("%Y/%m"),
                   breaks = date_breaks("1 month")) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap (~Station)
ggsave('pic/steph_plots.png', height = 12, width = 15, dpi=300)

