setwd("D:/Data Sains/Data CPUE")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
mydata <- read_excel('cpue_palabuhanratu.xlsx')

#Tabel CPUE
cpue <- mydata %>% group_by(tahun = format(as.Date(mydata$tanggal), format = "%Y"),ikan,alat_tangkap) %>% 
        summarise(total_produksi = sum(produksi),total_trip = n(), cpue1= total_produksi/total_trip
            , .groups= "drop") 

cpue2 <- filter(cpue, ikan=="Layur" & alat_tangkap=="Pancing Ulur")

#Grafik CPUE
mydata$tanggal <- as.Date(mydata$tanggal)

cpue_years <- mydata %>% group_by(tahun = format(as.Date(mydata$tanggal), format = "%Y"),ikan,alat_tangkap) %>% 
  summarise(total_produksi = sum(produksi),total_trip = n(), cpue1= total_produksi/total_trip
            , .groups= "drop") 
cpue3 <- filter(cpue_years, ikan=="Layur" & alat_tangkap=="Pancing Ulur")

plot1 <- ggplot(cpue3, aes(x=tahun, y=cpue1, group=1)) + geom_line(size=0.75) + geom_point(colour='blue',size=3) +
  labs(title = "CPUE Ikan Layur, Alat Tangkap Pancing Ulur",
       subtitle = "Based On Logbook (Bagian Statistik PPN Palabuhanratu)",
       caption = "IG: @teguhsantausa") + xlab("Tahun") + ylab("CPUE (Kg/Trip)")+
  geom_text(aes(label=sprintf("%0.2f", round(cpue1, digits = 2)), vjust=-0.8))+
  ylim(c(0,90))

plot1
