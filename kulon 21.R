setwd("C:/Users/user/Documents/RR/kawal kulon")
kulon
library(tidyverse)
table(kulon$Angkatan, kulon$Departemen)
table(kulon$Angkatan, kulon$`Rata-rata banyak tugas yang diberikan setiap minggunya?`)
prop.table(kulon$Angkatan, kulon$`Rata-rata banyak tugas yang diberikan setiap minggunya?`) 



###### Synchronus Analysis #####
kulon %>% ggplot(aes(`Platform apa yang lebih nyaman digunakan untuk kuliah online menurut Anda?`)) + geom_bar()
table(kulon$`Platform apa yang lebih nyaman digunakan untuk kuliah online menurut Anda?`)
###  zoom
kulonzsin <- kulon %>% filter(`Platform apa yang lebih nyaman digunakan untuk kuliah online menurut Anda?` == "Zoom") %>%separate(`Alasan memilih platform tersebut adalah`, c("Pil1","Pil2","Pil3","Pil4","Pil5"),";") %>%
  gather(pilihan, alasan, "Pil1":"Pil5")
kulonzsin <- kulonzsin[!is.na(kulonzsin$alasan),] 
kulonzsin %>% .$alasan %>% table()

#####webex
kulonwsin <- kulon %>% filter(`Platform apa yang lebih nyaman digunakan untuk kuliah online menurut Anda?` == "Webex") %>%separate(`Alasan memilih platform tersebut adalah`, c("Pil1","Pil2","Pil3","Pil4","Pil5"),";") %>%
  gather(pilihan, alasan, "Pil1":"Pil5")
kulonwsin <- kulonwsin[!is.na(kulonwsin$alasan),]
kulonwsin %>% .$alasan %>% table()

####gmeet
kulongsin <- kulon %>% filter(`Platform apa yang lebih nyaman digunakan untuk kuliah online menurut Anda?` == "Gmeet") %>%separate(`Alasan memilih platform tersebut adalah`, c("Pil1","Pil2","Pil3","Pil4","Pil5"),";") %>%
  gather(pilihan, alasan, "Pil1":"Pil5")
kulongsin <- kulongsin[!is.na(kulongsin$alasan),]
kulongsin %>% .$alasan %>% table()

###dll
kulonlsin <- kulon %>% filter(!(`Platform apa yang lebih nyaman digunakan untuk kuliah online menurut Anda?` %in% c("Webex","Zoom","Gmeet"))) %>%separate(`Alasan memilih platform tersebut adalah`, c("Pil1","Pil2","Pil3","Pil4","Pil5"),";") %>%
  gather(pilihan, alasan, "Pil1":"Pil5")
kulonlsin <- kulonlsin[!is.na(kulonlsin$alasan),]
kulonlsin %>% .$alasan %>% table()



##### Asynchronus Analysis #######
kulon %>% ggplot(aes(`Platform apa yang lebih nyaman digunakan untuk memberikan informasi dan mengumpulkan tugas?`)) + geom_bar()
table(kulon$`Platform apa yang lebih nyaman digunakan untuk memberikan informasi dan mengumpulkan tugas?`)
### WA
kulonasin <- kulon %>% filter(`Platform apa yang lebih nyaman digunakan untuk memberikan informasi dan mengumpulkan tugas?` == "WA Group") %>%separate(`Alasan memilih platform tersebut adalah_1`, c("Pil1","Pil2","Pil3","Pil4","Pil5"),";") %>%
  gather(pilihan, alasan, "Pil1":"Pil5")
kulonasin <- kulonasin[!is.na(kulonasin$alasan),]
kulonasin %>% .$alasan %>% table()
### Elok
kuloneasin <- kulon %>% filter(`Platform apa yang lebih nyaman digunakan untuk memberikan informasi dan mengumpulkan tugas?` == "E-Lok") %>%separate(`Alasan memilih platform tersebut adalah_1`, c("Pil1","Pil2","Pil3","Pil4","Pil5"),";") %>%
  gather(pilihan, alasan, "Pil1":"Pil5")
kuloneasin <- kuloneasin[!is.na(kuloneasin$alasan),]
kuloneasin %>% .$alasan %>% table()
### GClass

kuloncasin <- kulon %>% filter(`Platform apa yang lebih nyaman digunakan untuk memberikan informasi dan mengumpulkan tugas?` == "Google Classroom") %>%separate(`Alasan memilih platform tersebut adalah_1`, c("Pil1","Pil2","Pil3","Pil4","Pil5"),";") %>%
  gather(pilihan, alasan, "Pil1":"Pil5")
kuloncasin <- kuloncasin[!is.na(kuloncasin$alasan),]
kuloncasin %>% .$alasan %>% table()
### Simaster
kulonsasin <- kulon %>% filter(`Platform apa yang lebih nyaman digunakan untuk memberikan informasi dan mengumpulkan tugas?` == "Simaster") %>%separate(`Alasan memilih platform tersebut adalah_1`, c("Pil1","Pil2","Pil3","Pil4","Pil5"),";") %>%
  gather(pilihan, alasan, "Pil1":"Pil5")
kulonsasin <- kulonsasin[!is.na(kulonsasin$alasan),]
kulonsasin %>% .$alasan %>% table()
### dll
kulondasin <- kulon %>% filter(!(`Platform apa yang lebih nyaman digunakan untuk memberikan informasi dan mengumpulkan tugas?` %in% c("Simaster","E-Lok","Google Classroom", "WA Group"))) %>%separate(`Alasan memilih platform tersebut adalah_1`, c("Pil1","Pil2","Pil3","Pil4","Pil5"),";") %>%
  gather(pilihan, alasan, "Pil1":"Pil5")
kulondasin <- kulondasin[!is.na(kulondasin$alasan),]
kulondasin %>% .$alasan %>% table()

kulonsin$Pil5
kulonas <- kulon %>% separate(`Alasan memilih platform tersebut adalah_1`,c("Pil1","Pil2","Pil3","Pil4","Pil5","Pil6"),";")
kulonas$Pil6

hist(kulon$`Rata-rata banyak tugas yang diberikan setiap minggunya?`)
kulon %>% ggplot(aes(`Rata-rata banyak tugas yang diberikan setiap minggunya?`)) + geom_bar() + facet_wrap(,-Angkatan)
#### ordinal
table(kulon$`Seberapa besar tingkat konsentrasi Anda apabila kuliah dilaksanakan secara full daring selama 3 sks?`, kulon$Angkatan)
table(kulon$Angkatan, kulon$`Seberapa paham Anda terhadap materi yang disampaikan?`)

#### table
table(kulon$Angkatan, kulon$`Seberapa paham Anda terhadap materi yang disampaikan?`)
table(kulon$`Seberapa besar tingkat konsentrasi Anda apabila kuliah dilaksanakan secara full daring selama 3 sks?`) %>% prop.table()
table(kulon$`Seberapa paham Anda terhadap materi yang disampaikan?`) %>% prop.table()
#####

t <- kulon %>% filter(!(`Domisili saat ini` %in% c("DKI Jakarta", "D.I. Yogyakarta","Banten","Jawa Barat","Jawa Tengah","Jawa Timur")))
kulon %>% ggplot(aes(`Seberapa besar tingkat konsentrasi Anda apabila kuliah dilaksanakan secara full daring selama 3 sks?`)) + geom_bar()


######
1+1
kulon %>% ggplot(aes(x= `Domisili saat ini`)) + geom_bar() + 
