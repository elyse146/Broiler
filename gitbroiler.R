#Title: Renaming farm names in environmental data to match the all data 
#By: Elyse Wiederhorn 
#Date:06/15/2021


#Load Packages 
library(tidyverse)
library(lubridate)
library(janitor)


#compare farm names of all data with temp water and rename them----

#Import data 
read_csv("data/temp_water.csv")->temp_water
read_csv("data/all_data.csv")->alldata

#run these and then compare names 
unique(alldata$farm_name)
unique(temp_water$farmName)

#temp_water has farm names roble 1, roble 2, roble 3, and La semilla but none of those correlate to the all data farm names

temp_water$farmName <- recode(temp_water$farmName, `Querencia` = "La Querencia",
                              `Santa Luc\x92a` = "Santa Lucia",
                              `Monta\x96ita` = "Montanita",
                              `Gaviotas`="Gaviota",
                              `Comba 1`="Comba_1",
                              `La Mariana`="Mariana",
                              `El Palmar`="Palmar",
                              `Comba 2`="Comba_2",
                              `La Bonita`="Bonita", 
                              `Balcones OPAV`="Balcones",
                              `La Estancia`="Estancia",
                              `La Herradura`="Herradura",
                              `Porvenir Ant`="Porvenir") 
unique(temp_water$farmName) #run this again to see the changes made 

#compare farm names of all data with humidity and rename them----

#Import data 
read_csv("data/Humedad_apr.csv")->hume_apr
read_csv("data/Humedad_feb.csv")->hume_feb
read_csv("data/Humedad_jan.csv")->hume_jan

#all have same columns names so all we need to do is bind the rows 
bind_rows(hume_apr, hume_feb, hume_jan)-> humidity 

#run these and then compare names 
unique(humidity$farmName)
unique(alldata$farm_name)

#humidity has farm names roble 1, roble 2, roble 3, and La semilla but none of those correlate to the all data farm names

humidity$farmName <- recode(humidity$farmName, `Querencia` = "La Querencia",
                            `Santa Luc\x92a` = "Santa Lucia",
                            `Monta\x96ita` = "Montanita",
                            `Gaviotas`="Gaviota",
                            `Comba 1`="Comba_1",
                            `La Mariana`="Mariana",
                            `El Palmar`="Palmar",
                            `Comba 2`="Comba_2",
                            `La Bonita`="Bonita",
                            `Balcones OPAV`="Balcones",
                            `La Estancia`="Estancia",
                            `La Herradura`="Herradura",
                            `Porvenir Ant`="Porvenir")

unique(humidity$farmName) #run this again to see the changes made



#compare farm names of all data with temp and rename them----

#import the data 
read_csv("data/temp_1.csv") ->t1
read_csv("data/temp3.csv") ->t3
read_csv("data/Relacionid-granjaTEMP.csv") ->barn

# Only selecting columns that have observations 
t1 <-t1 %>% 
  select (-farmName, -house, -sensorName)

# Join t1 with barn
t1 <- t1 %>%
  left_join(barn, by ="raw_id")

#Now join t3 with barn (match the farmname, house, and sensor name through raw_id)
t3 <- t3 %>%
  left_join(barn, by="raw_id")

#Now bind rows for t3 and t1 b/c now they have all the same columns 
temp <- bind_rows(t3, t1)


#run these and then compare names 
unique(temp$farmName)
unique(alldata$farm_name)


#temp has farm names roble 1, roble 2, and roble 3 but none of those correlate to the all data farm names

temp$farmName <- recode(temp$farmName, `Querencia` = "La Querencia",
                        `Santa Luc\x92a` = "Santa Lucia",
                        `Monta\x96ita` = "Montanita",
                        `Gaviotas`="Gaviota",
                        `Comba 1`="Comba_1",
                        `La Mariana`="Mariana",
                        `El Palmar`="Palmar",
                        `Comba 2`="Comba_2",
                        `La Bonita`="Bonita",
                        `Balcones OPAV`="Balcones",
                        `La Estancia`="Estancia",
                        `La Herradura`="Herradura")


unique(temp$farmName) #run this again to see the changes made


#compare farm names of all data with pH and rename them----

#import the data
ph1 <- read_csv("data/pH_1.csv")
ph2 <- read_csv("data/pH_2.csv")


# bind rows because they all have the same columns. Want to put all observations in the same table 
ph <- bind_rows(ph1, ph2)

#run these and then compare names 
unique(ph$farmName)
unique(alldata$farm_name)



#ph has farm names roble 1, roble 2, roble 3, and La semilla but none of those correlate to the all data farm names

ph$farmName <- recode(ph$farmName, `Querencia` = "La Querencia",
                      `Santa Luc\x92a` = "Santa Lucia",
                      `Monta\x96ita` = "Montanita",
                      `Gaviotas`="Gaviota",
                      `Comba 1`="Comba_1",
                      `La Mariana`="Mariana",
                      `El Palmar`="Palmar",
                      `Comba 2`="Comba_2",
                      `La Bonita`="Bonita",
                      `Balcones OPAV`="Balcones",
                      `La Estancia`="Estancia",
                      `La Herradura`="Herradura",
                      `Porvenir Ant`="Porvenir")

#run this again to see the changes made
unique(ph$farmName)

#Change date column in alldata to JUST show ymd in a new column----

alldata1 <- alldata %>%
  mutate(date_0d= make_date(year=year_0d,
                            month=month_0d,
                            day=day_0d))                            
#Change date column in temp_water to a column with JUST ymd, to exclude hms----

temp_water <- temp_water%>% 
  mutate(date_0d=date(date)) %>%clean_names()

#Change date column in humidity to a column with JUST ymd, to exclude hms----

humidity <-humidity %>% 
  mutate(date_0d=date(date)) %>%clean_names()


#Change date column in temp to a column with JUST ymd, to exclude hms----

temp <-temp %>% 
  mutate(date_0d=date(date)) %>%clean_names()

#Change date column in pH to a column with JUST ymd, to exclude hms----

ph <- ph %>% 
  mutate(date_0d=date(date)) %>%clean_names()

# Calculate temp mean by ymd and farm_name and join with alldata1 and filter for 30C+ to see chick data----

temp1<- temp %>%
  group_by(date_0d, farm_name, house) %>%
  summarize(mean=mean(value, na.rm=TRUE)) %>%
  mutate(match="OK")


joined <- alldata1 %>% 
  inner_join(temp1, by= c("farm_name", "date_0d")) %>%
  filter(mean>=30)

temp1 %>% 
  filter(farm_name == "San Joaquin" & date_0d > ymd("2019-12-01") & date_0d < ymd("2020-04-30")) %>% 
  ggplot() +
  geom_line(aes(x = date_0d, y = mean, color = house)) +
  scale_x_date(date_breaks = "1 month")

alldata1 %>% 
  filter(farm_name == names)

names <- unique(temp$farm_name)

#create new date columns every 7 days in alldata

alldata12<- alldata1 %>%
  filter(date_0d>="2020-01-01") %>%
  mutate(date_7d=date_0d+7,
         date_14d=date_0d+14,
         date_21d=date_0d+21,
         date_28d=date_0d+28,
         date_35d=date_0d+35)

summary(alldata1$date_0d)

temp <- temp %>%
  filter(date_0d<="2020-05-26")

anjel- h