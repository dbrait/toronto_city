install.packages("dplyr")
install.packages("ggplot2")
install.packages("rgdal")
install.packages("httr")
install.packages("aws.s3")
install.packages("lubridate")
install.packages("purrr")
install.packages("ggmap")
install.packages("knitr")
install.packages("kableExtra")
install.packages("RgoogleMaps")
library(dplyr)
library(ggplot2)
library(rgdal)
library(httr)
library(aws.s3)
library(lubridate)
library(purrr)
library(ggmap)
library(RgoogleMaps)
library(knitr)
library(kableExtra)

url <- "https://s3.amazonaws.com/torontoksi/KSI.csv"
ksi <- read.csv(url)
str(ksi)
colnames(ksi)

#unique accidents
accidents <- distinct(ksi, ACCNUM, .keep_all=TRUE)

accidents$month <- month(accidents$DATE)

#cyclist
ksi$CYCLIST
cyclist <- accidents %>% filter(CYCLIST == "Yes")
cyclist_by_year <- cyclist %>% group_by(YEAR) %>%
  summarize(count = n())
cyclist_by_year$YEAR <- as.factor(cyclist_by_year$YEAR)

#cyclists by year
ggplot(cyclist_by_year, aes(x=YEAR, y=count)) +
  geom_bar(stat="identity") +
  ggtitle("Major Accidents Involving Cyclists") +
  xlab("Year") + ylab("Number of Major Accidents")
  #worst streets

#cyclists by month, sift through date
cyl_month <- cyclist %>% group_by(month) %>%
  summarize(count = n())
cyl_month$month <- as.factor(cyl_month$month)

ggplot(cyl_month, aes(month, count)) +
  geom_bar(stat="identity")

#cyclists by neighborhood
cyl_by_neighborhood <- cyclist %>% group_by(Hood_Name) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#afternoon rush hour
cyl_rush_hour <- cyclist %>% filter(Hour >= 16 & Hour <= 19)

#morning rush hour
cyl_morning_rush <- cyclist %>% filter(Hour >= 7 & Hour <= 10)

#during the day
cyl_daytime <- cyclist %>% filter(Hour >= 11 & Hour <= 15)

#late night
cyl_late_night <- cyclist %>% filter(Hour >=20 | Hour <= 6)

#cyclists by street
cyl_by_street_1 <- cyclist %>% group_by(STREET1) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
cyl_by_street_2 <- cyclist %>% group_by(STREET2) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#major streets - dundas (22+4), college (14+4), yonge (14+4), bloor (13+5), queen (11), 
#bathurst (9+6), king w (9), avenue ( 8),
#kingston (7) lakeshore west (7), queen e (7), danforth (6), bay (5+5), bloor e (5), 
#davenport (5+6), dufferin (5+4),
#dundas e (5), kennedy (5), spadina (5+5), st clair w (5), university ave(5)

#cyclists by intersection
st_var <- c(dundas_w, college, yonge, bloor_w, queen_w, bathurst,
            king_w, avenue, kingston, lake_shore_w, queen_e, danforth,
            bay, bloor_e, davenport, dufferin, dundas_e, kennedy,
            spadina, st_clair_w)

st_nam <- c("DUNDAS ST W", "COllEGE ST", "YONGE ST", "BLOOR ST W", "QUEEN ST W",
        "BATHURST ST", "KING ST W", "AVENUE RD", "KINGSTON RD", "LAKE SHORE BLVD W",
        "QUEEN ST E", "DANFORTH AVE", "BAY ST", "BLOOR ST E", "DAVENPORT RD", 
        "DUFFERIN ST", "DUNDAS ST E", "KENNEDY RD", "SPADINA AVE", "ST CLAIR AVE W")

#YOU NEED TO DO THIS AS A FUNCTION

stet <- function(st_nam) {
  st_addr <- cyclist %>% filter(STREET1 == st_nam | STREET2 == st_nam) %>%
    select(STREET1, STREET2)
}



yonge <- stet("YONGE ST")


dundas <- cyclist %>% filter(STREET1 == "DUNDAS ST W" | STREET2 == "DUNDAS ST W") %>%
  select(STREET1, STREET2)
college <- cyclist %>% filter(STREET1 == "COLLEGE ST"  | STREET2 == "COLLEGE ST") %>%
  select(STREET1, STREET2) 
yonge <- cyclist %>% filter(STREET1 == "YONGE ST" | STREET2 == "YONGE ST") %>%
  select(STREET1, STREET2)
bloor_w <- cyclist %>% filter(STREET1 == "BLOOR ST W" | STREET2 == "BLOOR ST W") %>%
  select(STREET1, STREET2)
queen_w <- cyclist %>% filter(STREET1 == "QUEEN ST W" | STREET2 == "QUEEN ST W") %>%
  select(STREET1, STREET2)
king_w <- cyclist %>% filter(STREET1 == "KING ST W" | STREET2 == "KING ST W") %>%
  select(STREET1, STREET2)
bathurst <- cyclist %>% filter(STREET1 == "BATHURST ST" | STREET2 == "BATHURST ST") %>%
  select(STREET1, STREET2)
avenue <- cyclist %>% filter(STREET1 == "AVENUE RD" | STREET2 == "AVENUE RD") %>%
  select(STREET1, STREET2)
kingston <- cyclist %>% filter(STREET1 == "KINGSTON RD" | STREET2 == "KINGSTON RD") %>%
  select(STREET1, STREET2)
lake_shore_w <- cyclist %>% filter(STREET1 == "LAKE SHORE BLVD W" | STREET2 == "LAKE SHORE BLVD W") %>%
  select(STREET1, STREET2)
queen_e<- cyclist %>% filter(STREET1 == "QUEEN ST E" | STREET2 == "QUEEN ST E") %>%
  select(STREET1, STREET2)
danforth <- cyclist %>% filter(STREET1 == "DANFORTH AVE" | STREET2 == "DANFORTH AVE") %>%
  select(STREET1, STREET2)
bay <- cyclist %>% filter(STREET1 == "BAY ST" | STREET2 == "BAY ST") %>%
  select(STREET1, STREET2)
bloor_e <- cyclist %>% filter(STREET1 == "BLOOR ST E" | STREET2 == "BLOOR ST E") %>%
  select(STREET1, STREET2)
davenport <- cyclist %>% filter(STREET1 == "DAVENPORT RD" | STREET2 == "DAVENPORT RD") %>%
  select(STREET1, STREET2)
dufferin <- cyclist %>% filter(STREET1 == "DUFFERIN ST" | STREET2 == "DUFFERIN ST") %>%
  select(STREET1, STREET2)
dundas_e <- cyclist %>% filter(STREET1 == "DUNDAS ST E" | STREET2 == "DUNDAS ST E") %>%
  select(STREET1, STREET2)
kennedy <- cyclist %>% filter(STREET1 == "KENNEDY RD" | STREET2 == "KENNEDY RD") %>%
  select(STREET1, STREET2)
spadina <- cyclist %>% filter(STREET1 == "SPADINA AVE" | STREET2 == "SPADINA AVE") %>%
  select(STREET1, STREET2)
st_clair_w <- cyclist %>% filter(STREET1 == "ST CLAIR AVE W" | STREET2 == "ST CLAIR AVE RD W") %>%
  select(STREET1, STREET2)


#cyclists by hour
cyl_hour <- cyclist %>% group_by(Hour) %>%
  summarize(count = n())



ggplot(cyl_hour, aes(Hour, count)) +
  geom_bar(stat="identity") +
  ggtitle("Cyclists Incidents by Hour of the Day") +
  xlab("Hour") + ylab("Number of Major Incidents")

#cyclists account
cyl_account <- cyclist %>% group_by(CYCACT) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
cyl_type <- cyclist %>% group_by(CYCLISTYPE) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#cyclist visiblity, light
cyl_vis <- cyclist %>% group_by(VISIBILITY) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
cyl_light <- cyclist %>% group_by(LIGHT) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


#pedestrians
pedestrian <- accidents %>% filter(PEDESTRIAN == "Yes")
ped_by_year <- pedestrian %>% group_by(YEAR) %>%
  summarize(count = n())
ped_by_year$YEAR <- as.factor(ped_by_year$YEAR)

#pedestrians by year
ggplot(ped_by_year, aes(x=YEAR, y=count)) +
  geom_bar(stat="identity") +
  ggtitle("Major Accidents Involving Pedestrians") +
  xlab("Year") + ylab("Number of Major Accidents")

ped_hour <- pedestrian %>% group_by(Hour) %>%
  summarize(count = n())

ggplot(ped_hour, aes(x=Hour, y=count)) +
  geom_bar(stat="identity") +
  ggtitle("Major Pedestrian Incidents Throughout the Day") +
  xlab("Hour") + ylab("Number of Incidents")

#afternoon rush hour
rush_hour <- pedestrian %>% filter(Hour >= 16 & Hour <= 19)

#morning rush hour
morning_rush <- pedestrian %>% filter(Hour >= 7 & Hour <= 10)

#during the day
daytime <- pedestrian %>% filter(Hour >= 11 & Hour <= 15)

#late night
late_night <- pedestrian %>% filter(Hour >=20 | Hour <= 6)

#pedestrians by month
head(pedestrian$DATE)
pedestrian$DATE <- as.Date(pedestrian$DATE)
pedestrian$month <- month(pedestrian$DATE)
pedestrian %>% select(DATE, month)

ped_month <- pedestrian %>% group_by(month) %>% summarize(count = n())
ped_month$month <- as.factor(ped_month$month)

ggplot(ped_month, aes(month, count)) +
  geom_bar(stat="identity") +
  ggtitle("Pedestrian Major Incidents by Month") +
  xlab("Month") + ylab("Number of Major Incidents")
#pedestrians by day of week

#pedestrians by neighborhood
ped_by_neighborhood <- pedestrian %>% group_by(Hood_Name) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
ped_by_neighborhood %>% kable("html") %>% kable_styling()

ped_by_street_1 <- pedestrian %>% group_by(STREET1) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
ped_by_street_2 <- pedestrian %>% group_by(STREET2) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#worst intersections
ped_dundas <- pedestrian %>% filter(STREET1 == "DUNDAS ST W" | STREET2 == "DUNDAS ST W") #%>% select(STREET1, STREET2)
ped_dund_bloor <- ped_dundas %>% filter(STREET1 == "BLOOR ST W" | STREET2 == "BLOOR ST W")
ped_college <- pedestrian %>% filter(STREET1 == "COLLEGE ST"  | STREET2 == "COLLEGE ST") %>%
  select(STREET1, STREET2) 
ped_yonge <- pedestrian %>% filter(STREET1 == "YONGE ST" | STREET2 == "YONGE ST") %>%
  select(STREET1, STREET2)
ped_bloor_w <- pedestrian %>% filter(STREET1 == "BLOOR ST W" | STREET2 == "BLOOR ST W") %>%
  select(STREET1, STREET2)
ped_queen_w <- pedestrian %>% filter(STREET1 == "QUEEN ST W" | STREET2 == "QUEEN ST W") %>%
  select(STREET1, STREET2)
ped_king_w <- pedestrian %>% filter(STREET1 == "KING ST W" | STREET2 == "KING ST W") %>%
  select(STREET1, STREET2)
ped_bathurst <- pedestrian %>% filter(STREET1 == "BATHURST ST" | STREET2 == "BATHURST ST") %>%
  select(STREET1, STREET2)
ped_avenue <- pedestrian %>% filter(STREET1 == "AVENUE RD" | STREET2 == "AVENUE RD") %>%
  select(STREET1, STREET2)
ped_kingston <- pedestrian %>% filter(STREET1 == "KINGSTON RD" | STREET2 == "KINGSTON RD") %>%
  select(STREET1, STREET2)
ped_lake_shore_w <- pedestrian %>% filter(STREET1 == "LAKE SHORE BLVD W" | STREET2 == "LAKE SHORE BLVD W") %>%
  select(STREET1, STREET2)
ped_queen_e<- pedestrian %>% filter(STREET1 == "QUEEN ST E" | STREET2 == "QUEEN ST E") %>%
  select(STREET1, STREET2)
ped_danforth <- pedestrian %>% filter(STREET1 == "DANFORTH AVE" | STREET2 == "DANFORTH AVE") %>%
  select(STREET1, STREET2)
ped_bay <- pedestrian %>% filter(STREET1 == "BAY ST" | STREET2 == "BAY ST") %>%
  select(STREET1, STREET2)
ped_bloor_e <- pedestrian %>% filter(STREET1 == "BLOOR ST E" | STREET2 == "BLOOR ST E") %>%
  select(STREET1, STREET2)
ped_davenport <- pedestrian %>% filter(STREET1 == "DAVENPORT RD" | STREET2 == "DAVENPORT RD") %>%
  select(STREET1, STREET2)
ped_dufferin <- pedestrian %>% filter(STREET1 == "DUFFERIN ST" | STREET2 == "DUFFERIN ST") %>%
  select(STREET1, STREET2)
ped_dundas_e <- pedestrian %>% filter(STREET1 == "DUNDAS ST E" | STREET2 == "DUNDAS ST E") %>%
  select(STREET1, STREET2)
ped_kennedy <- pedestrian %>% filter(STREET1 == "KENNEDY RD" | STREET2 == "KENNEDY RD") %>%
  select(STREET1, STREET2)
ped_spadina <- pedestrian %>% filter(STREET1 == "SPADINA AVE" | STREET2 == "SPADINA AVE") %>%
  select(STREET1, STREET2)
ped_st_clair_w <- pedestrian %>% filter(STREET1 == "ST CLAIR AVE W" | STREET2 == "ST CLAIR AVE RD W") %>%
  select(STREET1, STREET2)

#maps
map <- get_map(location="Toronto",zoom=15)
west_map <- get_map(location=west_end, zoom=13)
east_map <- get_map(location=east_end, zoom=13)
downtown <- get_map(location=downtown, zoom=13)
north <- get_map(location=north, zoom=13)
etobicoke <- get_map(location=etobicoke, zoom=13)
scarborough <- get_map(location=scarborough, zoom=13)

#pedestrian maps
ggmap(east_map, extent="device") +
  geom_point(aes(x=X, y=Y), data=pedestrian, alpha=.5)
ggmap(west_map, extent="device") +
  geom_point(aes(x=X, y=Y), data=pedestrian, alpha=.5)
ggmap(downtown, extend="device") +
  geom_point(aes(x=X, y=Y), data=pedestrian, alpha=.5)
ggmap(north, extend="device") +
  geom_point(aes(x=X, y=Y), data=pedestrian, alpha=.5)
ggmap(etobicoke, extend="device") +
  geom_point(aes(x=X, y=Y), data=pedestrian, alpha=.5)
ggmap(scarborough, extend="device") +
  geom_point(aes(x=X, y=Y), data=pedestrian, alpha=.5)


#cycling maps
ggmap(east_map, extent="device") +
  geom_point(aes(x=X, y=Y), data=cyclist, alpha=.5)
ggmap(west_map, extent="device") +
  geom_point(aes(x=X, y=Y), data=cyclist, alpha=.5)
ggmap(downtown, extent="device") +
  geom_point(aes(x=X, y=Y), data=cyclist, alpha=.5)
ggmap(north, extent="device") +
  geom_point(aes(x=X, y=Y), data=cyclist, alpha=.5)
ggmap(etobicoke, extend="device") +
  geom_point(aes(x=X, y=Y), data=cyclist, alpha=.5)
ggmap(scarborough, extend="device") +
  geom_point(aes(x=X, y=Y), data=cyclist, alpha=.5)

toronto <- qmap("Toronto, Ontario", zoom=11) + 
  geom_point(pedestiran, aes(x=X, y=Y, group=group, alpha=0.25))

west_end <- c(-79.4275, 43.6496)
downtown<- c(-79.3777, 43.6543)
east_end <- c(-79.3232, 43.67355)
north <- c(-79.4126, 43.7007)
etobicoke <- c(-79.4983, 43.6426)
scarborough <- c(-79.2475, 43.7357)

#worst wards

#road class
head(ksi$ROAD_CLASS)

wards <- accidents %>% group_by(Ward_Name, YEAR) %>% summarize(count = n())

#worst neighborhoods
neighborhoods <- accidents %>% group_by(Hood_Name, YEAR) %>% summarize(count = n())

#most common vehicle types
head(ksi$VEHTYPE)

#speeding accidents
head(ksi$SPEEDING)
speeding <- ksi %>% filter(SPEEDING == "Yes")

#number with passanger

#alcohol
head(ksi$ALCOHOL)
alcohol <- accidents %>% filter(ALCOHOL == "Yes")


alc_by_year <- alcohol %>% group_by(YEAR) %>%
  summarize(count = n())
alc_by_year$YEAR <- as.factor(alc_by_year$YEAR)

ggplot(alc_by_year, aes(x=YEAR, y=count)) +
  geom_bar(stat="identity") +
  ggtitle("Number of Major Accidents that Involve Alcohol") +
  xlab("Year") + ylab("Number of Major Accidents")

#redlight
red_light <- accidents %>% filter(REDLIGHT == "Yes")

#nubmer of trucks


#ag driving


#time of year

#time of day