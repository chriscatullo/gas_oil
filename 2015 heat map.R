library(tidyverse)
library(readr)
library(dplyr)
library(RColorBrewer)
library(rjson)
library(plotly)
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)

gasdata<-read.csv('2015allcounty.csv')

#replace not availables with zeroes for gas production
gasdata[is.na(gasdata)]<-0

#rename counties as "subregion" in order to join
colnames(gasdata)[1]<-"subregion"


#lowercase county names
gasdata[[1]]<-tolower(gasdata[[1]])

#get pa data
states <- map_data("state")
pa_df <- subset(states, region == "pennsylvania")

#get county data
counties <- map_data("county")
pa_county <- subset(counties, region == "pennsylvania")


#base thing taken from website
pa_base <- ggplot(data = pa_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
pa_base


pa_base + theme_nothing() + 
  geom_polygon(data = pa_county, fill = NA, color = "blue") +
  geom_polygon(color = "black", fill = NA)

#joining the gasdata & county datasets, connecting by subregion
join<-inner_join(gasdata,pa_county, by="subregion")

#gets rid of axes I guess
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


# idk why it's called elbow room but hey > elbow room 1 and 2 for GAS
elbow_room1 <- pa_base +
  geom_polygon(data = join, aes(fill = gas), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes


elbow_room1

#elbow room 2 enhances the color scale and transforms 
eb2 <- elbow_room1 +
scale_fill_gradientn(colors = rev(rainbow(7)),
breaks = c(10000, 50000, 250000, 500000, 5000000,10000000),trans="log10")

eb2

#same as elbow room 1, but for OIL
elbow_room3 <- pa_base +
  geom_polygon(data = join, aes(fill = oil), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes


elbow_room3

#same function as elbow room 2, but for OIL
eb4 <- elbow_room3 +
  scale_fill_gradientn(colors = rev(rainbow(7)),
                       breaks = c(0,100, 1000, 10000, 100000,250000,500000),trans="log10")

eb4
