library(devtools)
install_github("seankross/lego")
library(lego)
library(dplyr)
library(tidyverse)
library(ggplot2)


df <- dplyr::tbl_df(legosets)

legosets %>%
  filter(!is.na(GBP_MSRP)) %>%
  group_by(Year) %>%
  ggplot(aes(Pieces)) +
  geom_bar()

avg_price_per_year <- legosets %>%
  filter(!is.na(USD_MSRP)) %>%
  group_by(Year) %>%
  do(data.frame(Price = mean(.$USD_MSRP)))

med_price_per_year <- legosets %>%
  filter(!is.na(USD_MSRP)) %>%
  group_by(Year) %>%
  do(data.frame(Price = median(.$USD_MSRP)))

plot(avg_price_per_year, type = "l", col = "blue", 
     main = "Lego set prices over time", ylim = c(0, max(avg_price_per_year$Price)))
points(med_price_per_year, type = "l", col = "red")
legend("topleft", inset=c(0.2,0), legend=c("Average","Median"), lty=c(1,1), col=c("blue", "red"))

# By Year
legosets %>%
  select(Item_Number,Name,Theme, GBP_MSRP, Pieces, Year) %>%
  filter(GBP_MSRP > 30) %>%
  filter(Pieces > 100) %>%
  filter(Theme == 'Star Wars') %>%
  mutate(price_per_piece = GBP_MSRP / Pieces) %>%
  ggplot(aes(x=GBP_MSRP, y=price_per_piece)) + 
  geom_point(aes(color = Year))

# By Availability Type
legosets %>%
  select(Item_Number,Name,Theme, GBP_MSRP, Pieces, Availability) %>%
  filter(GBP_MSRP > 50) %>%
  filter(Pieces > 100) %>%
  #mutate(price_per_piece = GBP_MSRP / Pieces) %>%
  ggplot(aes(x=GBP_MSRP, y=Pieces)) + 
  geom_point(aes(color = Availability))
