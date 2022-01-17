### Fish Stat Assignment Data
### Lauren Gill
##### Apbi 319
##### Jan 15, 2022

library(here)
library(ggplot2)
library(tidyverse)
library(cowplot)

###comparing the development of aquaculture by 
###environment (i.e. marine vs brackish vs freshwater) in the last 15 years
quantity <- read.csv(here("quantity_watertype.csv"))
quantitygraph <- ggplot(quantity, aes(x = year, y = tons, colour = water_type))+
  geom_point()+
  labs(x = "Year", y = "Live Weight (Tons)", colour = "Water Type")+
  geom_smooth(method = 'lm')+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))+
  theme_classic()

quantitygraph  

valuegraph <- ggplot(quantity, aes(x = year, y = value, colour = water_type))+
  geom_point()+
  labs(x = "Year", y = "Value (USD, 1000s)", colour = "Water Type")+
  geom_smooth(method = 'lm')+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))+
  theme_classic()

valuegraph


##Question 2

species <- read_csv(here("question2.csv"))

brackish_value <- species %>% filter(water_type == "Brackishwater") %>%
  arrange(desc(value))%>% slice(1:10) %>% mutate(value = value/1000000)


freshwater_value <- species %>% filter(water_type == "Freshwater") %>%
  arrange(desc(value)) %>% mutate(value = value/1000000)
freshwater_value <- freshwater_value[-c(10), ]
freshwater_value <- freshwater_value %>% slice(1:10)

marine_value <- species %>% filter(water_type == "Marine") %>%
  arrange(desc(value)) %>% mutate(value = value/1000000)
marine_value <- marine_value[-c(8), ]
marine_value <- marine_value[-c(9), ]
marine_value <- marine_value[-c(9), ]
marine_value <- marine_value %>% slice(1:10)

############
brackish_quantity <- species %>% filter(water_type == "Brackishwater") %>%
  arrange(desc(quantity)) %>% mutate(quantity = quantity/1000000)
brackish_quantity <- brackish_quantity[-c(5), ]
brackish_quantity <- brackish_quantity %>% slice(1:10)

freshwater_quantity <- species %>% filter(water_type == "Freshwater") %>%
  arrange(desc(quantity)) %>% mutate(quantity = quantity/1000000)
freshwater_quantity <- freshwater_quantity[-c(6), ]
freshwater_quantity <- freshwater_quantity %>% slice(1:10)

marine_quantity <- species %>% filter(water_type == "Marine") %>%
  arrange(desc(quantity)) %>% mutate(quantity = quantity/1000000)
marine_quantity <- marine_quantity[-c(4), ]
marine_quantity <- marine_quantity[-c(5), ]
marine_quantity <- marine_quantity[-c(7), ]
marine_quantity <- marine_quantity %>% slice(1:10)

### Final datasets
value_top10 <- rbind(brackish_value, freshwater_value, marine_value) %>%
  select(-c(quantity))

quantity_top10 <- rbind(brackish_quantity, freshwater_quantity, marine_quantity) %>%
  select(-c(value))


###Graphs

##### Brackish
brackquant<- ggplot(brackish_quantity, aes(x=reorder(species, quantity), y=quantity))+
  geom_bar(stat = "identity") +
  theme_classic()+
  coord_flip()+
  labs(x = "Species", y= "Quantity (million tons)")
  
brackval <- ggplot(brackish_value, aes(x=reorder(species, value), y=value))+
  geom_bar(stat = "identity")+
  theme_classic() +
  coord_flip()+
  labs(x = "Species", y= "Value (billion USD)")

plot_grid(brackquant, brackval)

##### Freshwater
freshquant<- ggplot(freshwater_quantity, aes(x=reorder(species, quantity), y=quantity))+
  geom_bar(stat = "identity") +
  theme_classic()+
  coord_flip()+
  labs(x = "Species", y= "Quantity (million tons)")
freshval <- ggplot(freshwater_value, aes(x=reorder(species, value), y=value))+
  geom_bar(stat = "identity")+
  theme_classic() +
  coord_flip()+
  labs(x = "Species", y= "Value (billion USD)")

plot_grid(freshquant, freshval)

##### Marine
marinequant<- ggplot(marine_quantity, aes(x=reorder(species, quantity), y=quantity))+
  geom_bar(stat = "identity") +
  theme_classic()+
  coord_flip()+
  labs(x = "Species", y= "Quantity (million tons)")
marineval <- ggplot(marine_value, aes(x=reorder(species, value), y=value))+
  geom_bar(stat = "identity")+
  theme_classic() +
  coord_flip()+
  labs(x = "Species", y= "Value (billion USD)")

plot_grid(marinequant, marineval)
