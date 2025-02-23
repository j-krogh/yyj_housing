library(ggplot2)
library(dplyr)

#add supportive house data from BC Housing
#https://catalogue.data.gov.bc.ca/dataset/e6b53f5e-bbc9-4343-adf4-1d9e973422da
data <- read.csv("./data/2024_supportive_housing.csv")
colnames(data)[2] <- "Number"
colnames(data)[3] <- "Type"

data %>% filter(Type != "Total") %>%
ggplot(aes(x = reorder(Municipality, -Number), y = Number, fill = Type)) +
  geom_bar(stat = 'identity') +
  labs(y = "Units",
       x = "",
       title = "Emergency and Supportive Housing",
       caption = "Data Source: BC Housing as of March 31, 2024") +
  scale_y_continuous(labels = scales::comma, limits = c(0,2000)) +
  geom_text(aes(label = scales::comma(Emergency.Shelter)), vjust = 0.5, hjust = -0.25, angle = 90) +
  theme_bw()


muni$pop <- 0
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Central Saanich"] = 18689
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Colwood"] = 20766
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Esquimalt"] = 19155
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Highlands"] = 2599
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Langford"] = 49345
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Metchosin"] = 5142
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "North Saanich"] = 12671
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Oak Bay"] = 19211
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Saanich"] = 125853
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Sidney"] = 12569
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Sooke"] = 15991
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "Victoria"] = 96390
muni$pop[muni$ADMIN_AREA_ABBREVIATION == "View Royal"] = 12606


muni$ADMIN_AREA_ABBREVIATION[muni$ADMIN_AREA_ABBREVIATION == "Central Saanich"] = "C. Saanich"
muni$ADMIN_AREA_ABBREVIATION[muni$ADMIN_AREA_ABBREVIATION == "North Saanich"] = "N. Saanich"

p1 <- ggplot(muni, aes(x = reorder(ADMIN_AREA_ABBREVIATION, -pop), y = pop)) +
  geom_bar(stat = 'identity') +
  labs(y = "Population",
       x = "",
       title = "Population of The CRD",
       caption = "Data Source: Statistics Canada 2022 Population Estimates") +
  scale_y_continuous(labels = scales::comma, limits = c(0,145000)) +
  geom_text(aes(label = scales::comma(pop)), vjust = -1, hjust = 0, angle = 45) +
  theme_bw()
ggsave("crd_population_2022.png", width = 10, height = 6)


p2 <-ggplot(muni, aes(x = reorder(ADMIN_AREA_ABBREVIATION, -pop), y = supp_housing_untits)) +
  geom_bar(stat = 'identity') +
  labs(y = "Units",
       x = "",
       title = "Emergency Shelter and Housing for the Homeless",
       caption = "Data Source: BC Housing as of March 31, 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0,2000)) +
  geom_text(aes(label = scales::comma(supp_housing_untits)), vjust = 0.5, hjust = -0.25, angle = 90) +
  theme_bw()
ggsave("crd_supportive_housing_2023.png", width = 12, height = 6)

p3<-cowplot::plot_grid(p1,p2,ncol = 1,align = "v")

ggsave2('pop_and_shelters.png',p3, height = 10, width = 8)

ggplot(muni) + 
  geom_sf(aes(fill = ADMIN_AREA_ABBREVIATION, alpha = 0.3)) + 
  geom_sf_label(aes(label = supp_housing_untits)) +
  theme_void() +
  theme(legend.position = "none")



