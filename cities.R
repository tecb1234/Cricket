library(ggmap)

cities_raw <- combined_info_df %>%
  select(city) %>%
  distinct(city) %>%
  filter(!is.na(city)) %>%
  mutate(country = as.character(geocode(city, output = "more")$country))

#manually do Mirpur because that isn't on google list
cities_intermediate <- cities_raw %>%
  mutate(country1 = ifelse(city == "Mirpur", "Bangladesh", country)) 


#Change the UK ones to find the Leve1 Admin Area i.e England or Scotland
cities_intermediate <- cities_intermediate %>%
  mutate(country2 = ifelse(country1 == "United Kingdom", as.character(geocode(city, output = "more")$administrative_area_level_1), country1))


#King city - Canada
###West Indies
# Barbados
# Trinidad
# St Kitts
# Guyana
# St Lucia
# Antigua
#Hamilton - NZ
#Birmingham - England
#Cardiff - England
cities_intermediate <- cities_intermediate %>%
  mutate(country3 = ifelse(city == "King City", "Canada", country2)) %>%
  mutate(country4 = ifelse(city %in% c("Bardabos",
                                        "Trinidad",
                                        "St Kitts",
                                        "Guyana",
                                        "St Lucia",
                                        "Antigua"),
                           "West Indies", country3)) %>%
  mutate(country5 = ifelse(city == "Hamilton", "New Zealand", country4)) %>%
  mutate(country6 = ifelse(city == "Birmingham", "England", country5)) %>%
  mutate(country7 = ifelse(city == "Cardiff", "England", country6)) %>%
  mutate(country8 = ifelse(city == "East London", "South Africa", country7))

cities_revised <- cities_intermediate %>%
  select(city, country7) %>%
  rename(country = country7)

save(cities_revised, file = "cities.RData")

