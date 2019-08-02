library(tidyverse)
library(lubridate)
library(rel)
tring <- read.csv("20170704_sora_from_tring.csv", stringsAsFactors = FALSE) %>%
                select(location, month, day, year, sex, museum, species, assecion_number) %>%
                mutate(from="tringFILE")

fieldmuseum <- read.csv("20170713_sora_from_field_museum.csv", stringsAsFactors = FALSE) %>%
              select(location, month, day, year, sex, museum, species, assecion_number) %>%
                mutate(month = ifelse(month=="June",6,
                                      ifelse(month=="May",5,NA)),
                       from="fieldmuseumFILE",
                       assecion_number=as.character(assecion_number))

vertnet <- read.csv("vertnet.csv", stringsAsFactors = FALSE) %>%
            select(country, month, day, year, sex, institutioncode, scientificname, occurrenceid) %>%
            filter(institutioncode!="FMNH") %>%
            mutate(month = as.integer(month),
                   day = as.integer(day),
                   year = as.integer(year),
                   from="vertnetFILE")


colnames(vertnet) <- colnames(tring)

ebird <- read.csv("summer_notNA_ebird.csv", stringsAsFactors = FALSE) %>%
          mutate(institutioncode="FROMEBIRD") %>% 
          select(COUNTRY, Month, Day, Year, AGE.SEX, institutioncode, SCIENTIFIC.NAME, ?..GLOBAL.UNIQUE.IDENTIFIER ) %>%
          mutate(anumber = NA,
                 from="ebirdFILE")

colnames(ebird) <- colnames(tring)

gbif <- read.csv("summernotUSCA_gbif.csv", stringsAsFactors = FALSE) %>%
            mutate(sex = NA)  %>%
            select(countryCode, month, day, year,sex, institutionCode, scientificName,occurrenceID) %>%
            mutate(from="gbifFILE")
          
colnames(gbif) <- colnames(tring)



masterdat <- bind_rows(tring, fieldmuseum, vertnet, gbif, ebird)


northamerica <- c("USA","Canada","United States","United States of America","US","CA","UNITED STATES","")

droplocations <- c("No data","? Unknown","Locality Unknown","unknown")

mastersummer <- masterdat %>% 
                  filter(!(location %in% northamerica)) %>%
                  mutate(location = ifelse(location=="AW","Aruba",location),
                         location = ifelse(location=="BM","Bermuda",location),
                         location = ifelse(location=="BS","Bahamas",location),
                         location = ifelse(location=="BZ","Belize",location),
                         location = ifelse(location=="Belieze","Belize",location),
                         location = ifelse(location=="CO","Colombia",location),
                         location = ifelse(location=="CU","Cuba",location),
                         location = ifelse(location=="Havana, Cuba",'Cuba',location),
                         location = ifelse(location=="MX",'Mexico',location),
                         location = ifelse(location=="NO","Norway",location),
                         location = ifelse(location=="PR","Puerto Rico",location),
                         location = ifelse(location=="SE",'Sweden',location),
                         location = ifelse(location=="VI","Virgin Islands",location),
                         location = ifelse(location=="Virgin Islands (U.S.)",
                                                          "Virgin Islands",location),
                         location = ifelse(location=="British West Indies",
                                                          "Cayman Islands",location),
                         location = ifelse(location=="[Venezuela]",
                                                          "Venezuela",location),
                         location = ifelse(location=="TURKS AND CAICOS ISLANDS",
                                                "Turks and Caicos Islands", location),
                         location = ifelse(location=="Bartica Guyana",
                                                        "Guyana",location),
                         location = ifelse(location=="Carayacas Ecuador",
                                           "Ecuador",location),
                         location = ifelse(location=="Chapuleo Mexico",
                                           "Mexico",location),
                         location = ifelse(location=="Cozumela Island Mexico",
                                           "Mexico",location),
                         location = ifelse(location == "Cuban Virapaz Guatemala",
                                           "Guatemala",location),
                         location = ifelse(location == "Duenas Guatemala",
                                           "Guatemala",location),
                         location = ifelse(location=="Medellin Columbia",
                                           "Columbia",location),
                         location = ifelse(location=="Merida Venezuela",
                                           "Venezuela",location),
                         location = ifelse(location=="Narino, Columbia",
                                           "Columbia",location),
                         location = ifelse(location=="Nuestro Ano Costa Rica",
                                           "Costa Rica",location),
                         location = ifelse(location=="Progresso Mexico",
                                           "Mexico",location),
                         location = ifelse(location=="San Lucas Ecuador",
                                           "Ecuador",location),
                         location = ifelse(location=="Santa Elena Ecuador",
                                           "Ecuador",location),
                         location = ifelse(location=="Quinto Ecuador",
                                           "Ecuador",location),
                         location = ifelse(location=="Sion Hill Panama",
                                           "Panama",location)) %>%
        filter(!(location %in% droplocations))


# When there is new data: 
#write.csv(mastersummer, file="master_vagrants.csv", row.names = FALSE)

#go through and check ebird vs gbif by hand for duplicates

dat <- read.csv("master_vagrants.csv") %>%
  filter(cut!="X") 

# cut out wintering birds in central/south america/carribean

keep_for_winter <- c("Norway","Sweden","Philippines")

dat_reduced <- dat %>% 
          mutate(location = as.character(location),
                 location = ifelse(location=="Timpas Mexico","Mexico",location),
                 location = ifelse(location=="Valley of Mexico","Mexico",location)) %>%
          filter(!is.na(month)) %>%
          mutate(cut_place = ifelse(location %in% keep_for_winter, "outsideW","check"))


dat_reduced$cut <- NA

outsideW <- dat_reduced[dat_reduced$cut_place=="outsideW",]
check <- dat_reduced[dat_reduced$cut_place=="check",]

checked <- check %>%
            mutate(cut = ifelse(month<=4|month>=9, "cut","donotcut")) %>%
            filter(cut=="donotcut")


dat_combined <- rbind(outsideW, checked) %>% select(-cut, -cut_place)

write.csv(dat_combined, file=paste0(Sys.Date(),"_master_checked_for_ebird_dups.csv"), row.names = FALSE)


table(dat_combined$month, dat_combined$location)


a <- ggplot(data=dat_combined, aes(x=month))+
  geom_histogram()+
  theme_fournier()

b <- ggplot(data=dat_combined, aes(x=location))+
  geom_bar()+
  facet_wrap(~month, ncol=1)+
  theme_fournier()+
  xlab("Locations")+
  ylab("Count")

ggsave(a, file="month_bar_chart.jpeg", width=10, height=10, units="cm", dpi=300)

ggsave(b, file="location_month_bar_chart.jpeg", width=30, height=10, units="cm",dpi=300)

world <- map_data("world")


soracountries <- world[world$region %in% dat$location,]


soramap <- ggplot()+
  geom_polygon(data=world, 
               aes(x=long, y=lat, group=group), 
               fill="white", col="lightgrey", size=0.2)+
  geom_polygon(data=soracountries, 
               aes(x=long, y=lat, group=group), 
               fill="darkgrey", col="black", size=0.2)+
  theme_fournier()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

ggsave(soramap, file="map_of_locations.jpeg", width=15, height=10, units="cm", dpi=300)


