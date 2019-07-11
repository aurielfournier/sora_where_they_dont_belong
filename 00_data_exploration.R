library(tidyverse)
library(lubridate)

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

gbif <- read.csv("gbif.csv", stringsAsFactors = FALSE) %>%
            mutate(sex = NA)  %>%
            select(countryCode, month, day, year,sex, institutionCode, scientificName,occurrenceID) %>%
            mutate(from="gbifFILE")
          
colnames(gbif) <- colnames(tring)

# still waiting on ebird data 

masterdat <- bind_rows(tring, fieldmuseum, vertnet, gbif)


northamerica <- c("USA","Canada","United States","United States of America","US","CA","UNITED STATES","")

mastersummer <- masterdat %>% 
                  filter(month>=6&month<=7,
                         !(location %in% northamerica)) %>%
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
                         location = ifelse(location=="British West Indies","Cayman Islands",location)) %>%
                  filter(location!="Sweden",
                         location!="Norway",
                         location!="Philippines")

ggplot(data=mastersummer, aes(x=month))+
  geom_histogram()


ggplot(data=mastersummer, aes(x=location))+
  geom_bar()+
  facet_wrap(~month, ncol=1)+
  theme_bw()
  

table(mastersummer$month, mastersummer$location)


world <- map_data("world")
WestHem <- c("USA","Canada","Mexico","Brazil","Panama","Columbia",
             "Cuba","Nicaragua","Guatemala","Belize","Costa Rica",
             'El Salvador',"Honduras","Colombia","Ecuador","Venezuela",
             "Guyana","Suriname","Peru","Bolivia","Puerto Rico","Haiti",
             "Jamaica","Bahamas","French Guiana","Dominican Republic",
             "British Virgin Islands","Grenadines","Virgin Islands",
             "Saint Lucia","Barbados",'Trinidad and Tobago',"Arbua",
             "Curacao","Turks and Caicos","Montserrat","Dominica",
             "Chile","Argentina","Paraguay")


soracountries <- world[world$region %in% mastersummer$location,]

westhem <- world[world$region %in% WestHem,]

ggplot()+
  geom_polygon(data=westhem, 
               aes(x=long, y=lat, group=group), 
               fill="white", col="lightgrey", size=0.2)+
  geom_polygon(data=soracountries, 
               aes(x=long, y=lat, group=group), 
               fill="darkgrey", col="lightgrey", size=0.2)+
  coord_map("albers",
            lat0=0, lat1=60,
            xlim=c(-120,-60),ylim=c(-20,40))



closora <- read.csv("vertnet.csv", stringsAsFactors = FALSE) %>%
              filter(institutioncode=="CLO",
                     month=="6"|month=="7",
                     countrycode!="US",
                     countrycode!="CA")
