# load occurrences #
ne <- read.csv("NEC_list.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

#get rid of columns I don't need
ne <- ne[,c(2:20, 29:33, 53:60, 83, 84)]

# Filter occurrences #
ne <- ne[ne$month!= 0 & ne$year!= 0 & ne$day!= 0,] # filter specimens without complete colection dates
ne <- ne[ne$year > 1700 & ne$year < 2017,] # filter specimens with incorrect dates
ne <- ne[!is.na(ne$specificEpithet) & !is.na(ne$genus),] #filter specimens without names

ne$institutionCode <- as.factor(ne$institutionCode)
levels(ne$institutionCode)

#First, get data for species at HUH
ne <- ne[ne$institutionCode == "University of New Hampshire",] # get HUH & UCONN specimens
ne <- ne[ne$institutionCode == "University of Vermont",]

#Just Massachusetts and Rhode Island
ne.mass <- c("MAssachusetts", "Mass", "massachusetts", "mass", "Massachusetts", "MASSACHUSETTS", "MA")
ne <- ne[ne$stateProvince %in% ne.mass,]

#Just Rhode Island
#ne.ri <- c("Rhode Island", "RHODE ISLAND", "rhode island", "RI")
#ne <- ne[ne$stateProvince %in% ne.ri,]

# make sure state names match
ne$stateProvince <- gsub("Conn\\`|copnn|conn|Connect|Con|Conn|connecticut|Connecticut|CONNECTICUT|CT", "Connecticut", ne$stateProvince)
ne$stateProvince <- gsub("MAssachusetts|MASSACHUSETTS|Massachusetts|mass|Mass|massachusetts|MA", "Massachusetts", ne$stateProvince)
ne$stateProvince <- gsub("rhode island|RHODE ISLAND|Rhode Island|RI", "Rhode Island", ne$stateProvince)

#This is the rosid/asterid spp list
ne.speciesvec <- c("Clethra alnifolia", "Galium triflorum", "Cuscuta gronovii", "Epigaea repens", "Kalmia angustifolia",
                   "Lycopus americanus", "Lysimachia terrestris", "Mentha arvensis", "Vaccinium corymbosum", "Vaccinium macrocarpon",
                   "Gaylussacia baccata", "Baptisia tinctoria", "Lespedeza capitata", "Lespedeza hirta", "Ludwigia palustris", "Triadenum virginicum",
                   "Vitis labrusca", "Lechea intermedia", "Viola pedata", "Viola blanda", "Viola cucullata", "Polygala sanguinea")

ne.species <- ne[ne$scientificName %in% ne.speciesvec,]

# summarize by species and 20-year bins
df2 <- transform(ne.species, group=cut(year,  breaks=c(-Inf,1820, 1840, 1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2016, Inf),
                                       labels=c('to1820', 'to1840', 'to1860', 'to1880', 'to1900', 'to1920', 'to1940', 'to1960', 'to1980', 'to2000', 'to2016', 'after2016')))


df2$scientificName <- as.character(df2$scientificName)
out <- table(df2$scientificName, df2$group)

write.csv(out, "Rosidasteridlist_byyear_allherbaria_rhodeislandplants.csv")
write.csv(out, "Rosidasteridlist_byyear_vermont_massplants.csv")
write.csv(out, "Rosidasteridlist_byyear_newhampshire_massplants.csv")
