#Get random numbers for grid system
x4 <- sample(1:40, 2000, replace=T)
x4


# load occurrences #
ne <- read.csv("NEC_list.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
#stringsAsFactors=FALSE, fileEncoding="latin1"

#get rid of columns I don't need
ne <- ne[,c(2:20, 29:33, 53:60, 83, 84)]

# Filter occurrences #
ne <- ne[ne$month!= 0 & ne$year!= 0 & ne$day!= 0,] # filter specimens without complete colection dates
ne <- ne[ne$year > 1700 & ne$year < 2017,] # filter specimens with incorrect dates
ne <- ne[!is.na(ne$specificEpithet) & !is.na(ne$genus),] #filter specimens without names

#First, get data for species at HUH
ne <- ne[ne$institutionCode == "Harvard University",] # get HUH & UCONN specimens

# get New England data
ne.states <- c("Vermont", "New Hamshire", "Miane", "New Hampsire", "New Ham", "Conn`", "copnn", "New Hamphire", "conn",
               "Connect", "Con", "New Hamshire", "New Hampsire", "MAssachusetts", "New hampshire", "Mass", "massachusetts",
               "Conn", "connecticut", "New Hampshire", "mass", "Massachusetts", "Connecticut", "Rhode Island", "Maine", "maine",
               "RHODE ISLAND", "MAINE", "MASSACHUSETTS","vermont", "rhode island","VERMONT", "NEW HAMPSHIRE", "CONNECTICUT",
               "ME", "MA", "NH", "VT", "RI", "CT")

#ne <- ne[ne$stateProvince %in% ne.states,]

#Just Massachusetts
# get Massachusetts data
ne.mass <- c("MAssachusetts", "Mass", "massachusetts", "mass", "Massachusetts", "MASSACHUSETTS", "MA", "Conn`", "copnn", 
             "Connect", "Con", "Conn", "connecticut", "Connecticut", "Rhode Island", "RHODE ISLAND", "rhode island", "CONNECTICUT",
             "RI", "CT")

ne <- ne[ne$stateProvince %in% ne.mass,]

# get Middlesex data
#ne.county <- c("Middlesex")

#ne <- ne[ne$county == "Middlesex",]

# make sure state names match
ne$stateProvince <- gsub("Vermont|vermont|VERMONT|VT", "Vermont", ne$stateProvince)
ne$stateProvince <- gsub("Miane|MAINE|Maine|maine|ME", "Maine", ne$stateProvince)
ne$stateProvince <- gsub("New Hamshire|New Hampsire|New Ham|New Hamphire|New Hamshire|New Hampsire|New hampshire|New Hampshire|NEW HAMPSHIRE|NH", "New Hampshire", ne$stateProvince)
ne$stateProvince <- gsub("Conn\\`|copnn|conn|Connect|Con|Conn|connecticut|Connecticut|CONNECTICUT|CT", "Connecticut", ne$stateProvince)
ne$stateProvince <- gsub("MAssachusetts|MASSACHUSETTS|Massachusetts|mass|Mass|massachusetts|MA", "Massachusetts", ne$stateProvince)
ne$stateProvince <- gsub("rhode island|RHODE ISLAND|Rhode Island|RI", "Rhode Island", ne$stateProvince)

# get data for species of interest
ne.speciesvec <- c(#"Vaccinium angustifolium", "Aquilegia canadensis", "Gaylusaccia bacata",
                   #"Antennaria plantaginifolia", "Desmodium canadense", "Urtica dioica",
                   #"Ulmus americana", "Chelone glabra", "Spiraea tomentosa", "Aster novae-angilae",
                   #"Asclepias syriaca", "Quercus bicolor", "Quercus velutina", "Rosa carolina",
                   #"Rosa virginiana", "Rosa palustris", "Amelanchier arborea", "Anemone quinquefolia",
                   #"Arenaria leteriflora", "Caltha palustris", "Comandra umbellate", "Cornus canadensis",
                   #"Fragaria virginiana", "Geranium maculatum", "Houstonia caerulea", "Krigia virginica",
                   #"Potentilla canadensis", "Prunus pensylvanica", "Prunus serotina", "Prunus virginiana",
                   #"Rhododendron canadense", "Saxifraga virginiensis", "Senecio aureaus", "Silene caroliniana",
                   #"Smilax rotundifolia", "Trientalis borealis", "Trillium cernuum", "Vaccinium corymbosum", 
                   #"Viola cucullata", "Viola fimbriatula", "Viola pedata", "Acer rubrum", "Acer saccharinum", 
                   #"Betula alleghaniensis", "Betula lenta", "Betula papyrifera", "Carpinus caroliniana",
                   #"Carya glabra", "Carya ovata", "Fagus grandifolia", "Fraxinus americana", "Fraxinus nigra",
                   #"Fraxinus pennsylvanica", "Juglans cinerea", "Ostrya virginiana", "Populus tremuloides",
  "Vitis aestivalis",
  "Myrica gale",
  "Symphyotrichum cordifolium",
  "Ocleme acumita",
  "Symplocarpus foetidus",
  "Tradescantia virginia",
  "Mollugo verticillata",
  "Viola labradorica",
  "Oenothera biennis",
  "Solidago nemoralis",
  "Parthenocissus quinquefolia",
  "Gphalium uliginosum",
  "Clematis virginia",
  "Rhus copallinum",
  "Ilex mucrota",
  "Rorippa islandica",
  "Hypericum cadense",
  "Viola brittonia",
  "Lespedeza hirta",
  "Sisyrinchium atlanticum",
  "Vaccinium oxycoccos",
  "Symphyotrichum dumosum",
  "Gratiola aurea",
  "Sium suave",
  "Hypericum gentianoides",
  "Prunus seroti",
  "Ioctis liriifolius",
  "Apocynum androsaemifolium",
  "Polygonum sagittatum",
  "Hypericum punctatum",
  "Viola pedata",
  "Bidens laevis",
  "Mimulus ringens",
  "Drosera intermedia",
  "Iris prismatica",
  "Allium cadense",
  "Lupinus perennis",
  "Lactuca biennis",
  "Sisyrinchium angustifolium",
  "Epigaea repens",
  "Comandra umbellata",
  "Aphalis margaritacea",
  "Eupatoriadelphus dubius",
  "Lespedeza capitata",
  "Baptisia tinctoria",
  "Eupatorium perfoliatum",
  "Veronica scutellata",
  "Desmodium marilandicum",
  "Conyza cadensis",
  "Polygala sanguinea",
  "Saxifraga virginiensis",
  "Symphyotrichum puniceum",
  "Phytolacca america",
  "Solidago rugosa",
  "Campanula aparinoides",
  "Lobelia cardilis",
  "Pogonia ophioglossoides",
  "Polygala paucifolia",
  "Lycopus virginicus",
  "Platanthera psycodes",
  "Moehringia lateriflora",
  "Doellingeria infirma",
  "Bidens frondosa",
  "Acer pensylvanicum",
  "Podophyllum peltatum",
  "Packera aurea",
  "Utricularia macrorhiza",
  "Aquilegia cadensis",
  "Lysimachia quadrifolia",
  "Hieracium venosum",
  "Geranium maculatum",
  "Claytonia virginica",
  "Anemone quinquefolia",
  "Sambucus nigra",
  "Trichostema dichotomum",
  "Sicyos angulatus",
  "Solidago gigantea",
  "Cornus cadensis",
  "Ambrosia artemisiifolia",
  "Potentilla cadensis",
  "Trillium cernuum",
  "Lyonia ligustri",
  "Thalictrum pubescens",
  "Actaea rubra",
  "Doellingeria umbellata",
  "Goodyera pubescens",
  "Melampyrum lineare",
  "Lactuca cadensis",
  "Cuscuta gronovii",
  "Spiraea tomentosa",
  "Helianthemum cadense",
  "Pseudogphalium obtusifolium",
  "Lysimachia hybrida",
  "Amaranthus retroflexus",
  "Scutellaria lateriflora",
  "Erigeron pulchellus",
  "Prunus america",
  "Prunus virginia",
  "Rosa caroli",
  "Smilax herbacea",
  "Hypoxis hirsuta",
  "Sanicula marilandica",
  "Pyrola elliptica",
  "Pyrola chlorantha",
  "Solidago juncea",
  "Sanguiria cadensis",
  "Asclepias syriaca",
  "Symphyotrichum novaeangliae",
  "Vaccinium pallidum",
  "Mitchella repens",
  "Aureolaria pedicularia",
  "Verbe urticifolia",
  "Potentilla norvegica",
  "Kalmia angustifolia",
  "Medeola virginia",
  "Apios america",
  "Iris versicolor",
  "Cornus florida",
  "Agalinis paupercula",
  "Silene carolinia",
  "Viola cucullata",
  "Ludwigia alternifolia",
  "Alisma triviale",
  "Viola lanceolata",
  "Toxicodendron vernix",
  "Arisaema triphyllum",
  "Krigia virginica",
  "Erechtites hieraciifolia",
  "Sagittaria latifolia",
  "Bidens cernua",
  "Viola blanda",
  "Lysimachia thyrsiflora",
  "Geum cadense",
  "Cicuta bulbifera",
  "Rhexia virginica",
  "Chamaedaphne calyculata",
  "Houstonia caerulea",
  "Prunella vulgaris",
  "Asclepias incarta",
  "Chamaecrista fasciculata",
  "Cypripedium acaule",
  "Viburnum recognitum",
  "Vaccinium macrocarpon",
  "Chimaphila umbellata",
  "Lycopus americanus",
  "Mentha arvensis",
  "Lysimachia terrestris",
  "Galium circaezans",
  "Lespedeza virginica",
  "Sarracenia purpurea",
  "Rhododendron cadense",
  "Gaultheria procumbens",
  "Trillium grandiflorum",
  "Aralia nudicaulis",
  "Cicuta maculata",
  "Lepidium virginicum",
  "Caltha palustris",
  "Rhus glabra",
  "Cornus racemosa",
  "Ludwigia palustris",
  "Euthamia graminifolia",
  "Platanthera lacera",
  "Viburnum nudum",
  "Uvularia sessilifolia",
  "Lindera benzoin",
  "Ranunculus abortivus",
  "Thalictrum thalictroides",
  "Maianthemum racemosum",
  "Xanthorhiza simplicissima",
  "Gaylussacia baccata",
  "Cephalanthus occidentalis",
  "Clethra alnifolia",
  "Morella pensylvanica",
  "Vitis labrusca",
  "Viburnum acerifolium",
  "Amelanchier cadensis",
  "Monotropa hypopithys",
  "Desmodium nudiflorum",
  "Maianthemum cadense",
  "Kalmia latifolia",
  "Epilobium coloratum",
  "Hibiscus moscheutos",
  "Trientalis borealis",
  "Sparganium eurycarpum",
  "Prunus pensylvanica",
  "Impatiens capensis",
  "Catalpa bignonioides",
  "Decodon verticillatus",
  "Diervilla lonicera",
  "Robinia pseudoacacia",
  "Lobelia inflata",
  "Polygonum arifolium",
  "Cardamine bulbosa",
  "Lechea intermedia",
  "Nuttallanthus cadensis",
  "Viburnum lentago",
  "Amphicarpaea bracteata",
  "Actaea pachypoda",
  "Hieracium paniculatum",
  "Verbe hastata",
  "Toxicodendron radicans",
  "Vaccinium angustifolium",
  "Rhododendron viscosum",
  "Solidago odora",
  "Veronica peregri",
  "Rosa nitida",
  "Cornus alternifolia",
  "Symphyotrichum racemosum",
  "Solidago cadensis",
  "Vaccinium corymbosum",
  "Drosera rotundifolia",
  "Coptis trifolia",
  "Triadenum virginicum",
  "Penthorum sedoides",
  "Fragaria virginia",
  "Myosotis laxa",
  "Polygonum pensylvanicum",
  "Galium triflorum",
  "Viola sagittata",
  "Galium asprellum",
  "Viola pubescens",
  "Aralia hispida",
  "Acer negundo",
  "Hieracium cadense",
  "Erigeron philadelphicus",
  "Cardamine pratensis",
  "Monotropa uniflora",
  "Smilax rotundifolia",
  "Chelone glabra",
  "Polygonum hydropiperoides",
  "Triodanis perfoliata",
  "Boehmeria cylindrica",
  "Silene antirrhi",
  "Symphyotrichum novibelgii",
  "Campanula rotundifolia",
  "Solidago bicolor",
  "Spiraea alba",
  "Hieracium scabrum",
  "Brasenia schreberi",
  "Scutellaria galericulata",
  "Hypericum ellipticum",
  "Aureolaria flava",
  "Desmodium cadense",
  "Lilium philadelphicum",
  "Oxalis stricta",
  "Myosotis ver",
  "Vernonia noveboracensis",
  "Cornus rugosa",
  "Thalictrum dioicum",
  "Potentilla arguta",
  "Echinocystis lobata",
  "Polygonum lapathifolium",
  "Nuphar lutea",
  "Erigeron annuus",
  "Lespedeza frutescens",
  "Ranunculus recurvatus",
  "Ilex verticillata",
  "Xyris carolinia")

#These are just Dave Boufford's list, use if needed
ne.speciesvec <- c("Vaccinium angustifolium", "Aquilegia canadensis", "Gaylusaccia bacata",
  "Antennaria plantaginifolia", "Desmodium canadense", "Urtica dioica",
  "Ulmus americana", "Chelone glabra", "Spiraea tomentosa", "Aster novae-angilae",
  "Asclepias syriaca", "Quercus bicolor", "Quercus velutina", "Gaylussacia baccata", 
  "Carya ovata")

#This is the rosid/asterid spp list
ne.speciesvec <- c("Clethra alnifolia", "Cornus racemosa", "Cuscuta gronovii", "Epigaea repens", "Kalmia angustifolia",
                   "Lycopus americanus", "Lysimachia terrestris", "Mentha arvensis", "Vaccinium corymbosum", "Vaccinium macrocarpon",
                   "Gaylussacia baccata", "Baptisia tinctoria", "Lespedeza capitata", "Lespedeza hirta", "Ludwigia palustris", "Triadenum virginicum",
                   "Vitis labrusca", "Lechea intermedia", "Viola pedata", "Viola blanda", "Viola cucullata", "Polygala sanguinea")

ne.species <- ne[ne$scientificName %in% ne.speciesvec,]

# summarize by species and 20-year bins
df2 <- transform(ne.species, group=cut(year,  breaks=c(-Inf,1820, 1840, 1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2016, Inf),
                                    labels=c('to1820', 'to1840', 'to1860', 'to1880', 'to1900', 'to1920', 'to1940', 'to1960', 'to1980', 'to2000', 'to2016', 'after2016')))


df2$scientificName <- as.character(df2$scientificName)
out <- table(df2$scientificName, df2$group)
#, df2$institutionCode
#table(df2$scientificName, df2$institutionCode)

# write results to file
#row.names = FALSE
#write.csv(out, "herbarium_hopefuls_Harvard_latitude.csv")
write.csv(out, "Species_tosample_allNEherbaria.csv")




#Randomly sample 60
mysample <- sample(ne.speciesvec, 80)

ne.species_mysample <- ne[ne$scientificName %in% mysample,]

# summarize by species and 20-year bins
df3 <- transform(ne.species_mysample, group=cut(year,  breaks=c(-Inf,1820, 1840, 1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2016, Inf),
                                       labels=c('to1820', 'to1840', 'to1860', 'to1880', 'to1900', 'to1920', 'to1940', 'to1960', 'to1980', 'to2000', 'to2016', 'after2016')))

df3$scientificName <- as.character(df3$scientificName)
out <- table(df3$scientificName, df3$group)

# write results to file
write.csv(out, "herbarium_hopefuls_Harvard_random.csv")





#UConn
# load occurrences #
ne <- read.csv("NEC_list.csv", fileEncoding="latin1")

#get rid of columns I don't need
ne <- ne[,c(2:20, 29:33, 53:60, 83, 84)]

# Filter occurrences #
ne <- ne[ne$month!= 0 & ne$year!= 0 & ne$day!= 0,] # filter specimens without complete colection dates
ne <- ne[ne$year > 1700 & ne$year < 2017,] # filter specimens with incorrect dates
ne <- ne[!is.na(ne$specificEpithet) & !is.na(ne$genus),] #filter specimens without names

#First, get data for species at UConn
ne <- ne[ne$institutionCode == "UConn",] # get HUH & UCONN specimens

# get New England data
ne.states <- c("Vermont", "New Hamshire", "Miane", "New Hampsire", "New Ham", "Conn`", "copnn", "New Hamphire", "conn",
               "Connect", "Con", "New Hamshire", "New Hampsire", "MAssachusetts", "New hampshire", "Mass", "massachusetts",
               "Conn", "connecticut", "New Hampshire", "mass", "Massachusetts", "Connecticut", "Rhode Island", "Maine", "maine",
               "RHODE ISLAND", "MAINE", "MASSACHUSETTS","vermont", "rhode island","VERMONT", "NEW HAMPSHIRE", "CONNECTICUT",
               "ME", "MA", "NH", "VT", "RI", "CT")

ne <- ne[ne$stateProvince %in% ne.states,]


#Just Massachusetts
# get Massachusetts data
#ne.mass <- c("MAssachusetts", "Mass", "massachusetts", "mass", "Massachusetts", "MASSACHUSETTS", "MA")

ne.mass <- c("MAssachusetts", "Mass", "massachusetts", "mass", "Massachusetts", "MASSACHUSETTS", "MA", "Conn`", "copnn", 
             "Connect", "Con", "Conn", "connecticut", "Connecticut", "Rhode Island", "RHODE ISLAND", "rhode island", "CONNECTICUT",
             "RI", "CT")

ne <- ne[ne$stateProvince %in% ne.mass,]


# get Middlesex data
#ne.county <- c("Middlesex")

#ne <- ne[ne$county == "Middlesex",]

# make sure state names match
ne$stateProvince <- gsub("Vermont|vermont|VERMONT|VT", "Vermont", ne$stateProvince)
ne$stateProvince <- gsub("Miane|MAINE|Maine|maine|ME", "Maine", ne$stateProvince)
ne$stateProvince <- gsub("New Hamshire|New Hampsire|New Ham|New Hamphire|New Hamshire|New Hampsire|New hampshire|New Hampshire|NEW HAMPSHIRE|NH", "New Hampshire", ne$stateProvince)
ne$stateProvince <- gsub("Conn\\`|copnn|conn|Connect|Con|Conn|connecticut|Connecticut|CONNECTICUT|CT", "Connecticut", ne$stateProvince)
ne$stateProvince <- gsub("MAssachusetts|MASSACHUSETTS|Massachusetts|mass|Mass|massachusetts|MA", "Massachusetts", ne$stateProvince)
ne$stateProvince <- gsub("rhode island|RHODE ISLAND|Rhode Island|RI", "Rhode Island", ne$stateProvince)

# get data for species of interest
ne.speciesvec <- c(#"Vaccinium angustifolium", "Aquilegia canadensis", "Gaylusaccia bacata",
  #"Antennaria plantaginifolia", "Desmodium canadense", "Urtica dioica",
  #"Ulmus americana", "Chelone glabra", "Spiraea tomentosa", "Aster novae-angilae",
  #"Asclepias syriaca", "Quercus bicolor", "Quercus velutina", "Rosa carolina",
  #"Rosa virginiana", "Rosa palustris", "Amelanchier arborea", "Anemone quinquefolia",
  #"Arenaria leteriflora", "Caltha palustris", "Comandra umbellate", "Cornus canadensis",
  #"Fragaria virginiana", "Geranium maculatum", "Houstonia caerulea", "Krigia virginica",
  #"Potentilla canadensis", "Prunus pensylvanica", "Prunus serotina", "Prunus virginiana",
  #"Rhododendron canadense", "Saxifraga virginiensis", "Senecio aureaus", "Silene caroliniana",
  #"Smilax rotundifolia", "Trientalis borealis", "Trillium cernuum", "Vaccinium corymbosum", 
  #"Viola cucullata", "Viola fimbriatula", "Viola pedata", "Acer rubrum", "Acer saccharinum", 
  #"Betula alleghaniensis", "Betula lenta", "Betula papyrifera", "Carpinus caroliniana",
  #"Carya glabra", "Carya ovata", "Fagus grandifolia", "Fraxinus americana", "Fraxinus nigra",
  #"Fraxinus pennsylvanica", "Juglans cinerea", "Ostrya virginiana", "Populus tremuloides",
  "Vitis aestivalis",
  "Myrica gale",
  "Symphyotrichum cordifolium",
  "Ocleme acumita",
  "Symplocarpus foetidus",
  "Tradescantia virginia",
  "Mollugo verticillata",
  "Viola labradorica",
  "Oenothera biennis",
  "Solidago nemoralis",
  "Parthenocissus quinquefolia",
  "Gphalium uliginosum",
  "Clematis virginia",
  "Rhus copallinum",
  "Ilex mucrota",
  "Rorippa islandica",
  "Hypericum cadense",
  "Viola brittonia",
  "Lespedeza hirta",
  "Sisyrinchium atlanticum",
  "Vaccinium oxycoccos",
  "Symphyotrichum dumosum",
  "Gratiola aurea",
  "Sium suave",
  "Hypericum gentianoides",
  "Prunus seroti",
  "Ioctis liriifolius",
  "Apocynum androsaemifolium",
  "Polygonum sagittatum",
  "Hypericum punctatum",
  "Viola pedata",
  "Bidens laevis",
  "Mimulus ringens",
  "Drosera intermedia",
  "Iris prismatica",
  "Allium cadense",
  "Lupinus perennis",
  "Lactuca biennis",
  "Sisyrinchium angustifolium",
  "Epigaea repens",
  "Comandra umbellata",
  "Aphalis margaritacea",
  "Eupatoriadelphus dubius",
  "Lespedeza capitata",
  "Baptisia tinctoria",
  "Eupatorium perfoliatum",
  "Veronica scutellata",
  "Desmodium marilandicum",
  "Conyza cadensis",
  "Polygala sanguinea",
  "Saxifraga virginiensis",
  "Symphyotrichum puniceum",
  "Phytolacca america",
  "Solidago rugosa",
  "Campanula aparinoides",
  "Lobelia cardilis",
  "Pogonia ophioglossoides",
  "Polygala paucifolia",
  "Lycopus virginicus",
  "Platanthera psycodes",
  "Moehringia lateriflora",
  "Doellingeria infirma",
  "Bidens frondosa",
  "Acer pensylvanicum",
  "Podophyllum peltatum",
  "Packera aurea",
  "Utricularia macrorhiza",
  "Aquilegia cadensis",
  "Lysimachia quadrifolia",
  "Hieracium venosum",
  "Geranium maculatum",
  "Claytonia virginica",
  "Anemone quinquefolia",
  "Sambucus nigra",
  "Trichostema dichotomum",
  "Sicyos angulatus",
  "Solidago gigantea",
  "Cornus cadensis",
  "Ambrosia artemisiifolia",
  "Potentilla cadensis",
  "Trillium cernuum",
  "Lyonia ligustri",
  "Thalictrum pubescens",
  "Actaea rubra",
  "Doellingeria umbellata",
  "Goodyera pubescens",
  "Melampyrum lineare",
  "Lactuca cadensis",
  "Cuscuta gronovii",
  "Spiraea tomentosa",
  "Helianthemum cadense",
  "Pseudogphalium obtusifolium",
  "Lysimachia hybrida",
  "Amaranthus retroflexus",
  "Scutellaria lateriflora",
  "Erigeron pulchellus",
  "Prunus america",
  "Prunus virginia",
  "Rosa caroli",
  "Smilax herbacea",
  "Hypoxis hirsuta",
  "Sanicula marilandica",
  "Pyrola elliptica",
  "Pyrola chlorantha",
  "Solidago juncea",
  "Sanguiria cadensis",
  "Asclepias syriaca",
  "Symphyotrichum novaeangliae",
  "Vaccinium pallidum",
  "Mitchella repens",
  "Aureolaria pedicularia",
  "Verbe urticifolia",
  "Potentilla norvegica",
  "Kalmia angustifolia",
  "Medeola virginia",
  "Apios america",
  "Iris versicolor",
  "Cornus florida",
  "Agalinis paupercula",
  "Silene carolinia",
  "Viola cucullata",
  "Ludwigia alternifolia",
  "Alisma triviale",
  "Viola lanceolata",
  "Toxicodendron vernix",
  "Arisaema triphyllum",
  "Krigia virginica",
  "Erechtites hieraciifolia",
  "Sagittaria latifolia",
  "Bidens cernua",
  "Viola blanda",
  "Lysimachia thyrsiflora",
  "Geum cadense",
  "Cicuta bulbifera",
  "Rhexia virginica",
  "Chamaedaphne calyculata",
  "Houstonia caerulea",
  "Prunella vulgaris",
  "Asclepias incarta",
  "Chamaecrista fasciculata",
  "Cypripedium acaule",
  "Viburnum recognitum",
  "Vaccinium macrocarpon",
  "Chimaphila umbellata",
  "Lycopus americanus",
  "Mentha arvensis",
  "Lysimachia terrestris",
  "Galium circaezans",
  "Lespedeza virginica",
  "Sarracenia purpurea",
  "Rhododendron cadense",
  "Gaultheria procumbens",
  "Trillium grandiflorum",
  "Aralia nudicaulis",
  "Cicuta maculata",
  "Lepidium virginicum",
  "Caltha palustris",
  "Rhus glabra",
  "Cornus racemosa",
  "Ludwigia palustris",
  "Euthamia graminifolia",
  "Platanthera lacera",
  "Viburnum nudum",
  "Uvularia sessilifolia",
  "Lindera benzoin",
  "Ranunculus abortivus",
  "Thalictrum thalictroides",
  "Maianthemum racemosum",
  "Xanthorhiza simplicissima",
  "Gaylussacia baccata",
  "Cephalanthus occidentalis",
  "Clethra alnifolia",
  "Morella pensylvanica",
  "Vitis labrusca",
  "Viburnum acerifolium",
  "Amelanchier cadensis",
  "Monotropa hypopithys",
  "Desmodium nudiflorum",
  "Maianthemum cadense",
  "Kalmia latifolia",
  "Epilobium coloratum",
  "Hibiscus moscheutos",
  "Trientalis borealis",
  "Sparganium eurycarpum",
  "Prunus pensylvanica",
  "Impatiens capensis",
  "Catalpa bignonioides",
  "Decodon verticillatus",
  "Diervilla lonicera",
  "Robinia pseudoacacia",
  "Lobelia inflata",
  "Polygonum arifolium",
  "Cardamine bulbosa",
  "Lechea intermedia",
  "Nuttallanthus cadensis",
  "Viburnum lentago",
  "Amphicarpaea bracteata",
  "Actaea pachypoda",
  "Hieracium paniculatum",
  "Verbe hastata",
  "Toxicodendron radicans",
  "Vaccinium angustifolium",
  "Rhododendron viscosum",
  "Solidago odora",
  "Veronica peregri",
  "Rosa nitida",
  "Cornus alternifolia",
  "Symphyotrichum racemosum",
  "Solidago cadensis",
  "Vaccinium corymbosum",
  "Drosera rotundifolia",
  "Coptis trifolia",
  "Triadenum virginicum",
  "Penthorum sedoides",
  "Fragaria virginia",
  "Myosotis laxa",
  "Polygonum pensylvanicum",
  "Galium triflorum",
  "Viola sagittata",
  "Galium asprellum",
  "Viola pubescens",
  "Aralia hispida",
  "Acer negundo",
  "Hieracium cadense",
  "Erigeron philadelphicus",
  "Cardamine pratensis",
  "Monotropa uniflora",
  "Smilax rotundifolia",
  "Chelone glabra",
  "Polygonum hydropiperoides",
  "Triodanis perfoliata",
  "Boehmeria cylindrica",
  "Silene antirrhi",
  "Symphyotrichum novibelgii",
  "Campanula rotundifolia",
  "Solidago bicolor",
  "Spiraea alba",
  "Hieracium scabrum",
  "Brasenia schreberi",
  "Scutellaria galericulata",
  "Hypericum ellipticum",
  "Aureolaria flava",
  "Desmodium cadense",
  "Lilium philadelphicum",
  "Oxalis stricta",
  "Myosotis ver",
  "Vernonia noveboracensis",
  "Cornus rugosa",
  "Thalictrum dioicum",
  "Potentilla arguta",
  "Echinocystis lobata",
  "Polygonum lapathifolium",
  "Nuphar lutea",
  "Erigeron annuus",
  "Lespedeza frutescens",
  "Ranunculus recurvatus",
  "Ilex verticillata",
  "Xyris carolinia")


ne.species <- ne[ne$scientificName %in% ne.speciesvec,]

# summarize by species and 20-year bins
df2 <- transform(ne.species, group=cut(year,  breaks=c(-Inf,1820, 1840, 1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2016, Inf),
                                       labels=c('to1820', 'to1840', 'to1860', 'to1880', 'to1900', 'to1920', 'to1940', 'to1960', 'to1980', 'to2000', 'to2016', 'after2016')))

df2$scientificName <- as.character(df2$scientificName)
out <- table(df2$scientificName, df2$group)
#, df2$institutionCode
#table(df2$scientificName, df2$institutionCode)

# write results to file
#row.names = FALSE
write.csv(out, "herbarium_hopefuls_UConn.csv")


#Plot specimen numbers from HUH and UCONN
har<-read.csv("herbarium_hopefuls_Harvard.csv", row.names = 1)
con<-read.csv("herbarium_hopefuls_UConn.csv", row.names = 1)

qH<-rowSums(har)
qC<-rowSums(con)

sp<-NULL
num<-NULL
for (i in 1:length(qC)){
  sp[i]<-names(qC[i])
  
  if (length(qH[names(qH) == sp[i]])>0){
    num[i]<-qC[i] + qH[names(qH) == sp[i]]
  }else{
    num[i]<-qC[i]
  }
}

sp.sum<-num
names(sp.sum)<-sp

rank.sp<-sp.sum[order(sp.sum, decreasing = TRUE )]

for (n in 1:length(rank.sp)){
  readline("Press <return to continue") 
  sp1<-har[rownames(har) == names(rank.sp[n]),]
  sp2<-con[rownames(con) == names(rank.sp[n]),]
  sp.counts <- as.matrix(rbind(sp1, sp2))
  barplot(sp.counts, main=paste(names(rank.sp[n]),", n = ", (rowSums(sp1)+rowSums(sp2)), sep=""), xlab="Number of specimens", col=c("darkblue","red"), legend = c("Harvard", "Uconn")) 
}


#Randomly sample from species that have decent number of specimens at HUH and UCONN combined. Ran code 
#until I have 10 rosids and 10 asterids that were appropriate for our study.
df <- merge(har, con, by = "row.names")
dfsub <- subset(df, df$to1900.x + df$to1900.y > 15 & df$to1920.x + df$to1920.y > 15 & df$to1940.x + df$to1940.y > 15 &
                  df$to1960.x + df$to1960.y > 15 & df$to1980.x + df$to1980.y + df$to2000.x + df$to2000.y + df$to2016.x + df$to2016.y > 15)
dfsub$total_1980.2016 <- dfsub$to1980.x + dfsub$to1980.y + dfsub$to2000.x + dfsub$to2000.y + dfsub$to2016.x + dfsub$to2016.y
newdata <- dfsub[order(dfsub$total_1980.2016),] 
newdata <- as.data.frame(newdata)

newdata_t <- data.table(newdata)
newdata_sample <- newdata_t[sample(.N, 35)]

write.csv(newdata_sample, "species_list.csv")



