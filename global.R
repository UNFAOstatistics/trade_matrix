# Select countries for the preview - top 5 from each region based on import value on 2012
# see file country_relection.Rmd

# library(readr)
# library(dplyr)
# library(stringr)
# # 
# # 
# # Read trade-matrix data by country
# datasets <- list.files("../shiny_app_raw_data/trade_matrix_preview/", full.names = T)
# # Remove the path
# datasets_nopaths <- str_replace_all(datasets, "../shiny_app_raw_data/trade_matrix_preview//", "")
# filenames <- as.numeric(str_replace_all(datasets_nopaths, ".csv", ""))
# 
# avail_cntry <- data.frame()
# meta_list <- list()
# for (i in 1:length(datasets)) {
#   df <- read_csv(datasets[i])
#   # Subset data to include only 20 most important items and 20 most important partners in year 2012
#   im <- df[df$Element %in% c("Import Value","Import Quantity"),]
#   ex <- df[df$Element %in% c("Export Value","Export Quantity"),]
#   import_max <- max(im$Year)
#   import_min <- min(im$Year)
#   export_max <- max(ex$Year)
#   export_min <- min(ex$Year)
#   im_ss <- im[im$Year == import_max & im$Element == "Import Value",]
#   ex_ss <- ex[ex$Year == export_max & ex$Element == "Export Value",]
#   # Most valuable Items
#   im_items <- im_ss %>% group_by(Item) %>% 
#     dplyr::summarise(sum = sum(Value)) %>% 
#     arrange(-sum)
#   ex_items <- ex_ss %>% group_by(Item) %>% 
#     dplyr::summarise(sum = sum(Value)) %>% 
#     arrange(-sum)
#   ims <- im_items$Item[1:20]
#   exs <- ex_items$Item[1:20]
#   df <- df[df$Item %in% c(ims,exs),]
#   names(df) <- str_replace_all(names(df), " ", ".")
#   #save(df,file=paste0("datasets/",filenames[i],".RData"))
#   smr <- data.frame(FAOST_CODE = filenames[i],
#                     country = unique(df$Reporter.Countries),
#                     location = paste0("datasets/",filenames[i],".RData"),
#                     stringsAsFactors = FALSE)
#   avail_cntry <- rbind(avail_cntry,smr)
# 
#   # Create the metalist
#   country <- unique(df$Reporter.Countries)
#   location <- paste0("datasets/",filenames[i],".RData")
#   import_item_list <- ims
#   export_item_list <- exs
#   import_range <- import_min:import_max
#   export_range <- export_min:export_max
#   data <- df
#   meta_list <- list(country,location,import_item_list,export_item_list,import_range,export_range,data)
#   assign(paste0("metalist",filenames[i]),meta_list)
# }
# metalists <- apropos("^metalist")
# rm(list=setdiff(ls(), c("avail_cntry",metalists)))
# save.image(file="metadata.RData")
load("metadata.RData")

library(stringr)

# 
# 
# # # Doanload data from FAOSTAT
# library(FAOSTAT)
# library(dplyr)
# metdat <- data.frame(domainCode  = c("QC","QC","OA"),
#                      elementCode = c(5510,5510,511),
#                      itemCode    = c(486,1717,3010),
#                      start_year = c(1995,1995,1995),
#                      end_year = c(2012,2012,2012),
#                      indicator_name = c("banana.production",
#                                         "cereal.production",
#                                         "total.population"),
#                      stringsAsFactors = FALSE
# )
# # # Population data
# i <- 3
# dat <- getFAOtoSYB(domainCode = metdat[i,"domainCode"], 
#                    elementCode = metdat[i,"elementCode"],
#                    itemCode = metdat[i,"itemCode"],
#                    yearRange = metdat[i,"start_year"]:metdat[i,"end_year"])
# data <- dat[["entity"]]
# names(data) <- c("FAOST_CODE","Year",metdat[i,"indicator_name"])
# # 
# for (i in 1:2){
#   dat <- getFAOtoSYB(domainCode = metdat[i,"domainCode"], 
#                      elementCode = metdat[i,"elementCode"],
#                      itemCode = metdat[i,"itemCode"],
#                      yearRange = metdat[i,"start_year"]:metdat[i,"end_year"])
#   dat <- dat[["entity"]]
#   names(dat) <- c("FAOST_CODE","Year",metdat[i,"indicator_name"])
#   data <- merge(data,dat,by=c("FAOST_CODE","Year"))
# }
# 
# library(countrycode)
# data$Country <- countrycode(data$FAOST_CODE, "fao", "country.name")
# data <- data[!is.na(data$banana.h3("Buttons"),
#                     actionButton("action", label = "Action"),
#                     br(),
#                     br(), 
#                     submitButton("Submit"))production),]
# data <- data[!is.na(data$cereal.production),]
# data <- data[!is.na(data$total.population),]
# # Manual adds
# data$Country[data$FAOST_CODE == 41] <- "China"
# 
# # Lets take the region from gisfao shapefiles & the shapefiles
#library(gisfao)

# save(fao_world, file="fao_world.RData")
# save(fao_world_centroids, file="fao_world_centroids.RData")
# save(graticule, file="graticule.RData")

load("fao_world.RData")




#shape <- spTransform(fao_world, CRS("+proj=robin"))
#save(shape, file="shape.RData")
#grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
#save(grat_robin, file="grat_robin.RData")
load("grat_robin.RData")
load("shape.RData")

# Create background map here once to improve the speed!
library(ggplot2)
shape$id <- rownames(shape@data)
map.points <- fortify(shape, region = "id")
map.df <- merge(map.points, shape, by = "id")

# Create the background map
bgmap <- ggplot()
bgmap <- bgmap + geom_polygon(data=map.df,aes(long,lat,group=group), fill="#5087CE", color="white", size=.5, alpha=.25)

load("fao_world_centroids.RData")
load("graticule.RData")

# source("plot_map_gif.R")
# source("plot_line_gif.R")


# reg <- fao_world@data
# data2 <- reg[c("FAO_CODE","RAF","LAC","RAP","REU","RNE","ADM0_NAME")]
# data2$Region[data2$RAF == TRUE] <- "Africa"
# data2$Region[data2$RNE == TRUE] <- "Near East and North Africa"
# data2$Region[data2$LAC == TRUE] <- "Latin America and the Caribbean"
# data2$Region[data2$RAP == TRUE] <- "Asia and the Pacific"
# data2$Region[data2$REU == TRUE] <- "Europe and Central Asia"
# data2$Region[is.na(data2$Region)] <- "Other regions"
# 
# names(data2)[names(data2)=="FAO_CODE"] <- "FAOST_CODE"
# names(data2)[names(data2)=="ADM0_NAME"] <- "Country"
# 
# reg <- data2
# 
# save(reg, file="reg.RData")
load("reg.RData")

# data$Region <- as.factor(data$Region)
# data$Country <- as.factor(data$Country)
# data$Year <- as.numeric(data$Year)


# data3 <- readRDS("healthexp.Rds")
# data3$Region <- as.factor(data3$Region)
# h(data3)

#library(FAOSTAT)
#save(FAOmetaTable, file="FAOmetaTable.RData")
load("FAOmetaTable.RData")
#save(FAOcountryProfile, file="FAOcountryProfile.RData")
load("FAOcountryProfile.RData")

# 
# str(FAOmetaTable)
# 
# groupTable <- FAOmetaTable[[1]]
# domainTable <- FAOmetaTable[[2]]
# itemTable <- FAOmetaTable[[3]]
# itemAggTable <- FAOmetaTable[[4]]
# elementTable <- FAOmetaTable[[5]]
# 
# ### getFAO
# 
# getFAO = function(name = NULL, domainCode = "RL", elementCode = 5110,
#                   itemCode = 6621, query, printURL = FALSE, productionDB = FALSE,
#                   useCHMT = TRUE, outputFormat = "wide", returnNames = FALSE,
#                   returnFlags = FALSE, yearRange = NULL, countrySet = NULL){
#   
#   ## Year range
#   if (!is.null(yearRange)) {
#     if (!is.numeric(yearRange)) {
#       stop("Please, provide a numeric vector for the year range.")
#     } else {
#       yearRange = paste(yearRange, collapse = ":")
#     }
#   }
#   ## Country set
#   if (!is.null(countrySet)) {
#     if (!is.numeric(countrySet)) {
#       stop("Please, provide a numeric vector for the year range.")
#     } else {
#       countrySet = paste(countrySet, collapse = ":")
#     }
#   }
#   ## Query
#   if(!missing(query)){
#     if(NROW(query) > 1)
#       stop("Use 'getFAOtoSYB' for batch download")
#     domainCode = query$domainCode
#     itemCode = query$itemCode
#     elementCode = query$elementCode
#     if(is.null(query$name)){
#       name = with(query, paste(domainCode, itemCode, elementCode, sep = "_"))
#     } else {
#       name = query$name
#     }
#   }
#   ## Name
#   if(is.null(name))
#     name = paste(domainCode, itemCode, elementCode, sep = "_")
#   
#   if(productionDB){
#     ## Base
#     #         base = "http://ldvapp07.fao.org:8030/wds/api?"
#     #         base = "http://lprapp16.fao.org:4012/wds/api?"
#     #         base = "http://lprapp16.fao.org/wds/api?"
#     base = "http://ldvapp07.fao.org:8032/wds/api?"
#     ## Database
#     database = "db=faostatproddiss&"
#     ## Selection
#     selection = "select=D.AreaCode[FAOST_CODE],D.Year[Year],D.Value[Value]"
#     from = "&from=data[D],element[E]&"
#     condition = 
#       paste0("where=D.elementcode(", elementCode, "),D.itemcode(",
#              itemCode, "),D.domaincode('", domainCode, "')")
#     if (!is.null(yearRange)) {
#       condition = paste0(condition, ",D.year(", yearRange, ")")
#     }
#     if (!is.null(countrySet)) {
#       condition = paste0(condition, ",A.AreaCode(", countrySet, ")")
#     }
#     join = ",JOIN(D.elementcode:E.elementcode)&orderby=E.elementnamee,D.year"
#   } else {
#     base = c("http://fenix.fao.org/wds/api?", "http://faostat3.fao.org/wds/api?", "http://fenixapps.fao.org/wds/api?")
#     database = "db=faostat2&"
#     selection = "select=A.AreaCode[FAOST_CODE],D.year[Year],D.value[Value]"
#     from = "&from=data[D],element[E],item[I],area[A]&"
#     condition = paste0("where=D.elementcode(", elementCode, "),D.itemcode(",
#                        itemCode, "),D.domaincode('", domainCode, "')")
#     if (!is.null(yearRange)) {
#       condition = paste0(condition, ",D.year(", yearRange, ")")
#     }
#     if (!is.null(countrySet)) {
#       condition = paste0(condition, ",A.AreaCode(", countrySet, ")")
#     }
#     join = ",JOIN(D.elementcode:E.elementcode),JOIN(D.itemcode:I.itemcode),JOIN(D.areacode:A.areacode)&orderby=E.elementnamee,D.year"
#   }
#   ## Flags
#   if(returnFlags){
#     outputFormat = "long"
#     selection = paste0(selection, ",D.Flag[Flags]")
#   }
#   ## Names
#   if(returnNames)
#     selection = paste0(selection, "A.AreaNameE[AreaName],E.elementnamee[ElementName],I.itemnamee[ItemName]")
#   ## Call
#   out = "out=csv&"
#   url = paste0(base, out, database, selection, from, condition, join)
#   if(printURL)
#     print(url)
#   
#   ## Allowing multiple server if any failed.
#   for(i in 1:length(url)){
#     faoData = suppressWarnings(try(read.csv(file = url[i],
#                                             stringsAsFactors = FALSE), silent = TRUE))
#     if(!inherits(faoData, "try-error"))
#       break
#   }
#   faoData$FAOST_CODE = as.integer(faoData$FAOST_CODE)
#   faoData$Year = as.integer(faoData$Year)
#   ## CHMT
#   if(useCHMT)
#     faoData = CHMT(var = "Value", data = faoData, year = "Year")
#   ## Output format
#   if(outputFormat == "long" & NROW(faoData) != 0){
#     faoData$domainCode = domainCode
#     faoData$itemCode = itemCode
#     faoData$elementCode = elementCode
#     faoData$name = name
#     faoData$Value <- as.numeric(gsub("n.a.", "", faoData$Value))
#   } else if(outputFormat == "wide"){
#     colnames(faoData)[colnames(faoData) == "Value"] = name
#     faoData[, name] <- as.numeric(gsub("n.a.", "", faoData[, name]))
#   }
#   faoData
# }
# 
# ### getFAOtoSYB
# 
# getFAOtoSYB = function(name = NULL, domainCode = "RL",
#                        elementCode = 5110, itemCode = 6621, query, printURL = FALSE,
#                        productionDB = FALSE, useCHMT = TRUE, yearRange = NULL, countrySet = NULL,
#                        outputFormat = c("wide", "long"), returnFlags = FALSE){
#   outputFormat = match.arg(outputFormat)
#   if(returnFlags)
#     outputFormat = "long"
#   
#   if(!missing(query)){
#     domainCode = query$domainCode
#     itemCode = query$itemCode
#     elementCode = query$elementCode
#     if(is.null(query$name)){
#       name = with(query, paste(domainCode, itemCode, elementCode, sep = "_"))
#     } else {
#       name = query$name
#     }
#   }
#   
#   if(is.null(name))
#     name = paste(domainCode, itemCode, elementCode, sep = "_")
#   n = length(name)
#   if(any(length(domainCode) != n, length(elementCode) != n,
#          length(itemCode) != n))
#     stop("length of inputs are not all the same, check the number of names")
#   
#   faoData = data.frame(FAOST_CODE = integer(),
#                        Year = integer(), stringsAsFactors = FALSE)
#   results = data.frame(Name = name, Success = logical(length(name)),
#                        Reason = character(length(name)),
#                        Time = as.POSIXct(rep(NA, length(name))),
#                        stringsAsFactors = FALSE)
#   printLab(paste("FAOSTAT Data Download (", n, " in Total)", sep = ""))
#   
#   i = 1
#   retry = 1
#   while(i <= n){
#     if(retry == 1)
#       cat(paste("(", i, "): Downloading variable ", name[i], " ... ",
#                 sep = ""))
#     if(any(is.na(domainCode[i]), is.na(elementCode[i]), is.na(itemCode)[i])){
#       cat("FAIL\n\t Error: One of Domain, Element or Item code is missing\n")
#       results[i, "Success"] = FALSE
#       results[i, "Reason"] = "One of Domain, Element or Item code is missing"
#     } else {
#       tmp = try(getFAO(name = name[i],
#                        domainCode = domainCode[i],
#                        elementCode = elementCode[i],
#                        itemCode = itemCode[i], printURL = printURL,
#                        productionDB = productionDB,
#                        useCHMT = useCHMT, outputFormat = outputFormat,
#                        returnFlags = returnFlags,
#                        yearRange = yearRange,
#                        countrySet = countrySet))
#       if(!inherits(tmp, "try-error")){
#         ## This was to account sometimes the download is successful, yet
#         ## the data frame is empty
#         if(NROW(tmp) != 0){
#           cat("OK\n")
#           results[i, "Success"] = TRUE
#           results[i, "Reason"] = "Download Successful"
#           results[i, "Time"] = Sys.time()
#           if(outputFormat == "wide"){
#             faoData = merge(x = faoData, y = tmp, all = TRUE,
#                             by = c("FAOST_CODE", "Year"))
#           } else if(outputFormat == "long"){
#             faoData = rbind(faoData, tmp)
#           }
#           i = i + 1
#           retry = 1
#         } else {
#           tmp = c("The specified query has no data, consult FAOSTAT")
#           cat(paste(tmp, "\n"))
#           class(tmp) = "try-error"
#           attr(tmp, "condition") =
#             list(message = tmp, call = NULL)
#           i = i + 1
#           retry = 1
#         }
#       } else {
#         if(retry <=50){
#           print(retry)
#           retry = retry + 1
#         } else {
#           cat("Download fail after 50 tries\n")
#           results[i, "Success"] = FALSE
#           results[i, "Reason"] = attr(tmp, "condition")$message
#           i = i + 1
#           retry = 1
#         }
#       }
#     }
#   }
#   entity.df = arrange(with(faoData, faoData[FAOST_CODE %in%
#                                               FAOcountryProfile[, "FAOST_CODE"], ]), FAOST_CODE, Year)
#   region.df = arrange(with(faoData, faoData[!(FAOST_CODE %in%
#                                                 FAOcountryProfile[, "FAOST_CODE"]), ]), FAOST_CODE, Year)
#   cat(paste("\n Number of variables successfully downloaded: ",
#             sum(results$Success), " out of ", NROW(results), "\n\n", sep = ""))
#   list(entity = entity.df, aggregates = region.df, results = results)
# }
# 
# ### printLab 
# 
# printLab = function(label, span = FALSE, width = getOption("width")){
#   nc = nchar(label)
#   sides = (width - nc)/2 - 3
#   if(span){
#     pre = paste(c("\n\n", rep("-", width), "\n"), collapse = "")
#     post = paste(c("\n", rep("-", width), "\n\n"), collapse = "")
#   } else {
#     pre = paste(c("\n\n", rep(" ", sides), rep("-", nc + 6),
#                   rep(" ", sides), "\n"), collapse = "")
#     post = paste(c("\n", rep(" ", sides), rep("-", nc + 6),
#                    rep(" ", sides), "\n\n"), collapse = "")
#   }
#   sandwich = paste(c(rep(" ", sides), "** ", label, " **",
#                      rep(" ", sides)), collapse = "")
#   cat(paste(pre, sandwich, post, sep = ""))
# }
# 
# 
# 
# categories <- function(x, cat=5) {
#   
#   library(stringr)
#   levs <- as.data.frame(as.character(levels(cut_number(x, cat))))
#   names(levs) <- "orig"
#   levs$mod <- str_replace_all(levs$orig, "\\[", "")
#   levs$mod <- str_replace_all(levs$mod, "\\]", "")
#   levs$mod <- str_replace_all(levs$mod, "\\(", "")
#   levs$lower <- gsub(",.*$","", levs$mod)
#   levs$upper <- gsub(".*,","", levs$mod)
#   
#   levs$lower <- factor(levs$lower)
#   levs$lower <- round(as.numeric(levels(levs$lower))[levs$lower],0)
#   
#   levs$upper <- factor(levs$upper)
#   levs$upper <- round(as.numeric(levels(levs$upper))[levs$upper],0)
#   
#   levs$labs <- paste(levs$lower,levs$upper, sep=" - ")
#   
#   labs <- as.character(c(levs$labs))
#   y <- cut_number(x, cat, right = FALSE, labels = labs)
#   y <- as.character(y)
#   y[is.na(y)] <- "No Data"
#   y <- factor(y, levels=c("No Data",labs[1:cat]))
# }
# 
# 
# trade_palette <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")
# 
