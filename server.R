library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)
library(maptools)
library(sp)
library(rgeos)
library(plyr)
library(geosphere)
library(extrafont)
loadfonts()
library(stringr)
library(rgdal)
library(grid)
library(gridExtra)

shinyServer(function(input, output, session) {
  
  # Which group
  
  #   output$group <- renderUI({
  #     groupNames <- groupTable[["groupName"]]
  #     opts <- selectInput("gc_name", "Which Group are you looking for:",choices = groupNames, selected=groupNames[2])
  #     list(opts)
  #   })
  
  ### --------------------------------------------------------------------- ###
  # -- Which domain within group
  
  output$reporter_country <- renderUI({
    
    #reporter_countries <- unique(trade_data$Reporter.Countries)
    reporter_countries <- avail_cntry$country
    opts <- selectInput("reporterCountry", tags$h4("Pick a reporter country:"),
                        choices = reporter_countries, selected="Germany")
    list(opts)
  })
  
  
  ### --------------------------------------------------------------------- ###
  # Which item within domain?
  
  output$item <- renderUI({
    
    
    cntry_metadata <- get(paste0("metalist",avail_cntry[avail_cntry$country == input$reporterCountry,]$FAOST_CODE), envir = globalenv())
    
    if (input$dataType == "Import") {

      itemList <- cntry_metadata[[3]]
      
    }
    if (input$dataType == "Export") {
      
      itemList <- cntry_metadata[[4]]
    }
    if (input$dataType == "Both") {
      
      items_exp <- cntry_metadata[[3]]
      items_imp <- cntry_metadata[[4]]
      itemList <- items_exp[items_exp %in% items_imp]
      }
    opts <- selectInput("itemName", tags$h4("Pick an Item:"),
                        choices = itemList, selected = itemList[1])
    list(opts)
  })
  
  
  ### --------------------------------------------------------------------- ###
  ## -- Which element within item
  
  output$element <- renderUI({
    
    values <- c("Value","Quantity")
    opts <- radioButtons("elementName", tags$h4("Pick an Element:"),inline = TRUE,
                        choices = values, selected=values[1])
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###

  
  ### --------------------------------------------------------------------- ###
  ##-- Set the time for export-year 
  
  output$year_data <- renderUI({
    
    cntry_metadata <- get(paste0("metalist",avail_cntry[avail_cntry$country == input$reporterCountry,]$FAOST_CODE), envir = globalenv())
    
    if (input$dataType == "Import") opts <- cntry_metadata[[5]]
    if (input$dataType == "Export") opts <- cntry_metadata[[6]]
    if (input$dataType == "Both") opts <- cntry_metadata[[5]][cntry_metadata[[5]] %in% cntry_metadata[[6]]]

    opts <- sliderInput("yearData", tags$h4("Pick a year"), 
                        min = min(opts), max = max(opts), value = max(opts), step = 1, sep="",
                        animate=animationOptions(interval = 2500, loop = FALSE, playButton = NULL, pauseButton = NULL))
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###
  ## -- Subset trade-matrix data
  
  fao_data_export <- reactive({
    
    #cntry_metadata <- get(paste0("metalist",avail_cntry[avail_cntry$country == "Germany",]$FAOST_CODE), envir = globalenv())
    cntry_metadata <- get(paste0("metalist",avail_cntry[avail_cntry$country == input$reporterCountry,]$FAOST_CODE), envir = globalenv())
    
    df <- cntry_metadata[[7]]
    dat <- df[df$Item == input$itemName, ]

    if (input$elementName == "Value") dat <- dat[dat$Element == "Export Value",]
    if (input$elementName == "Quantity") dat <- dat[dat$Element == "Export Quantity",]
    dat
  })
  
  ### --------------------------------------------------------------------- ###
  ## -- Subset trade-matrix data
  
  fao_data_import <- reactive({
    
    cntry_metadata <- get(paste0("metalist",avail_cntry[avail_cntry$country == input$reporterCountry,]$FAOST_CODE), envir = globalenv())
    
    #load(cntry_metadata[[2]])
    df <- cntry_metadata[[7]]
    dat <- df[df$Item == input$itemName, ]

    if (input$elementName == "Value") dat <- dat[dat$Element == "Import Value",]
    if (input$elementName == "Quantity") dat <- dat[dat$Element == "Import Quantity",]
    dat
  })
  
  
#   output$mytable = renderDataTable({
#     
#     
#     df_export <- fao_data_export()
#     df_export <- df_export[df_export$Year <= input$yearData,]
#     
#     df_import <- fao_data_import()
#     df_import <- df_import[df_import$Year <= input$yearData,]
#     
# #     exp_sum <- df_export %>% 
# #       group_by(Year) %>% 
# #       dplyr::summarise(sum = sum(Value, na.rm = TRUE))
# #     exp_sum$var <- "Total Export"
#     
#     imp_sum <- df_import %>% 
#       group_by(Year) %>% 
#       dplyr::summarise(sum = sum(Value, na.rm = TRUE))
#     imp_sum$var <- "Total Import"
#     
#     #imp_sum[1:5,]
#     #rbind(imp_sum[1:5,],exp_sum[1:5,])
#     #rbind(df_export[1:5,],df_import[1:5,])
#     df_import <- fao_data_import()
#     df_import[1:5,]
#     
#   },options = list(pageLength = 10))
#   
#   
#   ### Limit the partner countries

  
    
  output$limit_partner <- renderUI({
    
    if (input$dataType == "Export" | input$dataType == "Both")    {
      df_export <- fao_data_export()
      partners <- df_export[df_export$Year == input$yearData,]

      partners <- partners %>% 
        group_by(Partner.Countries) %>% 
        dplyr::summarise(sum = sum(Value, na.rm = TRUE)) %>% 
        arrange(-sum)
      part_cntry <- partners$Partner.Countries
    }
    
    if (input$dataType == "Import" | input$dataType == "Both"){
      df_import <- fao_data_import()
      partners <- df_import[df_import$Year == input$yearData,]
      
      partners <- partners %>% 
        group_by(Partner.Countries) %>% 
        dplyr::summarise(sum = sum(Value, na.rm = TRUE)) %>% 
        arrange(-sum)
      part_cntry <- partners$Partner.Countries
    }
    

    opts <- selectizeInput("partnerCountry", tags$h4("Pick partner countries:"),
                        choices = part_cntry, selected= part_cntry[1:20],multiple=TRUE, width="auto")
    list(opts)
   })
  
  

  ### --------------------------------------------------------------------- ###
  ### --------------------------------------------------------------------- ###
  ## --- Outputs
  ### --------------------------------------------------------------------- ###
  ##-- Export Map
  

  

  #output$map <- reactivePlot(function() {
  plotInputMapExport <- reactive({
    
    
    shape2 <- fao_world
    
    cent <- cbind(shape2@data,as.data.frame(gCentroid(shape2,byid=TRUE)))
    cent <- cent[!duplicated(cent$FAO_CODE),]
    
    # fortifying the routes information to create a dataframe; function from ggplot's github site ... thanks to the comments section in AnthroSpace's post
    fortify.SpatialLinesDataFrame = function(model, data, ...) {
      ldply(model@lines, fortify)
    }
    
    
    if (input$dataType == "Export" | input$dataType == "Both")    {
      df_export <- fao_data_export()
      df_export <- df_export[df_export$Year == input$yearData,]
      df_export <- df_export[df_export$Partner.Countries %in% input$partnerCountry,]

      # Export data
      df_export3 <- merge(df_export,cent,by.x="Partner.Country.Code",by.y="FAO_CODE")
      tradepartners <- df_export3[c("Partner.Countries","Value","Item","x","y")]
      # correct country based on codes
      
      fao_code <- avail_cntry[avail_cntry$country == input$reporterCountry,]$FAOST_CODE
      origin <- cent[cent$FAO_CODE == fao_code,][c("x","y")]
      routes = gcIntermediate(origin, tradepartners[,c('x', 'y')], 200, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
      routes <- spTransform(routes, CRS("+proj=robin"))
      partners <- SpatialPointsDataFrame(tradepartners[4:5], tradepartners[1:3], proj4string = CRS("+proj=longlat +ellps=WGS84"))
      partners <- spTransform(partners, CRS("+proj=robin"))
      fortifiedpartners_export <- cbind(coordinates(partners), partners@data)
      greatcircles_export = fortify.SpatialLinesDataFrame(routes) 
      # An id for each country
      tradepartners$id=as.character(c(1:nrow(tradepartners))) 
      # Merge fortified routes with usopencountry information
      greatcircles_export = merge(greatcircles_export, tradepartners, all.x=T, by="id") 
      names(greatcircles_export)[names(greatcircles_export)=="x"] <- "long"
      names(greatcircles_export)[names(greatcircles_export)=="y"] <- "lat"
      names(fortifiedpartners_export)[names(fortifiedpartners_export)=="x"] <- "long"
      names(fortifiedpartners_export)[names(fortifiedpartners_export)=="y"] <- "lat"
    }
    
    if (input$dataType == "Import" | input$dataType == "Both"){
      
      df_import <- fao_data_import()
      df_import <- df_import[df_import$Year == input$yearData,]
      df_import <- df_import[df_import$Partner.Countries %in% input$partnerCountry,]

      # Import data
      df_import3 <- merge(df_import,cent,by.x="Partner.Country.Code",by.y="FAO_CODE")
      tradepartners <- df_import3[c("Partner.Countries","Value","Item","x","y")]
      # correct country based on codes
      fao_code <- avail_cntry[avail_cntry$country == input$reporterCountry,]$FAOST_CODE
      origin <- cent[cent$FAO_CODE == fao_code,][c("x","y")]
      
      routes = gcIntermediate(origin, tradepartners[,c('x', 'y')], 200, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
      routes <- spTransform(routes, CRS("+proj=robin"))
      partners <- SpatialPointsDataFrame(tradepartners[4:5], tradepartners[1:3], proj4string = CRS("+proj=longlat +ellps=WGS84"))
      partners <- spTransform(partners, CRS("+proj=robin"))
      fortifiedpartners_import <- cbind(coordinates(partners), partners@data)
      greatcircles_import = fortify.SpatialLinesDataFrame(routes) 
      # An id for each country
      tradepartners$id=as.character(c(1:nrow(tradepartners))) 
      # Merge fortified routes with usopencountry information
      greatcircles_import = merge(greatcircles_import, tradepartners, all.x=T, by="id") 
      names(greatcircles_import)[names(greatcircles_import)=="x"] <- "long"
      names(greatcircles_import)[names(greatcircles_import)=="y"] <- "lat"
      names(fortifiedpartners_import)[names(fortifiedpartners_import)=="x"] <- "long"
      names(fortifiedpartners_import)[names(fortifiedpartners_import)=="y"] <- "lat"
      
    }

    
    if (input$dataType == "Export") {
      lines_export <- geom_line(data=greatcircles_export,aes(long,lat,group=group, alpha=Value), size=1.5, color="Steel Blue", show_guide = FALSE)
      points_export <- geom_point(data=fortifiedpartners_export, aes(long,lat,size=Value),color="Steel Blue", shape=1)
      names_export <- geom_text(data=fortifiedpartners_export, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=4, alpha=.8)
      title_exp <- "export"
      #colors <- scale_color_manual(values="Steel Blue")
      lines_import <- element_blank()
      points_import <- element_blank()
      names_import <- element_blank()
      title_import <- element_blank()
    }
    if (input$dataType == "Import") {
      lines_export <- element_blank()
      points_export <- element_blank()
      names_export <- element_blank()
      #colors <- scale_color_manual(values="#FF3300")
      lines_import <- geom_line(data=greatcircles_import,aes(long,lat,group=group, alpha=Value), size=1.5, color="#FF3300", show_guide = FALSE)
      points_import <- geom_point(data=fortifiedpartners_import, aes(long,lat,size=Value),color="#FF3300", shape=1)
      names_import <- geom_text(data=fortifiedpartners_import, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=4, alpha=.8)
      title_exp <- "import"
    }
    if (input$dataType == "Both") {

      lines_export <- geom_line(data=greatcircles_export,aes(long,lat,group=group, alpha=Value), size=1.5, color="Steel Blue", show_guide = FALSE)
      points_export <- geom_point(data=fortifiedpartners_export, aes(long,lat,size=Value),color="Steel Blue", shape=1)
      names_export <- geom_text(data=fortifiedpartners_export, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=4, alpha=.8)
      lines_import <- geom_line(data=greatcircles_import,aes(long,lat,group=group, alpha=Value), size=1.5, color="#FF3300", show_guide = FALSE)
      points_import <- geom_point(data=fortifiedpartners_import, aes(long,lat,size=Value),color="#FF3300", shape=1)
      names_import <- geom_text(data=fortifiedpartners_import, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=4, alpha=.8)
      title_exp <- "exports and imports"
      #colors <- scale_color_manual(values=c("Steel Blue","#FF3300"))

    }
    
    if (input$elementName == "Quantity") legend_title <- "Quantity in tonnes"
    if (input$elementName == "Value") legend_title <- "Value in 1000 US$"
    

        
    year <- input$yearData
    
    p <- bgmap
    p <- p + lines_export + points_export + names_export
    p <- p + lines_import + points_import + names_import
    # exports
#     p <- p + geom_line(data=greatcircles_export,aes(long,lat,group=group, alpha=Value), color="#FF3300", size=.5)
#     p <- p + geom_point(data=fortifiedpartners_export, aes(long,lat,size=Value),color="#FF3300", shape=1)
#     p <- p + geom_text(data=fortifiedpartners_export, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=2, alpha=.6)
#     # imports
#     p <- p + geom_line(data=greatcircles_import,aes(long,lat,group=group, alpha=Value), color="Steel Blue", size=.5)
#     p <- p + geom_point(data=fortifiedpartners_import, aes(long,lat,size=Value),color="Steel Blue", shape=1)
#     p <- p + geom_text(data=fortifiedpartners_import, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=2, alpha=.6)
    # 
    p <- p + scale_size(name = legend_title, range = c(1,6))
    p <- p + coord_equal()
    #p <- p + colors
    p <- p + labs(title = paste(input$reporterCountry,"-",title_exp,"of",input$itemName,"in",year),
                  x=NULL, 
                  y=NULL)
    p <- p + guides(color = guide_legend(title = ""))
    p <- p + theme(legend.position = c(0.20,0.20),
                   legend.justification=c(0,0),
                   legend.key.size=unit(5,'mm'),
                   legend.direction = "vertical",
                   legend.background=element_rect(colour=NA, fill=NA),
                   #text = element_text(family = "Open Sans"),
                   legend.text=element_text(size=11),
                   legend.key = element_blank(),
                   legend.title=element_text(size=11, face="bold"),
                   title=element_text(size=14, color="Dim Grey"),
                   panel.grid.minor=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.background = element_blank(),
                   plot.background = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm")#,
                   #text=element_text(family = "Open Sans")
                   )
    #p <- p + theme(legend.position = "none")
    #p <- p + annotate("text", x = Inf, y = -Inf, label = paste0("Year ",year),hjust=2, vjust=-4.5, col="#5087CE", cex=8,fontface = "bold", alpha = 0.4)
    #p <- p + annotate("text", x = Inf, y = -Inf, label = "*countries with less than 50 tonnes excluded", hjust=1.5, vjust=-1.0, col="Dim Grey", cex=3,fontface = "bold", alpha = 0.6)
    p
    
    

  })
  
  
  output$export_map <- renderPlot(function(){
    plotInputMapExport()
  })
  
  addPopover(session, 
             "export_map", 
             "About the map", 
             content = paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis ornare nunc arcu, vitae consequat sapien sollicitudin vel. Donec ",
                               "gravida eros ac ante posuere interdum. Mauris vehicula elementum dolor, ut eleifend ex accumsan et. Etiam sed",
                               "condimentum neque, et suscipit orci. Nunc efficitur tempor suscipit. Donec volutpat tincidunt lacinia. Praesent vel sollicitudin.",
                              "augue. Cras sit amet diam in arcu scelerisque imperdiet et at mi. Donec rutrum lacinia est, et facilisis odio vehicula quis. ",
                              "Aliquam erat volutpat. Phasellus hendrerit efficitur ligula, eget luctus enim. In eget dignissim ipsum. Cras rhoncus ligula enim, ",
                              "sed vehicula ligula pretium eu. Ut eget efficitur turpis. "), 
              trigger = 'click', placement="top")
  
  addPopover(session, 
             "export_timeseries", 
             "About the time-series", 
             content = paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis ornare nunc arcu, vitae consequat sapien sollicitudin vel. Donec ",
                              "gravida eros ac ante posuere interdum. Mauris vehicula elementum dolor, ut eleifend ex accumsan et. Etiam sed",
                              "condimentum neque, et suscipit orci. Nunc efficitur tempor suscipit. Donec volutpat tincidunt lacinia. Praesent vel sollicitudin.",
                              "augue. Cras sit amet diam in arcu scelerisque imperdiet et at mi. Donec rutrum lacinia est, et facilisis odio vehicula quis. ",
                              "Aliquam erat volutpat. Phasellus hendrerit efficitur ligula, eget luctus enim. In eget dignissim ipsum. Cras rhoncus ligula enim, ",
                              "sed vehicula ligula pretium eu. Ut eget efficitur turpis. "), 
             trigger = 'click', placement="top")
  
  
  addPopover(session, 
             "export_Barchart", 
             "About the barchart", 
             content = paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis ornare nunc arcu, vitae consequat sapien sollicitudin vel. Donec ",
                              "gravida eros ac ante posuere interdum. Mauris vehicula elementum dolor, ut eleifend ex accumsan et. Etiam sed",
                              "condimentum neque, et suscipit orci. Nunc efficitur tempor suscipit. Donec volutpat tincidunt lacinia. Praesent vel sollicitudin.",
                              "augue. Cras sit amet diam in arcu scelerisque imperdiet et at mi. Donec rutrum lacinia est, et facilisis odio vehicula quis. ",
                              "Aliquam erat volutpat. Phasellus hendrerit efficitur ligula, eget luctus enim. In eget dignissim ipsum. Cras rhoncus ligula enim, ",
                              "sed vehicula ligula pretium eu. Ut eget efficitur turpis. "), 
             trigger = 'click', placement="top")
  

  output$dlMap <- downloadHandler(
      filename = 'map.pdf',
      content = function(file) {
        device <- function(..., width, height) grDevices::pdf(..., width = 11.7, height = 8.3)
#       filename = 'map.png',
#       content = function(file) {
#       device <- function(..., width, height) grDevices::png(..., width = 1200, height = 750) 
      ggsave(file, plot = plotInputMapExport(), device = device)
    }
  )
  
  
  
  
  

  plotInputTimeseries <- reactive({
    
    

    if (input$dataType == "Export") {
      
      # Because fao_data_import() subset also the item, and we want them all
      cntry_metadata <- get(paste0("metalist",avail_cntry[avail_cntry$country == input$reporterCountry,]$FAOST_CODE), envir = globalenv())
      
      #load(cntry_metadata[[2]])
      df <- cntry_metadata[[7]]
      dat <- df[df$Item == input$itemName, ]
      
      if (input$elementName == "Value") dat <- dat[dat$Element == "Import Value",]
      if (input$elementName == "Quantity") dat <- dat[dat$Element == "Import Quantity",]
      dat
      
      
      df_export <- fao_data_export()
      
      df_export <- df_export[df_export$Partner.Countries %in% input$partnerCountry,]
      
      minYear <- min(df_export$Year)
      maxYear <- max(df_export$Year)
      
      lines_export <- geom_line(data=df_export,aes(x=Year,y=Value,group=Partner.Countries,color="Export"), alpha=.4)
      points_export <- geom_point(data=df_export,aes(x=Year,y=Value,group=Partner.Countries), color="Steel Blue", size=2, show_guide = FALSE, alpha=.4)
      names_export <- geom_text(data=merge(df_export, aggregate(Year ~ Partner.Countries, df_export, max), by=c("Year","Partner.Countries")), aes(x=Year, y = Value, label=Partner.Countries), hjust=-0.1,vjust=-1,size=4, alpha=.4, color="Steel Blue")
      title_exp <- "export"
      colors <- scale_color_manual(values="Steel Blue")
      lines_import <- element_blank()
      points_import <- element_blank()
      names_import <- element_blank()
      title_import <- element_blank()
    }
    if (input$dataType == "Import") {
      df_import <- fao_data_import()
      
      df_import <- df_import[df_import$Partner.Countries %in% input$partnerCountry,]
      
      
      minYear <- min(df_import$Year)
      maxYear <- max(df_import$Year)
      
      lines_export <- element_blank()
      points_export <- element_blank()
      names_export <- element_blank()
      colors <- scale_color_manual(values="#FF3300")
      lines_import <- geom_line(data=df_import,aes(x=Year,y=Value,group=Partner.Countries,color="Import"), alpha=.4)
      points_import <- geom_point(data=df_import,aes(x=Year,y=Value,group=Partner.Countries), color="#FF3300", size=2, show_guide = FALSE, alpha=.4)
      names_import <- geom_text(data=merge(df_import, aggregate(Year ~ Partner.Countries, df_import, max), by=c("Year","Partner.Countries")), aes(x=Year, y = Value, label=Partner.Countries), hjust=-0.1,vjust=-.5,size=4, alpha=.4,color="#FF3300")
      title_exp <- "import"
    }
    if (input$dataType == "Both") {
      df_export <- fao_data_export()
      df_import <- fao_data_import()
      
      df_export <- df_export[df_export$Partner.Countries %in% input$partnerCountry,]
      df_import <- df_import[df_import$Partner.Countries %in% input$partnerCountry,]
      
      minYear <- min(df_export$Year)
      maxYear <- max(df_export$Year)
      
    lines_export <- geom_line(data=df_export,aes(x=Year,y=Value,group=Partner.Countries,color="Export"), alpha=.4)
    points_export <- geom_point(data=df_export,aes(x=Year,y=Value,group=Partner.Countries), color="Steel Blue", size=2, show_guide = FALSE, alpha=.4)
    names_export <- geom_text(data=merge(df_export, aggregate(Year ~ Partner.Countries, df_export, max), by=c("Year","Partner.Countries")), aes(x=Year, y = Value, label=Partner.Countries), hjust=-0.1,vjust=-1,size=4, alpha=.4, color="Steel Blue")
    lines_import <- geom_line(data=df_import,aes(x=Year,y=Value,group=Partner.Countries,color="Import"), alpha=.4)
    points_import <- geom_point(data=df_import,aes(x=Year,y=Value,group=Partner.Countries), color="#FF3300", size=2, show_guide = FALSE, alpha=.4)
    names_import <- geom_text(data=merge(df_import, aggregate(Year ~ Partner.Countries, df_import, max), by=c("Year","Partner.Countries")), aes(x=Year, y = Value, label=Partner.Countries), hjust=-0.1,vjust=-.5,size=4, alpha=.4,color="#FF3300")
      title_exp <- "exports and imports"
      colors <- scale_color_manual(values=c("Steel Blue","#FF3300"))
      
      
    }
    
    if (input$elementName == "Quantity") legend_title <- "Quantity in tonnes"
    if (input$elementName == "Value") legend_title <- "Value in 1000 US$"
    
    
    
    #year <- input$yearData
    
    p <- ggplot()
    p <- p + lines_export + points_export + names_export
    p <- p + lines_import + points_import + names_import
    #p <- p + geom_vline(xintercept=input$yearData, color="Dim Grey", alpha=.4, linetype="dashed")
    p <- p + coord_cartesian(xlim=c(minYear,maxYear+3))
    p <- p + scale_x_continuous(breaks=minYear:maxYear)
    p <- p + colors
#     p <- p + coord_equal()
    p <- p + labs(title = paste(input$reporterCountry,"-",title_exp,"of",input$itemName),
                  x=NULL, 
                  y=legend_title)
    p <- p + theme(legend.position = "top",
                   legend.justification=c(0,0),
                   legend.key.size=unit(5,'mm'),
                   legend.key = element_blank(),
                   legend.direction = "horizontal",
                   legend.background=element_rect(colour=NA, fill=NA),
                   #text = element_text(family = "Open Sans"),
                   legend.text=element_text(size=11),
                   legend.title=element_blank(),
                   title=element_text(size=14, color="Dim Grey"),
                   #panel.grid.minor=element_blank(),
                   #panel.grid.major=element_blank(),
                   panel.background = element_blank(),
                   plot.background = element_blank()
                   #axis.text = element_blank(),
                   #axis.ticks = element_blank()#,
                   #plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm")#,
                   #text=element_text(family = "Open Sans")
    )
    #p <- p + theme(legend.position = "none")
    #p <- p + annotate("text", x = Inf, y = -Inf, label = paste0("Year ",year),hjust=2, vjust=-4.5, col="#5087CE", cex=8,fontface = "bold", alpha = 0.4)
    #p <- p + annotate("text", x = Inf, y = -Inf, label = "*countries with less than 50 tonnes excluded", hjust=1.5, vjust=-1.0, col="Dim Grey", cex=3,fontface = "bold", alpha = 0.6)
    p
    
    
  })
  
  
  output$export_timeseries <- renderPlot(function(){
    plotInputTimeseries()
  })
  
  output$dlTimeseries <- downloadHandler(
    filename = 'timeseries_by_country.pdf',
    content = function(file) {
      device <- function(..., width, height) grDevices::pdf(..., width = 11.7, height = 8.3)
#     filename = 'timeseries_by_country.png',
#     content = function(file) {
#       device <- function(..., width, height) grDevices::png(..., width = 1200, height = 750)
      ggsave(file, plot = plotInputTimeseries(), device = device)
    }
  )
  
  
  ### --------------------------------------------------------------------- ###
  ## -- Subset trade-matrix data
  
  fao_data_export_bar <- reactive({
    
    cntry_metadata <- get(paste0("metalist",avail_cntry[avail_cntry$country == input$reporterCountry,]$FAOST_CODE), envir = globalenv())
    
    df <- cntry_metadata[[7]]
    dat <- df[df$Year == input$yearData,]
    
    if (input$elementName == "Value") dat <- dat[dat$Element == "Export Value",]
    if (input$elementName == "Quantity") dat <- dat[dat$Element == "Export Quantity",]
    dat
  })
  
  ### --------------------------------------------------------------------- ###
  ## -- Subset trade-matrix data
  
  fao_data_import_bar <- reactive({
    
    cntry_metadata <- get(paste0("metalist",avail_cntry[avail_cntry$country == input$reporterCountry,]$FAOST_CODE), envir = globalenv())
    
    df <- cntry_metadata[[7]]
    dat <- df[df$Year == input$yearData,]
    
    if (input$elementName == "Value") dat <- dat[dat$Element == "Import Value",]
    if (input$elementName == "Quantity") dat <- dat[dat$Element == "Import Quantity",]
    dat
  })
  
  
  
  ### Barchart
  
  plotInputBarchart <- reactive({
    
    
    
    if (input$dataType == "Import" | input$dataType == "Both") {
      
      df_import <- fao_data_import_bar()
      
      #barData <- dataa[dataa$Element %in% c("Import Quantity","Import Value"),]
      bars_i <- df_import %>% 
        #filter(Element == "Import Value") %>% 
        group_by(Item) %>% 
        dplyr::summarise(sum = sum(Value)) %>% 
        arrange(sum)
      bars_i$Item <- factor(bars_i$Item, levels = bars_i$Item)
      bars_i$var <- "Import items"
      bars <- bars_i
      
      title_exp <- "import items"
      fill_order <- scale_fill_manual(values=c("#FF3300"))
    }
    if (input$dataType == "Export" | input$dataType == "Both") {
      
      df_export <- fao_data_export_bar()
    
      bars_e <- df_export %>% 
        group_by(Item) %>% 
        dplyr::summarise(sum = sum(Value)) %>% 
        arrange(sum)
      bars_e$Item <- factor(bars_e$Item, levels = bars_e$Item)
      bars_e$var <- "Export items"
      bars <- bars_e
      
      title_exp <- "export items"
      fill_order <- scale_fill_manual(values=c("#5087CE"))
    }
    if (input$dataType == "Both") {
      
      bars <- rbind(bars_i,bars_e)
      
      fill_order <- scale_fill_manual(values=c("#5087CE","#FF3300"))
      title_exp <- "export and import items"
    }
    

    
    if (input$elementName == "Quantity") legend_title <- "Quantity in tonnes"
    if (input$elementName == "Value") legend_title <- "Value in 1000 US$"
    
    p <- ggplot(bars, aes(x=Item,y=sum,fill=var))
    p <- p + geom_bar(stat="identity", alpha=.60, position="dodge")
    p <- p + fill_order
    p <- p + coord_flip()
    p <- p + labs(title = paste(input$reporterCountry,"top 30",title_exp,"in",input$yearData),
                   x=NULL, 
                   y=legend_title)
    p <- p + theme(legend.position = "top",
                   legend.justification=c(0,0),
                   legend.key.size=unit(5,'mm'),
                   legend.key = element_blank(),
                   legend.direction = "horizontal",
                   legend.background=element_rect(colour=NA, fill=NA),
                   #text = element_text(family = "Open Sans"),
                   legend.text=element_text(size=11),
                   legend.title=element_blank(),
                   title=element_text(size=14, color="Dim Grey"),
                   #panel.grid.minor=element_blank(),
                   #panel.grid.major=element_blank(),
                   panel.background = element_blank(),
                   plot.background = element_blank()
                   #axis.text = element_blank(),
                   #axis.ticks = element_blank()#,
                   #plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm")#,
                   #text=element_text(family = "Open Sans")
    )
    #p <- p + facet_wrap(~var, scales = "free")
    p
    
    
  })

  output$export_Barchart <- reactivePlot(function(){
    plotInputBarchart()
  })
  
  output$dlBarchart <- downloadHandler(
    filename = 'top_30_items.pdf',
    content = function(file) {
      device <- function(..., width, height) grDevices::pdf(..., width = 11.7, height = 8.3)
#     filename = 'top_30_items.png',
#     content = function(file) {
#       device <- function(..., width, height) grDevices::png(..., width = 1200, height = 750)
    ggsave(file, plot = plotInputBarchart(), device = device)
    }
  )
    
  
  
})