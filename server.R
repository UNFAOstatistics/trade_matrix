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
    
    reporter_countries <- unique(trade_data$Reporter.Countries)
    opts <- selectInput("reporterCountry", tags$p("Pick a reporter country:"),
                        choices = reporter_countries, selected=reporter_countries[1])
    list(opts)
  })
  
  
  ### --------------------------------------------------------------------- ###
  # Which item within domain?
  
  output$item <- renderUI({
    
    dataa <- trade_data[trade_data$Reporter.Countries == input$reporterCountry, ]
    
    #dataa <- trade_data[trade_data$Reporter.Countries == "Italy", ]
    dataa <- dataa[dataa$Year == max(dataa$Year),]
    itemOrder <- dataa %>% 
      group_by(Item) %>% 
      dplyr::summarise(n = n())
    itemOrder <- arrange(itemOrder, -n)
    topItems <- itemOrder$Item
    allItems <- sort(unique(trade_data[trade_data$Reporter.Countries == input$reporterCountry, ]$Item))
    item_list <- c(topItems,"----",allItems)
    #item_list <- item_list[!is.na(item_list)]
    #item_list <- topItems
    #item_list <- allItems
    opts <- selectInput("itemName", tags$p("Pick an Item:"),
                        choices = topItems)
    list(opts)
  })
  
  
  ### --------------------------------------------------------------------- ###
  ## -- Which element within item
  
  output$element <- renderUI({
    
    values <- c("Quantity","Value")
    opts <- selectInput("elementName", tags$p("Pick an Element:"),
                        choices = values, selected=values[1])
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###
  ### --------------------------------------------------------------------- ###
  ## -- Subset trade-matrix data
  
  fao_data_export <- reactive({
    
    df <- trade_data[trade_data$Reporter.Countries == input$reporterCountry & trade_data$Item == input$itemName,]
    if (input$elementName == "Value") df <- df[df$Element == "Export Value",]
    if (input$elementName == "Quantity") df <- df[df$Element == "Export Quantity",]
    df
  })
  
  ### --------------------------------------------------------------------- ###
  ##-- Set the time for export-year 
  
  output$year_data <- renderUI({
    
    data <- fao_data_export()
    data <- data[!is.na(data$Year),]
    maxim <- max(data$Year)
    minim <- min(data$Year)
    med <- median(data$Year)
    opts <- sliderInput("yearData", tags$p("Pick a year"), min = minim, max = maxim, value = med, step = 1, animate=animationOptions(interval = 4000, loop = TRUE, playButton = NULL, pauseButton = NULL))
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###
  ## -- Subset trade-matrix data
  
  fao_data_import <- reactive({
    
    df <- trade_data[trade_data$Reporter.Countries == input$reporterCountry & trade_data$Item == input$itemName,]
    if (input$elementName == "Value") df <- df[df$Element == "Import Value",]
    if (input$elementName == "Quantity") df <- df[df$Element == "Import Quantity",]
    df
  })
  

  ### --------------------------------------------------------------------- ###
  ### --------------------------------------------------------------------- ###
  ## --- Outputs
  ### --------------------------------------------------------------------- ###
  ##-- Export Map
  

  

  #output$map <- reactivePlot(function() {
  plotInputMapExport <- reactive({
    
    df_export <- fao_data_export()
    df_export <- df_export[df_export$Year == input$yearData,]
    #names(df_export) <- str_replace_all(names(df_export), " ", ".")
    
    df_import <- fao_data_import()
    df_import <- df_import[df_import$Year == input$yearData,]
    #names(df_import) <- str_replace_all(names(df_import), " ", ".")
    
    # Filtering
#     if (max(df_import$Value) > max(df_export$Value)) {
#       quart1 <- mean(df_import$Value, na.rm=TRUE)*.0001
#       df_import <- df_import[df_import$Value > quart1,]
#       df_export <- df_import[df_export$Value > quart1,]
#     }
#     if (max(df_import$Value) < max(df_export$Value)) {
#       quart1 <- mean(df_export$Value, na.rm=TRUE)*.0001
#       df_import <- df_import[df_import$Value > quart1,]
#       df_export <- df_import[df_export$Value > quart1,]
#     }
    

    
    
    
    
    shape2 <- fao_world
    
    library(ggplot2)
    shape$id <- rownames(shape@data)
    map.points <- fortify(shape, region = "id")
    map.df <- merge(map.points, shape, by = "id")
    
    title_export_line = "Total wine exports\n in tonnes"
    title_import_line = "Total wine imports\n in tonnes"
    title_export_map = "Total wine exports\n in tonnes"
    title_import_map = "Total wine imports\n in tonnes"
    origin_country = "Italy"
    data_export="wine_italy.csv"
    data_import="wine_italy_import.csv"
    quantity_limit=50

    #df_export <- df_export[df_export$Value >= quantity_limit,]
    
    cent <- cbind(shape2@data,as.data.frame(gCentroid(shape2,byid=TRUE)))
    cent <- cent[!duplicated(cent$FAO_CODE),]
    
    # fortifying the routes information to create a dataframe; function from ggplot's github site ... thanks to the comments section in AnthroSpace's post
    fortify.SpatialLinesDataFrame = function(model, data, ...) {
      ldply(model@lines, fortify)
    }
    
    # Export data
    df_export3 <- merge(df_export,cent,by.x="Partner.Country.Code",by.y="FAO_CODE")
    tradepartners <- df_export3[c("Partner.Countries","Value","Item","x","y")]
    origin <- cent[cent$ADM0_NAME == input$reporterCountry,][c("x","y")]
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
    
    
    # Import data
    df_import3 <- merge(df_import,cent,by.x="Partner.Country.Code",by.y="FAO_CODE")
    tradepartners <- df_import3[c("Partner.Countries","Value","Item","x","y")]
    origin <- cent[cent$ADM0_NAME == input$reporterCountry,][c("x","y")]
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
    
    
    if (input$dataType == "Export") {
      lines_export <- geom_line(data=greatcircles_import,aes(long,lat,group=group, alpha=Value), size=.5, color="Steel Blue", show_guide = FALSE)
      points_export <- geom_point(data=fortifiedpartners_import, aes(long,lat,size=Value),color="Steel Blue", shape=1)
      names_export <- geom_text(data=fortifiedpartners_import, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=3, alpha=.8, family="Open Sans")
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
      lines_import <- geom_line(data=greatcircles_export,aes(long,lat,group=group, alpha=Value), size=.5, color="#FF3300", show_guide = FALSE)
      points_import <- geom_point(data=fortifiedpartners_export, aes(long,lat,size=Value),color="#FF3300", shape=1)
      names_import <- geom_text(data=fortifiedpartners_export, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=3, alpha=.8)
      title_exp <- "import"
    }
    if (input$dataType == "Both") {

      lines_export <- geom_line(data=greatcircles_import,aes(long,lat,group=group, alpha=Value), size=.5, color="Steel Blue", show_guide = FALSE)
      points_export <- geom_point(data=fortifiedpartners_import, aes(long,lat,size=Value),color="Steel Blue", shape=1)
      names_export <- geom_text(data=fortifiedpartners_import, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=3, alpha=.8)
      lines_import <- geom_line(data=greatcircles_export,aes(long,lat,group=group, alpha=Value), size=.5, color="#FF3300", show_guide = FALSE)
      points_import <- geom_point(data=fortifiedpartners_export, aes(long,lat,size=Value),color="#FF3300", shape=1)
      names_import <- geom_text(data=fortifiedpartners_export, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=3, alpha=.8)
      title_exp <- "exports and imports"
      #colors <- scale_color_manual(values=c("Steel Blue","#FF3300"))

    }
    
    if (input$elementName == "Quantity") legend_title <- "Quantity in tonnes"
    if (input$elementName == "Value") legend_title <- "Value in 1000 US$"
    

        
    year <- input$yearData
    
    p <- ggplot()
    p <- p + geom_polygon(data=map.df,aes(long,lat,group=group), fill="#5087CE", color="white", size=.5, alpha=.1)
    p <- p + lines_export + points_export + names_export
    p <- p + lines_import + points_import + names_import
    # exports
#     p <- p + geom_line(data=greatcircles_export,aes(long,lat,group=group, alpha=Value), color="#FF3300", size=.5)
#     p <- p + geom_point(data=fortifiedpartners_export, aes(long,lat,size=Value),color="#FF3300", shape=1)
#     p <- p + geom_text(data=fortifiedpartners_export, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=2, alpha=.6, family="Open Sans")
#     # imports
#     p <- p + geom_line(data=greatcircles_import,aes(long,lat,group=group, alpha=Value), color="Steel Blue", size=.5)
#     p <- p + geom_point(data=fortifiedpartners_import, aes(long,lat,size=Value),color="Steel Blue", shape=1)
#     p <- p + geom_text(data=fortifiedpartners_import, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=2, alpha=.6, family="Open Sans")
    # 
    p <- p + scale_size(name = legend_title, range = c(1,6))
    p <- p + coord_equal()
    #p <- p + colors
    p <- p + labs(title = paste(input$reporterCountry,"-",title_exp,"of",input$itemName,"in",year),
                  x=NULL, 
                  y=NULL)
    p <- p + guides(color = guide_legend(title = ""))
    p <- p + theme(legend.position = "top",
                   legend.justification=c(0,0),
                   legend.key.size=unit(5,'mm'),
                   legend.direction = "horizontal",
                   legend.background=element_rect(colour=NA, fill=NA),
                   #text = element_text(family = "Open Sans"),
                   legend.text=element_text(size=11),
                   legend.key = element_blank(),
                   legend.title=element_text(size=11),
                   title=element_text(size=12, color="Dim Grey"),
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
  
  
  output$export_map <- reactivePlot(function(){
    plotInputMapExport()
  })
  
  plotInputSumLine <- reactive({
    
    df_export <- fao_data_export()
    maxyear <- max(df_export$Year)
    minyear <- min(df_export$Year)
    #df_export <- df_export[df_export$Year <= input$yearData,]
   
    df_import <- fao_data_import()
    #df_import <- df_import[df_import$Year <= input$yearData,]
   
  exp_sum <- df_export %>% 
    group_by(Year) %>% 
    dplyr::summarise(sum = sum(Value, na.rm = TRUE))
  exp_sum$var <- "Total Export"
  
  imp_sum <- df_import %>% 
    group_by(Year) %>% 
    dplyr::summarise(sum = sum(Value, na.rm = TRUE))
  imp_sum$var <- "Total Import"
  
  
  if (input$dataType == "Export") {
    lines_export <- geom_line(data=exp_sum,aes(x=Year,y=sum,color="Export"), size=.5, alpha=.4)
    points_export <- geom_point(data=exp_sum,aes(x=Year,y=sum), color="Steel Blue", size=2, show_guide = FALSE, alpha=.4)
    colors <- scale_color_manual(values="Steel Blue")
    lines_import <- element_blank()
    points_import <- element_blank()
    title_exp <- "exports"
  }
  if (input$dataType == "Import") {
    lines_export <- element_blank()
    points_export <- element_blank()
    colors <- scale_color_manual(values="#FF3300")
    lines_import <- geom_line(data=imp_sum,aes(x=Year,y=sum,color="Import"), size=.5, alpha=.4)
    points_import <- geom_point(data=imp_sum,aes(x=Year,y=sum), color="#FF3300", size=2, show_guide = FALSE, alpha=.4)
    title_exp <- "imports"
  }
  if (input$dataType == "Both") {
    lines_import <- geom_line(data=imp_sum,aes(x=Year,y=sum,color="Import"), size=.5, alpha=.4)
    points_import <- geom_point(data=imp_sum,aes(x=Year,y=sum), color="#FF3300", size=2, show_guide = FALSE, alpha=.4)
    lines_export <- geom_line(data=exp_sum,aes(x=Year,y=sum,color="Export"), size=.5, alpha=.4)
    points_export <- geom_point(data=exp_sum,aes(x=Year,y=sum), color="Steel Blue", size=2, show_guide = FALSE, alpha=.4)
    title_exp <- "exports and imports"
    colors <- scale_color_manual(values=c("Steel Blue","#FF3300"))
    
    
  }

  if (input$elementName == "Quantity") legend_title <- "Quantity in tonnes"
  if (input$elementName == "Value") legend_title <- "Value in 1000 US$"
  
    
  plot_data <- rbind(exp_sum,imp_sum)
  
  #p <- ggplot(plot_data, aes(x=Year,y=sum,color=var))
  p <- ggplot()
  p <- p + lines_export + points_export #+ names_export
  p <- p + lines_import + points_import #+ names_import
  #p <- p + geom_point() + geom_line()
  p <- p + colors
  p <- p + coord_cartesian(xlim=c(minyear,maxyear))
  p <- p + scale_x_continuous(breaks=minyear:maxyear)
  p <- p + geom_vline(xintercept=input$yearData, color="black", alpha=.4, linetype="dashed")
  p <- p + labs(title = paste(input$reporterCountry,"-","Total",title_exp,"of \n",input$itemName),
                x=NULL, 
                y= legend_title)
  #p <- p + scale_color_manual(values=c("Steel Blue","#FF3300"))
  p <- p + theme(legend.position = "top",
                 axis.text.x  = element_text(angle=90, vjust= 0.5),
                 legend.justification=c(0,0),
                 legend.key.size=unit(5,'mm'),
                 legend.key = element_blank(),
                 legend.direction = "horizontal",
                 legend.background=element_rect(colour=NA, fill=NA),
                 #text = element_text(family = "Open Sans"),
                 legend.text=element_text(size=11),
                 legend.title=element_blank(),
                 title=element_text(size=12, color="Dim Grey"),
                 #panel.grid.minor=element_blank(),
                 #panel.grid.major=element_blank(),
                 panel.background = element_blank(),
                 plot.background = element_blank()
                 #axis.text = element_blank(),
                 #axis.ticks = element_blank()#,
                 #plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm")#,
                 #text=element_text(family = "Open Sans")
  )
  p
  })
  
  output$sumLine <- reactivePlot(function(){
    plotInputSumLine()
  })
  
  output$dlSumLine <- downloadHandler(
    filename = 'timeseries.pdf',
    content = function(file) {
      device <- function(..., width, height) grDevices::pdf(..., width = 11.7, height = 8.3)
      ggsave(file, plot = plotInputSumLine(), device = device)
    }
  )
  
  
  output$mytable = renderDataTable({
    
    
    df_export <- fao_data_export()
    df_export <- df_export[df_export$Year <= input$yearData,]
    
    df_import <- fao_data_import()
    df_import <- df_import[df_import$Year <= input$yearData,]
    
    exp_sum <- df_export %>% 
      group_by(Year) %>% 
      dplyr::summarise(sum = sum(Value, na.rm = TRUE))
    exp_sum$var <- "Total Export"
    
    imp_sum
    
  },options = list(pageLength = 10))
  
  
  
  
  
  output$export_map2 <- reactivePlot(function(){
    plot(cars)
  })
  
  
  output$dlMap <- downloadHandler(
    filename = 'map_export.pdf',
    content = function(file) {
      device <- function(..., width, height) grDevices::pdf(..., width = 11.7, height = 8.3)
      ggsave(file, plot = plotInputMapExport(), device = device)
    }
  )
  
  ### --------------------------------------------------------------------- ###
  ## Download data
  
  output$dlDataMap <- downloadHandler(
    filename = function() { paste(input$element_name,input$item_name,'mapdata.csv', sep="-") }, 
    content = function(file) {
      write.csv(fao_data_map(), file, row.names = FALSE)
    }
  )

  ### --------------------------------------------------------------------- ###
  ##-- Export Map
  
  
  #output$map <- reactivePlot(function() {
  plotInputTimeseries <- reactive({
    
    df_export <- fao_data_export()
    df_import <- fao_data_import()
    
    
    
    
    
    if (input$dataType == "Export") {
      lines_export <- geom_line(data=df_export,aes(x=Year,y=Value,group=Partner.Countries,color="Export"), size=.5, alpha=.4)
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
      lines_export <- element_blank()
      points_export <- element_blank()
      names_export <- element_blank()
      colors <- scale_color_manual(values="#FF3300")
      lines_import <- geom_line(data=df_import,aes(x=Year,y=Value,group=Partner.Countries,color="Import"), size=.5, alpha=.4)
      points_import <- geom_point(data=df_import,aes(x=Year,y=Value,group=Partner.Countries), color="#FF3300", size=2, show_guide = FALSE, alpha=.4)
      names_import <- geom_text(data=merge(df_import, aggregate(Year ~ Partner.Countries, df_import, max), by=c("Year","Partner.Countries")), aes(x=Year, y = Value, label=Partner.Countries), hjust=-0.1,vjust=-.5,size=4, alpha=.4,color="#FF3300")
      title_exp <- "import"
    }
    if (input$dataType == "Both") {
      
    lines_export <- geom_line(data=df_export,aes(x=Year,y=Value,group=Partner.Countries,color="Export"), size=.5, alpha=.4)
    points_export <- geom_point(data=df_export,aes(x=Year,y=Value,group=Partner.Countries), color="Steel Blue", size=2, show_guide = FALSE, alpha=.4)
    names_export <- geom_text(data=merge(df_export, aggregate(Year ~ Partner.Countries, df_export, max), by=c("Year","Partner.Countries")), aes(x=Year, y = Value, label=Partner.Countries), hjust=-0.1,vjust=-1,size=4, alpha=.4, color="Steel Blue")
    lines_import <- geom_line(data=df_import,aes(x=Year,y=Value,group=Partner.Countries,color="Import"), size=.5, alpha=.4)
    points_import <- geom_point(data=df_import,aes(x=Year,y=Value,group=Partner.Countries), color="#FF3300", size=2, show_guide = FALSE, alpha=.4)
    names_import <- geom_text(data=merge(df_import, aggregate(Year ~ Partner.Countries, df_import, max), by=c("Year","Partner.Countries")), aes(x=Year, y = Value, label=Partner.Countries), hjust=-0.1,vjust=-.5,size=4, alpha=.4,color="#FF3300")
      title_exp <- "exports and imports"
      colors <- scale_color_manual(values=c("Steel Blue","#FF3300"))
      
      
    }
    
    if (input$elementName == "Quantity") legend_title <- "Quantity in tonnes"
    if (input$elementName == "Value") legend_title <- "Value in 1000 US$"
    
    
    
    year <- input$yearData
    
    p <- ggplot()
    p <- p + lines_export + points_export + names_export
    p <- p + lines_import + points_import + names_import
    p <- p + geom_vline(xintercept=input$yearData, color="Dim Grey", alpha=.4, linetype="dashed")
    p <- p + coord_cartesian(xlim=c(min(df_export$Year),max(df_export$Year)+3))
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
                   title=element_text(size=12, color="Dim Grey"),
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
  
  
  output$export_timeseries <- reactivePlot(function(){
    plotInputTimeseries()
  })
  
  output$dlTimeseries <- downloadHandler(
    filename = 'timeseries_by_country.pdf',
    content = function(file) {
      device <- function(..., width, height) grDevices::pdf(..., width = 11.7, height = 8.3)
      ggsave(file, plot = plotInputTimeseries(), device = device)
    }
  )
  
  
    
  #### Bivariate stuff
  
  
  # output$var_x_item <- renderUI({
  #   itemNames <- itemTable[["itemName"]]
  #   opts <- selectizeInput('varXitem', "Which item are you looking for:", choices = itemNames, selected=itemNames[1], multiple = FALSE)
  #   list(opts)
  # })
  # 
  # output$var_x_element <- renderUI({
  #   domainCode <- itemTable[itemTable[["itemName"]] == input$varXitem, ]$domainCode
  #   elementNames <- as.character(elementTable[elementTable[["domainCode"]] %in% domainCode, ]$elementName)
  #   opts <- selectizeInput('varXelement', "Which element are you looking for:", choices = elementNames, multiple = FALSE)
  #   list(opts)
  # })
  # 
  # 
  # output$var_y_item <- renderUI({
  #   itemNames <- itemTable[["itemName"]]
  #   opts <- selectizeInput('varYitem', "Which item are you looking for:", choices = itemNames, selected=itemNames[3], multiple = FALSE)
  #   list(opts)
  # })
  # 
  # output$var_y_element <- renderUI({
  #   domainCode <- itemTable[itemTable[["itemName"]] == input$varYitem, ]$domainCode
  #   elementNames <- as.character(elementTable[elementTable[["domainCode"]] %in% domainCode, ]$elementName)
  #   opts <- selectizeInput('varYelement', "Which element are you looking for:", choices = elementNames, multiple = FALSE)
  #   list(opts)
  # })
  # 
  # 
  # bivar_data <- reactive({
  #   
  #   domainCodeX <- itemTable[itemTable[["itemName"]] == input$varXitem, ]$domainCode
  #   domainCodeY <- itemTable[itemTable[["itemName"]] == input$varYitem, ]$domainCode
  #   
  #   dcX <- domainCodeX
  #   ecX <- elementTable[elementTable$elementName == input$varXelement & elementTable$domainCode == dcX,]$elementCode
  #   icX <- itemTable[itemTable[["itemName"]] == input$varXitem, ]$itemCode
  #   
  #   dcY <- domainCodeY
  #   ecY <- elementTable[elementTable$elementName == input$varYelement & elementTable$domainCode == dcY,]$elementCode
  #   icY <- itemTable[itemTable[["itemName"]] == input$varYitem, ]$itemCode
  #   
  #   
  #   Xvar <- getFAOtoSYB(domainCode = dcX, 
  #                      elementCode = ecX,
  #                      itemCode = icX)
  #   X <- Xvar[["entity"]]
  #   
  #   Yvar <- getFAOtoSYB(domainCode = dcY, 
  #                       elementCode = ecY,
  #                       itemCode = icY)
  #   Y <- Yvar[["entity"]]
  # 
  #   XY <- inner_join(X, Y, by = c("FAOST_CODE","Year"))
  #   na.omit(XY) 
  #     
  # })
  # 
  # 
  # output$bivar_year <- renderUI({
  #   year_ss <- bivar_data()
  #   unique(year_ss$Year)
  #   opts <- sliderInput("bivarYear", "", min = min(year_ss$Year), max = max(year_ss$Year), value = max(year_ss$Year), step = 1)
  #   list(opts)
  # })
  # 
  # 
  # 
  # 
  # 
  # output$single_scatter <- reactivePlot(function() {
  #   
  #   plot_data <- bivar_data()
  #   plot_data <- plot_data[plot_data$Year == input$bivarYear,]
  #   names(plot_data) <- c("FAOST_CODE","Year","valueX","valueY")
  #   plot_data <- merge(plot_data,reg,by="FAOST_CODE")
  #   
  #   p <- ggplot(plot_data, aes(x=valueX, y=valueY,group=1,color=Region,label=Country))
  #   p <- p + geom_point()
  #   p <- p + geom_text()
  #   p <- p + theme_minimal() + 
  #     theme(legend.position = "top") + 
  #     theme(text = element_text(family = "Open Sans", size= 12)) +
  #     theme(legend.title = element_text(size = 10, face = "bold")) +
  #     theme(axis.text= element_text(size = 10)) +
  #     theme(axis.title = element_text(size = 10, face = "bold")) +
  #     theme(legend.text= element_text(size = 10)) +
  #     theme(strip.text = element_text(size = 14, face="bold")) +
  #     guides(colour = guide_legend(override.aes = list(size=4)))
  #   p <- p + labs(x=input$varXitem,y=input$varYitem)
  #   p <- p + geom_smooth(method = "loess")
  #   print(p)
  # })
  # 
  # 
  # output$multi_scatter <- reactivePlot(function() {
  #   
  #   plot_data <- bivar_data()
  #   names(plot_data) <- c("FAOST_CODE","Year","valueX","valueY")
  #   plot_data <- merge(plot_data,reg,by="FAOST_CODE")
  #   
  #   p <- ggplot(plot_data, aes(x=valueX, y=valueY, group=1,color=Region,label=Country))
  #   p <- p + geom_point()
  #   p <- p + geom_text(size=2.5)
  #   p <- p + theme_minimal() + 
  #     theme(legend.position = "top") + 
  #     theme(text = element_text(family = "Open Sans", size= 12)) +
  #     theme(legend.title = element_text(size = 10, face = "bold")) +
  #     theme(axis.text= element_text(size = 10)) +
  #     theme(axis.title = element_text(size = 10, face = "bold")) +
  #     theme(legend.text= element_text(size = 10)) +
  #     theme(strip.text = element_text(size = 14, face="bold")) +
  #     guides(colour = guide_legend(override.aes = list(size=4)))
  #   p <- p + labs(x=input$varXitem,y=input$varyitem)
  #   p <- p + facet_wrap(~Year)
  #   p <- p + geom_smooth(method = "loess")
  #   print(p)
  # })
  
  
})