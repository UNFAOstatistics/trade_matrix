library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(maptools)
library(sp)
library(rgeos)

shinyServer(function(input, output, session) {
  
  # Which group

#   output$group <- renderUI({
#     groupNames <- groupTable[["groupName"]]
#     opts <- selectInput("gc_name", "Which Group are you looking for:",choices = groupNames, selected=groupNames[2])
#     list(opts)
#   })

  ### --------------------------------------------------------------------- ###
  # -- Which domain within group
     
  output$domain <- renderUI({
    
    #gc <- groupTable[groupTable$groupName == input$gc_name,]$groupCode
    gc <- groupTable[groupTable$groupName == "Trade",]$groupCode
    subdomainTable <- domainTable[domainTable$groupCode == gc,]
    domainNames <- subdomainTable[["domainName"]]
    opts <- selectInput("domain_name", "Pick a Domain:",
                        choices = domainNames[1:2], selected=domainNames[1])
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###
  # Which item within domain?
  
  output$item <- renderUI({
    
    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    subitemTable <- itemTable[itemTable$domainCode == dc,]
    values <- subitemTable$itemName
    opts <- selectInput("item_name", "Pick an Item:",
                        choices = values, selected=values[4])
    list(opts)
  })
  
  
  ### --------------------------------------------------------------------- ###
  ## -- Which element within item

  output$element <- renderUI({
    
    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    subelementTable = elementTable[elementTable$domainCode == dc,]
    values <- as.character(subelementTable$elementName)
    opts <- selectInput("element_name", "Pick an Element:",
                        choices = values, selected=values[1])
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###
  ### --------------------------------------------------------------------- ###
  ## -- Fetch data from FAOSTAT
  
  fao_data <- reactive({

    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    ec <- elementTable[elementTable$elementName == input$element_name & elementTable$domainCode == dc,]$elementCode
    ic <- itemTable[itemTable$itemName == input$item_name & itemTable$domainCode == dc,]$itemCode

    dat <- getFAOtoSYB(domainCode = dc, 
                       elementCode = ec,
                       itemCode = ic)
     dat <- dat[["entity"]]
     names(dat) <- c("FAOST_CODE","Year","value")
     na.omit(dat) 
  })
  
  ### --------------------------------------------------------------------- ###
  ##-- Time-series slider based on particular data
  
  output$yearRange <- renderUI({
    
    data <- fao_data()
    maxim <- max(data$Year)
    minim <- min(data$Year)
    opts <- sliderInput("year_range", "Select year range", min = minim, max = maxim, value = c(minim,maxim), step = 1)
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###
  ##-- Time-series yaxis slider based on particular data
  
  output$yRange <- renderUI({
    
    data <- fao_data()
    maxim <- max(data$value)
    minim <- min(data$value)
    opts <- sliderInput("y_range", "Select Y-axis range", min = minim, max = maxim, value = c(minim,maxim), step = 1)
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###
  ##-- Bar year slider based on particular data
  
  output$yearBar <- renderUI({
    
    data <- fao_data()
    maxim <- max(data$Year)
    minim <- min(data$Year)
    opts <- sliderInput("year_bar", "Select year bar-plot", min = minim, max = maxim, value = maxim, step = 1, animate = T)
    list(opts)
  })
  
  output$countryBar <- renderUI({
    
    data <- fao_data()
    rows <- length(unique(data$FAOST_CODE))
    opts <- sliderInput("country_bar", "Select number of countries from top", min = 1, max = rows, value = rows, step = 1)
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###
  ##-- Map year slider based on particular data
  
  output$yearMap <- renderUI({
    
    data <- fao_data()
    maxim <- max(data$Year)
    minim <- min(data$Year)
    opts <- sliderInput("year_map", "Select year for the map", min = minim, max = maxim, value = maxim, step = 1)
    list(opts)
  })
  
  ### --------------------------------------------------------------------- ###
  ## Data filtering for time-series
  
  fao_data_timeseries <- reactive({
    
    data <- fao_data()
    data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>% 
      filter(value >= input$y_range[1] & value <= input$y_range[2]) %>% 
      arrange(FAOST_CODE)
  })
  
  ### --------------------------------------------------------------------- ###
  ## Data filtering for bars
  
  fao_data_bar <- reactive({
    
    data <- fao_data()
    data <- data %>% 
      filter(Year == input$year_bar) %>% 
      arrange(-value)
    data[1:input$country_bar,]
  })
  
  ### --------------------------------------------------------------------- ###
  ## Data filtering for maps
  
  fao_data_map <- reactive({
    
    data <- fao_data()
    data %>%
      filter(Year == input$year_map) %>% 
      arrange(FAOST_CODE)
  })
  

  ### --------------------------------------------------------------------- ###
  ### --------------------------------------------------------------------- ###
  ## --- Outputs
  ### --------------------------------------------------------------------- ###
  ##-- Time-series plot
  
  #-- Plot function
  
  
  plotInputTimeseries <- reactive({


    plot_data <- fao_data_timeseries()
    #names(plot_data) <- c("FAOST_CODE","Year","value")
    plot_data <- merge(plot_data,reg,by="FAOST_CODE")
    
    p <- ggplot(plot_data, aes(x=Year, y=value,group=FAOST_CODE,color=Region))
    p <- p + geom_point() + geom_line()
    p <- p + geom_text(data=plot_data[plot_data$Year ==input$year_range[2],],
                       aes(x=Year,y=value,label=Country))
    p <- p + theme_minimal() + 
      theme(legend.position = "top") + 
      #theme(text = element_text(family = "Open Sans", size= 12)) +
      theme(legend.title = element_text(size = 10, face = "bold")) +
      theme(axis.text= element_text(size = 10)) +
      theme(axis.title = element_text(size = 10, face = "bold")) +
      theme(legend.text= element_text(size = 10)) +
      theme(strip.text = element_text(size = 14, face="bold")) +
      guides(colour = guide_legend(override.aes = list(size=4)))
    p <- p + guides(color = guide_legend(title = "UNFAO Regions", 
                                         title.position = "top", 
                                         title.hjust=.5, 
                                         nrow = 2))
    p <- p + labs(title=paste(input$item_name,"(",input$domain_name,min(plot_data$Year),"-",max(plot_data$Year),")"),
                  y=input$element_name,x="")
    p <- p + scale_color_manual(values = trade_palette)
    p
  })
  
  #-- Plot Output
  
  output$timeseries <- reactivePlot(function(){
    plotInputTimeseries()
  })
  
  ### --------------------------------------------------------------------- ###
  ## Download plot
  
  output$dlTimeseries <- downloadHandler(
    filename = 'timeseries.pdf',
    content = function(file) {
      device <- function(..., width, height) grDevices::pdf(..., width = 11.7, height = 8.3)
      ggsave(file, plot = plotInputTimeseries(), device = device)
    }
)
    
  
  ### --------------------------------------------------------------------- ###
  ## Download data
  
  output$dlDataTimeseries <- downloadHandler(
    filename = function() { paste(input$domain_name,input$element_name,input$item_name,input$year_range[1],input$year_range[2],'timeseriesdata.csv', sep="-") }, 
    content = function(file) {
      write.csv(fao_data_timeseries(), file, row.names = FALSE)
    }
  )
  
  ### --------------------------------------------------------------------- ###
  ## bar-plot plot
  
  plotInputBar <- reactive({
    
    plot_data <- fao_data_bar()
    #names(plot_data) <- c("FAOST_CODE","Year","value")
    plot_data <- merge(plot_data,reg,by="FAOST_CODE")
    
    p <- ggplot(plot_data, aes(x=reorder(Country, value), y=value,fill=Region))
    p <- p + geom_bar(stat="identity")
#     p <- p + geom_text(data=plot_data[plot_data$Year ==input$year_range[2],],
#                        aes(x=Year,y=value,label=Country))
    p <- p + theme_minimal() + 
      theme(legend.position = "top") + 
      #theme(text = element_text(family = "Open Sans", size= 12)) +
      theme(legend.title = element_text(size = 10, face = "bold")) +
      theme(axis.text= element_text(size = 10)) +
      theme(axis.title = element_text(size = 10, face = "bold")) +
      theme(legend.text= element_text(size = 10)) +
      theme(strip.text = element_text(size = 14, face="bold")) +
      guides(colour = guide_legend(override.aes = list(size=4)))
    p <- p + coord_flip()
    p <- p + guides(fill = guide_legend(title = "UNFAO Regions", 
                                         title.position = "top", 
                                         title.hjust=.5, 
                                         nrow = 2))
    p <- p + labs(title=paste(input$item_name,"(",input$domain_name,max(plot_data$Year),")"),
                  y=input$element_name,x="")
    p <- p + scale_fill_manual(values = trade_palette)
    p
  })
  
  #-- Plot Output
  
  output$bar <- reactivePlot(function(){
    plotInputBar()
  })
  
  ### --------------------------------------------------------------------- ###
  ## Download plot
  
  output$dlBar <- downloadHandler(
    filename = 'bar.pdf',
    content = function(file) {
      device <- function(..., width, height) grDevices::pdf(..., width = 8.3, height = 11.7)
      ggsave(file, plot = plotInputBar(), device = device)
    }
  )
  
  
  ### --------------------------------------------------------------------- ###
  ## Download data
  
  output$dlDataBar <- downloadHandler(
    filename = function() { paste(input$element_name,input$item_name,'bardata.csv', sep="-") }, 
    content = function(file) {
      write.csv(fao_data_bar(), file, row.names = FALSE)
    }
  )
  
  
  
  ### --------------------------------------------------------------------- ###
  ## map
  

  #output$map <- reactivePlot(function() {
  plotInputMap <- reactive({
    
    dat <- fao_data_map()
    #names(dat) <- c("FAOST_CODE","Year","value")
    rows_for_breaks <- nrow(dat)
    #
    FAOST_CODE <- as.character(shape$FAOST_CODE)
    VarX <- rep(NA, 187)
    df.d <- data.frame(FAOST_CODE, VarX)
    # then we shall merge this with Eurostat data.frame
    dat2 <- merge(dat, df.d, by.x = "FAOST_CODE", all.y = TRUE)
    ## merge this manipulated attribute data with the spatialpolygondataframe
    ## rownames
    row.names(dat2) <- dat2$FAOST_CODE
    row.names(shape) <- as.character(shape$FAO_CODE)
    ## order data
    dat2 <- dat2[order(row.names(dat2)), ]
    shape <- shape[order(row.names(shape)), ]
    ## join
    
    shape2 <- spCbind(shape, dat2)
    # Apply the categories-function
    
    if (rows_for_breaks <= 10) breaks <- 2; fill_values <- c("Dim Grey","#66c2a4","#006d2c")
    if (rows_for_breaks > 10) breaks <- 3; fill_values <- c("Dim Grey","#edf8fb","#66c2a4","#006d2c")
    if (rows_for_breaks > 25) breaks <- 4; fill_values <- c("Dim Grey","#edf8fb","#66c2a4","#2ca25f","#006d2c")
    if (rows_for_breaks > 50) breaks <- 5; fill_values <- c("Dim Grey","#edf8fb","#b2e2e2","#66c2a4","#2ca25f","#006d2c")
    #if (rows_for_breaks > 120) breaks <- 6
    
    shape2$value_cat <- categories(shape2$value, cat=breaks)
    
    
    # Fortify the shape with the attribute data
    shape2$id <- rownames(shape2@data)
    map.points <- fortify(shape2, region = "id")
    map.df <- merge(map.points, shape2, by = "id")
    
    # graticule
    grat_df_robin <- fortify(grat_robin)
    # Create the plot
    p <- ggplot(data=map.df, aes(long,lat,group=group))
    # Grey for the non-data regions
    p <- p + geom_path(data = grat_df_robin, aes(long, lat, group = group, fill = NULL), linetype = "solid", color = "Dim Grey", size = .1)
    p <- p + geom_polygon(data = map.df, aes(long,lat),fill=NA,colour="white",size = .7)
    p <- p + geom_polygon(aes(fill = value_cat),colour="white",size=.2)
    p <- p + scale_fill_manual(values=fill_values) 
    p <- p + theme(legend.position = c(0.05,0.05), 
                   legend.justification=c(0,0),
                   legend.key.size=unit(6,'mm'),
                   legend.direction = "vertical",
                   legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                   legend.text=element_text(size=12), 
                   legend.title=element_text(size=12), 
                   title=element_text(size=16), 
                   panel.background = element_blank(), 
                   plot.background = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_blank(), 
                   axis.title = element_blank(), 
                   axis.ticks = element_blank())
    p <- p + guides(fill = guide_legend(title = input$element_name,
                                        title.position = "top", 
                                        title.hjust=0))
    p <- p + labs(title=paste(input$item_name,"(",input$domain_name,max(dat$Year),")"),
                  y="",x="")
    p
  })
  
    
    output$map <- reactivePlot(function(){
      plotInputMap()
    })
    

    output$dlMap <- downloadHandler(
      filename = 'map.pdf',
      content = function(file) {
        device <- function(..., width, height) grDevices::pdf(..., width = 11.7, height = 8.3)
        ggsave(file, plot = plotInputMap(), device = device)
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