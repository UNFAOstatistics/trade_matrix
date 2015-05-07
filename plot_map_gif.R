plot_map_gif <- function(title_map) {
  
  df3 <- merge(df2,cent,by.x="Partner.Country.Code",by.y="FAO_CODE")
  tradepartners <- df3[c("Partner.Countries","Value","Item","x","y")]
  origin <- cent[cent$ADM0_NAME == origin_country,][c("x","y")]
  routes = gcIntermediate(origin, tradepartners[,c('x', 'y')], 200, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
  routes <- spTransform(routes, CRS("+proj=robin"))
  partners <- SpatialPointsDataFrame(tradepartners[4:5], tradepartners[1:3], proj4string = CRS("+proj=longlat +ellps=WGS84"))
  partners <- spTransform(partners, CRS("+proj=robin"))
  fortifiedpartners <- cbind(coordinates(partners), partners@data)
  fortifiedroutes = fortify.SpatialLinesDataFrame(routes) 
  # An id for each country
  tradepartners$id=as.character(c(1:nrow(tradepartners))) 
  # Merge fortified routes with usopencountry information
  greatcircles = merge(fortifiedroutes, tradepartners, all.x=T, by="id") 
  names(greatcircles)[names(greatcircles)=="x"] <- "long"
  names(greatcircles)[names(greatcircles)=="y"] <- "lat"
  names(fortifiedpartners)[names(fortifiedpartners)=="x"] <- "long"
  names(fortifiedpartners)[names(fortifiedpartners)=="y"] <- "lat"
  
  p <- ggplot()
  p <- p + geom_polygon(data=map.df,aes(long,lat,group=group), fill="#5087CE", color="white", size=.5, alpha=.1)
  p <- p + geom_line(data=greatcircles,aes(long,lat,group=group, alpha=Value), color="#FF3300", size=.5)
  p <- p + geom_point(data=fortifiedpartners, aes(long,lat,size=Value),color="#FF3300", shape=1)
  p <- p + geom_text(data=fortifiedpartners, aes(long,lat,label=Partner.Countries),color="Dim Grey", size=2, alpha=.6, family="Open Sans")
  p <- p + scale_size(name = "Quantity in tonnes", range = c(1,6))
  p <- p + coord_equal()
  p <- p + labs(title = paste0(title_map," - ",origin_country),
                x=NULL, 
                y=NULL)
  p <- p + theme(legend.position = c(0.10,0.25),
                 legend.justification=c(0,0),
                 legend.key.size=unit(5,'mm'),
                 legend.direction = "vertical",
                 legend.background=element_rect(colour=NA, fill=NA),
                 text = element_text(family = "Open Sans"),
                 legend.text=element_text(size=11),
                 legend.title=element_text(size=11),
                 title=element_text(size=12, color="Dim Grey"),
                 panel.grid.minor=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.background = element_blank(),
                 plot.background = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm"),
                 text=element_text(family = "Open Sans"))
  p <- p + theme(legend.position = "none")
  p <- p + annotate("text", x = Inf, y = -Inf, label = paste0("Year ",year),hjust=2, vjust=-2.5, col="#5087CE", cex=8,fontface = "bold", alpha = 0.4, family="Open Sans")
  p <- p + annotate("text", x = Inf, y = -Inf, label = "*countries with less than 50 tonnes excluded",
                    hjust=1.5, vjust=-1.0, col="Dim Grey", cex=3,fontface = "bold", alpha = 0.6, family="Open Sans")
  p
}