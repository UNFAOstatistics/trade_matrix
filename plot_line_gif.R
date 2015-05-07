plot_line_gif <- function(z,title_line){
  s <- ggplot(z, aes(x=Year,y=Total))
  s <- s + geom_point(color="#FF3300", shape=1, size=1) + geom_line(color="#FF3300", size=.3)
  s <- s + coord_cartesian(xlim=c(1986:2011),ylim=c(0,2600000))
  s <- s + scale_x_continuous(breaks=1986:2011)
  s <- s + theme_minimal()
  s <- s + theme(text=element_text(family="Open Sans"),
                 axis.text=element_text(size=4, color="#5087CE"),
                 title=element_text(family="Open Sans", size=8, color="Dim Grey"),
                 axis.text.x  = element_text(angle=90, vjust= 0.5),
                 axis.title  = element_blank(),
                 axis.ticks  = element_blank())
  s <- s + labs(title=title_line)
}