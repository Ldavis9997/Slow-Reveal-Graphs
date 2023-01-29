
knitr::opts_chunk$set(echo = TRUE)\
library(tidyverse)
library(ggtext)
library(googlesheets4)
library(ggrepel)
library(showtext)
library(extrafont)
library(grid)
library(magick)



pop_colors <- (rep(c('#39553A'), 40))
iraq_data<- read_sheet('https://docs.google.com/spreadsheets/d/1Oegm5JgkLwkgHyIqE2-6j8TT6JvQ3gq61gB2FEXAxWk/edit?usp=sharing')

#imports your image to be able to put onto the graph
Label= "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 9</span>"  
Label1= "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 10</span>"
Label2 = "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 11</span>"
Label3 = "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 12</span>"
Label4 = "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 13</span>"
Label5= paste0("Iraq halts expors\nfor 30 days during \"revised\" \nsanctions debate; start of\nPhase 10 delayed")
Label6= paste0("Iraq halts exports for\n30 days in \"show of\n support\" for Palestines")

stamp <- image_read(here::here("Data vis for Iraq's Petroleum and Gas Infrastructure", "Picture2.png"))
square<- image_read(here::here("Data vis for Iraq's Petroleum and Gas Infrastructure", "square.png"))
plot(as.raster(stamp))
plot(as.raster(square))

picture <- image_graph(width = 900, height = 500)
ggplot(iraq_data,  
       aes(x = year, y= value))+
  geom_rect(aes(xmin = 30, xmax = 51, ymin = 0, ymax = 3.5), 
            fill = "#E7E3FB", alpha=.02)+
  geom_rect(aes(xmin = 80, xmax = 104, ymin = 0, ymax = 3.5), 
            fill = "#E7E3FB", alpha=.02)+
  geom_bar(stat= 'identity', fill= '#37573E', width = .8)+ theme_classic()+ scale_x_discrete(expand= expansion(mult=c(.03,0))) +   scale_y_continuous(breaks = c(0,.5,1,1.5,2,2.5,3,3.5), limits= c(0,3.5), expand= expansion(mult=c(.15,.4)))+ 
  geom_line(aes(x= year, y= cumul_mean1, group =1), size=1, color='white')+
  geom_line(aes(x= year, y= cumul_mean1, group =1), size=.5, color='#c5494d')+labs(title= "<p><span style = 'font-size:16pt; color: #FFFFFF'>I</span><span style = 'font-size:11pt; color: #FFFFFF'>RAQI</span><span style = 'font-size:16pt; color: #FFFFFF'> O</span><span style = 'font-size:11pt; color: #FFFFFF'>IL</span><span style = 'font-size:16pt; color: #FFFFFF'> E</span><span style = 'font-size:11pt; color: #FFFFFF'>XPORTS</span><span style = 'font-size:16pt; color: #FFFFFF'> U</span><span style = 'font-size:11pt; color: #FFFFFF'>NDER</span><span style = 'font-size:16pt; color: #FFFFFF'> P</span><span style = 'font-size:11pt; color: #FFFFFF'>HASES</span></span><span style = 'font-size:16pt; color: #FFFFFF'> 9,10,11,12</span><span style = 'font-size:11pt; color: #FFFFFF'> AND</span><span style = 'font-size:16pt; color: #FFFFFF'> 13</span><br><span style = 'font-size:10.5pt; color: #FFFFFF'>December 2000 Through January 2003</span>")+
  #subtitle= "<span style = 'font-size:8pt; color: #000000'>Weekly Average</span><br><span style = 'font-size:8pt; color: #000000'>Cumulatice average within phase</span>")+
  theme(plot.title= element_textbox_simple(family= "Open",size = 8.2, lineheight = 1, 
                                           width = grid::unit(12.7, "in"), # fixed width
                                           hjust = .45, # alignment of box relative to plot
                                           linetype = 0, # turn on border
                                           box.color = "black", # border color
                                           fill = "#6D7166", # background fill color
                                           r = grid::unit(0, "pt"), # radius for rounded corners
                                           padding = margin(0, 5, 7, 10), # padding around text inside the box
                                           margin = margin(-0, 0, 0, 0)),
        #plot.subtitle=  element_textbox_simple(size= 5, lineheight = 2, 
        #width = grid::unit(2.3, "in"), # fixed width
        #hjust = 0,
        #linetype = 1, # turn on border
        #box.color = "black", # border color
        #fill = "white", # background fill color
        #r = grid::unit(0, "pt"), # radius for rounded corners
        #padding = margin(10, 5, 7, 10), # padding around text inside the box
        #margin = margin(.5, .5, 5, 0)),
        legend.position= "none",                                             
        axis.line.x=element_blank(),                                                                       
        axis.title = element_blank(),
        axis.ticks.x= element_blank(),
        axis.ticks.y= element_blank(),
        axis.line.y= element_blank(),
        panel.grid.major.x= element_blank(),
        panel.grid.minor.x= element_blank(),
        panel.grid.major.y= element_line(color="black", size=.15),
        panel.grid.minor.y= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_blank(),
        plot.margin=unit(c(0,.5,.5,.5), "cm"),
        plot.background= element_rect(color="black"))

dev.off()
img <- image_draw(picture)

grid.raster(stamp, .9, .795, height = .2)+
  grid.raster(square, .2, .795, height = .05)

#labels for phases
text(130, 170, substitute(paste(italic("Phase 9"))), family = "Text", cex = .9, col = "black")+
  text(340, 170, substitute(paste(italic("Phase 10"))), family = "Text", cex = .9, col = "black")+
  text(550,170, substitute(paste(italic("Phase 11"))), family = "Text", cex = .9, col = "black")+
  text(735, 170, substitute(paste(italic("Phase 12"))), family = "Text", cex = .9, col = "black")+
  text(855,170, substitute(paste(italic("Phase 13"))), family = "Text", cex = .9, col = "black")+
  
  #labels above phases
  text(343, 130, substitute(paste(italic("UN begins retreoactove\npricing policy in late 2001;\nIraqi exports drop over\nPhases 11 and 12"))), family = "Text", cex = .75, col = "black", pos = 4)+
  text(50, 130, substitute(paste(italic("Iraqi surcharges continue; first\neported in late 2000"))), family = "Text", cex = .75, col = "black", pos =4)+
  text(220, 130, substitute(paste(italic("Iraq halts expors\nfor 30 days during \"revised\" \nsanctions debate; start of\nPhase 10 delayed"))), family = "Text", cex = .75, col = "black", pos =4)+
  text(553, 130, substitute(paste(italic("Iraq halts exports for\n30 days in \"show of\nsupport\" for Palestines"))), family = "Text", cex = .75, col = "black", pos =4)+
  #labels for y axis
  text(20, 447, "0", family = "Text", cex = .75, col = "black")+
  text(20, 407, ".5", family = "Text", cex = .75, col = "black")+
  text(22, 367, "1.0", family = "Text", cex = .75, col = "black")+
  text(22, 327, "1.5", family = "Text", cex = .75, col = "black")+
  text(22, 289, "2.0", family = "Text", cex = .75, col = "black")+
  text(22, 243, "2.5", family = "Text", cex = .75, col = "black")+
  text(22, 203, "3.0", family = "Text", cex = .75, col = "black")+
  text(22, 163, "3.5", family = "Text", cex = .75, col = "black")+
  text(10, 145, substitute(paste(italic("Million\nb/d"))), family = "Text", cex = .75, col = "black", pos=4)+
  
  #months x axis labels
  
  text(51, 447, "Dec", family = "Text", cex = .75, col = "black")+
  text(77.9, 447, "Jan", family = "Text", cex = .75, col = "black")+
  text(115, 447, "Feb", family = "Text", cex = .75, col = "black")+
  text(152.2, 447, "Mar", family = "Text", cex = .75, col = "black")+
  text(182.2, 447, "Apr", family = "Text", cex = .75, col = "black")+
  text(212.2, 447, "May", family = "Text", cex = .75, col = "black")+
  text(249.2, 447, "Jun", family = "Text", cex = .75, col = "black")+
  text(282.2, 447, "Jul", family = "Text", cex = .75, col = "black")+
  text(309.2, 447, "Aug", family = "Text", cex = .75, col = "black")+
  text(345.2, 447, "Sep", family = "Text", cex = .75, col = "black")+
  text(375.2, 447, "Oct", family = "Text", cex = .75, col = "black")+
  text(406.2, 447, "Nov", family = "Text", cex = .75, col = "black")+
  text(445.2, 447, "Dec", family = "Text", cex = .75, col = "black")+
  text(475.2, 447, "Jan", family = "Text", cex = .75, col = "black")+
  text(507.2, 447, "Feb", family = "Text", cex = .75, col = "black")+
  text(538.2, 447, "Mar", family = "Text", cex = .75, col = "black")+
  text(572.2, 447, "Apr", family = "Text", cex = .75, col = "black")+
  text(610.2, 447, "May", family = "Text", cex = .75, col = "black")+
  text(641.2, 447, "Jun", family = "Text", cex = .75, col = "black")+
  text(671.2, 447, "Jul", family = "Text", cex = .75, col = "black")+
  text(704.2, 447, "Aug", family = "Text", cex = .75, col = "black")+
  text(734.2, 447, "Sep", family = "Text", cex = .75, col = "black")+
  text(764.2, 447, "Oct", family = "Text", cex = .75, col = "black")+
  text(794.2, 447, "Nov", family = "Text", cex = .75, col = "black")+
  text(832.2, 447, "Dec", family = "Text", cex = .75, col = "black")+
  text(868.2, 447, "Jan", family = "Text", cex = .75, col = "black")+
  text(50, 465, substitute(paste(italic('2000'))), family = "Text", cex = .87, col = "black")+
  text(80, 465, substitute(paste(italic('2001'))), family = "Text", cex = .87, col = "black") +
  text(868,465, substitute(paste(italic('2003'))), family = 'Text', cex = .87, col = 'black')

segments(x0=39, x1=39, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=63, x1=63, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=97, x1=97, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=130, x1=130, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=165, x1=165, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=195, x1=195, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=225, x1=225, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=266, x1=266, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=295, x1=295, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=325, x1=325, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=362, x1=362, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=391, x1=391, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=423, x1=423, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=456, x1=456, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=492, x1=492, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=521, x1=521, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=561, x1=561, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=590, x1=590, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=624, x1=624, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=658, x1=658, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=685, x1=685, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=717, x1=717, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=747, x1=747, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=777, x1=777, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=814, x1=814, y0=440, y1=455, lwd = 1, lty = "solid")+
  segments(x0=853, x1=853, y0=440, y1=455, lwd = 1, lty = "solid")

dev.off()

Label= "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 9</span>"  
Label1= "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 10</span>"
Label2 = "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 11</span>"
Label3 = "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 12</span>"
Label4 = "<span style = 'font-size:9pt; color:#000000'><i><b>Phase 13</span>"
Label5= paste0("Iraq halts expors\nfor 30 days during \"revised\" \nsanctions debate; start of\nPhase 10 delayed")
Label6= paste0("Iraq halts exports for\n30 days in \"show of\n support\" for Palestines")

area<- (ggplot(iraq_data,  
               aes(x = year, y= value))+
          geom_rect(aes(xmin = 30, xmax = 51, ymin = 0, ymax = 3.5), 
                    fill = "#E7E3FB", alpha=.02)+
          geom_rect(aes(xmin = 80, xmax = 104, ymin = 0, ymax = 3.5), 
                    fill = "#E7E3FB", alpha=.02)+
          geom_bar(stat= 'identity', fill= '#37573E', width = .8)+ theme_classic()+ scale_x_discrete(expand= expansion(mult=c(.03,0))) +   scale_y_continuous(breaks = c(0,.5,1,1.5,2,2.5,3,3.5), limits= c(0,3.5), expand= expansion(mult=c(.15,.4)))+ 
          geom_line(aes(x= year, y= cumul_mean1, group =1), size=1, color='white')+
          geom_line(aes(x= year, y= cumul_mean1, group =1), size=.5, color='#c5494d'))
new<- area +labs(title= "<p><span style = 'font-size:16pt; color: #FFFFFF'>I</span><span style = 'font-size:11pt; color: #FFFFFF'>RAQI</span><span style = 'font-size:16pt; color: #FFFFFF'> O</span><span style = 'font-size:11pt; color: #FFFFFF'>IL</span><span style = 'font-size:16pt; color: #FFFFFF'> E</span><span style = 'font-size:11pt; color: #FFFFFF'>XPORTS</span><span style = 'font-size:16pt; color: #FFFFFF'> U</span><span style = 'font-size:11pt; color: #FFFFFF'>NDER</span><span style = 'font-size:16pt; color: #FFFFFF'> P</span><span style = 'font-size:11pt; color: #FFFFFF'>HASES</span></span><span style = 'font-size:16pt; color: #FFFFFF'> 9,10,11,12</span><span style = 'font-size:11pt; color: #FFFFFF'> AND</span><span style = 'font-size:16pt; color: #FFFFFF'> 13</span><br><span style = 'font-size:10.5pt; color: #FFFFFF'>December 2000 Through January 2003</span>")+
  #subtitle= "<span style = 'font-size:8pt; color: #000000'>Weekly Average</span><br><span style = 'font-size:8pt; color: #000000'>Cumulatice average within phase</span>")+
  theme(plot.title= element_textbox_simple(family= "Open",size = 8.2, lineheight = 1, 
                                           width = grid::unit(10.1, "in"), # fixed width
                                           hjust = .45, # alignment of box relative to plot
                                           linetype = 0, # turn on border
                                           box.color = "black", # border color
                                           fill = "#6D7166", # background fill color
                                           r = grid::unit(0, "pt"), # radius for rounded corners
                                           padding = margin(0, 5, 7, 10), # padding around text inside the box
                                           margin = margin(-0, 0, 0, 0)),
        #plot.subtitle=  element_textbox_simple(size= 5, lineheight = 2, 
        #width = grid::unit(2.3, "in"), # fixed width
        #hjust = 0,
        #linetype = 1, # turn on border
        #box.color = "black", # border color
        #fill = "white", # background fill color
        #r = grid::unit(0, "pt"), # radius for rounded corners
        #padding = margin(10, 5, 7, 10), # padding around text inside the box
        #margin = margin(.5, .5, 5, 0)),
        legend.position= "none",                                             
        axis.line.x=element_blank(),                                                                       
        axis.title = element_blank(),
        axis.ticks.x= element_blank(),
        axis.ticks.y= element_blank(),
        axis.line.y= element_blank(),
        panel.grid.major.x= element_blank(),
        panel.grid.minor.x= element_blank(),
        panel.grid.major.y= element_line(color="black", size=.15),
        panel.grid.minor.y= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_blank(),
        plot.margin=unit(c(0,.5,.5,.5), "cm"),
        plot.background= element_rect(color="black"))


# y axis custom breaks
final<- new +
  annotation_custom(grob = grid::textGrob(label = "0", hjust=0, gp=gpar(col="black", cex=.8)),xmin = -2, xmax = -2, ymin = -.1, ymax = -.1)+
  annotate(geom="text", x=-1.9, y=.4, label=".5",
           color="black", size= 3.8)+
  annotate(geom="text", x=-1.3, y=.9, label="1.0",
           color="black", size= 3.8)+
  annotate(geom="text", x=-1.3, y=1.4, label="1.5",
           color="black", size= 3.8)+
  annotate(geom="text", x=-1.3, y=1.9, label="2.0",
           color="black", size= 3.8)+
  annotate(geom="text", x=-1.3, y=2.4, label="2.5",
           color="black", size= 3.8)+
  annotate(geom="text", x=-1.3, y=2.9, label="3.0",
           color="black", size= 3.8)+
  annotate(geom="text", x=-1.3, y=3.4, label="3.5",
           color="black", size= 3.8)+
  #X-Axis custom breaks
  annotation_custom(grob = grid::textGrob(label = "Dec", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 1.1, xmax = 1.1, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Jan", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 5, xmax = 5, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Feb", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 9.3, xmax = 9.3, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Mar", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 14.02, xmax = 14.02, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Apr", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 17.21, xmax = 19.21, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "May", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 22.42, xmax = 22.42, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Jun", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 27.11, xmax = 27.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Jul", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 31.11, xmax = 32.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Aug", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 34.11, xmax = 36.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Sep", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 39.11, xmax = 41.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Oct", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 44.11, xmax = 44.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Nov", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 48.11, xmax = 48.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Dec", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 53.11, xmax = 53.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Jan", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 57.11, xmax = 57.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Feb", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 61.11, xmax = 61.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Mar", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 65.5, xmax = 65.5, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Apr", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 70.11, xmax = 70.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "May", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 74.8, xmax = 74.8, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Jun", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 79.11, xmax = 79.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Jul", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 83.11, xmax = 83.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Aug", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 87.11, xmax = 87.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Sep", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 91.11, xmax = 91.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Oct", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 95.11, xmax = 95.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Nov", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 99.5, xmax = 99.5, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Dec", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 104.5, xmax = 104.5, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Jan", hjust=0, gp=gpar(col="black", cex=.7)),xmin = 109.11, xmax =109.11, ymin = -.1, ymax = -.1)+
  annotation_custom(grob = grid::textGrob(label = "Iraqi surcharges continue; first\neported in late 2000", hjust=0, gp=gpar(col="black", cex=.7, lineheight=1, fontface= "italic")),xmin = 0, xmax =5, ymin = 3.75, ymax = 3.75)+
  annotation_custom(grob = grid::textGrob(label = "UN begins retreoactove\npricing policy in late 2001;\nIraqi exports drop over\nPhases 11 and 12", hjust=0, gp=gpar(col="black", cex=.7, lineheight=1, fontface='italic')),xmin =44, xmax =44, ymin = 3.9, ymax = 3.9)+
  annotation_custom(grob = grid::textGrob(label = Label5, hjust=0, gp=gpar(col="black", cex=.7, lineheight=1, fontface='italic')),xmin =25, xmax =25, ymin = 3.9, ymax = 3.9)+
  annotation_custom(grob = grid::textGrob(label = Label6, hjust=0, gp=gpar(col="black", cex=.7, lineheight=1, fontface='italic')),xmin=67, xmax =67, ymin = 3.9, ymax = 3.9)+
  annotation_custom(grob = grid::textGrob(label = "Million\n b/d", hjust=0, gp=gpar(col="black", cex=.7, lineheight=1, fontface='italic')),xmin =-2.5, xmax =-2.5, ymin = 3.7, ymax = 3.7)+
  annotation_custom(grob = grid::textGrob(label = "2000", hjust=0, gp=gpar(col="black", cex=.7, lineheight=1, fontface='italic')),xmin =.7, xmax =1, ymin = -3.6, ymax = 3)+
  annotation_custom(grob = grid::textGrob(label = "2001", hjust=0, gp=gpar(col="black", cex=.7, lineheight=1, fontface='italic')),xmin =8, xmax =1, ymin = -3.6, ymax = 3)+
  annotation_custom(grob = grid::textGrob(label = "2002", hjust=0, gp=gpar(col="black", cex=.7, lineheight=1, fontface='italic')),xmin =.9, xmax =112, ymin = -3.6, ymax = 3)+
  annotation_custom(grob = grid::textGrob(label = "2003", hjust=0, gp=gpar(col="black", cex=.7, lineheight=1, fontface='italic')),xmin =.9, xmax =217, ymin = -3.6, ymax = 3)+
  
  #Sector Labels
  geom_richtext(aes(x= 12.5, y=3.3, label=Label, size=4.5), fill= NA, label.color=NA)+
  geom_richtext(aes(x= 41, y=3.3, label=Label1, size=4.5), fill= NA, label.color=NA)+
  geom_richtext(aes(x= 66.5, y=3.3, label=Label2, size=4.5), fill= NA, label.color=NA)+
  geom_richtext(aes(x= 91.5, y=3.3, label=Label3, size=4.5), fill= NA, label.color=NA)+
  geom_richtext(aes(x= 107.5, y=3.3, label=Label4, size=4.5), fill= NA, label.color=NA)
final
ggsave("final.pdf")

