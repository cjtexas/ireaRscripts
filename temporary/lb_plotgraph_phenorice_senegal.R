library (ggplot2)
library(plyr)
library(gridExtra)
library(scales)
library(lbRscripts)


file_in = '//10.0.1.252/nr_working/shared/PhenoRice/Documentes/Paper/mirco/data/Phenorice_dataset_graphs.csv'
file_in_vi = 'D:/Documents/Personal/Articles/abstracts/ESA_LP_2016/vis_senegal.txt'

data_in = read_csv2(file_in)
data_in_vi = read_delim(file_in_vi, delim = ';')
names(data_in)

levels(data_in$Country) = c('IND','IT','PHL')
levels(data_in_vi$Country) = c('IND','IT','PHL')

data_sub = droplevels(subset(data_in, Variable %in% c("Min_DOY_1st_Quarter","Min_DOY_2nd_Quarter", "Min_DOY_3rd_Quarter","Min_DOY_4th_Quarter",
                                           "Max_DOY_1st_Quarter" ,"Max_DOY_2nd_Quarter","Max_DOY_3rd_Quarter","Max_DOY_4th_Quarter")))

data_in = data.frame(Country = 'SEN', Site = '1', E = '', N = '', x = '', y = '', Season = 3,
                     Variable = c("Min_DOY_2nd_Quarter", "Min_DOY_3rd_Quarter", "Max_DOY_2nd_Quarter", "Max_DOY_3rd_Quarter"),
                     value = c(65,225,129,297))


# data_ind_vi = droplevels(subset(data_in_vi, Country == 'PHL' ))
data_in_vi$date = lb_doytodate(data_in_vi$DOY, 2013)

data_vi = data_in_vi
data_ph = data_in
data_ph$type [1:2] =  "Rice Sowing Date"
data_ph$type [3:4] =  "Rice Flowering Date"
data_ph$type = as.factor(data_ph$type)
data_ph = rbind(data_ph, data_ph)
data_ph$y = -0.1
data_ph$date = lb_doytodate(data_ph$value, 2013)
data_ph$y [((length(data_ph$date)/2)+1):length(data_ph$date)] = 1
data_ph$quart = rep(c(1,2,3,4),2)


p = ggplot(data_vi) + lb_theme_bw()
p = p + geom_line(aes(x = date, y = NDVI/10000, colour = 'EVI - Smoothed'),size = 1.5)
p = p + geom_line(aes(x = date, y = NDFI/10000, colour = 'NDFI'),size = 1.5)
# p = p + geom_point(aes(x = date , y = EVI_Raw/10000, colour = 'EVI - Raw') ,pch = 5)
p = p + geom_line(data = data_ph, aes(x = date,  y = y, color = type, group = quart), lty = 3,size = 1.5)
p = p + guides(colour=guide_legend(override.aes=list(linetype=c(1,1,3,3),shape=c(NA,NA,NA,NA))))
p = p + scale_x_date(date_breaks = "2 month", limits = c(as.Date("2012-12-31"), as.Date("2013-12-31")),date_labels = "%b" )
p = p + coord_cartesian(ylim = c(0,0.8))
p = p + xlab('Date')+ ylab("VI") + scale_colour_manual('',values = c('red','cadetblue','darkgreen','darkorange'))

p = p + theme(legend.position= 'bottom',legend.direction = 'horizontal', legend.background = element_rect(colour = 'black'))
p = p + theme(legend.text= element_text())
p + theme(axis.ticks = element_line(size = 1),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 18))
# browser()
# p = p + annotate("text", aes(x = 0,y = 1) ,data = ann, label = ann$txt)
# if (!(cy == 'PHL' & site == '3')) {p = p + guides(colour=FALSE)}
p = p + guides(colour=FALSE)
p


phrice_plot = function(data, cy = cy, site = site, title = NULL, ann = ann) {
  data_vi = droplevels(subset(data, Country == cy & Site == site))
  data_ph = droplevels(subset(data_sub, Country == cy & Site == site ))

  data_ph$type [1:4] =  "Crop Establishment"
  data_ph$type [5:8] =  "Crop Flowering"
  data_ph$type = as.factor(data_ph$type)
  data_ph = rbind(data_ph, data_ph)
  data_ph$y = -0.1
  data_ph$date = lb_doytodate(data_ph$Value, 2014)
  data_ph$y [((length(data_ph$date)/2)+1):length(data_ph$date)] = 1

  data_ph$quart = rep(c(1,2,3,4,5,6,7,8),2)
  data_ph = droplevels(subset(data_ph, Country == cy & Site == site & Value != 0))
  if (cy == 'IT') {data_ph$date = data_ph$date-8}
  if (cy == 'IND') {data_ph$date = data_ph$date-8}
  # data_ph$date[data_ph$date < as.Date('2014-08-01')] = data_ph$date[data_ph$date < as.Date('2014-08-01')]-8


  p = ggplot(data_vi) + lb_theme_bw(title = title)
  p = p + geom_line(aes(x = date, y = EVI_Smooth/10000, colour = 'EVI - Smoothed'),size = 1)
  p = p + geom_line(aes(x = date, y = NDFI/10000, colour = 'NDFI'),size = 1)
  p = p + geom_point(aes(x = date , y = EVI_Raw/10000, colour = 'EVI - Raw') ,pch = 5)
  p = p + geom_line(data = data_ph, aes(x = date,  y = y, color = type, group = quart), lty = 3,size = 1.1)
  p = p + guides(colour=guide_legend(override.aes=list(linetype=c(3,3,0,1,1),shape=c(NA,NA,5,NA,NA))))
  p = p + scale_x_date(date_breaks = "2 month", limits = c(as.Date("2013-09-01"), as.Date("2014-10-31")),date_labels = "%b" )
  p = p + coord_cartesian(ylim = c(0,0.8))
  p = p + ggtitle(title) + xlab('Date')+ ylab("VI") + scale_colour_manual('',values = c('orange','chartreuse3','black','red','blue'))
  p = p + theme(legend.position= 'bottom',legend.direction = 'horizontal', legend.background = element_rect(colour = 'black'))
  p = p + theme(legend.text= element_text())
  # browser()
  # p = p + annotate("text", aes(x = 0,y = 1) ,data = ann, label = ann$txt)
  # if (!(cy == 'PHL' & site == '3')) {p = p + guides(colour=FALSE)}
  p = p + guides(colour=FALSE)
  p
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
p1 = phrice_plot(data_in_vi, cy = 'IT', site = "1", title = "Italy -  One Rice Season", ann = data.frame(txt = 'a)'))
# legend <- get_legend(p)
p2 = phrice_plot(data_in_vi, cy = 'PHL', site = "1", title = "Philippines -  One Rice Season", ann = data.frame(txt ='b)'))
p3 = phrice_plot(data_in_vi, cy = 'IND', site = "1", title = "India -  One Rice Season", ann = data.frame(txt ='c)'))
p4 = phrice_plot(data_in_vi, cy = 'PHL', site = "2", title = "Philippines -  Two Rice Seasons", ann = data.frame(txt ='d)'))
p5 = phrice_plot(data_in_vi, cy = 'IND', site = "2", title = "India -  Two Rice Seasons", ann = data.frame(txt ='e)'))
p6 = phrice_plot(data_in_vi, cy = 'PHL', site = "3", title = "Philippines -  Three Rice Seasons", ann = data.frame(txt ='f)'))
p7 = phrice_plot(data_in_vi, cy = 'IND', site = "3", title = "India -  Three Rice Seasons", ann = data.frame(txt ='g)'))

blank = ggplot()+geom_blank()+ theme(panel.background = element_rect(fill = 'white'))
p_tot = grid.arrange(p1,p2,p3,blank,p4,p5,blank, p6,p7)


p = ggplot(data_in_vi, aes(x = date, y = EVI_Raw )) + theme_bw()
p = p + geom_point() + geom_line(aes(x = date, y = EVI_Smooth)) + facet_grid(Country~Site, scales = "free_x")
p = p + scale_x_date(date_breaks = "2 month", limits = c(as.Date("2013-05-01"), as.Date("2014-06-01")),date_labels = "%m %d")
p1

