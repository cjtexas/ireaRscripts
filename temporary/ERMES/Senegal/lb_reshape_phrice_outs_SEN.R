# Funcrion to extract Sowing dates data from full phenorice results
#

library(raster)

in_folder = "//10.0.1.252/nr_working/shared/PhenoRice/Processing/Senegal/Outputs/new_Elab2"
out_folder = "D:/Temp/PhenoRice/Processing/SEN/Outputs/new_Elab2/sow_rasters"
years = paste0(seq(2004,2014,1),"//")
in_folders = file.path(in_folder, years, 'raster')

yy = 1
years = seq(2004,2014,1)

for(yy in 1:length(years)) {

  print(yy)
  in_file = list.files(in_folders[[yy]], pattern = 'old_min_identification_Full_Out?', full.names = TRUE)[1]
  out_file_dry = file.path(out_folder,'sos', 'dry',paste0("sow_dry_", years[yy],'.tiff'))
  out_file_wet = file.path(out_folder,'sos','wet',paste0("sow_wet_", years[yy],'.tiff'))
  out_file_dry_eos = file.path(out_folder,'eos','dry',paste0("eos_dry_", years[yy],'.tiff'))
  out_file_wet_eos = file.path(out_folder,'eos','wet',paste0("eos_wet_", years[yy],'.tiff'))
  out_file_dry_flow = file.path(out_folder,'flow','dry',paste0("flow_dry_", years[yy],'.tiff'))
  out_file_wet_flow = file.path(out_folder,'flow','wet',paste0("flow_wet_", years[yy],'.tiff'))
  dir.create(dirname(out_file_dry),recursive = TRUE)
  dir.create(dirname(out_file_wet),recursive = TRUE)
  dir.create(dirname(out_file_dry_eos),recursive = TRUE)
  dir.create(dirname(out_file_wet_eos),recursive = TRUE)
  dir.create(dirname(out_file_dry_flow),recursive = TRUE)
  dir.create(dirname(out_file_wet_flow),recursive = TRUE)

  # OPen the raster
  n_seas <- raster(in_file, band = 1) ; n_seas_data <- values(n_seas)
  sow_1 <- raster(in_file, band = 2) ; sow_1_data <- values(sow_1)
  sow_2 <- raster(in_file, band = 3) ; sow_2_data <- values(sow_2)
  sow_3 <- raster(in_file, band = 4) ; sow_3_data <- values(sow_3)

  eos_1 <- raster(in_file, band = 29) ; eos_1_data <- values(eos_1)
  eos_2 <- raster(in_file, band = 30) ; eos_2_data <- values(eos_2)
  eos_3 <- raster(in_file, band = 31) ; eos_3_data <- values(eos_3)

  flow_1 <- raster(in_file, band = 5) ; flow_1_data <- values(flow_1)
  flow_2 <- raster(in_file, band = 6) ; flow_2_data <- values(flow_2)
  flow_3 <- raster(in_file, band = 7) ; flow_3_data <- values(flow_3)

  # remove NA results

  sow_1_data[which(sow_1_data >= 365)] <- NA
  sow_2_data[which(sow_2_data >= 365)] <- NA
  sow_3_data[which(sow_3_data >= 365)] <- NA

  sow_1_data[which(sow_1_data == 0)] <- NA
  sow_2_data[which(sow_2_data == 0)] <- NA
  sow_3_data[which(sow_3_data == 0)] <- NA

  flow_1_data[which(flow_1_data >= 400)] <- NA
  flow_2_data[which(flow_2_data >= 400)] <- NA
  flow_3_data[which(flow_3_data >= 400)] <- NA

  flow_1_data[which(flow_1_data == 0)] <- NA
  flow_2_data[which(flow_2_data == 0)] <- NA
  flow_3_data[which(flow_3_data == 0)] <- NA

  eos_1_data[which(eos_1_data >= 450)] <- NA
  eos_2_data[which(eos_2_data >= 450)] <- NA
  eos_3_data[which(eos_3_data >= 450)] <- NA

  eos_1_data[which(eos_1_data == 0)] <- NA
  eos_2_data[which(eos_2_data == 0)] <- NA
  eos_3_data[which(eos_3_data == 0)] <- NA

  sow_1[] <- sow_1_data
  sow_2[] <- sow_2_data
  sow_3[] <- sow_3_data

  eos_1[] <- eos_1_data
  eos_2[] <- eos_2_data
  eos_3[] <- eos_3_data

  flow_1[] <- flow_1_data
  flow_2[] <- flow_2_data
  flow_3[] <- flow_3_data

  setValues(sow_1, sow_1_data)
  setValues(sow_2, sow_2_data)
  setValues(sow_3, sow_3_data)

  setValues(flow_1, flow_1_data)
  setValues(flow_2, flow_2_data)
  setValues(flow_3, flow_3_data)

  setValues(eos_1, eos_1_data)
  setValues(eos_2, eos_2_data)
  setValues(eos_3, eos_3_data)

  sow_1_dry = sow_1
  sow_1_dry[sow_1_dry >= 130] = NA
  sow_1_dry[sow_1_dry <= 0] = NA

  eos_1_dry = eos_1
  flow_1_dry = flow_1
  eos_1_dry[sow_1 >= 130] = NA
  eos_1_dry[sow_1 <= 0] = NA
  flow_1_dry[sow_1 >= 130] = NA
  flow_1_dry[sow_1 <= 0] = NA

  eos_2_dry = eos_2
  flow_2_dry = flow_2
  sow_2_dry = sow_2
  sow_2_dry[sow_2 >= 130] = NA
  sow_2_dry[sow_2 <= 0] = NA

  eos_2_dry[sow_2 >= 130] = NA
  eos_2_dry[sow_2 <= 0] = NA
  flow_2_dry[sow_2 >= 130] = NA
  flow_2_dry[sow_2 <= 0] = NA

  sow_3_dry = sow_3
  sow_3_dry[sow_3 >= 130] = NA
  sow_3_dry[sow_3 <= 0] = NA

  eos_3_dry = eos_3
  flow_3_dry = flow_3

  eos_3_dry[sow_3 >= 130] = NA
  eos_3_dry[sow_3 <= 0] = NA
  flow_3_dry[sow_3 >= 130] = NA
  flow_3_dry[sow_3 <= 0] = NA

  sow_1_wet = sow_1
  sow_1_wet[sow_1 <= 170] = NA

  sow_2_wet = sow_2
  sow_2_wet[sow_2 <= 170] = NA

  sow_3_wet = sow_3
  sow_3_wet[sow_3 <= 170] = NA

  eos_1_wet = eos_1
  flow_1_wet = flow_1

  eos_1_wet[sow_1 <= 170] = NA
  eos_1_wet[sow_1 <= 0] = NA
  flow_1_wet[sow_1 <= 170] = NA
  flow_1_wet[sow_1 <= 0] = NA


  eos_2_wet = eos_2
  flow_2_wet = flow_2
  eos_2_wet[sow_2 <= 170] = NA
  eos_2_wet[sow_2 <= 0] = NA
  flow_2_wet[sow_2 <= 170] = NA
  flow_2_wet[sow_2 <= 0] = NA

  eos_3_wet = eos_3
  flow_3_wet = flow_3
  eos_3_wet[sow_3 <= 170] = NA
  eos_3_wet[sow_3 <= 0] = NA
  flow_3_wet[sow_3 <= 170] = NA
  flow_3_wet[sow_3 <= 0] = NA

  sow_dry = sum(stack(sow_1_dry, sow_2_dry, sow_3_dry), na.rm = T)
  sow_wet = sum(stack(sow_1_wet, sow_2_wet, sow_3_wet), na.rm = T)
  sow_dry[sow_dry == 0] = NA
  sow_wet[sow_wet == 0] = NA
  sow_dry[sow_dry >= 365] = NA
  sow_wet[sow_wet >= 365] = NA

  eos_dry = sum(stack(eos_1_dry, eos_2_dry, eos_3_dry), na.rm = T)
  eos_wet = sum(stack(eos_1_wet, eos_2_wet, eos_3_wet), na.rm = T)
  eos_dry[eos_dry == 0] = NA
  eos_wet[eos_wet == 0] = NA
  eos_dry[eos_dry >= 450] = NA
  eos_wet[eos_wet >= 450] = NA

  flow_dry = sum(stack(flow_1_dry, flow_2_dry, flow_3_dry), na.rm = T)
  flow_wet = sum(stack(flow_1_wet, flow_2_wet, flow_3_wet), na.rm = T)
  flow_dry[flow_dry == 0] = NA
  flow_wet[flow_wet == 0] = NA
  flow_dry[flow_dry >= 400] = NA
  flow_wet[flow_wet >= 400] = NA


  writeRaster(sow_dry, filename = out_file_dry, overwrite = T)
  writeRaster(sow_wet, filename = out_file_wet, overwrite = T)

  writeRaster(flow_dry, filename = out_file_dry_flow, overwrite = T)
  writeRaster(flow_wet, filename = out_file_wet_flow, overwrite = T)

  writeRaster(eos_dry, filename = out_file_dry_eos, overwrite = T)
  writeRaster(eos_wet, filename = out_file_wet_eos, overwrite = T)


}

dry_stack = stack(list.files(file.path(out_folder, 'sos','dry'), '.tiff$', full.names = TRUE))
wet_stack = stack(list.files(file.path(out_folder, 'sos', 'wet'), '.tiff$', full.names = TRUE))
dry_stack_eos = stack(list.files(file.path(out_folder,'eos','dry'), '.tiff$', full.names = TRUE))
wet_stack_eos = stack(list.files(file.path(out_folder,'eos', 'wet'), '.tiff$', full.names = TRUE))
dry_stack_flow = stack(list.files(file.path(out_folder,'flow','dry'), '.tiff$', full.names = TRUE))
wet_stack_flow = stack(list.files(file.path(out_folder,'flow', 'wet'), '.tiff$', full.names = TRUE))

in_dates = as.Date('2004-01-01')+365*seq(0,length(years)-1,1)
in_dates[2:11] = in_dates[2:11] + 1
in_dates[6:11] = in_dates[6:11] + 1
in_dates[8:11] = in_dates[8:11] + 1
lb_writeenvits(dry_stack, in_dates = in_dates, out_file = "D:/Temp/PhenoRice/Processing/SEN/Outputs/new_Elab2/sow_rasters/sos/dry/sow_dry_2004_2014.dat")
lb_writeenvits(wet_stack, in_dates = in_dates, out_file = "D:/Temp/PhenoRice/Processing/SEN/Outputs/new_Elab2/sow_rasters/sos/wet/sow_wet_2004_2014.dat")

lb_writeenvits(dry_stack_eos, in_dates = in_dates, out_file = "D:/Temp/PhenoRice/Processing/SEN/Outputs/new_Elab2/sow_rasters/eos/dry/eos_dry_2004_2014.dat")
lb_writeenvits(wet_stack_eos, in_dates = in_dates, out_file = "D:/Temp/PhenoRice/Processing/SEN/Outputs/new_Elab2/sow_rasters/eos/wet/eos_wet_2004_2014.dat")

lb_writeenvits(dry_stack_flow, in_dates = in_dates, out_file = "D:/Temp/PhenoRice/Processing/SEN/Outputs/new_Elab2/sow_rasters/flow/dry/flow_dry_2004_2014.dat")
lb_writeenvits(wet_stack_flow, in_dates = in_dates, out_file = "D:/Temp/PhenoRice/Processing/SEN/Outputs/new_Elab2/sow_rasters/flow/wet/flow_wet_2004_2014.dat")

sow_data_dry = data.frame(year = NULL, DOY = NULL)
eos_data_dry =  data.frame(year = NULL, DOY = NULL)
flow_data_dry =  data.frame(year = NULL, DOY = NULL)
sow_data_wet =  data.frame(year = NULL, DOY = NULL)
eos_data_wet =  data.frame(year = NULL, DOY = NULL)
flow_data_wet =  data.frame(year = NULL, DOY = NULL)

sow_dry = getValues(dry_stack)
sow_wet = getValues(wet_stack)
eos_dry = getValues(dry_stack_eos)
eos_wet = getValues(wet_stack_eos)
flow_dry = getValues(dry_stack_flow)
flow_wet = getValues(wet_stack_flow)

for (yy in 1:length(years)){

  sow_data_dry = rbind(sow_data_dry, data.frame(year = years[yy], DOY = sow_dry[,yy][!is.na(sow_dry[,yy])]))
  sow_data_wet = rbind(sow_data_wet, data.frame(year = years[yy], DOY = sow_wet[,yy][!is.na(sow_wet[,yy])]))

  eos_data_dry = rbind(eos_data_dry, data.frame(year = years[yy], DOY = eos_dry[,yy][!is.na(eos_dry[,yy])]))
  eos_data_wet = rbind(eos_data_wet, data.frame(year = years[yy], DOY = eos_wet[,yy][!is.na(eos_wet[,yy])]))

  flow_data_dry = rbind(flow_data_dry, data.frame(year = years[yy], DOY = flow_dry[,yy][!is.na(flow_dry[,yy])]))
  flow_data_wet = rbind(flow_data_wet, data.frame(year = years[yy], DOY = flow_wet[,yy][!is.na(flow_wet[,yy])]))

}



sowdry_stats = summarise(group_by(sow_data_dry, year), mean = mean(DOY), sd = sd(DOY), min = min(DOY), max = max(DOY),N = length (DOY))
sowwet_stats = summarise(group_by(sow_data_wet, year), mean = mean(DOY), sd = sd(DOY), min = min(DOY), max = max(DOY),N = length (DOY))

eosdry_stats = summarise(group_by(eos_data_dry, year), mean = mean(DOY), sd = sd(DOY), min = min(DOY), max = max(DOY), N = length (DOY))
eoswet_stats = summarise(group_by(eos_data_wet, year), mean = mean(DOY), sd = sd(DOY), min = min(DOY), max = max(DOY), N = length (DOY))

flowdry_stats = summarise(group_by(flow_data_dry, year), mean = mean(DOY), sd = sd(DOY), N = length (DOY), min = min(DOY), max = max(DOY),N = length (DOY))
flowwet_stats = summarise(group_by(flow_data_wet, year), mean = mean(DOY), sd =sd(DOY), N = length (DOY), min = min(DOY), max = max(DOY),N = length (DOY))

save(sowdry_stats,eosdry_stats,flowdry_stats, sowwet_stats, eoswet_stats, flowwet_stats, file = 'D:/Temp/PhenoRice/Processing/SEN/Outputs/new_Elab2/sow_rasters/Statistics.RData')

#
# sow_data_all = rbind(sow_data_dry, sow_data_wet)
# sow_data_all$Date = lb_doytodate(sow_data_all$DOY, 2000)
# p = ggplot(sow_data_all) + lb_theme_bw()
# p = p + geom_histogram(aes (x = DOY), binwidth = 8) + facet_wrap(~year)
# p
#
#
# p = ggplot(sow_data_all, aes(Date))+theme_bw()
# # p = p + geom_histogram(aes(y=100*..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(sow_data_all$Date),max(sow_data_all$Date),'8 day')), color = 'black')
# p = p + geom_histogram(aes(y =100*(..count.. / (sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(sow_data_all$Date),max(sow_data_all$Date),'8 day')), color = 'black')
# # p = p + geom_density(aes(y = 100*(..count../(sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))), position = 'identity', fill = 'transparent', adjust = 1.2)
# p = p + theme(plot.title = element_text(vjust = 1, hjust = 0)) + facet_wrap(~year)
# p2_mins = p + theme(axis.title = element_text(size = 20)) + ylab('Frequency [%]') +
#   scale_x_date(breaks = date_breaks('2 month'), labels = date_format("%b")) +
#   theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))
# p2_mins + theme(axis.text = element_text(size = 18)) + theme(strip.text.x= element_text(size = 18))
#
#
# in_areafile = "D:/Documents/Personal/Articles/abstracts/ESA_LP_2016/Poster/Areas.csv"
# areadata = read_delim(in_areafile, delim = ';')
# # areamelt = melt(areadata, id.vars = c("Year", "MOD_Winter","MOD_total","MOD_Summer"))
# # names (areamelt)[6] = 'x'
# # areamelt = melt(areamelt, id.vars = c("Year", "variable","x"))
# # names (areamelt)[5] = 'y'
# areadata$yy = substr(areadata$Year,3,4)
#
# ggplot(data = areamelt, aes(x = x, y = y, label=y))+
#   stat_smooth(geom="text",method="lm",hjust=0,parse=TRUE) +
#   geom_smooth(method="lm",se=FALSE) +
#   geom_point() + facet_wrap(~variable)
#
#
# p = ggplot(areadata)
# p = p + geom_point(aes(y = MOD_Winter, x = Stat_winter), size = 5)
# p = p + geom_text(aes(y = MOD_Winter, x = Stat_winter, label = yy), hjust=0, vjust=1.1, size = 7)
# p = p + geom_smooth(aes(y = MOD_Winter, x = Stat_winter),method = "lm", se=FALSE, color="orange", formula = y~x, lty = 'dashed')
# p = p + lb_theme_bw() + xlim(-50,100) + ylim (-50,100)
# # p = p + geom_hline(yintercept = 0) + geom_vline(xintercept = 0 )
# p = p + ylab("Phenorice Rice Area\n % difference with 2003 values")
# p = p + xlab("Official Rice Area\n % difference with 2003 values")
# p = p + geom_text(aes(x = -65, y = 130, label = lm_eqn(lm(MOD_Winter ~ Stat_winter,areadata))), parse = TRUE, size = 7)
# p_wet= p + theme(axis.title = element_text(size = 20),
#     axis.text = element_text(size = 20),
#     plot.title = element_text(size = 20))
#
# p = ggplot(areadata)
# p = p + geom_point(aes(y = MOD_Summer, x = Stat_Summer), color = 'red', size = 5)
# p = p + geom_text(aes(y = MOD_Summer, x = Stat_Summer, label = yy), hjust=0, vjust=1.1, size = 7)
# p = p + geom_smooth(aes(y = MOD_Summer, x = Stat_Summer),method = "lm", se=FALSE, color="orange", formula = y~x, lty = 'dashed')
# p = p + lb_theme_bw() + xlim(-50,900) + ylim (-50,900)
# # p = p + geom_hline(yintercept = 0) + geom_vline(xintercept = 0 )
# p = p + ylab("Phenorice Rice Area\n % difference with 2003 values")
# p = p + xlab("Official Rice Area\n % difference with 2003 values")
# p = p + geom_text(aes(x = -65, y = 130, label = lm_eqn(lm(MOD_Summer ~ Stat_Summer,areadata))), parse = TRUE, size = 7)
# p_dry= p + theme(axis.title = element_text(size = 20),
#                  axis.text = element_text(size = 20),
#                  plot.title = element_text(size = 20))
#
# grid.arrange(p_wet, p_dry, ncol = 2)
#
# p = p + geom_point(aes(y = MOD_total, x = Stat_Total), color = 'orange')
# p = p + geom_text(aes(y = MOD_total, x = Stat_Total, label = Year), hjust=0, vjust=1.1)
#
#
#
# lm_eqn = function(m) {
#
#   l <- list(a = format(coef(m)[1], digits = 2),
#             b = format(abs(coef(m)[2]), digits = 2),
#             r2 = format(summary(m)$r.squared, digits = 3));
#
#   if (coef(m)[2] >= 0)  {
#     eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
#   } else {
#     eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
#   }
#
#   as.character(as.expression(eq));
# }
#
