library(readxl)
library(raster)
library(lubridate)
library(sp)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(ggpmisc)

Wheat = readRDS('DHW/Wheat_re_df.rds')

Wheat = Wheat[c('Yield', "site", 'name', "year", "lon", "lat", "plant", "JT_das", "HD_das", "MT_das","PTJT_GDD", "JTHD_GDD", "HDMT_GDD", "PTJT_EDD", "JTHD_EDD", "HDMT_EDD", "PTJT_FDD", "JTHD_FDD", "HDMT_FDD", "PTJT_Prec", "JTHD_Prec",  "HDMT_Prec")]

Wheat$JTdate = as.Date(Wheat$plant)+Wheat$JT_das
							 plant
Wheat$HDdate = as.Date(Wheat$plant)+Wheat$HD_das
							 plant
Wheat$MTdate = as.Date(Wheat$plant)+Wheat$MT_das
							 plant
Wheat$PTdate = as.Date(Wheat$plant)


Wheat$JTdoy = format((Wheat$JTdate), "%m-%d")
Wheat$HDdoy = format((Wheat$HDdate), "%m-%d")
Wheat$MTdoy = format((Wheat$MTdate), "%m-%d")
Wheat$PTdoy = format((Wheat$PTdate), "%m-%d")

##################
##################
##################
Wheat$JT_DOY= yday(Wheat$JTdate)
Wheat$HD_DOY= yday(Wheat$HDdate)
Wheat$MT_DOY= yday(Wheat$MTdate)
Wheat$PT_DOY= yday(Wheat$PTdate)


PNY_Mean = Wheat[Wheat$site %in% Fit_YSoclivars$site,] %>% group_by(site,name) %>% summarise_at(vars('lon','lat',"JT_DOY", 'HD_DOY', 'MT_DOY','PT_DOY'),list(mean));

coordinates(PNY_Mean) = ~lon+lat
proj4string(PNY_Mean) <- CRS("+init=epsg:4480")
PNY_Mean_sf  =  st_as_sf(PNY_Mean,coords = 1:2)

# st_crs(PNY_Mean_sf)$proj4string = st_transform(PNY_Mean_sf, crs = st_crs(Provience_line))

# plot(wheat_are)
# plot(PNY_Mean,add = T)
PT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name!='Spring wheat',],aes(color=PT_DOY, size = PT_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(260,310,5)),
                        range = c(.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(260,310,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('PT_DOY','PT_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(a) Planting')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

JT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name!='Spring wheat',],
                             aes(color=JT_DOY, size = JT_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(70,120,5)),
                        range = c(.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(70,120,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('JT_DOY','JT_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(b) Jointing')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

HD_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name!='Spring wheat',],
                             aes(color=HD_DOY, size = HD_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(100,140,5)),
                        range = c(0.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(100,140,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('HD_DOY','HD_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(c) Heading')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

MT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name!='Spring wheat',],
                             aes(color=MT_DOY, size = MT_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(140,200,5)),
                        range = c(0.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(140,200,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('HD_DOY','HD_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(d) Heading')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S2.tiff',
     units = "cm", width=16,height=9.7, res = 1500, compression = 'lzw',pointsize = 11)

ggdraw() +
  draw_plot(PT_ST_gg, x = -0.01, y = 0.48, width =0.515, height = 0.56) +
  draw_plot(JT_ST_gg, x = 0.48,  y = 0.48, width =0.515, height = 0.56)+
  draw_plot(HD_ST_gg, x = -0.01, y = -0.02, width =0.515, height = 0.56)+
  draw_plot(MT_ST_gg, x = 0.48,  y = -0.02, width =0.515, height = 0.56)
dev.off()




SWPT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name=='Spring wheat',],
                             aes(color=PT_DOY, size = PT_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(80,120,5)),
                        range = c(.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(80,120,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('PT_DOY','PT_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(a) Planting')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

SWJT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name=='Spring wheat',],
                             aes(color=JT_DOY, size = JT_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(120,170,5)),
                        range = c(.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(120,170,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('JT_DOY','JT_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(b) Jointing')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

SWHD_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name=='Spring wheat',],
                             aes(color=HD_DOY, size = HD_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(140,190,5)),
                        range = c(0.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(140,190,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('HD_DOY','HD_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(c) Heading')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

SWMT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name=='Spring wheat',],
                             aes(color=MT_DOY, size = MT_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(180,240,5)),
                        range = c(0.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(180,240,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('HD_DOY','HD_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(d) Heading')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S3.tiff',
     units = "cm", width=16,height=9.7, res = 1500, compression = 'lzw',pointsize = 11)

ggdraw() +
  draw_plot(SWPT_ST_gg, x = -0.01, y = 0.48, width =0.515, height = 0.56) +
  draw_plot(SWJT_ST_gg, x = 0.48,  y = 0.48, width =0.515, height = 0.56)+
  draw_plot(SWHD_ST_gg, x = -0.01, y = -0.02, width =0.515, height = 0.56)+
  draw_plot(SWMT_ST_gg, x = 0.48,  y = -0.02, width =0.515, height = 0.56)
dev.off()
##################
##################
##################



####################
###################step 2
###################
Wheat$site[Wheat$site==54398] = 54416
site    =  unique(Wheat$site) 


Widow_df = NULL
for(i in 1:length(site)){
  files = list.dirs('Data/ERA5/', full.names = T)
  
  temp_loc = Wheat[Wheat$site == site[i], ]
  
  coordinates(temp_loc) = ~  lon +lat
  
  T2m = NULL
  for (j in 2:length(ldis)) {
    
    path  = paste0(files[j] ,'/data_stream-oper.nc')
    t2m   = brick(path, varname = 't2m')
    temp_t2m = data.frame(t(extract(t2m, temp_loc))[,1])
    colnames(temp_t2m)[1] = 'value'
    
    temp_t2m$date = as_datetime(c(t2m@z$`valid_time (seconds since 1970-01-01)`))
    
    T2m  = rbind(T2m, temp_t2m) 
    
  }
  
  
  pathp = 'Data/ERA5/Prcp/'
  pathps = list.files(pathp) 
  
  Prceps = NULL
  for (k in 20:58) {
    prec_root = paste0(pathp,pathps[k])
    
    prec = brick(prec_root)
    
    temp_prec = data.frame(t(extract(prec, temp_loc))[,1])
    colnames(temp_prec)[1] = 'value'
    
    temp_prec$date = as_datetime(c(prec@z$`Date/time`))
    
    Prceps = rbind(Prceps, temp_prec)
  }
  
  temp_lox = Wheat[Wheat$site == site[i], ]
  
  temp_margin = Wheat_margin[Wheat_margin$Provience==temp_lox$Provience[1],]
  
  TP = NULL
  for (n in 1:length(temp_lox$Provience)) {
    
    if(temp_margin$JTdoy_fn1>temp_margin$JTdoy_fn2&temp_margin$JTdoy_fn1>="10-01"){
      year1 = temp_lox$year[n]-1
      year2 = temp_lox$year[n] 
    }else{
      year1 = temp_lox$year[n]
      year2 = temp_lox$year[n] 
    }
    
    JT_int =  which(T2m$date %within% interval(as.Date(paste0(year1,'-',temp_margin$JTdoy_fn1)), 
                                               as.Date(paste0(year2,'-',temp_margin$JTdoy_fn2))))
    
    HD_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn2))))
    
    MT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn2))))
    
    PT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn2))))
    
    MK_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$MKdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$MKdoy_fn2))))
    
    DT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$DTdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$DTdoy_fn2))))
    
    
    
    JT_inp =  which(Prceps$date %within% interval(as.Date(paste0(year1,'-',temp_margin$JTdoy_fn1)), 
                                                  as.Date(paste0(year2,'-',temp_margin$JTdoy_fn2))))
    
    HD_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn2))))
    
    MT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn2))))
    
    PT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn2))))
    
    MK_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$MKdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$MKdoy_fn2))))
    
    DT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$DTdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$DTdoy_fn2))))
    
    
    
    JT_t = mean(T2m$value[JT_int]-273.15)
    HD_t = mean(T2m$value[HD_int]-273.15)
    MT_t = mean(T2m$value[MT_int]-273.15)
    PT_t = mean(T2m$value[PT_int]-273.15)
    MK_t = mean(T2m$value[MK_int]-273.15)
    DT_t = mean(T2m$value[DT_int]-273.15)
    
    JT_p = sum(Prceps$value[JT_inp])
    HD_p = sum(Prceps$value[HD_inp])
    MT_p = sum(Prceps$value[MT_inp])
    PT_p = sum(Prceps$value[PT_inp])
    MK_p = sum(Prceps$value[MK_inp])
    DT_p = sum(Prceps$value[DT_inp])
    
    temp_TP = cbind(JT_t,HD_t,MT_t,PT_t, MK_t,DT_t,
                    JT_p, HD_p,MT_p,PT_p, MK_p,DT_p)
    
    TP = rbind(TP, temp_TP)
    
  }
  
  temp_lox = cbind(temp_lox, TP)
  
  Widow_df = rbind(Widow_df, temp_lox)
}

###############
############### step 3
###############
###################WWRF
library(randomForest)
Widow_df$JTDoy = yday(Widow_df$JTdate)
WWJTdoyrf <- randomForest(JTDoy~JT_p+JT_t+lat+year, Widow_df[Widow_df$name=='Winter wheat',])


Widow_df$HDDoy = yday(Widow_df$HDdate)
WWHDdoyrf <- randomForest(HDDoy~HD_p+HD_t+lat+year, Widow_df[Widow_df$name=='Winter wheat',])

Widow_df$MTDoy = yday(Widow_df$MTdate)
WWMTdoyrf <- randomForest(MTDoy~MT_p+MT_t+lat+year, Widow_df[Widow_df$name=='Winter wheat',])


Widow_df$PTDoy = yday(Widow_df$PTdate)
WWPTdoyrf <- randomForest(PTDoy~PT_p+PT_t+lat+year, Widow_df[Widow_df$name=='Winter wheat',])


Widow_df$MKDoy = yday(Widow_df$Milk_stage)
WWMKdoyrf <- randomForest(MKDoy~MK_p+MK_t+lat+year, Widow_df[Widow_df$name=='Winter wheat',])

Widow_df$DTDoy = yday(Widow_df$dough_stage)
WWDTdoyrf <- randomForest(DTDoy~DT_p+DT_t+lat+year, Widow_df[Widow_df$name=='Winter wheat',])


###################SWRF
Widow_df$JTDoy = yday(Widow_df$JTdate)
SWJTdoyrf <- randomForest(JTDoy~JT_p+JT_t+lat+year, Widow_df[Widow_df$name=='Spring wheat',])


Widow_df$HDDoy = yday(Widow_df$HDdate)
SWHDdoyrf <- randomForest(HDDoy~HD_p+HD_t+lat+year, Widow_df[Widow_df$name=='Spring wheat',])

Widow_df$MTDoy = yday(Widow_df$MTdate)
SWMTdoyrf <- randomForest(MTDoy~MT_p+MT_t+lat+year, Widow_df[Widow_df$name=='Spring wheat',])


Widow_df$PTDoy = yday(Widow_df$PTdate)
SWPTdoyrf <- randomForest(PTDoy~PT_p+PT_t+lat+year, Widow_df[Widow_df$name=='Spring wheat',])

Widow_df$MKDoy = yday(Widow_df$Milk_stage)
SWMKdoyrf <- randomForest(MKDoy~MK_p+MK_t+lat+year, Widow_df[Widow_df$name=='Spring wheat',])

Widow_df$DTDoy = yday(Widow_df$dough_stage)
SWDTdoyrf <- randomForest(DTDoy~DT_p+DT_t+lat+year, Widow_df[Widow_df$name=='Spring wheat',])

nameRF = randomForest(factor(name)~Provience+lat+lon+PT_p+PT_t+JT_p+JT_t+HD_p+HD_t+MK_p+MK_t+DT_p+DT_t+MT_p+MT_t+year, Widow_df,proximity=TRUE)

###############RF train plot
Ph_gg_df = data.frame(predicted = c(WWPTdoyrf$predicted, WWJTdoyrf$predicted,WWHDdoyrf$predicted,WWMTdoyrf$predicted,
                                    SWPTdoyrf$predicted, SWJTdoyrf$predicted,SWHDdoyrf$predicted,SWMTdoyrf$predicted), 
                      Obs = c(Widow_df[Widow_df$name=='Winter wheat','PTDoy'], Widow_df[Widow_df$name=='Winter wheat','JTDoy'],
                              Widow_df[Widow_df$name=='Winter wheat','HDDoy'], Widow_df[Widow_df$name=='Winter wheat','MTDoy'],
                              Widow_df[Widow_df$name=='Spring wheat','PTDoy'], Widow_df[Widow_df$name=='Spring wheat','JTDoy'],
                              Widow_df[Widow_df$name=='Spring wheat','HDDoy'], Widow_df[Widow_df$name=='Spring wheat','MTDoy']),
                      stage = c(rep(c('Planting (doy)','Jointing (doy)','Heading (doy)','Maturity (doy)'),each=length(WWPTdoyrf$predicted)),
                                rep(c('Planting (doy)','Jointing (doy)','Heading (doy)','Maturity (doy)'),each=length(SWPTdoyrf$predicted))),
                      group = c(rep('Winter wheat', length(WWPTdoyrf$predicted)*4),rep('Spring wheat', length(SWPTdoyrf$predicted)*4)))

ph_rf_gg = ggplot(data = Ph_gg_df, aes(x = Obs, y = predicted,fill = stage,color = stage)) +
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = stage))+
  geom_point(alpha = 0.3, size =0.6) +xlab('Observations') +ylab('Predictions')+
  geom_smooth(method = "lm", se=T,color = 'gray75', formula = y ~ x) +
  scale_fill_manual(name = '',values = brewer.pal(8, "Dark2"))+
  scale_color_manual(name = '',values = brewer.pal(8, "Dark2"))+
  stat_poly_line(color = 'gray75') +theme_bw()+
  stat_poly_eq(use_label(c("eq", "R2")),color = 'black',size = 2) +
  facet_wrap(stage~group, scales = 'free',ncol = 4)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1.45,'cm'),
        # axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        legend.position = 'none',
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.18,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 6),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

# HDW_type_YTrend_gg

tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S6.tiff',
     units = "cm", width=15,height=10, res = 1500, compression = 'lzw',pointsize = 11)
ph_rf_gg
dev.off()



###############
############### step 4
###############

wheat_are = raster('E:\\Post PHD documents\\Crop yield loss at DHW\\Crop Area\\Crop area.tif')
plot(wheat_are)

stations       = readxl::read_xls('E:/Post PHD documents/Crop yield loss at DHW/data/sitstation.xls') 
colnames(stations)       = stations[2,]
stations       = stations[-c(1:2),]

colnames(stations)[c(1:2,9:10)] = c('Provience', 'site','lat','lon')

stations$site  = as.numeric(stations$site)
stations       = na.omit(stations)

coordinates(stations) = ~  lon +lat
plot(stations, add = T) 

wheat_stations = data.frame(extract(wheat_are, stations)) 

wheat_sts      = cbind(wheat_stations, stations)

colnames(wheat_sts)[1] = 'values'

# plot(wheat_are)
# coordinates(wheat_sts) = ~  lon +lat
# plot(wheat_sts, add =T)
# setdiff(unique(wheat_sts$site),site)

wheat_sts = wheat_sts[!(wheat_sts$site %in% site), ]


Incer_windF = function(i){
  library(readxl)
  library(raster)
  library(lubridate)
  library(sp)
  library(tidyverse)
  library(tidyr)
  detach("package:tidyr", unload = TRUE)
  
   site =  unique(wheat_sts$site)
   setwd('E:/Post PHD documents/Crop yield loss at DHW/')
  
    files = list.dirs('E:/Post PHD documents/Crop yield loss at DHW/Data/ERA5/', full.names = T)
    
    temp_loc = wheat_sts[wheat_sts$site == site[i], ]
    
    coordinates(temp_loc) = ~  lon +lat
    
    T2m = NULL
    for (j in 2:length(files)) {
      
      path  = paste0(files[j] ,'/data_stream-oper.nc')
      t2m   = brick(path, varname = 't2m')
      temp_t2m = data.frame(t(extract(t2m, temp_loc))[,1])
      colnames(temp_t2m)[1] = 'value'
      
      temp_t2m$date = as_datetime(c(t2m@z$`valid_time (seconds since 1970-01-01)`))
      
      T2m  = rbind(T2m, temp_t2m) 
      
    }

    pathp = 'Data/ERA5/Prcp/'
    pathps = list.files(pathp) 
    
    Prceps = NULL
    for (k in 20:58) {
      prec_root = paste0(pathp,pathps[k])
      
      prec = brick(prec_root)
      
      temp_prec = data.frame(t(extract(prec, temp_loc))[,1])
      colnames(temp_prec)[1] = 'value'
      
      temp_prec$date = as_datetime(c(prec@z$`Date/time`))
      
      Prceps = rbind(Prceps, temp_prec)
    }
    
    temp_lox = wheat_sts[wheat_sts$site == site[i], ]
    temp_lox = temp_lox[colnames(temp_lox)[c(2,3,10,11)]]
    temp_lox = eval(parse(text = paste0('rbind(', paste0(rep('temp_lox',38),collapse = ','),')')))
      
    temp_lox$year = 1981:2018
    temp_margin = Wheat_margin[Wheat_margin$Provience==temp_lox$Provience[1],]
    
    TP = NULL
    for (n in 1:length(temp_lox$Provience)) {
      
      if(temp_margin$JTdoy_fn1>temp_margin$JTdoy_fn2&temp_margin$JTdoy_fn1>="10-01"){
        year1 = temp_lox$year[n]-1
        year2 = temp_lox$year[n] 
      }else{
        year1 = temp_lox$year[n]
        year2 = temp_lox$year[n] 
      }
      
      JT_int =  which(T2m$date %within% interval(as.Date(paste0(year1,'-',temp_margin$JTdoy_fn1)), 
                                                 as.Date(paste0(year2,'-',temp_margin$JTdoy_fn2))))
      
      HD_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn1)), 
                                                 as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn2))))
      
      MT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn1)), 
                                                 as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn2))))
      
      PT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn1)), 
                                                 as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn2))))
      
      MK_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$MKdoy_fn1)), 
                                                 as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$MKdoy_fn2))))
      
      DT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$DTdoy_fn1)), 
                                                 as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$DTdoy_fn2))))
      
      
      
      JT_inp =  which(Prceps$date %within% interval(as.Date(paste0(year1,'-',temp_margin$JTdoy_fn1)), 
                                                    as.Date(paste0(year2,'-',temp_margin$JTdoy_fn2))))
      
      HD_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn1)), 
                                                    as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn2))))
      
      MT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn1)), 
                                                    as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn2))))
      
      PT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn1)), 
                                                    as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn2))))
      
      MK_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$MKdoy_fn1)), 
                                                    as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$MKdoy_fn2))))
      
      DT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$DTdoy_fn1)), 
                                                    as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$DTdoy_fn2))))
      
      
      
      JT_t = mean(T2m$value[JT_int]-273.15)
      HD_t = mean(T2m$value[HD_int]-273.15)
      MT_t = mean(T2m$value[MT_int]-273.15)
      PT_t = mean(T2m$value[PT_int]-273.15)
      MK_t = mean(T2m$value[MK_int]-273.15)
      DT_t = mean(T2m$value[DT_int]-273.15)
      
      JT_p = sum(Prceps$value[JT_inp])
      HD_p = sum(Prceps$value[HD_inp])
      MT_p = sum(Prceps$value[MT_inp])
      PT_p = sum(Prceps$value[PT_inp])
      MK_p = sum(Prceps$value[MK_inp])
      DT_p = sum(Prceps$value[DT_inp])
      
      temp_TP = cbind(JT_t,HD_t,MT_t,PT_t, MK_t,DT_t,
                      JT_p, HD_p,MT_p,PT_p, MK_p,DT_p)
      
      TP = rbind(TP, temp_TP)
      
    }
    
    temp_lox = cbind(temp_lox, TP)
}


library(foreach)
library(doParallel)

n.cores = parallel::detectCores() - 2
#create the cluster
my.cluster = parallel::makeCluster(n.cores, type = "PSOCK")

print(my.cluster)

doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()

system.time( Intercet_wind <- foreach (
  i = 1:length(unique(wheat_sts$site)),.combine = 'rbind') %dopar% {
    temp = Incer_windF(i)
    return(temp)})



parallel::stopCluster(cl = my.cluster)

Intercet_wind$name = predict(nameRF,Intercet_wind)

unique(Widow_df$Provience[Widow_df$name %in% 'Spring wheat'])
unique(Widow_df$Provience[Widow_df$name %in% 'Winter wheat'])

unique(Intercet_wind$Provience[Intercet_wind$name %in% 'Spring wheat'])
unique(Intercet_wind$Provience[Intercet_wind$name %in% 'Winter wheat'])



Intercet_wind$JTDoy =  ifelse(Intercet_wind$name=='Spring wheat',
                              round(predict(SWJTdoyrf,Intercet_wind),0),
                              round(predict(WWJTdoyrf,Intercet_wind),0))

Intercet_wind$HDDoy =  ifelse(Intercet_wind$name=='Spring wheat',
                              round(predict(SWHDdoyrf,Intercet_wind),0),
                              round(predict(WWHDdoyrf,Intercet_wind)))

Intercet_wind$MTDoy =  ifelse(Intercet_wind$name=='Spring wheat',
                              round(predict(SWMTdoyrf,Intercet_wind),0),
                              round(predict(WWMTdoyrf,Intercet_wind),0))

Intercet_wind$PTDoy =  ifelse(Intercet_wind$name=='Spring wheat',
                              round(predict(SWPTdoyrf,Intercet_wind),0),
                              round(predict(WWPTdoyrf,Intercet_wind),0))
                              
Intercet_wind$MKDoy =  ifelse(Intercet_wind$name=='Spring wheat',
                              round(predict(SWMKdoyrf,Intercet_wind),0),
                              round(predict(WWMKdoyrf,Intercet_wind),0))
                              
Intercet_wind$DTDoy =  ifelse(Intercet_wind$name=='Spring wheat',
                              round(predict(SWDTdoyrf,Intercet_wind),0),
                              round(predict(WWDTdoyrf,Intercet_wind),0))

Intercet_wind$JTdate = as.Date(Intercet_wind$JTDoy, origin = as.Date(paste0(Intercet_wind$year,'-01-01')))
Intercet_wind$HDdate = as.Date(Intercet_wind$HDDoy, origin = as.Date(paste0(Intercet_wind$year,'-01-01')))
Intercet_wind$MTdate = as.Date(Intercet_wind$MTDoy, origin = as.Date(paste0(Intercet_wind$year,'-01-01')))
Intercet_wind$PTdate = ifelse(Intercet_wind$name=='Spring wheat',
                             as.character(as.Date(Intercet_wind$PTDoy, 
                                     origin = as.Date(paste0(Intercet_wind$year,'-01-01')))),
                             as.character(as.Date(Intercet_wind$PTDoy, 
                                     origin = as.Date(paste0(c(Intercet_wind$year-1),'-01-01')))))
Intercet_wind$MKdate = as.Date(Intercet_wind$MKDoy, origin = as.Date(paste0(Intercet_wind$year,'-01-01')))
Intercet_wind$DTdate = as.Date(Intercet_wind$DTDoy, origin = as.Date(paste0(Intercet_wind$year,'-01-01')))

Intercet_wind$Yield = NA

Intercet_wind$plant  = as.Date(as.character(Intercet_wind$PTdate))


Intercet_wind$JT_das    = NA 
Intercet_wind$HD_das    = NA 
Intercet_wind$MT_das    = NA 
Intercet_wind$PTJT_GDD  = NA
Intercet_wind$JTHD_GDD  = NA
Intercet_wind$HDMT_GDD  = NA
Intercet_wind$PTJT_EDD  = NA
Intercet_wind$JTHD_EDD  = NA
Intercet_wind$HDMT_EDD  = NA
Intercet_wind$PTJT_FDD  = NA
Intercet_wind$JTHD_FDD  = NA
Intercet_wind$HDMT_FDD  = NA
Intercet_wind$PTJT_Prec = NA
Intercet_wind$JTHD_Prec = NA
Intercet_wind$HDMT_Prec = NA


colnames(Widow_df)[c(33:34)] = c("MKdate", "DTdate")
Wheat_dhwdf = rbind(Intercet_wind,Widow_df[,-c(3,29:32,35:36)])
Wheat_dhwdf  = Wheat_dhwdf  %>% distinct()


Intercet_df = Intercet_wind

Intercet_df$JT_das = Intercet_wind$JTdate-as.Date(Intercet_wind$PTdate)


write.table(Wheat_dhwdf, 'Data/Wheat_dhwdf.txt')

##############
#############
##############

WWPhenology_mean = Wheat_dhwdf[Wheat_dhwdf$name=="Winter wheat",] %>% group_by(site) %>% summarise_at(vars("PTDoy","JTDoy","HDDoy","MTDoy"),list(mean));

PT_mean_sf = left_join(help_data_sf, WWPhenology_mean[,c(1,2)], by = c('site'))

WWPTdoy_mean_gg =   ggplot() +geom_sf(data = PT_mean_sf,aes(fill=PTDoy), 
                                    linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(259,315,5)), labels =  c(seq(259,315,5)),
                    limits = c(259,315),na.value = "white",
                    values = scales::rescale(seq(259,315,5)),name = '(doy)')+
  labs(subtitle = bquote((a)~Planting))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


JT_mean_sf = left_join(help_data_sf, WWPhenology_mean[,c(1,3)], by = c('site'))

WWJTdoy_mean_gg =   ggplot() +geom_sf(data = JT_mean_sf,aes(fill=JTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(50,132,5)), labels =  c(seq(50,132,5)),
                    limits = c(50,132),na.value = "white",
                    values = scales::rescale(seq(50,132,5)),name = '(doy)')+
  labs(subtitle = bquote((b)~Jointing))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


HD_mean_sf = left_join(help_data_sf, WWPhenology_mean[,c(1,4)], by = c('site'))

WWHDdoy_mean_gg =   ggplot() +geom_sf(data = HD_mean_sf,aes(fill=HDDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(90,160,5)), labels =  c(seq(90,160,5)),
                    limits = c(90,160),na.value = "white",
                    values = scales::rescale(seq(90,160,5)),name = '(doy)')+
  labs(subtitle = bquote((c)~Heading))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                      # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


MT_mean_sf = left_join(help_data_sf, WWPhenology_mean[,c(1,5)], by = c('site'))

WWMTdoy_mean_gg =   ggplot() +geom_sf(data = MT_mean_sf,aes(fill=MTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(130,205,5)), labels =  c(seq(130,205,5)),
                    limits = c(130,205),na.value = "white",
                    values = scales::rescale(seq(130,205,5)),name = '(doy)')+
  labs(subtitle = bquote((d)~Maturity))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 4.5,                     # Vertical adjustment
                                     lineheight = 3,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S7.tiff',
     units = "cm", width=16,height=9.7, res = 1500, compression = 'lzw',pointsize = 11)
ggdraw() +
  draw_plot(WWPTdoy_mean_gg, x = -0.01, y = 0.48, width =0.515, height = 0.56) +
  draw_plot(WWJTdoy_mean_gg, x = 0.48,  y = 0.48, width =0.515, height = 0.56)+
  draw_plot(WWHDdoy_mean_gg, x = -0.01, y = -0.02, width =0.515, height = 0.56)+
  draw_plot(WWMTdoy_mean_gg, x = 0.48,  y = -0.02, width =0.515, height = 0.56)
dev.off() 


############
############
###########

SWPhenology_mean = Wheat_dhwdf[Wheat_dhwdf$name=="Spring wheat",] %>% group_by(site) %>% summarise_at(vars("PTDoy","JTDoy","HDDoy","MTDoy"),list(mean));

SPT_mean_sf = left_join(help_data_sf, SWPhenology_mean[,c(1,2)], by = c('site'))

SWPTdoy_mean_gg =   ggplot() +geom_sf(data = SPT_mean_sf,aes(fill=PTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(60,123,5)), labels =  c(seq(60,123,5)),
                    limits = c(60,123),na.value = "white",
                    values = scales::rescale(seq(60,123,5)),name = '(doy)')+
  labs(subtitle = bquote((a)~Planting))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


SJT_mean_sf = left_join(help_data_sf, SWPhenology_mean[,c(1,3)], by = c('site'))

SWJTdoy_mean_gg =   ggplot() +geom_sf(data = SJT_mean_sf,aes(fill=JTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(114,174,5)), labels =  c(seq(114,174,5)),
                    limits = c(114,174),na.value = "white",
                    values = scales::rescale(seq(114,174,5)),name = '(doy)')+
  labs(subtitle = bquote((b)~Jointing))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


SHD_mean_sf = left_join(help_data_sf, SWPhenology_mean[,c(1,4)], by = c('site'))

SWHDdoy_mean_gg =   ggplot() +geom_sf(data = SHD_mean_sf,aes(fill=HDDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(130,195,5)), labels =  c(seq(130,195,5)),
                    limits = c(130,195),na.value = "white",
                    values = scales::rescale(seq(130,195,5)),name = '(doy)')+
  labs(subtitle = bquote((c)~Heading))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                      # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


SMT_mean_sf = left_join(help_data_sf, SWPhenology_mean[,c(1,5)], by = c('site'))

SWMTdoy_mean_gg =   ggplot() +geom_sf(data = SMT_mean_sf,aes(fill=MTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(165,250,5)), labels =  c(seq(165,250,5)),
                    limits = c(165,250),na.value = "white",
                    values = scales::rescale(seq(165,250,5)),name = '(doy)')+
  labs(subtitle = bquote((d)~Maturity))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 4.5,                     # Vertical adjustment
                                     lineheight = 3,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S8.tiff',
     units = "cm", width=16,height=9.7, res = 1500, compression = 'lzw',pointsize = 11)
ggdraw() +
  draw_plot(SWPTdoy_mean_gg, x = -0.01, y = 0.48, width =0.515, height = 0.56) +
  draw_plot(SWJTdoy_mean_gg, x = 0.48,  y = 0.48, width =0.515, height = 0.56)+
  draw_plot(SWHDdoy_mean_gg, x = -0.01, y = -0.02, width =0.515, height = 0.56)+
  draw_plot(SWMTdoy_mean_gg, x = 0.48,  y = -0.02, width =0.515, height = 0.56)
dev.off()




##############
#############
##############
