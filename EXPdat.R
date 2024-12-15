library(readxl)
library(raster)
library(lubridate)
library(sp)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(fixest)
library(cowplot)
library(tsibble)
library(feasts)
library(stats)
library(utils)
library(dplR)
library(ggpubr)
library(distributional)
library(ggdist)
library(ggpattern)


EXDat = read_xlsx('experiment data.xlsx', sheet = 'Sheet3') 

EXDat = EXDat[EXDat$site %in% Fit_YSoclivars$site,]

EXDat_mer = merge(EXDat[,c(1,5,6,24)], Fit_YSoclivars,  by=c('site','year'))
EXDat_mer = na.omit(EXDat_mer)
EXDat_mer = EXDat_mer[EXDat_mer$Yield_S!=0,]

EXDat_sp = EXDat_mer %>% group_by(site) %>% summarise_at(vars('lon','lat','Yield_S'),list(mean));

coordinates(EXDat_sp) = ~lon+lat
proj4string(EXDat_sp) <- CRS("+init=epsg:4480")
EXDat_sp_sf  =  st_as_sf(EXDat_sp,coords = 1:2)

# st_crs(PNY_Mean_sf)$proj4string = st_transform(PNY_Mean_sf, crs = st_crs(Provience_line))

# plot(wheat_are)
# plot(PNY_Mean,add = T)
EXDat_MY_gg = ggplot() +geom_sf(data = EXDat_sp_sf,aes(color=Yield_S, size = Yield_S), 
                             linewidth = 0.1, alpha = 0.5) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = bquote((kg~ha^-1)))+
  scale_size_continuous(breaks = c(seq(5000,15000,1000)),
                        range = c(.1,3),name = bquote((kg~ha^-1)))+
  scale_color_stepsn(colors = c(brewer.pal(9, "YlGn")),
                     breaks =  c(seq(5000,15000,1000)),
                     name =bquote((kg~ha^-1)))+
  guides(color= guide_legend(c('Yield_S','Yield_S')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(a) Mean yield of field trials of 2006-2018')+
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
        legend.key.size = unit(0.6,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.89, 0.36),
        #legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

EXDat_mean = EXDat_mer %>% group_by(year) %>% summarise_at(vars('Yield_S'),list(mean,sd));

EXDat_Mean_gg = ggplot(EXDat_mean, aes(x=year, y=fn1)) + 
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), width=.1) +
  ylab(bquote('2006-2018 yield'~(kg~ha^-1)))+xlab('Year')+
  geom_line() + geom_point()+labs(subtitle = '(b)')+
  scale_color_brewer(palette="Paired")+theme_bw()+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(2.0,'cm'),
        axis.title = element_text(size = 7.0),
        axis.text = element_text(size = 6.0),
        legend.position = c(0.60, .85),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S5.tiff',
     units = "cm", width=16,height=6, res = 1500, compression = 'lzw',pointsize = 11)

ggdraw() +
  draw_plot(EXDat_MY_gg, x = -0.01, y = 0.0,  width =0.56, height = 1) +
draw_plot(EXDat_Mean_gg, x = 0.54,  y = 0.0,  width =0.46, height = 1)

dev.off()

############################
###########################
############################
library(ggfixest)
EXDat_mer_SD = EXDat_mer %>% mutate_at(c(colnames(EXDat_mer)[c(4,50:161,164:166)]), ~(scale(.) %>% as.vector))


BootReg_EX    = function(fmula, boot, size){
  
  fmula_1    = as.formula(paste0('Yield_S~',paste0(colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)]),collapse = '+'),'+site+year+Culs'))
  
  fmula_2    = as.formula(paste0('Yield_S~',paste0(colnames(Fit_YSoclivars[c(59:69,92:102,125:135,150:159)]),collapse = '+'),'+site+year+Culs'))
  
  fmula_3    = as.formula(paste0('Yield_S~',paste0(colnames(Fit_YSoclivars[c(70:80,103:113,136:145,150:159)]),collapse = '+'),'+site+year++Culs'))
  
  fmula_4    = as.formula(paste0('Yield_S~',paste0(colnames(Fit_YSoclivars[c(147:159)]),collapse = '+'),
                               '+PJ_Prcp2+JH_Prcp2+HM_Prcp2+site+year+Culs'))
  
  fmula_5    = as.formula(paste0('Yield_S~',paste0(colnames(Fit_YSoclivars[c(150:159)]),collapse = '+'),'+site+year+Culs'))
  fmula_6    = as.formula(paste0('Yield_S~',paste0(colnames(Fit_YSoclivars[c(157:159)]),collapse = '+'),'+site+year++Culs'))
  
  # +year+site
  reg_data = EXDat_mer_SD
  # reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  reg_data$site = as.character(reg_data$site)
  reg_data$year = as.character(reg_data$year)
  
  
  bootcoefs = c()
  # # boot = 100
  # # size = 6000
  # Rsq       = NULL
  # for (n in 1:boot) {
    
    # rsamp_data = sample(1:nrow(reg_data),size=size,replace = T)  
    # 
    # rsamp_data = reg_data[rsamp_data,]
    
    cropfit    = feols(fmula_1, data = reg_data) 
    #  resid_panel(cropfit)
    
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    # coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  # }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef #%>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean,sd));
  
  Mbootcoef$rsq  = mean(Rsq)
  
  return(Mbootcoef)
}

set.seed(1234)
a = Sys.time()

EX_fit_1 =   BootReg_EX(fmula_1, 1000, 4000)  
EX_fit_2 =   BootReg_EX(fmula_2, 1000, 4000)  
EX_fit_3 =   BootReg_EX(fmula_3, 1000, 4000)  
EX_fit_4 =   BootReg_EX(fmula_4, 1000, 4000)  
EX_fit_5 =   BootReg_EX(fmula_5, 1000, 4000)  
EX_fit_6 =   BootReg_EX(fmula_6, 1000, 4000)  

b = Sys.time()
c = b-a 
c


EX_fit_1$Model  = 'MSMT'
EX_fit_2$Model  = 'MSMM'
EX_fit_3$Model  = 'MMSB'
EX_fit_4$Model  = 'MPer'
EX_fit_5$Model  = 'MEFD'
EX_fit_6$Model  = 'MDHW'

EX_fit_df = rbind(EX_fit_1,
                  EX_fit_2,
                  EX_fit_3,
                  EX_fit_4,
                  EX_fit_5,
                  EX_fit_6)


level_order = c('PJFDD', 'JHFDD', 'JHEDD', 'HMEDD',"hdw_hour1", "hdw_hour2")


EX_df_get   = EX_fit_df[EX_fit_df$Var %in% c("JHEDD","HMEDD","PJFDD", "JHFDD","hdw_hour1", "hdw_hour2"),]
EX_df_mean  = EX_df_get  ;EX_df_mean$Estimate = EX_df_mean$Estimate*100
colnames(EX_df_mean)[2] = 'sd'
EX_df_mean$sd = EX_df_mean$sd*100
library(smplot2)
CL_df_mean  = df_mean %>% group_by(Var)  %>% summarise_at(vars("Estimate"),list(mean,sd));
EX_df_means = EX_df_mean %>% group_by(Var)  %>% summarise_at(vars("Estimate"),list(mean,sd));
CL_df_mean$Group  = 'County level'
EX_df_means$Group = 'Field trail'
colnames(CL_df_mean)[2:3] = c("Estimate", 'sd')
colnames(EX_df_means)[2:3] = c("Estimate", 'sd')
CLEX_df_means =  rbind(CL_df_mean,EX_df_means)
CLEX_df_means$col = paste0(CLEX_df_means$Var,'_', CLEX_df_means$Group)


CLEX_df_means_gg = ggplot(CLEX_df_means, aes(x = Var, y = Estimate,
                                             color = col,fill = col))+
  geom_bar_pattern(stat = "identity",alpha = 0.7, lwd= 0.05,aes(pattern = col),
                   position = position_dodge(.9),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.000001,
                   pattern_size = .05,
                   pattern_spacing = 0.1,
                   pattern_key_scale_factor = 0.1) +
  geom_errorbar(aes(ymin=Estimate-sd, ymax=Estimate+sd), width=.2,
                linewidth = 0.5, position=position_dodge(.9))+
  scale_pattern_manual(values = c("none", "stripe","none", "stripe","none", "stripe",
                                  "none", "stripe","none", "stripe","none", "stripe",
                                  "none", "stripe"),name = '') +
  scale_fill_manual(values = c("orangered2","orangered2", "orangered3","orangered3", 
                               "orangered4","orangered4",brewer.pal(11, "PRGn")[3],
                               brewer.pal(11, "PRGn")[3],brewer.pal(11, "PRGn")[2],
                               brewer.pal(11, "PRGn")[2],brewer.pal(11, "PRGn")[9],
                               brewer.pal(11, "PRGn")[9],brewer.pal(11, "PRGn")[10],
                               brewer.pal(11, "PRGn")[10]))+
  scale_color_manual(values = c("orangered2","orangered2", "orangered3","orangered3", 
                                "orangered4","orangered4",brewer.pal(11, "PRGn")[3],
                                brewer.pal(11, "PRGn")[3],brewer.pal(11, "PRGn")[2],
                                brewer.pal(11, "PRGn")[2],brewer.pal(11, "PRGn")[9],
                                brewer.pal(11, "PRGn")[9],brewer.pal(11, "PRGn")[10],
                                brewer.pal(11, "PRGn")[10]))+ 
  scale_x_discrete('Standardize climate indices',labels = c(
    "PJFDD" = bquote(FDD[pT-JT]),
    "JHFDD" = bquote(FDD[JT-HD]),
    "JHEDD" = bquote(EDD[JT-HD]),
    "HMEDD" = bquote(EDD[HD-MT]),
    "hdw_hour1" = bquote(DHW[HTLH]),
    'hdw_hour2' = bquote(DHW[PRGW]),
    "hdw_hour3" = bquote(DHW[DTWD])
  ))+ylab("Yield effects by per standard unit (%)")+theme_bw()+#ylim(-30,30)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 4),
        legend.key.size = unit(0.15,'cm'),
        panel.spacing = unit(0, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 6,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 6,angle = 0,vjust = 0.5),
        axis.title = element_text(size = 7),
        legend.position = c(10.3,0.95),
        #legend.direction = "horizontal",
        legend.key.height = unit(0.12,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 3.6),
        legend.title= element_text(size = 4),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))




EX_df_DHW_bar_gg  = ggplot(EX_df_mean[EX_df_mean$Model%in%'MSMM',],
                     aes(x = factor(Var,level_order),
                    y = Estimate, color = Var,fill = Var))+
  geom_bar(stat="identity", color="black", position=position_dodge(),alpha = 0.6) +
  geom_errorbar(aes(ymin=Estimate-sd, ymax=Estimate+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = c("orangered2", "orangered3", "orangered4",
                               brewer.pal(11, "PRGn")[c(3,2,9,10)]))+
  scale_color_manual(values = c("orangered2", "orangered3", "orangered4",
                                brewer.pal(11, "PRGn")[c(3,2,9,10)]))+ 
  scale_x_discrete('Standardize climate indices',labels = c(
    "PJFDD" = bquote(FDD[pT-JT]),
    "JHFDD" = bquote(FDD[JT-HD]),
    "JHEDD" = bquote(EDD[JT-HD]),
    "HMEDD" = bquote(EDD[HD-MT]),
    "hdw_hour1" = bquote(DHW[HTLH]),
    'hdw_hour2' = bquote(DHW[PRGW]),
    "hdw_hour3" = bquote(DHW[DTWD])
  ))+ylab("Yield loss per standard unit (%)")+
  labs(subtitle = c('(b)'))+theme_bw()+#ylim(-10,5)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 6),
        legend.key.size = unit(0.3,'cm'),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 5,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(100,0.92),
        legend.key.height = unit(0.22,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 5),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


EX_dfS          = EX_df_mean[EX_df_mean$Var %in% c("hdw_hour1","hdw_hour2"),]
# EX_dfS$Estimate = EX_dfS$Estimate*100
EX_dfS$Group = 'Field trail'

EX_DHW_sing_gg  = ggplot(EX_dfS, aes(x = Model, y = Estimate, color = Var,fill = Var))+
  geom_bar(stat="identity", color="black", position=position_dodge(),alpha = 0.6) +
  geom_errorbar(aes(ymin=Estimate-sd, ymax=Estimate+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = c(brewer.pal(11, "PRGn")[c(1,2,3)]),
                    name = '',label = c('HTLH','PRGW','DTWD') )+
  scale_color_manual(values =  c(brewer.pal(11, "PRGn")[c(1,2,3)]),
                     name = '',label = c('HTLH','PRGW','DTWD') )+ 
  scale_x_discrete('Yield models',labels = c(
    "PJFDD" = bquote(FDD[pT-JT]),
    "JHFDD" = bquote(FDD[JT-HD]),
    "JHEDD" = bquote(EDD[JT-HD]),
    "HMEDD" = bquote(EDD[HD-MT]),
    "hdw_hour1" = bquote(DHW[HTLH]),
    'hdw_hour2' = bquote(DHW[PRGW]),
    "hdw_hour3" = bquote(DHW[DTWD])
  ))+ylab("Yield loss per standard unit (%)")+
  labs(subtitle = c('(a)'))+theme_bw()+#ylim(-30,30)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 6),
        legend.key.size = unit(0.3,'cm'),
        # axis.ticks = element_blank(),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(0.90,0.93),
        legend.key.height = unit(0.22,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 5),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


Cul_efcts = EX_fit_df[!EX_fit_df$Var %in% colnames(EXDat_mer)[c(4,50:161,164:166)],]
Cul_efcts = Cul_efcts[substr(Cul_efcts$Var,1,4)!='site'&substr(Cul_efcts$Var,1,4)!='year'&Cul_efcts$Va!='(Intercept)',]
Cul_efcts = Cul_efcts %>% group_by(Var) %>% summarise_at(vars("Estimate"),list(mean));
# Cul_efcts_pca <- prcomp(t(Cul_efcts[,2]))
# 
# #Build a data frame
# pcaData <- as.data.frame(Cul_efcts_pca$x[, 1:2]) # extract first two PCs
# pcaData <- cbind(pcaData, iris$Species) # add species to df
# colnames(pcaData) <- c("PC1", "PC2", "Species") # change column names
# 
# #Plot
# ggplot(pcaData) +
#   aes(PC1, PC2, color = Species, shape = Species) + # define plot area
#   geom_point(size = 2) + # adding data points
#   coord_fixed() # fixing coordinates


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S12.tiff',
     units = "cm", width=14,height=6, res = 1500, compression = 'lzw',pointsize = 11)
CLEX_df_means_gg
dev.off()


Site_fix_df  = EX_fit_df[substr(EX_fit_df$Var,1,4)=='site',]
Site_fix_df$site = as.numeric(substr(Site_fix_df$Var,5,10))
EXDat_sp = EXDat_mer %>% group_by(site) %>% summarise_at(vars('lon','lat','Yield_S'),list(mean));
Site_fix_df_mer = left_join(Site_fix_df, EXDat_sp[,1:3], by = c('site'))

coordinates(Site_fix_df_mer) = ~lon+lat
proj4string(Site_fix_df_mer) <- CRS("+init=epsg:4480")
Site_fix_df_mer_sf  =  st_as_sf(Site_fix_df_mer,coords = 1:2)

# st_crs(PNY_Mean_sf)$proj4string = st_transform(PNY_Mean_sf, crs = st_crs(Provience_line))

# plot(wheat_are)
# plot(PNY_Mean,add = T)
Site_fix_gg = ggplot() +geom_sf(data = Site_fix_df_mer_sf,aes(color=Estimate*100, size = Estimate*100), 
                                linewidth = 0.1, alpha = 0.8) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(%)')+
  scale_size_continuous(breaks = c(seq(0,100,10)),
                        range = c(.1,3),name ='(%)')+
  scale_color_stepsn(colors = c(brewer.pal(9, "YlGn")),
                     breaks =  c(seq(0,100,10)),
                     name ='(%)')+
  guides(color= guide_legend(c('Estimate','Estimate')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+facet_wrap(.~Model)+
  theme_bw()+labs(subtitle = '(a) Random effects of site')+
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
        legend.key.size = unit(0.2,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.97, 0.23),
        #legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


EXYear_fix_df  = EX_fit_df[substr(EX_fit_df$Var,1,4)=='year',]
EXYear_fix_df$year = as.numeric(substr(EXYear_fix_df$Var,5,10))


EXYear_fix_gg = ggplot(EXYear_fix_df, aes(x= year, y = Estimate*100, color = Model))+
  geom_line(alpha = 0.6)+geom_point(size = 1.2)+theme_bw()+
  scale_color_manual(values = brewer.pal(8, "Dark2"))+
  labs(subtitle = '(b) Random effects of year')+
  ylab(bquote(Year~fixed~effects~("%")))+facet_wrap(.~Model,ncol=3)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(2.2,'cm'),
        # axis.ticks = element_blank(),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(10.63, 1.15),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S13.tiff',
     units = "cm", width=15,height=13, res = 1500, compression = 'lzw',pointsize = 11)

ggdraw() +
  draw_plot(Site_fix_gg,  x = 0, y = 0.46,  width =1, height = 0.58) +
  draw_plot(EXYear_fix_gg,x = 0, y = 0.00,  width =1, height = 0.48)
dev.off()

#####################
#####################
#####################
#####################

EX_BootReg_change    = function(fmula, boot){
  
  climvars = colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula_1   = as.formula(paste0('Yield_S~',paste0(vars,collapse = '+'),'+site+Culs'))
  
  climvars = colnames(Fit_YSoclivars[c(c(59:69,92:102,125:135,150:159))])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula_2   = as.formula(paste0('Yield_S~',paste0(vars,collapse = '+'),'+site+Culs'))
  
  
  climvars = colnames(Fit_YSoclivars[c(c(70:80,103:113,136:145,150:159))])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula_3   = as.formula(paste0('Yield_S~',paste0(vars,collapse = '+'),'+site+Culs'))
  
  
  climvars = colnames(Fit_YSoclivars[c(147:159,162:164)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula_4   = as.formula(paste0('Yield_S~',paste0(vars,collapse = '+'),'+site+Culs'))
  
  
  climvars = colnames(Fit_YSoclivars[c(150:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula_5   = as.formula(paste0('Yield_S~',paste0(vars,collapse = '+'),'+site+Culs'))
  
  climvars = colnames(Fit_YSoclivars[c(157:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula_6   = as.formula(paste0('Yield_S~',paste0(vars,collapse = '+'),'+site'))
  # 
  # +year+site
  reg_data = EXDat_mer
  # reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  # reg_data$site = as.character(reg_data$site)
  reg_data$year = as.numeric(reg_data$year)
  
  
  bootcoefs = c()
  # boot = 100
  # size = 6000
  yrs.to.samp = 2006:2018
  Rsq       = NULL
  for (n in 1:boot) {
    
    yrsamp = sample(yrs.to.samp,size= 12,replace = T)
    tempdf = reg_data %>% filter(year == yrsamp[1]) 
    
    for (k in 2:12) tempdf = rbind(tempdf, reg_data %>% filter(year == yrsamp[k]))
    
    cropfit    = feols(fmula, data = tempdf) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  }
  bootcoef = data.frame(bootcoefs)
  
  # Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean,sd));
  # 
  # Mbootcoef$rsq  = mean(Rsq)
  
  hdw_hour1perc =  -1*((bootcoef$Estimate[bootcoef$Var=='hdw_hour1'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour1:year'] * 2018) - (bootcoef$Estimate[bootcoef$Var=='hdw_hour1'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour1:year'] * 2006)) 
  hdw_hour2perc =  -1*((bootcoef$Estimate[bootcoef$Var=='hdw_hour2'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour2:year'] * 2018) - (bootcoef$Estimate[bootcoef$Var=='hdw_hour2'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour2:year'] * 2007)) 
  # hdw_hour3perc =  1*((bootcoef$Estimate[bootcoef$Var=='hdw_hour3'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour3:year'] * 2018) / (bootcoef$Estimate[bootcoef$Var=='hdw_hour3'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour3:year'] * 1981) - 1)
  
  Mbootcoef     = data.frame(value = c(hdw_hour1perc, hdw_hour2perc),
                             varbs = rep(c('HTLH','PRGW'), each = length(hdw_hour1perc))) 
  return(Mbootcoef)
}


EX_Reg1 = EX_BootReg_change(fmula_1, boot = 1000)
EX_Reg2 = EX_BootReg_change(fmula_2, boot = 1000)
EX_Reg3 = EX_BootReg_change(fmula_3, boot = 1000)
EX_Reg4 = EX_BootReg_change(fmula_4, boot = 1000)
EX_Reg5 = EX_BootReg_change(fmula_5, boot = 1000)
EX_Reg6 = EX_BootReg_change(fmula_6, boot = 1000)

EX_Reg1$model = 'MSMT'; EX_Reg1$ID = c(1:nrow(EX_Reg1))
EX_Reg2$model = 'MSMM'; EX_Reg2$ID = c(1:nrow(EX_Reg2))
EX_Reg3$model = 'MMSB'; EX_Reg3$ID = c(1:nrow(EX_Reg3))
EX_Reg4$model = 'MPer'; EX_Reg4$ID = c(1:nrow(EX_Reg4))
EX_Reg5$model = 'MEFD'; EX_Reg5$ID = c(1:nrow(EX_Reg5))
EX_Reg6$model = 'MDHW'; EX_Reg6$ID = c(1:nrow(EX_Reg6))

EX_Year_change = rbind(EX_Reg1,EX_Reg2,EX_Reg3,EX_Reg4,EX_Reg5,EX_Reg6)

EX_DHW_gg_df = EX_Year_change#[Year_change$varbs!='DTWD'&Year_change$model=='MSMM',]  


library(distributional)
library(ggdist)

EX_DHW_mean_df = EX_DHW_gg_df %>% group_by(ID,varbs) %>% summarise_at(vars("value"),list(mean));
EX_DHW_mean_df$Group = 'Field trial'
sens_change_df = rbind(EX_DHW_mean_df, YS_DHW_mean_df)


EX_MYS_DHW_gg =  ggplot(sens_change_df) + 
  stat_halfeye(aes(x = varbs, y = value,fill = varbs),adjust = .6, width = .4,
               .width = 0, alpha = 0.3,justification = -.6,  point_colour = NA) + 
  # ggdist::stat_dots(side = "left", dotsize = .8, justification = 1.05, binwidth = .5) +
  geom_point(aes(x = varbs, y = value),
             shape = 45,size = 8,alpha = .01,color = "#1D785A") +
  stat_pointinterval(aes(x = varbs, y = value,color = varbs),size = 1.5,
                     position = position_dodge(width = -.7, preserve = "single"))+
  ## draw horizontal lines instead of points
  geom_boxplot(aes(x = varbs, y = value), color = 'gray75',
               width = .15, outlier.shape = NA,fill = 'gray75', alpha = 0.6) +
  scale_color_manual(name = '',values = brewer.pal(8, "Set2")[2:3])+
  scale_fill_manual(name = '',values = brewer.pal(8, "Set2")[2:3])+
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'black')+
  stat_summary(aes(x = varbs, y = value,label=round(..y..,1)),
               hjust = -0.8,hjust = -0.3,fun.y=mean, geom="text", size=2, color="black") +
  labs(subtitle = "(c)")+theme_bw()+#facet_wrap(model~.,ncol =3)+
  scale_y_continuous(limits = c(-300,200),breaks= seq(-300, 200,50))+
  ylab(bquote('Change in yield sensitivity to DHW 2006-2018'~(kg~ha^-1~hour^-1)))+
  scale_x_discrete('',labels = c("DTWD" = bquote(DHW['DTWD']),"HTLH" = bquote(DHW['HTLH']),
                                 "PRGW" = bquote(DHW['PRGW'])))+coord_flip()+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 4),
        axis.text.x = element_text(size = 5,angle = 0,vjust = 0.5),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(10.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 4),
        panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

library(ggh4x)
sens_change_DHW_gg =  ggplot(sens_change_df) + 
  stat_halfeye(aes(x = varbs, y = value,fill = varbs),adjust = .6, width = 2.1,
               .width = 0.0, alpha = 0.3,justification = -.15,  point_colour = NA) +
  stat_pointinterval(aes(x = varbs, y = value,color = varbs),size = 1.8,
                     position = position_dodge(width = -.7, preserve = "single"))+
  scale_color_manual(name = '',values = brewer.pal(8, "Set2"))+
  scale_fill_manual(name = '',values = brewer.pal(8, "Set2"))+
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'black')+
  stat_summary(aes(x = varbs, y = value,label=round(..y..,1), color = varbs),
               hjust = -0.8,vjust = -0.5,fun.y=mean, geom="text", size=2) +
  labs(subtitle = "(b)")+theme_bw()+#facet_wrap(model~.,ncol =3)+
  scale_y_continuous(limits = c(-300,200),breaks= seq(-300, 200,50))+
  ylab(bquote(atop('Changes over time in yield sensitivity','to DHW'~(kg~ha^-1~hour^-1))))+
  scale_x_discrete('',labels = c("DTWD" = bquote(DHW['DTWD']),"HTLH" = bquote(DHW['HTLH']),
                                 "PRGW" = bquote(DHW['PRGW'])))+#coord_flip()+
  facet_grid(~ Group, scales = "free_x",space = "free_x",switch = "x") +
  theme(panel.spacing = unit(0, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.background = element_rect(fill = "white"))+ # removes the background from the state names
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 4),
        axis.text.x = element_text(size = 5,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(10.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 4),
        # panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


EX_YS_DHW_gg =  ggplot(EX_DHW_gg_df) + 
  stat_halfeye(aes(x = varbs, y = value,fill = varbs),adjust = .6, width = .9,
               .width = 0, alpha = 0.3,justification = -.3,  point_colour = NA) + 
  # ggdist::stat_dots(side = "left", dotsize = .8, justification = 1.05, binwidth = .5) +
  geom_point(aes(x = varbs, y = value),
             shape = 45,size = 8,alpha = .01,color = "#1D785A") +
  stat_pointinterval(aes(x = varbs, y = value,color = varbs),size = 1.5,
                     position = position_dodge(width = -.7, preserve = "single"))+
  ## draw horizontal lines instead of points
  geom_boxplot(aes(x = varbs, y = value), color = 'gray65',
               width = .15, outlier.shape = NA,fill = 'gray65', alpha = 0.6) +
  scale_color_manual(name = '',values = brewer.pal(8, "Set2"))+
  scale_fill_manual(name = '',values = brewer.pal(8, "Set2"))+
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'black')+
  stat_summary(aes(x = varbs, y = value,label=round(..y..,1)),
               hjust = -0.8,hjust = -0.3,fun.y=mean, geom="text", size=2, color="black") +
  theme_bw()+facet_wrap(model~.,ncol =3)+
  scale_y_continuous(limits = c(-400,400),breaks= seq(-400, 400,100))+
  ylab(bquote('Change in yield sensitivity to DHW 2006-2018'~(kg~ha^-1~hour^-1)))+
  scale_x_discrete('',labels = c("DTWD" = bquote(DHW['DTWD']),"HTLH" = bquote(DHW['HTLH']),
                                 "PRGW" = bquote(DHW['PRGW'])))+coord_flip()+
  theme(strip.text = element_text(size = 5),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 4),
        axis.text.x = element_text(size = 5,angle = 0,vjust = 0.5),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(10.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 4),
        panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



EXDat_mer_DHW = EXDat_mer[EXDat_mer$hdw_hour1!=0,]
EXDat_Myield_DHW = EXDat_mer_DHW %>% group_by(site,year) %>% summarise_at(vars("Yield_S"),list(mean))

EXDat_Mmer_DHW = merge(EXDat_Myield_DHW, EXDat_mer_DHW,  by = c('site','year'))

EXDat_Mmer_DHW$Y_anomly = EXDat_Mmer_DHW$Yield_S.y-EXDat_Mmer_DHW$Yield_S.x

EXDat_Mmer_DHW = EXDat_Mmer_DHW[,c(1:8,168)]

CUL_expourn  = length(unique(EXDat_Mmer_DHW$Culs))
CUL_nexpourn = length(unique(EXDat_mer$Culs))-CUL_expourn

CUL_lossn   = length(unique(EXDat_Mmer_DHW$Culs[EXDat_Mmer_DHW$Y_anomly<0]))
CUL_nlossn  = CUL_expourn-CUL_lossn

PC_CUL =  data.frame(value = c(CUL_expourn*100/(CUL_expourn+CUL_nexpourn),
                               CUL_nexpourn*100/(CUL_expourn+CUL_nexpourn),
                               CUL_lossn*100/(CUL_lossn+CUL_nlossn),
                               CUL_nlossn*100/(CUL_lossn+CUL_nlossn)),
                     Group = c('Percentage of varieties under DHW exposure',
                               'Percentage of varieties under DHW exposure',
                               'Percentage of varieties in yield loss attributed to DHW',
                               'Percentage of varieties in yield loss attributed to DHW'),
                     Type = c('DHW exposure', 'No DHW exposure', 'Yield loss', 'No Yield loss'))

EXDat_Mmer_DHW$Group = ifelse(EXDat_Mmer_DHW$Y_anomly<0, 'Yield loss', 'Yield gain')
EXDat_Mmer_DHW$Y_anomly_pc = EXDat_Mmer_DHW$Y_anomly*100/EXDat_Mmer_DHW$Yield_S.x

PC_CUL_gg1 =  ggplot(PC_CUL[PC_CUL$Group=='Percentage of varieties under DHW exposure',],
                     aes(x = Group, y = value, fill = Type)) +
  geom_col() +geom_text(aes(label = paste0(round(value,1), "%")),
                        position = position_stack(vjust = 0.5),size = 2) +
  scale_fill_brewer(palette = "Set2",name = '') +
  theme_minimal(base_size = 16) +
  coord_flip()+ylab(" ") +labs(title = '(a)')+
  xlab(NULL)+labs(subtitle = "Percentage of varieties under DHW exposure")+
  theme(plot.subtitle = element_text(size = 4, hjust = 0.02, vjust = -6,lineheight = 1),
        plot.title = element_text(size = 7, hjust = 0.02, vjust = -4,lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 4),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 6),
        legend.position = 'right',#c(10.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 4),
        panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

PC_CUL_gg2 =  ggplot(PC_CUL[PC_CUL$Group=='Percentage of varieties in yield loss attributed to DHW',], 
                     aes(x = Group, y = value, fill = Type)) +
  geom_col() +geom_text(aes(label = paste0(round(value,1), "%")),
                        position = position_stack(vjust = 0.5),size = 2) +
  scale_fill_brewer(palette = "Set2",name = '') +
  theme_minimal(base_size = 16) +
  ylab("Percentage (%)") +coord_flip()+
  xlab(NULL)+labs(subtitle = "Percentage of cultivar in yield loss attributed to DHW")+
  theme(plot.subtitle = element_text(size = 4,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = -6,                      # Vertical adjustment
                                     lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 4),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 6),
        legend.position = 'right',#c(.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 4),
        panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

EX_Y_anomly_gg =  ggplot(EXDat_Mmer_DHW) + 
  geom_boxplot(aes(x = Group, y = Y_anomly_pc,fill = Group),
               width = .5, outlier.shape = NA) +
  scale_fill_brewer(palette = "Set2") +coord_flip()+
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'black')+
  stat_summary(aes(x = Group, y = Y_anomly_pc,label=round(..y..,1)),
               vjust = -1.5,hjust = -0.3,fun.y=mean, geom="text", size=2, color="black") +
  theme_minimal(base_size = 16) +xlab('')+
  scale_y_continuous(limits = c(-25,25),breaks= seq(-25,25,5))+
  ylab('Changes in yield of varieties cluster with DHW exposure (%)')+
  theme(plot.subtitle = element_text(size = 4,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 4),
        axis.text.x = element_text(size = 5,angle = 0,vjust = 0.5),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(6.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 4),
        panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure 5.tiff',
     units = "cm", width=14, height=11, res = 1500, compression = 'lzw', pointsize = 11)
ggdraw() +
  # draw_plot(MYS_DHW_gg,       x = 0,    y = 0.49,  width = 0.51, height = 0.52) +
  # draw_plot(Sens_Pro_mean_gg, x = 0.52, y = 0.49,  width = 0.48, height = 0.52)+
  # draw_plot(EX_MYS_DHW_gg,    x = 0, y = 0.00,  width = .51, height = 0.50) +
  # draw_plot(PC_CUL_gg1,       x = 0.52, y = 0.33,  width = .48, height = 0.21) +
  # draw_plot(PC_CUL_gg2,       x = 0.52, y = 0.22,  width = .48, height = 0.19) +
  # draw_plot(EX_Y_anomly_gg,   x = 0.51, y = 0.00,  width = .47, height = 0.24)
  
  draw_plot(PC_CUL_gg1,       x = 0.0, y = 0.82,  width = .48, height = 0.21) +
  draw_plot(PC_CUL_gg2,       x = 0.0, y = 0.71,  width = .48, height = 0.19) +
  draw_plot(EX_Y_anomly_gg,   x = 0.0, y = 0.50,  width = .47, height = 0.24) +
  draw_plot(sens_change_DHW_gg,x = 0.52, y = 0.49,  width = 0.48, height = 0.52)+
  draw_plot(Sens_Pro_mean_gg, x = 0, y = 0,  width = 1, height = 0.52)
dev.off()

tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S15.tiff',
     units = "cm", width=14, height=14, res = 1500, compression = 'lzw', pointsize = 11)
EX_YS_DHW_gg
dev.off()