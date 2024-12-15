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

setwd('E:/Post PHD documents/Crop yield loss at DHW/')

#####################
#################### model fite
########################

Y_Mean = Fit_YSoclivars %>% group_by(site) %>% summarise_at(vars("Yield_Y"),list(mean,sd));

Y_Mean_sf = left_join(help_data_sf, Y_Mean, by = c('site'))


Y_mean_gg = ggplot() +geom_sf(data = Y_Mean_sf,aes(fill=fn1), 
                             linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(11, "YlGn")),
                    breaks =  c(seq(0,10000,1000)),
                    labels =  c(seq(0,10000,1000)),
                    limits = c(0,10000),na.value = "white",
                    values = scales::rescale(c(seq(0,10000,1000))),
                    name = bquote((kg~ha^-1)))+
  theme_bw()+labs(subtitle = '(a) 1981-2018 Mean yield')+
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
        legend.position = 'left',#c(0.60, 1.08),
        #legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.60,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


YYS_Mean = Fit_YSoclivars %>% group_by(year) %>% summarise_at(vars("Yield_Y"),list(mean,sd))

YYS_Mean_gg = ggplot(YYS_Mean, aes(x=year, y=fn1)) + 
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), width=.1) +
  ylab(bquote('1981-2018 yield'~(kg~ha^-1)))+xlab('Year')+
  geom_line() + geom_point()+labs(subtitle = '(b)')+
  scale_color_brewer(palette="Paired")+theme_bw()+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(2.2,'cm'),
        axis.title = element_text(size = 5.0),
        axis.text = element_text(size = 4.0),
        legend.position = c(0.60, .85),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S1.tiff',
     units = "cm", width=16,height=5, res = 1500, compression = 'lzw',pointsize = 11)

ggdraw() +
  draw_plot(Y_mean_gg,   x = 0,   y = 0.0,  width =0.54, height = 1) +
  draw_plot(YYS_Mean_gg, x = 0.54, y = 0.0,  width =0.46, height = 1)
dev.off()
#####################
#################### model fite
########################
Zscore = Fit_YSoclivars %>%  summarise_at(vars(colnames(Fit_YSoclivars)[c(153:159)]),list(sd));

#####################
#################### model fite
########################

Fit_YSoclivars_SD = Fit_YSoclivars %>% mutate_at(c(colnames(Fit_YSoclivars)[c(48:160,162:164)]), ~(scale(.) %>% as.vector))


BootReg    = function(fmula, boot, size){
  
    fmula1    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)]),collapse = '+'),'+year+site'))
    
    fmula2    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(59:69,92:102,125:135,150:159)]),collapse = '+'),'+year+site'))
    
    fmula3    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(70:80,103:113,136:145,150:159)]),collapse = '+'),'+year+site'))
    
    fmula4    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(147:159)]),collapse = '+'),
                                  '+PJ_Prcp2+JH_Prcp2+HM_Prcp2+year+site'))
    
    fmula5    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(150:159)]),collapse = '+'),'+year+site'))
    fmula6    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(157:159)]),collapse = '+'),'+year+site'))
    
    reg_data = Fit_YSoclivars_SD
    reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  

    bootcoefs = c()
    Rsq       = NULL
    for (n in 1:boot) {
      
      rsamp_data = sample(1:nrow(reg_data),size=size,replace = T)  
      
      rsamp_data = reg_data[rsamp_data,]
      
      cropfit    = feols(fmula, data = rsamp_data) 
      
      Rsq        = c(Rsq, cropfit$sq.cor)
      
      coeftable = cropfit$coeftable
      coeftable$ID = n
      coeftable$Var = rownames(coeftable)
      bootcoefs = rbind(bootcoefs, coeftable)
   
      # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
    }
    bootcoef = data.frame(bootcoefs)
    
    Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean,sd));

    Mbootcoef$rsq  = mean(Rsq)

  return(Mbootcoef)
}

set.seed(1234)
a = Sys.time()

DHW_yield_fit_1 =   BootReg(fmula1, 1000, 10000)  
DHW_yield_fit_2 =   BootReg(fmula2, 1000, 10000)  
DHW_yield_fit_3 =   BootReg(fmula3, 1000, 10000)  
DHW_yield_fit_4 =   BootReg(fmula4, 1000, 10000)  
DHW_yield_fit_5 =   BootReg(fmula5, 1000, 10000)  
DHW_yield_fit_6 =   BootReg(fmula6, 1000, 10000)  

b = Sys.time()
c = b-a 
c

DHW_yield_fit_1$Model  = 'MSMT'
DHW_yield_fit_2$Model  = 'MSMM'
DHW_yield_fit_3$Model  = 'MMSB'
DHW_yield_fit_4$Model  = 'MPer'
DHW_yield_fit_5$Model  = 'MEFD'
DHW_yield_fit_6$Model  = 'MDHW'

DHW_yield_fit_df = rbind(DHW_yield_fit_1,
                         DHW_yield_fit_2,
                         DHW_yield_fit_3,
                         DHW_yield_fit_4,
                         DHW_yield_fit_5,
                         DHW_yield_fit_6)

RSQ_Mean = DHW_yield_fit_df %>% group_by(Model) %>% summarise_at(vars("rsq"),list(mean));
COf_Mean = DHW_yield_fit_df[substr(DHW_yield_fit_df$Var,1,4)!='site'&substr(DHW_yield_fit_df$Var,1,4)!='year',] %>% group_by(Model,Var) %>% summarise_at(vars("Estimate","Std..Error","t.value", "Pr...t.."),list(mean));
write.table(COf_Mean,'COf_Mean.txt')
########################
######################## Visualization with ggplot1=2
########################
colnames(Fit_YSoclivars)

level_order = c('PJFDD', 'JHFDD', 'JHEDD', 'HMEDD',"hdw_hour1", "hdw_hour2", "hdw_hour3")
# library(ggpubr)
# library(rstatix)
df       = DHW_yield_fit_df[DHW_yield_fit_df$Var %in% c("JHEDD","HMEDD","PJFDD", "JHFDD","hdw_hour1", "hdw_hour2","hdw_hour3"),]
df_mean  = df ;df_mean$Estimate = df_mean$Estimate*100
library(smplot2)

DHW_bar_gg  = ggplot(df_mean[df_mean$Model%in%'MSMM',], aes(x = factor(Var,level_order),
                                                            y = Estimate, color = Var,fill = Var))+
              sm_bar(errorbar_type = "sd",position = position_dodge(width = 1),
                     err.params = list(size = 0.5, color = "black"),
                     bar.params = list(width = 0.8,alpha = 0.7,size = 0.3),
                     point.params = list(alpha = 0.08,size = 0.3,
                     position = position_jitterdodge(jitter.width = 0.12,
                     dodge.width = 1)),legends = T)+
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
  labs(subtitle = c('(d)'))+theme_bw()+
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
  

dfS          = DHW_yield_fit_df[DHW_yield_fit_df$Var %in% c("hdw_hour1", "hdw_hour2","hdw_hour3"),]
dfS$Estimate = dfS$Estimate*100
dfS_mean     = dfS %>% group_by(Model,Var) %>% summarise_at(vars('Estimate'),list(mean,sd));
colnames(dfS_mean)[3:4] = c('Estimate', 'sd')
dfS_mean$Group = 'County level'
DHW_SEN_df = rbind(dfS_mean,EX_dfS[c(1,2,5,6,7)]) 

DHW_SEN_df$col = paste0(DHW_SEN_df$Var,'_', DHW_SEN_df$Group) 

library(ggpattern)
DHW_SEN_sing_gg  = ggplot(DHW_SEN_df, aes(x = Var, y = Estimate,
                                          color = col,fill = col))+
  geom_bar_pattern(stat = "identity",alpha = 0.6, lwd= 0.05,aes(pattern = col),
                   position = position_dodge(.9),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.000001,
                   pattern_size = .05,
                   pattern_spacing = 0.1,
                   pattern_key_scale_factor = 0.1) +
  geom_errorbar(aes(ymin=Estimate-sd, ymax=Estimate+sd), width=.2,
                position=position_dodge(.9))+guides(fill=guide_legend(ncol=2),
                                                    label.position = "top")+
  scale_pattern_manual(values = c("none", "stripe","none", "stripe","none"),name = '') +
  scale_fill_manual(values =  c("orangered4",'brown4',
                                "orangered3", "orangered2", 'orange3'),
                    name = '',label = c('County level: HTLH','Field trail: HTLH',
                                        'County level: PRGW','Field trail: PRGW',
                                        'County level: DTWD') )+
  scale_color_manual(values =   c("orangered4",'brown4',
                                  "orangered3", "orangered2", 'orange3'),
                     name = '',label = c('County level: HTLH','Field trail: HTLH',
                                         'County level: PRGW','Field trail: PRGW',
                                         'County level: DTWD'))+ 
  scale_x_discrete('Yield models',labels = c(
    "PJFDD" = bquote(FDD[pT-JT]),
    "JHFDD" = bquote(FDD[JT-HD]),
    "JHEDD" = bquote(EDD[JT-HD]),
    "HMEDD" = bquote(EDD[HD-MT]),
    "hdw_hour1" = bquote(DHW[HTLH]),
    'hdw_hour2' = bquote(DHW[PRGW]),
    "hdw_hour3" = bquote(DHW[DTWD])
  ))+ylab("Yield loss per standard unit (%)")+
  facet_grid(~Model, switch = "x", scales = "free_x") +
  labs(subtitle = c('(c)'))+theme_bw()+#ylim(-30,30)+
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
        axis.text.x = element_text(size = 5,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 5,angle = 0,vjust = 0.5),
        axis.title = element_text(size = 6),
        legend.position = c(10.3,0.95),
        #legend.direction = "horizontal",
        legend.key.height = unit(0.12,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 3.6),
        legend.title= element_text(size = 4),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


DHW_sing_gg  = ggplot(dfS, aes(x = Model, y = Estimate, color = Var,fill = Var))+
  sm_bar(errorbar_type = "sd", position = position_dodge(width = 1),
         err.params = list(size = 0.5, color = "black"),
         bar.params = list(width = 0.8,size=0.3,alpha = 0.7),
         point.params = list(alpha = 0.05,size= 0.3, 
                             position = position_jitterdodge(jitter.width = 0.12,
                              dodge.width = 1)),legends = T)+
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
  labs(subtitle = c('(c)'))+theme_bw()+
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



BootReg_O    = function(fmula, boot, size){
  
  fmula1    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)]),collapse = '+'),'+site+year'))
  
  fmula2    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(59:69,92:102,125:135,150:159)]),collapse = '+'),'+site+year'))
  
  fmula3    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(70:80,103:113,136:145,150:159)]),collapse = '+'),'+site+year'))
  
  fmula4    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(147:159)]),collapse = '+'),
                                '+PJ_Prcp2+JH_Prcp2+HM_Prcp2+site+year'))
  
  fmula5    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(150:159)]),collapse = '+'),
                                '+site+year'))
  fmula6    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(157:159)]),collapse = '+'),
                                '+site+year'))
  
  
  reg_data = Fit_YSoclivars
  reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  reg_data$site = as.character(reg_data$site)
  reg_data$year = as.character(reg_data$year)
  
  bootcoefs = c()
  # boot = 100
  # size = 26000
  Rsq       = NULL
  for (n in 1:boot) {
    
    rsamp_data = sample(1:nrow(reg_data),size=size,replace = T)  
    
    rsamp_data = reg_data[rsamp_data,]
    
    cropfit    = feols(fmula, data = rsamp_data) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean,sd));
  
  Mbootcoef$rsq  = mean(Rsq)
  
  return(Mbootcoef)
}

set.seed(1234)
a = Sys.time()

DHW_yield_fit_1O =   BootReg_O(fmula1, 1000, 6000)  
DHW_yield_fit_2O =   BootReg_O(fmula2, 1000, 6000)  
DHW_yield_fit_3O =   BootReg_O(fmula3, 1000, 6000)  
DHW_yield_fit_4O =   BootReg_O(fmula4, 1000, 6000)  
DHW_yield_fit_5O =   BootReg_O(fmula5, 1000, 6000)  
DHW_yield_fit_6O =   BootReg_O(fmula6, 1000, 6000)  

b = Sys.time()
c = b-a 
c


DHW_yield_fit_1O$Model  = 'MSMT'
DHW_yield_fit_2O$Model  = 'MSMM'
DHW_yield_fit_3O$Model  = 'MMSB'
DHW_yield_fit_4O$Model  = 'MPer'
DHW_yield_fit_5O$Model  = 'MEFD'
DHW_yield_fit_6O$Model  = 'MDHW'

DHW_yield_fit_dfO = rbind(DHW_yield_fit_1O,
                          DHW_yield_fit_2O,
                          DHW_yield_fit_3O,
                          DHW_yield_fit_4O,
                          DHW_yield_fit_5O,
                          DHW_yield_fit_6O)

ORSQ_Mean = DHW_yield_fit_dfO %>% group_by(Model) %>% summarise_at(vars("rsq"),list(mean));
OCOf_Mean = DHW_yield_fit_dfO[substr(DHW_yield_fit_dfO$Var,1,4)!='site'&substr(DHW_yield_fit_dfO$Var,1,4)!='year',] %>% group_by(Model,Var) %>% summarise_at(vars("Estimate_fn1","Std..Error_fn1","t.value_fn1", "Pr...t.._fn1"),list(mean));
write.table(OCOf_Mean,'OCOf_Mean.txt')

colnames(Fit_YSoclivars)

Slevel_order = c(paste0('q_',seq(5,105,10),'_hurs'))

dfSM      = DHW_yield_fit_dfO[DHW_yield_fit_dfO$Var %in% colnames(Fit_YSoclivars)[48:146],]
dfSM$stg  = substr(dfSM$Var,1,2)
dfSM$soil = substr(dfSM$Var,10,nchar(dfSM$Var))
dfSM$styp = substr(dfSM$Var,8,8)  
dfSM$stg[dfSM$stg=='PJ'] = 'PT-JT'
dfSM$stg[dfSM$stg=='JH'] = 'JT-HD'
dfSM$stg[dfSM$stg=='HM'] = 'HD-MT'

Soil_point_gg =   ggplot(dfSM) +
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'gray75')+
  geom_point(aes(x= factor(soil,Slevel_order), y= Estimate_fn1*100,
                 shape=Model, color=Model,fill = Model),size=1,
             position = position_dodge(0.5))+
  geom_errorbar(aes(x= soil,ymin=(Estimate_fn1-Estimate_fn2)*100,
                    ymax=(Estimate_fn1+Estimate_fn2)*100,
                    color=Model),linewidth=0.25, 
                width=.45,position=position_dodge(0.5))+
  scale_fill_manual(values = brewer.pal(11, "Dark2"))+
  scale_color_manual(values = brewer.pal(11, "Dark2"))+
  facet_grid(stg~.)+ylab(bquote(Yield~sensitivty~to~soil~moisture~(kg~ha^-2~hour^-1)))+
  scale_x_discrete('Soil moisture bins at different quantiles',labels = c(
    "q_5_hurs"  = bquote(SM['5% quantile']),
    "q_15_hurs" = bquote(SM['15% quantile']),
    "q_25_hurs" = bquote(SM['25% quantile']),
    "q_35_hurs" = bquote(SM['35% quantile']),
    "q_45_hurs" = bquote(SM['45% quantile']),
    'q_55_hurs' = bquote(SM['55% quantile']),
    "q_65_hurs" = bquote(SM['65% quantile']),
    "q_75_hurs" = bquote(SM['75% quantile']),
    'q_85_hurs' = bquote(SM['85% quantile']),
    "q_95_hurs" = bquote(SM['95% quantile']),
    "q_105_hurs" = expression(">SM"['95% quantile'])))+
  # ylab("Yield loss per standard unit (%)")+
  labs(subtitle = c('(a)'))+theme_bw()+
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
        legend.position = c(0.91,0.25),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 5),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



dfper      = data.frame(DHW_yield_fit_dfO[DHW_yield_fit_dfO$Var %in% colnames(Fit_YSoclivars)[c(147:149,162:164)],])

Per_Fit    = data.frame(PJ_per = sort(Fit_YSoclivars$PJ_Prcp),
                        JH_per = sort(Fit_YSoclivars$JH_Prcp),
                        HM_per = sort(Fit_YSoclivars$HM_Prcp))

Per_Fit$PJ_per_fit = Per_Fit$PJ_per*(dfper[5,2])+ Per_Fit$PJ_per^2*(dfper[6,2])
Per_Fit$JH_per_fit = Per_Fit$JH_per*(dfper[3,2])+ Per_Fit$JH_per^2*(dfper[4,2])
Per_Fit$HM_per_fit = Per_Fit$HM_per*(dfper[1,2])+ Per_Fit$HM_per^2*(dfper[2,2])

Per_Fit$PJ_per_fit_sd = Per_Fit$PJ_per*(dfper[5,6])
Per_Fit$JH_per_fit_sd = Per_Fit$JH_per*(dfper[3,6])
Per_Fit$HM_per_fit_sd = Per_Fit$HM_per*(dfper[1,6])

Per_Fit_long1      = pivot_longer(Per_Fit[c('PJ_per','JH_per','HM_per')], 
                                 cols = c('PJ_per','JH_per','HM_per'),
                            names_to = "STG", values_to = "Per")

Per_Fit_long2      = pivot_longer(Per_Fit[c('PJ_per_fit','JH_per_fit','HM_per_fit')], 
                                 cols = c('PJ_per_fit','JH_per_fit','HM_per_fit'),
                                 names_to = "STG", values_to = "Per")

Per_Fit_long3      = pivot_longer(Per_Fit[c('PJ_per_fit_sd','JH_per_fit_sd','HM_per_fit_sd')], 
                                  cols = c('PJ_per_fit_sd','JH_per_fit_sd','HM_per_fit_sd'),
                                  names_to = "STG", values_to = "sd")

Per_Fit_long         = Per_Fit_long1
Per_Fit_long$per_fit = Per_Fit_long2$Per
Per_Fit_long$sd      = Per_Fit_long3$sd


Perp_sensity_gg = ggplot(Per_Fit_long) +  
  geom_histogram(aes(x = Per, y = ..density..,group = STG, fill = STG),
                 color= 'gray75',alpha = 0.8, size = 0.4) +
  # geom_density(aes(x = Per, group = STG, fill = STG),color = 'gray65',lwd = 1,linetype = 5,alpha = 0.3) +
  geom_line(aes(x = Per,y = 0.03+per_fit/30000,group = STG,color = STG),size = 0.6)+
  geom_ribbon(aes(x = Per, ymin = 0.03+(per_fit-sd)/30000,
                  ymax = 0.03+(per_fit+sd)/30000,fill = STG),alpha = 0.3)+
  scale_fill_manual(values = c(brewer.pal(11, "PRGn")[c(9:11)]),
                    name = '',label = c('HD-MT','JT-HD', 'PT-JT') )+
  scale_color_manual(values = c(brewer.pal(11, "PRGn")[c(9:11)]),
                     name = '',label = c('HD-MT','JT-HD', 'PT-JT') ) +
  scale_y_continuous(name = "Density of country-year precipitation",
                     sec.axis = sec_axis(~(.*30000)-30000*0.03,
                                         name= bquote(Sensitivity~of~yield~to~Prcp~(kg~ha^-2~mm^-1)))) +
  geom_hline(yintercept=0.03, color = "gray75",linetype = 5, size=0.4)+
  xlab('Cumulative precipitation (mm)')+
  labs(subtitle = c('(b)'))+theme_bw()+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 6),
        legend.key.size = unit(0.3,'cm'),
        # axis.ticks = element_blank(),
        axis.text = element_text(size = 5,angle = 0),
        axis.title = element_text(size = 6),
        legend.position = c(0.78,0.22),
        legend.key.height = unit(0.22,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 5),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

Sens_Pro_mean = Sens_Pro_df_mer %>% group_by(Prov,varbs)  %>% summarise_at(vars('mean'),list(mean));

Prov_DHW_SEN_DF = NULL
for(i in (1:nrow(Sens_Pro_mean))){
 temp = Sens_Pro_mean$mean[i]*0:10
 df   = data.frame(Values = temp, Hs = 0:10)
 df$Prov = Sens_Pro_mean$Prov[i]
 df$varbs = Sens_Pro_mean$varbs[i]
 
 Prov_DHW_SEN_DF = rbind(Prov_DHW_SEN_DF, df)
}
unique(Prov_DHW_SEN_DF$Prov)

Prov_DHW_SEN_gg = ggplot(Prov_DHW_SEN_DF, aes(x=Hs, y=Values, color=Prov)) +
  geom_line(alpha = 0.8)+xlab('∆ DHW (hour)')+ ylab(expression('Yield respond'~(kg~ha^-2)))+
  scale_color_manual(values = c('Black', brewer.pal(8, "Dark2"),
                                brewer.pal(8, "Accent")), name = '')+
  #guides(color=guide_legend(ncol=4,label.position = "top",keylength = unit(0.1, "cm") ))+
  facet_grid(.~varbs, switch = "x", scales = "free_y") +
  labs(subtitle = c('(d)'))+theme_bw()+#ylim(-30,30)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 4),
        legend.key.size = unit(0.02,'cm'),
        panel.spacing = unit(0.05, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(0.15, .42),
        # legend.justification = c("right", "top"),
        # legend.box.just = "right",
        legend.key.height = unit(0.02,'cm'),
        legend.key.width = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 5),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



library("cowplot")
tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure 3.tiff',
     units = "cm", width=14,height=12, res = 1500, compression = 'lzw',pointsize = 11)

ggdraw() +
  draw_plot(Soil_point_gg, x = 0, y = 0.40,  width =0.5, height = 0.65) +
  draw_plot(Perp_sensity_gg, x = 0.5, y = 0.40,  width =0.5, height = 0.65)+
  draw_plot(DHW_SEN_sing_gg, x = 0, y = 0,  width =0.5, height = 0.48) +
  draw_plot(Prov_DHW_SEN_gg, x = 0.5, y = 0,  width =0.5, height = 0.48)
dev.off()

#################################
#################################
#################################
#################################
C_fix_df = DHW_yield_fit_df %>% group_by(Model, Var) %>% summarise_at(vars("Estimate"),list(mean));

C_fix_df = C_fix_df[substr(C_fix_df$Var,1,4)=='site',]

C_fix_df$site = as.numeric(substr(C_fix_df$Var,5,9))

C_fix_df_lon = pivot_longer(C_fix_df, cols = c(Estimate),
                            names_to = "Column", values_to = "value")
C_fix_df_lon$value = C_fix_df_lon$value*100
C_fix_df_sf = left_join(help_data_sf, C_fix_df_lon, by = c('site'))


C_fix_gg = ggplot() +geom_sf(data = C_fix_df_sf[is.na(C_fix_df_sf$Model)!=T,],aes(fill=value), 
                             linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(11, "PRGn")),
                    breaks =  c(seq(-100,200,10),400), 
                    labels =  c(seq(-100,200,10),'>200'),
                    limits = c(-100,400),na.value = "white",
                    values = scales::rescale(c(seq(-100,200,10),400)),name = '(%)')+
  theme_bw()+facet_wrap(.~Model,ncol=3)+labs(subtitle = 'Random effect of counties')+
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
        legend.key.size = unit(2.2,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.63, 1.15),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S9.tiff',
     units = "cm", width=16,height=7.8, res = 1500, compression = 'lzw',pointsize = 11)
C_fix_gg
dev.off()


y_fix_df = DHW_yield_fit_df %>% group_by(Model, Var) %>% summarise_at(vars("Estimate"),list(mean));

y_fix_df = y_fix_df[substr(y_fix_df$Var,1,4)=='year',]

y_fix_df$Year = as.numeric(substr(y_fix_df$Var,5,8))

y_fix_gg = ggplot(y_fix_df, aes(x= Year, y = Estimate*100, color = Model))+
  geom_line(alpha = 0.6)+geom_point(size = 1.2)+theme_bw()+
  scale_color_manual(values = brewer.pal(8, "Dark2"))+
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
        legend.position = c(10.63, 1.15),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S10.tiff',
     units = "cm", width=16,height=7.8, res = 1500, compression = 'lzw', pointsize = 11)
y_fix_gg
dev.off()

#################################
#################################
#################################
#################################


BootReg90    = function(fmula, boot, size){
  
  fmula1    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)]),collapse = '+'),'+site+year'))
  
  fmula2    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(59:69,92:102,125:135,150:159)]),collapse = '+'),'+site+year'))
  
  fmula3    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(70:80,103:113,136:145,150:159)]),collapse = '+'),'+site+year'))
  
  fmula4    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(147:159)]),collapse = '+'),
                                '+PJ_Prcp2+JH_Prcp2+HM_Prcp2+site+year'))
  
  fmula5    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(150:159)]),collapse = '+'),
                                '+site+year'))
  fmula6    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(157:159)]),collapse = '+'),
                                '+site+year'))
  
  
  reg_data = Fit_YSoclivars[Fit_YSoclivars$year<2000,]
  reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  reg_data$site = as.character(reg_data$site)
  reg_data$year = as.character(reg_data$year)
  reg_data = reg_data %>% mutate_at(c(colnames(reg_data)[c(48:160,162:164)]), ~(scale(.) %>% as.vector))
  
  bootcoefs = c()
  # boot = 100
  # size = 26000
  Rsq       = NULL
  for (n in 1:boot) {
    
    rsamp_data = sample(1:nrow(reg_data),size=size,replace = T)  
    
    rsamp_data = reg_data[rsamp_data,]
    
    cropfit    = feols(fmula1, data = rsamp_data) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean,sd));
  
  Mbootcoef$rsq  = mean(Rsq)
  
  return(Mbootcoef)
}

set.seed(1234)
a = Sys.time()

DHW_yield_fit_1_90 =   BootReg90(fmula1, 1000, 5000)  
DHW_yield_fit_2_90 =   BootReg90(fmula2, 1000, 5000)  
DHW_yield_fit_3_90 =   BootReg90(fmula3, 1000, 5000)  
DHW_yield_fit_4_90 =   BootReg90(fmula4, 1000, 5000)  
DHW_yield_fit_5_90 =   BootReg90(fmula5, 1000, 5000)  
DHW_yield_fit_6_90 =   BootReg90(fmula6, 1000, 5000)  

b = Sys.time()
c = b-a 
c
  
BootReg10    = function(fmula, boot, size){
  
  fmula1    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)]),collapse = '+'),'+site+year'))
  
  fmula2    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(59:69,92:102,125:135,150:159)]),collapse = '+'),'+site+year'))
  
  fmula3    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(70:80,103:113,136:145,150:159)]),collapse = '+'),'+site+year'))
  
  fmula4    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(147:159)]),collapse = '+'),
                                '+PJ_Prcp2+JH_Prcp2+HM_Prcp2+site+year'))
  
  fmula5    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(150:159)]),collapse = '+'),
                                '+site+year'))
  fmula6    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(157:159)]),collapse = '+'),
                                '+site+year'))
  
  
  reg_data = Fit_YSoclivars[Fit_YSoclivars$year>=2000,]
  reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  reg_data$site = as.character(reg_data$site)
  reg_data$year = as.character(reg_data$year)
  reg_data = reg_data %>% mutate_at(c(colnames(reg_data)[c(48:160,162:164)]), ~(scale(.) %>% as.vector))
  
  bootcoefs = c()
  # boot = 100
  # size = 26000
  Rsq       = NULL
  for (n in 1:boot) {
    
    rsamp_data = sample(1:nrow(reg_data),size=size,replace = T)  
    
    rsamp_data = reg_data[rsamp_data,]
    
    cropfit    = feols(fmula, data = rsamp_data) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean,sd));
  
  Mbootcoef$rsq  = mean(Rsq)
  
  return(Mbootcoef)
}

set.seed(1234)
a1 = Sys.time()

DHW_yield_fit_1_10 =   BootReg10(fmula1, 1000, 5000)  
DHW_yield_fit_2_10 =   BootReg10(fmula2, 1000, 5000)  
DHW_yield_fit_3_10 =   BootReg10(fmula3, 1000, 5000)  
DHW_yield_fit_4_10 =   BootReg10(fmula4, 1000, 5000)  
DHW_yield_fit_5_10 =   BootReg10(fmula5, 1000, 5000)  
DHW_yield_fit_6_10 =   BootReg10(fmula6, 1000, 5000)  

b1 = Sys.time()
c1 = b1-a1 
c1


DHW_yield_fit_1_90$Model  = 'MSMT'
DHW_yield_fit_2_90$Model  = 'MSMM'
DHW_yield_fit_3_90$Model  = 'MMSB'
DHW_yield_fit_4_90$Model  = 'MPer'
DHW_yield_fit_5_90$Model  = 'MEFD'
DHW_yield_fit_6_90$Model  = 'MDHW'
DHW_yield_fit_1_10$Model  = 'MSMT'
DHW_yield_fit_2_10$Model  = 'MSMM'
DHW_yield_fit_3_10$Model  = 'MMSB'
DHW_yield_fit_4_10$Model  = 'MPer'
DHW_yield_fit_5_10$Model  = 'MEFD'
DHW_yield_fit_6_10$Model  = 'MDHW'

DHW_yield_fit_1_90$PUR  = '1981-1999'
DHW_yield_fit_2_90$PUR  = '1981-1999'
DHW_yield_fit_3_90$PUR  = '1981-1999'
DHW_yield_fit_4_90$PUR  = '1981-1999'
DHW_yield_fit_5_90$PUR  = '1981-1999'
DHW_yield_fit_6_90$PUR  = '1981-1999'
DHW_yield_fit_1_10$PUR  = '2000-2018'
DHW_yield_fit_2_10$PUR  = '2000-2018'
DHW_yield_fit_3_10$PUR  = '2000-2018'
DHW_yield_fit_4_10$PUR  = '2000-2018'
DHW_yield_fit_5_10$PUR  = '2000-2018'
DHW_yield_fit_6_10$PUR  = '2000-2018'

DHW_yield_fit_df_1090 = rbind(DHW_yield_fit_1_90,
                              DHW_yield_fit_2_90,
                              DHW_yield_fit_3_90,
                              DHW_yield_fit_4_90,
                              DHW_yield_fit_5_90,
                              DHW_yield_fit_6_90,
                              DHW_yield_fit_1_10,
                              DHW_yield_fit_2_10,
                              DHW_yield_fit_3_10,
                              DHW_yield_fit_4_10,
                              DHW_yield_fit_5_10,
                              DHW_yield_fit_6_10)


level_order = c('PJFDD', 'JHFDD', 'JHEDD', 'HMEDD',"hdw_hour1", "hdw_hour2", "hdw_hour3")


Change_yloss_gg = ggplot(DHW_yield_fit_df_1090[DHW_yield_fit_df_1090$Var %in% c("hdw_hour1", "hdw_hour2","hdw_hour3"),], 
                         aes(x=factor(Var,level_order), y=Estimate_fn1*100, colour = PUR,fill=PUR,group =PUR)) + 
  geom_bar(stat="identity", color="black", size=0.1,
           position=position_dodge(),alpha=0.9) +
  geom_errorbar(aes(ymin=(Estimate_fn1-Estimate_fn2)*100,
                    ymax=(Estimate_fn1+Estimate_fn2)*100,
                    colour =PUR),size = 0.3,
                width=.2,position=position_dodge(.9))+
  scale_fill_manual(values = brewer.pal(9, "RdPu")[c(6,9)],name = '')+
  scale_color_manual(values = brewer.pal(9, "RdPu")[c(6,9)],name = '')+
  scale_colour_manual(values = brewer.pal(9, "RdPu")[c(6,9)],name = '')+
  scale_x_discrete(' ',labels = c(
    "PJFDD" = bquote(FDD[planting-Jointing]),
    "JHFDD" = bquote(FDD[Jointing-Heading]),
    "JHEDD" = bquote(EDD[Jointing-Heading]),
    "HMEDD" = bquote(EDD[Heading-Maturity]),
    "hdw_hour1" = bquote(DHW[HTLH]),
    'hdw_hour2' = bquote(DHW[PRGW]),
    "hdw_hour3" = bquote(DHW[DTWD])
  ))+ylab("Yield loss per standard unit (%)")+ theme_bw()+facet_grid(.~Model)+
  labs(subtitle = c('(b)'))+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1),
        strip.text = element_text(size = 6),
        legend.key.size = unit(0.28,'cm'),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 5,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(0.94,0.13),
        legend.key.height = unit(0.20,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 5),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

# , margin = margin(20, 0, 0, 0)              # Line spacing
Change_yloss_gg

OBootReg90    = function(fmula, boot, size){
  
  fmula1    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)]),collapse = '+'),'+site+year'))
  
  fmula2    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(59:69,92:102,125:135,150:159)]),collapse = '+'),'+site+year'))
  
  fmula3    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(70:80,103:113,136:145,150:159)]),collapse = '+'),'+site+year'))
  
  fmula4    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(147:159)]),collapse = '+'),
                                '+PJ_Prcp2+JH_Prcp2+HM_Prcp2+site+year'))
  
  fmula5    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(150:159)]),collapse = '+'),
                                '+site+year'))
  fmula6    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(157:159)]),collapse = '+'),
                                '+site+year'))
  
  # Fit_YSoclivars$year = as.numeric(Fit_YSoclivars$year)
  reg_data = Fit_YSoclivars[Fit_YSoclivars$year<2000,]
  reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  reg_data = reg_data[reg_data$Provience %in% c("黑龙江", "内蒙古", "吉林", "新疆","甘肃", "青海", "河北","山西","宁夏","陕西", "河南",
                                                  "北京","辽宁","天津","山东","江苏","安徽"),]
  reg_data$site = as.character(reg_data$site)
  reg_data$year = as.character(reg_data$year)
  
  # reg_data  = reg_data[reg_data$hdw_hour1!=0|reg_data$hdw_hour2!=0|reg_data$hdw_hour3!=0,]
  bootcoefs = c()
  # boot = 100
  # size = 6000
  Rsq       = NULL
  for (n in 1:boot) {
    
    rsamp_data = sample(1:nrow(reg_data),size=size,replace = T)  
    
    rsamp_data = reg_data[rsamp_data,]
    
    cropfit    = feols(fmula, data = rsamp_data) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean,sd));
  
  Mbootcoef$rsq  = mean(Rsq)
  
  return(Mbootcoef)
}

set.seed(1234)
a = Sys.time()

ODHW_yield_fit_1_90 =   OBootReg90(fmula1, 1000, 6000)  
ODHW_yield_fit_2_90 =   OBootReg90(fmula2, 1000, 6000)  
ODHW_yield_fit_3_90 =   OBootReg90(fmula3, 1000, 6000)  
ODHW_yield_fit_4_90 =   OBootReg90(fmula4, 1000, 6000)  
ODHW_yield_fit_5_90 =   OBootReg90(fmula5, 1000, 6000)  
ODHW_yield_fit_6_90 =   OBootReg90(fmula6, 1000, 6000)  

b = Sys.time()
c = b-a 
c

OBootReg10    = function(fmula, boot, size){
  
  fmula1    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)]),collapse = '+'),'+site+year'))
  
  fmula2    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(59:69,92:102,125:135,150:159)]),collapse = '+'),'+site+year'))
  
  fmula3    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(70:80,103:113,136:145,150:159)]),collapse = '+'),'+site+year'))
  
  fmula4    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(147:159)]),collapse = '+'),
                                '+PJ_Prcp2+JH_Prcp2+HM_Prcp2+site+year'))
  
  fmula5    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(150:159)]),collapse = '+'),
                                '+site+year'))
  fmula6    = as.formula(paste0('Yield_Y~',paste0(colnames(Fit_YSoclivars[c(157:159)]),collapse = '+'),
                                '+site+year'))
  
  
  reg_data = Fit_YSoclivars[Fit_YSoclivars$year>=2000,]
  reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  reg_data = reg_data[reg_data$Provience %in% c("黑龙江", "内蒙古", "吉林", "新疆","甘肃", "青海", "河北","山西","宁夏","陕西", "河南",
                                                  "北京","辽宁","天津","山东","江苏","安徽"),]
  reg_data$site = as.character(reg_data$site)
  reg_data$year = as.character(reg_data$year)

  bootcoefs = c()
  # boot = 100
  # size = 26000
  Rsq       = NULL
  for (n in 1:boot) {
    
    rsamp_data = sample(1:nrow(reg_data),size=size,replace = T)  
    
    rsamp_data = reg_data[rsamp_data,]
    
    cropfit    = feols(fmula, data = rsamp_data) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean,sd));
  
  Mbootcoef$rsq  = mean(Rsq)
  
  return(Mbootcoef)
}

set.seed(1234)
a1 = Sys.time()

ODHW_yield_fit_1_10 =   OBootReg10(fmula1, 1000, 6000)  
ODHW_yield_fit_2_10 =   OBootReg10(fmula2, 1000, 6000)  
ODHW_yield_fit_3_10 =   OBootReg10(fmula3, 1000, 6000)  
ODHW_yield_fit_4_10 =   OBootReg10(fmula4, 1000, 6000)  
ODHW_yield_fit_5_10 =   OBootReg10(fmula5, 1000, 6000)  
ODHW_yield_fit_6_10 =   OBootReg10(fmula6, 1000, 6000)  

b1 = Sys.time()
c1 = b1-a1 
c1


DHWA=CroPDHWs_mean_sfA
DHWB=CroPDHWs_mean_sfB
DHWA_1 = DHWA;DHWB_1 = DHWB;
DHWA_2 = DHWA;DHWB_2 = DHWB;
DHWA_3 = DHWA;DHWB_3 = DHWB;
DHWA_4 = DHWA;DHWB_4 = DHWB;
DHWA_5 = DHWA;DHWB_5 = DHWB;
DHWA_6 = DHWA;DHWB_6 = DHWB;

DHWA_1$hdw_hour[DHWA_1$group=='HTLH'] = DHWA_1$hdw_hour[DHWA_1$group=='HTLH']*
  abs(ODHW_yield_fit_1_90$Estimate_fn1[ODHW_yield_fit_1_90$Var=='hdw_hour1'])

DHWA_1$hdw_hour[DHWA_1$group=='PRGW'] = DHWA_1$hdw_hour[DHWA_1$group=='PRGW']*
  abs(ODHW_yield_fit_1_90$Estimate_fn1[ODHW_yield_fit_1_90$Var=='hdw_hour2'])

DHWA_1$hdw_hour[DHWA_1$group=='DTWD'] = DHWA_1$hdw_hour[DHWA_1$group=='DTWD']*
  abs(ODHW_yield_fit_1_90$Estimate_fn1[ODHW_yield_fit_1_90$Var=='hdw_hour3'])


DHWA_2$hdw_hour[DHWA_2$group=='HTLH'] = DHWA_2$hdw_hour[DHWA_2$group=='HTLH']*
  abs(ODHW_yield_fit_2_90$Estimate_fn1[ODHW_yield_fit_2_90$Var=='hdw_hour1'])

DHWA_2$hdw_hour[DHWA_2$group=='PRGW'] = DHWA_2$hdw_hour[DHWA_2$group=='PRGW']*
  abs(ODHW_yield_fit_2_90$Estimate_fn1[ODHW_yield_fit_2_90$Var=='hdw_hour2'])

DHWA_2$hdw_hour[DHWA_2$group=='DTWD'] = DHWA_2$hdw_hour[DHWA_2$group=='DTWD']*
  abs(ODHW_yield_fit_2_90$Estimate_fn1[ODHW_yield_fit_2_90$Var=='hdw_hour3'])


DHWA_3$hdw_hour[DHWA_3$group=='HTLH'] = DHWA_3$hdw_hour[DHWA_3$group=='HTLH']*
  abs(ODHW_yield_fit_3_90$Estimate_fn1[ODHW_yield_fit_3_90$Var=='hdw_hour1'])

DHWA_3$hdw_hour[DHWA_3$group=='PRGW'] = DHWA_3$hdw_hour[DHWA_3$group=='PRGW']*
  abs(ODHW_yield_fit_3_90$Estimate_fn1[ODHW_yield_fit_3_90$Var=='hdw_hour2'])

DHWA_3$hdw_hour[DHWA_3$group=='DTWD'] = DHWA_3$hdw_hour[DHWA_3$group=='DTWD']*
  abs(ODHW_yield_fit_3_90$Estimate_fn1[ODHW_yield_fit_3_90$Var=='hdw_hour3'])


DHWA_4$hdw_hour[DHWA_4$group=='HTLH'] = DHWA_4$hdw_hour[DHWA_4$group=='HTLH']*
  abs(ODHW_yield_fit_4_90$Estimate_fn1[ODHW_yield_fit_4_90$Var=='hdw_hour1'])

DHWA_4$hdw_hour[DHWA_4$group=='PRGW'] = DHWA_4$hdw_hour[DHWA_4$group=='PRGW']*
  abs(ODHW_yield_fit_4_90$Estimate_fn1[ODHW_yield_fit_4_90$Var=='hdw_hour2'])

DHWA_4$hdw_hour[DHWA_4$group=='DTWD'] = DHWA_4$hdw_hour[DHWA_4$group=='DTWD']*
  abs(ODHW_yield_fit_4_90$Estimate_fn1[ODHW_yield_fit_4_90$Var=='hdw_hour3'])



DHWA_5$hdw_hour[DHWA_5$group=='HTLH'] = DHWA_5$hdw_hour[DHWA_5$group=='HTLH']*
  abs(ODHW_yield_fit_5_90$Estimate_fn1[ODHW_yield_fit_5_90$Var=='hdw_hour1'])

DHWA_5$hdw_hour[DHWA_5$group=='PRGW'] = DHWA_5$hdw_hour[DHWA_5$group=='PRGW']*
  abs(ODHW_yield_fit_5_90$Estimate_fn1[ODHW_yield_fit_5_90$Var=='hdw_hour2'])

DHWA_5$hdw_hour[DHWA_5$group=='DTWD'] = DHWA_5$hdw_hour[DHWA_5$group=='DTWD']*
  abs(ODHW_yield_fit_5_90$Estimate_fn1[ODHW_yield_fit_5_90$Var=='hdw_hour3'])


DHWA_6$hdw_hour[DHWA_6$group=='HTLH'] = DHWA_6$hdw_hour[DHWA_6$group=='HTLH']*
  abs(ODHW_yield_fit_6_90$Estimate_fn1[ODHW_yield_fit_6_90$Var=='hdw_hour1'])

DHWA_6$hdw_hour[DHWA_6$group=='PRGW'] = DHWA_6$hdw_hour[DHWA_6$group=='PRGW']*
  abs(ODHW_yield_fit_6_90$Estimate_fn1[ODHW_yield_fit_6_90$Var=='hdw_hour2'])

DHWA_6$hdw_hour[DHWA_6$group=='DTWD'] = DHWA_6$hdw_hour[DHWA_6$group=='DTWD']*
  abs(ODHW_yield_fit_6_90$Estimate_fn1[ODHW_yield_fit_6_90$Var=='hdw_hour3'])

####################################
DHWB_1$hdw_hour[DHWB_1$group=='HTLH'] = DHWB_1$hdw_hour[DHWB_1$group=='HTLH']*
  abs(ODHW_yield_fit_1_10$Estimate_fn1[ODHW_yield_fit_1_10$Var=='hdw_hour1'])

DHWB_1$hdw_hour[DHWB_1$group=='PRGW'] i= DHWB_1$hdw_hour[DHWB_1$group=='PRGW']*
  abs(ODHW_yield_fit_1_10$Estimate_fn1[ODHW_yield_fit_1_10$Var=='hdw_hour2'])

DHWB_1$hdw_hour[DHWB_1$group=='DTWD'] = DHWB_1$hdw_hour[DHWB_1$group=='DTWD']*
  abs(ODHW_yield_fit_1_10$Estimate_fn1[ODHW_yield_fit_1_10$Var=='hdw_hour3'])


DHWB_2$hdw_hour[DHWB_2$group=='HTLH'] = DHWB_2$hdw_hour[DHWB_2$group=='HTLH']*
  abs(ODHW_yield_fit_2_10$Estimate_fn1[ODHW_yield_fit_2_10$Var=='hdw_hour1'])

DHWB_2$hdw_hour[DHWB_2$group=='PRGW'] = DHWB_2$hdw_hour[DHWB_2$group=='PRGW']*
  abs(ODHW_yield_fit_2_10$Estimate_fn1[ODHW_yield_fit_2_10$Var=='hdw_hour2'])

DHWB_2$hdw_hour[DHWB_2$group=='DTWD'] = DHWB_2$hdw_hour[DHWB_2$group=='DTWD']*
  abs(ODHW_yield_fit_2_10$Estimate_fn1[ODHW_yield_fit_2_10$Var=='hdw_hour3'])


DHWB_3$hdw_hour[DHWB_3$group=='HTLH'] = DHWB_3$hdw_hour[DHWB_3$group=='HTLH']*
  abs(ODHW_yield_fit_3_10$Estimate_fn1[ODHW_yield_fit_3_10$Var=='hdw_hour1'])

DHWB_3$hdw_hour[DHWB_3$group=='PRGW'] = DHWB_3$hdw_hour[DHWB_3$group=='PRGW']*
  abs(ODHW_yield_fit_3_10$Estimate_fn1[ODHW_yield_fit_3_10$Var=='hdw_hour2'])

DHWB_3$hdw_hour[DHWB_3$group=='DTWD'] = DHWB_3$hdw_hour[DHWB_3$group=='DTWD']*
  abs(ODHW_yield_fit_3_10$Estimate_fn1[ODHW_yield_fit_3_10$Var=='hdw_hour3'])


DHWB_4$hdw_hour[DHWB_4$group=='HTLH'] = DHWB_4$hdw_hour[DHWB_4$group=='HTLH']*
  abs(ODHW_yield_fit_4_10$Estimate_fn1[ODHW_yield_fit_4_10$Var=='hdw_hour1'])

DHWB_4$hdw_hour[DHWB_4$group=='PRGW'] = DHWB_4$hdw_hour[DHWB_4$group=='PRGW']*
  abs(ODHW_yield_fit_4_10$Estimate_fn1[ODHW_yield_fit_4_10$Var=='hdw_hour2'])

DHWB_4$hdw_hour[DHWB_4$group=='DTWD'] = DHWB_4$hdw_hour[DHWB_4$group=='DTWD']*
  abs(ODHW_yield_fit_4_10$Estimate_fn1[ODHW_yield_fit_4_10$Var=='hdw_hour3'])



DHWB_5$hdw_hour[DHWB_5$group=='HTLH'] = DHWB_5$hdw_hour[DHWB_5$group=='HTLH']*
  abs(ODHW_yield_fit_5_10$Estimate_fn1[ODHW_yield_fit_5_10$Var=='hdw_hour1'])

DHWB_5$hdw_hour[DHWB_5$group=='PRGW'] = DHWB_5$hdw_hour[DHWB_5$group=='PRGW']*
  abs(ODHW_yield_fit_5_10$Estimate_fn1[ODHW_yield_fit_5_10$Var=='hdw_hour2'])

DHWB_5$hdw_hour[DHWB_5$group=='DTWD'] = DHWB_5$hdw_hour[DHWB_5$group=='DTWD']*
  abs(ODHW_yield_fit_5_10$Estimate_fn1[ODHW_yield_fit_5_10$Var=='hdw_hour3'])


DHWB_6$hdw_hour[DHWB_6$group=='HTLH'] = DHWB_6$hdw_hour[DHWB_6$group=='HTLH']*
  abs(ODHW_yield_fit_6_10$Estimate_fn1[ODHW_yield_fit_6_10$Var=='hdw_hour1'])

DHWB_6$hdw_hour[DHWB_6$group=='PRGW'] = DHWB_6$hdw_hour[DHWB_6$group=='PRGW']*
  abs(ODHW_yield_fit_6_10$Estimate_fn1[ODHW_yield_fit_6_10$Var=='hdw_hour2'])

DHWB_6$hdw_hour[DHWB_6$group=='DTWD'] = DHWB_6$hdw_hour[DHWB_6$group=='DTWD']*
  abs(ODHW_yield_fit_6_10$Estimate_fn1[ODHW_yield_fit_6_10$Var=='hdw_hour3'])


#######################

DHWA_1$Model = 'MSMT';DHWB_1$Model = 'MSMT';
DHWA_2$Model = 'MSMM';DHWB_2$Model = 'MSMM';
DHWA_3$Model = 'MMSB';DHWB_3$Model = 'MMSB';
DHWA_4$Model = 'MPer';DHWB_4$Model = 'MPer';
DHWA_5$Model = 'MEFD';DHWB_5$Model = 'MEFD';
DHWA_6$Model = 'MDHW';DHWB_6$Model = 'MDHW';


DHWA_1$sens = '1981-1999';DHWB_1$sens = '2000-2018';
DHWA_2$sens = '1981-1999';DHWB_2$sens = '2000-2018';
DHWA_3$sens = '1981-1999';DHWB_3$sens = '2000-2018';
DHWA_4$sens = '1981-1999';DHWB_4$sens = '2000-2018';
DHWA_5$sens = '1981-1999';DHWB_5$sens = '2000-2018';
DHWA_6$sens = '1981-1999';DHWB_6$sens = '2000-2018';


DHWAB16 = rbind(DHWA_1,DHWA_2,DHWA_3,DHWA_4,DHWA_5,DHWA_6,
                DHWB_1,DHWB_2,DHWB_3,DHWB_4,DHWB_5,DHWB_6)


HDW_type_YTrend_gg =ggplot()+geom_sf(data = DHWAB16[DHWAB16$Model=='MMSB',],
                                     aes(fill=hdw_hour), 
          linewidth = 0.05, color = 'gray85') +
  scale_fill_stepsn(colors = c(brewer.pal(9, "RdPu")),
                    breaks =  c(seq(0,20,2),25,seq(50,300,50)),
                    labels =  c(seq(0,20,2),25,seq(50,300,50)),
                    limits = c(0,300),na.value = "white",
                    values = scales::rescale(c(seq(0,20,2),25,seq(50,300,50))),
                    name = bquote((kg~ha^-1~season^-1)))+
  labs(subtitle = bquote((a)~Yield~loss~attributed~to~DHW))+
  facet_grid(sens~group)+theme_bw()+ xlim(-2500000,2000000)+ylim(4000000,6300000)+
  geom_sf(data=China_line, color="grey65", linewidth = 0.1)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.1)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.1)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.1)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1.45,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.70, 1.175),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.18,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 6),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
# HDW_type_YTrend_gg



tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure 4.tiff',
     units = "cm", width=15,height=13, res = 1500, compression = 'lzw',pointsize = 11)

ggdraw() +
  draw_plot(HDW_type_YTrend_gg, x = 0, y = 0.46,  width =1, height = 0.60) +
  draw_plot(Change_yloss_gg,    x = 0, y = 0.00,  width =1, height = 0.52)
dev.off()

unique(DHWAB16$Model)
HDW_MSMT_YTrend_gg =ggplot()+geom_sf(data = DHWAB16[DHWAB16$Model=='MSMT',],
                                     aes(fill=hdw_hour), 
                                     linewidth = 0.05, color = 'gray85') +
  scale_fill_stepsn(colors = c(brewer.pal(9, "RdPu")),
                    breaks =  c(seq(0,20,2),25,seq(50,300,50)),
                    labels =  c(seq(0,20,2),25,seq(50,300,50)),
                    limits = c(0,300),na.value = "white",
                    values = scales::rescale(c(seq(0,20,2),25,seq(50,300,50))),
                    name = bquote((kg~ha^-1~season^-1)))+
  labs(subtitle = bquote((a)~Yield~loss~attributed~to~DHW))+
  facet_grid(sens~group)+theme_bw()+ xlim(-2500000,2000000)+ylim(4000000,6300000)+
  geom_sf(data=China_line, color="grey65", linewidth = 0.1)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.1)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.1)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.1)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1.45,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.70, 1.175),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.18,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 6),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S14.tiff',
     units = "cm", width=15,height=7, res = 1500, compression = 'lzw',pointsize = 11)
HDW_MSMT_YTrend_gg
dev.off()


unique(DHWAB16$Model)
HDW_MSMM_YTrend_gg =ggplot()+geom_sf(data = DHWAB16[DHWAB16$Model=='MSMM',],
                                     aes(fill=hdw_hour), 
                                     linewidth = 0.05, color = 'gray85') +
  scale_fill_stepsn(colors = c(brewer.pal(9, "RdPu")),
                    breaks =  c(seq(0,20,2),25,seq(50,300,50)),
                    labels =  c(seq(0,20,2),25,seq(50,300,50)),
                    limits = c(0,300),na.value = "white",
                    values = scales::rescale(c(seq(0,20,2),25,seq(50,300,50))),
                    name = bquote((kg~ha^-1~season^-1)))+
  labs(subtitle = bquote((a)~Yield~loss~attributed~to~DHW))+
  facet_grid(sens~group)+theme_bw()+ xlim(-2500000,2000000)+ylim(4000000,6300000)+
  geom_sf(data=China_line, color="grey65", linewidth = 0.1)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.1)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.1)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.1)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1.45,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.70, 1.175),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.18,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 6),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S15.tiff',
     units = "cm", width=15,height=7, res = 1500, compression = 'lzw',pointsize = 11)
HDW_MSMM_YTrend_gg
dev.off()

unique(DHWAB16$Model)
HDW_MPer_YTrend_gg =ggplot()+geom_sf(data = DHWAB16[DHWAB16$Model=='MPer',],
                                     aes(fill=hdw_hour), 
                                     linewidth = 0.05, color = 'gray85') +
  scale_fill_stepsn(colors = c(brewer.pal(9, "RdPu")),
                    breaks =  c(seq(0,20,2),25,seq(50,300,50)),
                    labels =  c(seq(0,20,2),25,seq(50,300,50)),
                    limits = c(0,300),na.value = "white",
                    values = scales::rescale(c(seq(0,20,2),25,seq(50,300,50))),
                    name = bquote((kg~ha^-1~season^-1)))+
  labs(subtitle = bquote((a)~Yield~loss~attributed~to~DHW))+
  facet_grid(sens~group)+theme_bw()+ xlim(-2500000,2000000)+ylim(4000000,6300000)+
  geom_sf(data=China_line, color="grey65", linewidth = 0.1)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.1)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.1)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.1)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1.45,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.70, 1.175),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.18,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 6),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S16.tiff',
     units = "cm", width=15,height=7, res = 1500, compression = 'lzw',pointsize = 11)
HDW_MPer_YTrend_gg
dev.off()


unique(DHWAB16$Model)
HDW_MEFD_YTrend_gg =ggplot()+geom_sf(data = DHWAB16[DHWAB16$Model=='MEFD',],
                                     aes(fill=hdw_hour), 
                                     linewidth = 0.05, color = 'gray85') +
  scale_fill_stepsn(colors = c(brewer.pal(9, "RdPu")),
                    breaks =  c(seq(0,20,2),25,seq(50,300,50)),
                    labels =  c(seq(0,20,2),25,seq(50,300,50)),
                    limits = c(0,300),na.value = "white",
                    values = scales::rescale(c(seq(0,20,2),25,seq(50,300,50))),
                    name = bquote((kg~ha^-1~season^-1)))+
  labs(subtitle = bquote((a)~Yield~loss~attributed~to~DHW))+
  facet_grid(sens~group)+theme_bw()+ xlim(-2500000,2000000)+ylim(4000000,6300000)+
  geom_sf(data=China_line, color="grey65", linewidth = 0.1)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.1)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.1)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.1)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1.45,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.70, 1.175),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.18,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 6),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S17.tiff',
     units = "cm", width=15,height=7, res = 1500, compression = 'lzw',pointsize = 11)
HDW_MEFD_YTrend_gg
dev.off()


unique(DHWAB16$Model)
HDW_MDHW_YTrend_gg =ggplot()+geom_sf(data = DHWAB16[DHWAB16$Model=='MDHW',],
                                     aes(fill=hdw_hour), 
                                     linewidth = 0.05, color = 'gray85') +
  scale_fill_stepsn(colors = c(brewer.pal(9, "RdPu")),
                    breaks =  c(seq(0,20,2),25,seq(50,300,50)),
                    labels =  c(seq(0,20,2),25,seq(50,300,50)),
                    limits = c(0,300),na.value = "white",
                    values = scales::rescale(c(seq(0,20,2),25,seq(50,300,50))),
                    name = bquote((kg~ha^-1~season^-1)))+
  labs(subtitle = bquote((a)~Yield~loss~attributed~to~DHW))+
  facet_grid(sens~group)+theme_bw()+ xlim(-2500000,2000000)+ylim(4000000,6300000)+
  geom_sf(data=China_line, color="grey65", linewidth = 0.1)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.1)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.1)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.1)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1.45,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.70, 1.175),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.18,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 6),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S18.tiff',
     units = "cm", width=15,height=7, res = 1500, compression = 'lzw',pointsize = 11)
HDW_MDHW_YTrend_gg
dev.off()
#########################################
#########################################
#########################################
