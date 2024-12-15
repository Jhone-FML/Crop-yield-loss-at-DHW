library(ggpubr)


BootReg_change    = function(fmula, boot){
  
  climvars = colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula1   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  climvars = colnames(Fit_YSoclivars[c(c(59:69,92:102,125:135,150:159))])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula2   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))

  
  climvars = colnames(Fit_YSoclivars[c(c(70:80,103:113,136:145,150:159))])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula3   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))

  
  climvars = colnames(Fit_YSoclivars[c(147:159,162:164)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula4   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))

  
  climvars = colnames(Fit_YSoclivars[c(150:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula5   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  climvars = colnames(Fit_YSoclivars[c(157:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula6   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  # 
  # +year+site
  reg_data = Fit_YSoclivars
  reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  reg_data$site = as.numeric(reg_data$site)
  reg_data$year = as.numeric(reg_data$year)
  
  
  bootcoefs = c()
  # boot = 100
  # size = 6000
  yrs.to.samp = 1981:2018
  Rsq       = NULL
  for (n in 1:boot) {
    
    yrsamp = sample(yrs.to.samp,size= 38,replace = T)
    tempdf = reg_data %>% filter(year == yrsamp[1]) 
    
    for (k in 2:38) tempdf = rbind(tempdf, reg_data %>% filter(year == yrsamp[k]))
    
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
  
  hdw_hour1perc =  -1*((bootcoef$Estimate[bootcoef$Var=='hdw_hour1'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour1:year'] * 2018) - (bootcoef$Estimate[bootcoef$Var=='hdw_hour1'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour1:year'] * 1981) )
  hdw_hour2perc =  -1*((bootcoef$Estimate[bootcoef$Var=='hdw_hour2'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour2:year'] * 2018) - (bootcoef$Estimate[bootcoef$Var=='hdw_hour2'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour2:year'] * 1981) )
  hdw_hour3perc =  -1*((bootcoef$Estimate[bootcoef$Var=='hdw_hour3'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour3:year'] * 2018) - (bootcoef$Estimate[bootcoef$Var=='hdw_hour3'] + bootcoef$Estimate[bootcoef$Var=='hdw_hour3:year'] * 1981) )
  
  Mbootcoef     = data.frame(value = c(hdw_hour1perc,hdw_hour2perc,hdw_hour3perc),
                             varbs = rep(c('HTLH','PRGW','DTWD'), each = length(hdw_hour1perc))) 
  return(Mbootcoef)
}

  
Reg1 = BootReg_change(fmula1, boot = 200)
Reg2 = BootReg_change(fmula2, boot = 200)
Reg3 = BootReg_change(fmula3, boot = 200)
Reg4 = BootReg_change(fmula4, boot = 200)
Reg5 = BootReg_change(fmula5, boot = 200)
Reg6 = BootReg_change(fmula6, boot = 200)

Reg1$model = 'MSMT'; Reg1$ID = c(1:nrow(Reg1))
Reg2$model = 'MSMM'; Reg2$ID = c(1:nrow(Reg2))
Reg3$model = 'MMSB'; Reg3$ID = c(1:nrow(Reg3))
Reg4$model = 'MPer'; Reg4$ID = c(1:nrow(Reg4))
Reg5$model = 'MEFD'; Reg5$ID = c(1:nrow(Reg5))
Reg6$model = 'MDHW'; Reg6$ID = c(1:nrow(Reg6))

Year_change = rbind(Reg1,Reg2,Reg3,Reg4,Reg5,Reg6)

YS_DHW_gg_df = Year_change#[Year_change$varbs!='DTWD'&Year_change$model=='MSMM',]  
####################################################
YS_DHW_mean_df = YS_DHW_gg_df %>% group_by(ID,varbs) %>% summarise_at(vars("value"),list(mean));
YS_DHW_mean_df$Group = 'County level'

MYS_DHW_gg =  ggplot(YS_DHW_mean_df) + 
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
  scale_color_manual(name = '',values = brewer.pal(8, "Set2"))+
  scale_fill_manual(name = '',values = brewer.pal(8, "Set2"))+
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'black')+
  stat_summary(aes(x = varbs, y = value,label=round(..y..,1)),
               hjust = -0.8,hjust = -0.3,fun.y=mean, geom="text", size=2, color="black") +
  labs(subtitle = "(a)")+theme_bw()+#facet_wrap(model~.,ncol =3)+
  scale_y_continuous(limits = c(0, 100),breaks= seq(0, 100,20))+
  ylab(bquote('Change in yield sensitivity to DHW 1981-2018'~(kg~ha^-1~hour^-1)))+
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



######################################################
library(distributional)
library(ggdist)

YS_DHW_gg =  ggplot(YS_DHW_gg_df) + 
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
  labs(subtitle = "(a)")+theme_bw()+facet_wrap(model~.,ncol =3)+
  scale_y_continuous(limits = c(-40, 100),breaks= seq(-40, 100,20))+
  ylab(bquote('Change in yield sensitivity to DHW 1981-2018'~(kg~ha^-1~hour^-1)))+
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




# YS_DHW_gg =  ggplot(YS_DHW_gg_df,aes(varbs,value/10, colour = varbs))+
#   geom_jitter(aes(colour = varbs), size=0.8, alpha=0.1) +
#   geom_boxplot(alpha = 0.4,outliers = F)+
#   stat_summary(fun.y=mean, geom="point", shape=19, size=2, color="red", fill="red") +
#   stat_summary(aes(label=round(..y..,1)),hjust = -0.5,fun.y=mean, geom="text", size=2, color="black") +
#   labs(subtitle = "(a)")+theme_bw()+facet_wrap(model~.,ncol =3)+
#   scale_y_continuous(limits = c(-100, 100),breaks= seq(-100, 100,20))+
#   ylab('Change in yield sensitivity to DHW 1981-2018 (%)')+
#   geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'gray75')+
#   scale_colour_manual(name = '',values = brewer.pal(8, "Dark2"))+
#   scale_x_discrete('',labels = c(
#     "DTWD" = bquote(DHW['DTWD']),
#     "HTLH" = bquote(DHW['HTLH']),
#     "PRGW" = bquote(DHW['PRGW'])))+coord_flip()+
#   theme(plot.subtitle = element_text(size = 7,                     # Font size
#                                      hjust = 0.02,                     # Horizontal adjustment
#                                      vjust = 1.5,                     # Vertical adjustment
#                                      lineheight = 2),
#         strip.text = element_text(size = 5),
#         legend.key.size = unit(0.3,'cm'),
#         # axis.ticks = element_blank(),
#         axis.text.x = element_text(size = 5,angle = 0,vjust = 0.5),
#         axis.text.y = element_text(size = 5),
#         axis.title = element_text(size = 6),
#         legend.position = c(10.10,0.08),
#         legend.key.height = unit(0.2,'cm'),
#         legend.background = element_blank(),
#         legend.text = element_text(size = 6),
#         legend.title= element_text(size = 6),
#         plot.background = element_rect(fill = "transparent",
#                                        colour = NA_character_))


Provience = unique(Fit_YSoclivars$Provience)
Provience = c(Provience,list(Provience))

BootReg_Prov  = function(fmula, boot, size, Prov){
  
  climvars = colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula1   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  climvars = colnames(Fit_YSoclivars[c(c(59:69,92:102,125:135,150:159))])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula2   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  
  climvars = colnames(Fit_YSoclivars[c(c(70:80,103:113,136:145,150:159))])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula3   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  
  climvars = colnames(Fit_YSoclivars[c(147:159,162:164)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula4   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  
  climvars = colnames(Fit_YSoclivars[c(150:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula5   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  climvars = colnames(Fit_YSoclivars[c(157:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula6   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  # 
  # 
  # +year+site
  # Prov     = "北京"
  reg_data = Fit_YSoclivars
  reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  reg_data$site = as.numeric(reg_data$site)
  reg_data$year = as.numeric(reg_data$year)
  reg_data = reg_data[reg_data$Provience %in% Prov,]
  
  yrs.to.samp = 1981:2018
  Rsq       = NULL
  bootcoefs = NULL
  for (n in 1:boot) {
    
    yrsamp = sample(yrs.to.samp,size= 38,replace = T)
    tempdf = reg_data %>% filter(year == yrsamp[1]) 
    
    for (k in 2:38) tempdf = rbind(tempdf, reg_data %>% filter(year == yrsamp[k]))
    
    cropfit    = feols(fmula, data = tempdf) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean));
  # 
  # Mbootcoef$rsq  = mean(Rsq)
 
  hdw_hour1delt =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1:year'] * 1981) - (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1:year'] * 2018)
  hdw_hour2delt =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2:year'] * 1981) - (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2:year'] * 2018)
  # hdw_hour3delt =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3:year'] * 1981) - (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3:year'] * 2018)
  
  hdw_hour1mean =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1:year'] * ((1981+2018)/2)) 
  hdw_hour2mean =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2:year'] * ((1981+2018)/2)) 
  # hdw_hour3mean =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3:year'] * ((1981+2018)/2)) 
  
  Mbootcoef     = data.frame(Change = c(hdw_hour1delt,hdw_hour2delt),
                             mean   = c(hdw_hour1mean,hdw_hour2mean),
                             varbs  = rep(c('HTLH','PRGW'), each = length(hdw_hour1delt)),
                             Prov   = Prov) 
  return(Mbootcoef)
}

Sens_Pro_df = NULL
for (i in 8:length(Provience)) {
  
  Reg_prov1 = BootReg_Prov(fmula1, boot = 100, size = 6000, Prov = Provience[[i]])
  Reg_prov2 = BootReg_Prov(fmula2, boot = 100, size = 6000, Prov = Provience[[i]])
  Reg_prov3 = BootReg_Prov(fmula3, boot = 100, size = 6000, Prov = Provience[[i]])
  Reg_prov4 = BootReg_Prov(fmula4, boot = 100, size = 6000, Prov = Provience[[i]])
  Reg_prov5 = BootReg_Prov(fmula5, boot = 100, size = 6000, Prov = Provience[[i]])
  Reg_prov6 = BootReg_Prov(fmula6, boot = 100, size = 6000, Prov = Provience[[i]])
  
  Reg_prov1$model = 'MSMT'
  Reg_prov2$model = 'MSMM'
  Reg_prov3$model = 'MMSB'
  Reg_prov4$model = 'MPer'
  Reg_prov5$model = 'MEFD'
  Reg_prov6$model = 'MDHW'
  
  Sens_Pro_df = rbind(Sens_Pro_df, Reg_prov1,Reg_prov2,Reg_prov3,Reg_prov4,Reg_prov5,Reg_prov6)
}

# DHW_fit_df_mean = DHW_yield_fit_df %>% group_by(Var,Model) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean));
# DHW_fit_df_mean = DHW_fit_df_mean[146:163,]
# colnames(DHW_fit_df_mean)[1:2] = c('varbs','model')
# DHW_fit_df_mean$varbs[DHW_fit_df_mean$varbs=='hdw_hour1'] = 'HTLH'
# DHW_fit_df_mean$varbs[DHW_fit_df_mean$varbs=='hdw_hour2'] = 'PRGW'
# DHW_fit_df_mean$varbs[DHW_fit_df_mean$varbs=='hdw_hour3'] = 'DTWD'
# 
# Sens_Pro_df_mer = left_join(Sens_Pro_df, DHW_fit_df_mean[,1:3], by = c('varbs','model'))

BootReg_ALL  = function(fmula, boot, size, Prov){
  
  climvars = colnames(Fit_YSoclivars[c(48:58,81:91,114:124,150:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula1   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  climvars = colnames(Fit_YSoclivars[c(c(59:69,92:102,125:135,150:159))])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula2   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  
  climvars = colnames(Fit_YSoclivars[c(c(70:80,103:113,136:145,150:159))])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula3   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  
  climvars = colnames(Fit_YSoclivars[c(147:159,162:164)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula4   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  
  climvars = colnames(Fit_YSoclivars[c(150:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula5   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  
  climvars = colnames(Fit_YSoclivars[c(157:159)])
  vars     = c(climvars,'year',paste0('year:',climvars))
  fmula6   = as.formula(paste0('Yield_Y~',paste0(vars,collapse = '+'),'+site'))
  # 
  # 
  # +year+site
  # Prov     = "北京"
  reg_data = Fit_YSoclivars
  reg_data = reg_data[is.na(reg_data$Yield_Y)!=T,]
  reg_data$site = as.numeric(reg_data$site)
  reg_data$year = as.numeric(reg_data$year)
  reg_data = reg_data#[reg_data$Provience %in% Prov,]
  
  yrs.to.samp = 1981:2018
  Rsq       = NULL
  bootcoefs = NULL
  for (n in 1:boot) {
    
    yrsamp = sample(yrs.to.samp,size= 38,replace = T)
    tempdf = reg_data %>% filter(year == yrsamp[1]) 
    
    for (k in 2:38) tempdf = rbind(tempdf, reg_data %>% filter(year == yrsamp[k]))
    
    cropfit    = feols(fmula, data = tempdf) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean));
  # 
  # Mbootcoef$rsq  = mean(Rsq)
  
  hdw_hour1delt =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1:year'] * 1981) - (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1:year'] * 2018)
  hdw_hour2delt =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2:year'] * 1981) - (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2:year'] * 2018)
  # hdw_hour3delt =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3:year'] * 1981) - (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3:year'] * 2018)
  
  hdw_hour1mean =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour1:year'] * ((1981+2018)/2)) 
  hdw_hour2mean =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour2:year'] * ((1981+2018)/2)) 
  # hdw_hour3mean =  (Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3'] + Mbootcoef$Estimate[Mbootcoef$Var=='hdw_hour3:year'] * ((1981+2018)/2)) 
  
  Mbootcoef     = data.frame(Change = c(hdw_hour1delt,hdw_hour2delt),
                             mean   = c(hdw_hour1mean,hdw_hour2mean),
                             varbs  = rep(c('HTLH','PRGW'), each = length(hdw_hour1delt)),
                             Prov   = 'All') 
  return(Mbootcoef)
}

Reg_ALL1 = BootReg_ALL(fmula1, boot = 100)
Reg_ALL2 = BootReg_ALL(fmula2, boot = 100)
Reg_ALL3 = BootReg_ALL(fmula3, boot = 100)
Reg_ALL4 = BootReg_ALL(fmula4, boot = 100)
Reg_ALL5 = BootReg_ALL(fmula5, boot = 100)
Reg_ALL6 = BootReg_ALL(fmula6, boot = 100)

Reg_ALL1$model = 'MSMT'; Reg_ALL1$ID = c(1:nrow(Reg_ALL1));Sens_Pro_df$ID = c(1:nrow(Sens_Pro_df))
Reg_ALL2$model = 'MSMM'; Reg_ALL2$ID = c(1:nrow(Reg_ALL2))
Reg_ALL3$model = 'MMSB'; Reg_ALL3$ID = c(1:nrow(Reg_ALL3))
Reg_ALL4$model = 'MPer'; Reg_ALL4$ID = c(1:nrow(Reg_ALL4))
Reg_ALL5$model = 'MEFD'; Reg_ALL5$ID = c(1:nrow(Reg_ALL5))
Reg_ALL6$model = 'MDHW'; Reg_ALL6$ID = c(1:nrow(Reg_ALL6))

Sens_Pro_df_mer = rbind(Sens_Pro_df, Reg_ALL1,Reg_ALL2,
                        Reg_ALL3,Reg_ALL4,Reg_ALL5,Reg_ALL6)

Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="北京"] = 'Beijing'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="天津"] = 'Tianjing'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="河北"] = 'Hebei'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="山西"] = 'Shanxi'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="内蒙古"] = 'Inner Mongolia'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="黑龙江"] = 'Heilongjiang'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="江苏"] = 'Jiangsu'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="山东"] = 'Shandong'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="河南"] = 'Henan'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="陕西"] = 'Shaanxi'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="甘肃"] = 'Gansu'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="青海"] = 'Qinghai'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="宁夏"] = 'Ningxia'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="新疆"] = 'Xinjiang'
Sens_Pro_df_mer$Prov[Sens_Pro_df_mer$Prov=="安徽"] = 'Anhiu'

Sens_Pro_gg_df = Sens_Pro_df_mer#[Sens_Pro_df_mer$model=='MSMM',]
#Sens_Pro_gg_df = Sens_Pro_gg_df[-c(4:8,11:14,19:20),]

Sens_Pro_gg = ggscatter(Sens_Pro_gg_df, x = "mean", y = "Change",color = "Prov",shape = 'varbs',
          palette =  c(brewer.pal(8, "Dark2"), brewer.pal(11, "Paired")),alpha =0.4,
          label = "Prov",font.label = list(size = 4),size = 1,repel = TRUE)+
  facet_wrap(model~.,ncol = 3)+theme_bw()+
  ylab(bquote('Δ sensitivity to DHW 1981-2018 ' (kg~ha^-2~hour^-1)))+
  xlab(bquote('Average sensitivity to DHW ' (kg~ha^-2~hour^-1)))+
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'Black')+
  geom_vline(xintercept = 0,lwd = 0.5, linetype = "dashed", color = 'Black')+
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

Sens_Pro_mean_df = Sens_Pro_gg_df %>% group_by(varbs,Prov) %>% summarise_at(vars('Change', 'mean'),list(mean));

Sens_Pro_mean_gg = ggscatter(Sens_Pro_mean_df, x = "mean", y = "Change",color = "Prov",shape = 'varbs',
                        palette =  c(brewer.pal(8, "Dark2"), brewer.pal(11, "Paired")),alpha =0.4,
                        label = "Prov",font.label = list(size = 4),size = 2,repel = TRUE)+
  theme_bw()+ylab(bquote(atop('Time trends in yield sensitivity','to DHW 1981-2018 ' (kg~ha^-2~hour^-1))))+
  xlab(bquote('Average yield sensitivity to DHW ' (kg~ha^-2~hour^-1)))+
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'Black')+
  geom_vline(xintercept = 0,lwd = 0.5, linetype = "dashed", color = 'Black')+
  facet_grid(~ varbs, scales = "free_x",space = "free_x",switch = "x") +
  theme(panel.spacing = unit(0.5, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.background = element_rect(fill = "white"))+ # removes the background from the state names
  labs(subtitle = "(c)")+
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
        # panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


# Sens_Pro_gg = ggplot(Sens_Pro_gg_df,aes(mean,Change))+
#   geom_text(aes(label = Prov), size =1,alpha = 0.5)+
#   geom_point(aes(colour = Prov,shape = varbs), size=2, alpha=0.4) +
#   facet_grid(model~.,scales = "free_x")+
#   labs(subtitle = "(b)")+theme_bw()+
#   ylab(bquote('Δ sensitivity to DHW 1981-2018 ' (kg~ha^-2~hour^-1)))+
#   xlab(bquote('Average sensitivity to DHW ' (kg~ha^-2~hour^-1)))+
#   geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'gray75')+
#   geom_vline(xintercept = 0,lwd = 0.5, linetype = "dashed", color = 'gray75')+
#   scale_colour_manual(name = '',values = c(brewer.pal(8, "Dark2"), brewer.pal(11, "Paired")))+
#   theme(plot.subtitle = element_text(size = 7,                     # Font size
#                                      hjust = 0.02,                     # Horizontal adjustment
#                                      vjust = 1.5,                     # Vertical adjustment
#                                      lineheight = 2),
#         strip.text = element_text(size = 5),
#         legend.key.size = unit(0.3,'cm'),
#         # axis.ticks = element_blank(),
#         axis.text.x = element_text(size = 5,angle = 0,vjust = 0.5),
#         axis.text.y = element_text(size = 5),
#         axis.title = element_text(size = 6),
#         legend.position = c(.12,0.80),
#         legend.key.height = unit(0.2,'cm'),
#         legend.background = element_blank(),
#         legend.text = element_text(size = 4),
#         legend.title= element_text(size = 4),
#         plot.background = element_rect(fill = "transparent",
#                                        colour = NA_character_))


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S13.tiff',
     units = "cm", width=14, height=12, res = 1500, compression = 'lzw', pointsize = 11)
YS_DHW_gg
dev.off()


tiff('E:/Post PHD documents/Crop yield loss at DHW/Submission/Figure/Figure S14.tiff',
     units = "cm", width=14, height=14, res = 1500, compression = 'lzw', pointsize = 11)
Sens_Pro_gg
dev.off()