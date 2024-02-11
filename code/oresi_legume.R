#OREI kernza legume intercropping analysis
library(googlesheets4)
gs4_deauth()
dat<-read_sheet("https://docs.google.com/spreadsheets/d/1Rt3Rx7fnD5eUEdQ3GMInHGlX_qkXgwR4VyFzC6_UxAI/edit#gid=0",
                col_types="ccccnnnnnnnnnnnnnnnnnc")
str(dat)
library(Rmisc)
library(magrittr)
library(tidyverse)
dat %<>% mutate(across(where(is.character), as_factor))
library(nlme)
library(emmeans)
library(multcomp)
#coefficient to convert yields per plot to kg/ha
#two quadrats were sampled per plot, each was 61cm by 61cm, so a total
#sample area of 0.7442 square meters. Divide biomass by this and multiply by 10,
#or simply divide by 0.07442
yldconvert<-0.07442
sdat<-dat %>% 
  dplyr::select("Year", "Plot", "Trt_code", "Treatment", "Threshed_grain_no_bag(g)",
                "Dry_straw_no_bag(g)", "Dry_Red_clover_no_bag(g)","Dry_Alfalfa_no_bag(g)",
                "Dry_weeds_no_bag(g)") %>% 
  rename(c(yr="Year", plot="Plot", trt_code="Trt_code", trt="Treatment",
           grain="Threshed_grain_no_bag(g)",
           straw="Dry_straw_no_bag(g)", clover="Dry_Red_clover_no_bag(g)",
           alfalfa="Dry_Alfalfa_no_bag(g)",
           weeds="Dry_weeds_no_bag(g)")) %>% 
  mutate(grainyld=grain/yldconvert,
         strawyld=straw/yldconvert,
         cloveryld=ifelse(is.na(clover), NA, clover)/yldconvert,
         alfalfayld=ifelse(is.na(alfalfa), NA, alfalfa)/yldconvert,
         weedyld=ifelse(is.na(weeds), NA, weeds)/yldconvert,
         legumeyld=rowSums(across(cloveryld:alfalfayld), na.rm=T),
         IWGyld=grainyld+strawyld,
         rep=as.numeric(substr(as.character(plot), 1, 1))) %>% 
  mutate(legumeyld = if_else(is.na(cloveryld) & is.na(alfalfayld), NA_real_, legumeyld))


sdat2<-summarySE(sdat, "grainyld", c("yr", "rep", "trt_code", "trt"),
                 na.rm=TRUE)[,1:6]
sdat2$strawyld<-summarySE(sdat, "strawyld", c("yr", "rep", "trt_code","trt"), na.rm=TRUE)[,6]
sdat2$legumeyld<-summarySE(sdat, "legumeyld", c("yr", "rep","trt_code", "trt"), na.rm=TRUE)[,6]
sdat2$weedyld<-summarySE(sdat, "weedyld", c("yr", "rep","trt_code", "trt"), na.rm=TRUE)[,6]
sdat2$g_c<-rep(summarySE(subset(sdat, trt=="K control"), "grainyld", 
                         c("yr", "rep", "trt_code","trt"), na.rm=TRUE)[,6], each=10)
sdat2$IWG_c<-rep(summarySE(subset(sdat, trt=="K control"), "IWGyld", 
                         c("yr", "rep","trt_code", "trt"), na.rm=TRUE)[,6], each=10)
sdat2$g_.5<-rep(summarySE(subset(sdat, trt=="K+manure 0.5x"), "grainyld", 
                         c("yr", "rep", "trt_code","trt"), na.rm=TRUE)[,6], each=10)
sdat2$IWG_.5<-rep(summarySE(subset(sdat, trt=="K+manure 0.5x"), "IWGyld", 
                           c("yr", "rep","trt_code", "trt"), na.rm=TRUE)[,6], each=10)
sdat2$g_1<-rep(summarySE(subset(sdat, trt=="K+manure 1x"), "grainyld", 
                          c("yr", "rep","trt_code", "trt"), na.rm=TRUE)[,6], each=10)
sdat2$IWG_1<-rep(summarySE(subset(sdat, trt=="K+manure 1x"), "IWGyld", 
                            c("yr", "rep", "trt_code","trt"), na.rm=TRUE)[,6], each=10)
sdat2$g_2<-rep(summarySE(subset(sdat, trt=="K+manure 2x"), "grainyld", 
                         c("yr", "rep", "trt_code","trt"), na.rm=TRUE)[,6], each=10)
sdat2$IWG_2<-rep(summarySE(subset(sdat, trt=="K+manure 2x"), "IWGyld", 
                           c("yr", "rep", "trt_code","trt"), na.rm=TRUE)[,6], each=10)
sdat2$g_ry<-sdat2$grainyld/sdat2$g_c
sdat2$g_ry.5<-sdat2$grainyld/sdat2$g_.5
sdat2$g_ry1<-sdat2$grainyld/sdat2$g_1
sdat2$g_ry2<-sdat2$grainyld/sdat2$g_2
df2<-data.frame(trt_code=as.factor(1:10), legtrt=c(rep("Red clover",3), rep("Alfalfa", 3), rep("none",4)),
                timetrt=c(rep(c("fall","spring","summer"), 2),rep("none",4)),
                fert=c(rep("none",6),"0.5x","1x","2x", "0"))
sdat2 <- sdat2 %>% 
  left_join(df2, by="trt_code")
sdat2$legtrt<-factor(sdat2$legtrt, levels=c("Alfalfa","Red clover", "none"))
sdat2$timetrt<-factor(sdat2$timetrt, levels=c("fall","spring", "summer", "none"))
sdat2$fert<-factor(sdat2$fert, levels=c("0","0.5x", "1x", "2x", "none"))
#Plot it
pps = position_dodge(width = .75)
#dplyr way
sdat2 %>% 
  group_by(yr, trt_code, trt, legtrt, timetrt) %>% 
  summarise(sd_ry = sd(g_ry, na.rm=T), 
            n_ry = n(),
            g_ry = mean(g_ry, na.rm=T)) %>% #order matters if you want to keep the g_ry column name, must do mean after sd
  mutate(se_ry=sd_ry/sqrt(n_ry)) %>%
  filter(trt!="K control"&trt!="K+manure 0.5x"&trt!="K+manure 1x"&trt!="K+manure 2x") %>% 
ggplot(aes(x=legtrt, y=g_ry, color=timetrt)) + 
  geom_point(size=1.5, position = pps)+#, stat="identity", position = pps) + 
  facet_grid(~yr) +
  geom_hline(yintercept = 1, color="black")+
  geom_errorbar(aes(ymin=g_ry-se_ry, ymax=g_ry+se_ry), width=0.2, position = pps) +
  xlab("Treatment") +
  ylab("Grain yield relative to 0 fert")+
  ylim(0,3) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.y = element_text(size=10),
        strip.background =element_rect(color="black", fill="white"),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.text=element_text(size=10),
        legend.title=element_blank(),
        legend.key.size = unit(6,"mm"),
        legend.position=c(0.5,0.8),
        axis.text.y=element_text(size=10, color="black"),
        axis.title.y=element_text(size=10, color="black"),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=10,
                                 hjust=0.95, color='black'))
ggsave("GrainRYcontrol.png", width=7, height=3.5, units="in", path="/Users/junge037/Documents/Projects/orei_legume/figures/")

sdat2 %>% 
  group_by(yr, trt_code, trt, legtrt, timetrt) %>% 
  summarise(sd_ry = sd(g_ry.5, na.rm=T), 
            n_ry = n(),
            g_ry.5 = mean(g_ry.5, na.rm=T)) %>% #order matters if you want to keep the g_ry column name, must do mean after sd
  mutate(se_ry=sd_ry/sqrt(n_ry)) %>%
  filter(trt!="K control"&trt!="K+manure 0.5x"&trt!="K+manure 1x"&trt!="K+manure 2x") %>% 
  ggplot(aes(x=legtrt, y=g_ry.5, color=timetrt)) + 
  geom_point(size=1.5, position = pps)+#, stat="identity", position = pps) + 
  facet_grid(~yr) +
  geom_hline(yintercept = 1, color="black")+
  geom_errorbar(aes(ymin=g_ry.5-se_ry, ymax=g_ry.5+se_ry), width=0.2, position = pps) +
  xlab("Treatment") +
  ylab("Grain yield relative to 0.5x fert")+
  ylim(0,3) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.y = element_text(size=10),
        strip.background =element_rect(color="black", fill="white"),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.text=element_text(size=10),
        legend.title=element_blank(),
        legend.key.size = unit(6,"mm"),
        legend.position=c(0.5,0.8),
        axis.text.y=element_text(size=10, color="black"),
        axis.title.y=element_text(size=10, color="black"),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=10,
                                 hjust=0.95, color='black'))
ggsave("GrainRY.5x.png", width=7, height=3.5, units="in", path="/Users/junge037/Documents/Projects/orei_legume/figures/")

sdat2 %>% 
  group_by(yr, trt_code, trt, legtrt, timetrt) %>% 
  summarise(sd_ry = sd(g_ry1, na.rm=T), 
            n_ry = n(),
            g_ry1 = mean(g_ry1, na.rm=T)) %>% #order matters if you want to keep the g_ry column name, must do mean after sd
  mutate(se_ry=sd_ry/sqrt(n_ry)) %>%
  filter(trt!="K control"&trt!="K+manure 0.5x"&trt!="K+manure 1x"&trt!="K+manure 2x") %>% 
ggplot(aes(x=legtrt, y=g_ry1, color=timetrt)) + 
  geom_point(size=1.5, position = pps)+#, stat="identity", position = pps) + 
  facet_grid(~yr) +
  geom_hline(yintercept = 1, color="black")+
  geom_errorbar(aes(ymin=g_ry1-se_ry, ymax=g_ry1+se_ry), width=0.2, position = pps) +
  xlab("Treatment") +
  ylab("Grain yield relative to 1x fert")+
  ylim(0,3) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.y = element_text(size=10),
        strip.background =element_rect(color="black", fill="white"),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.text=element_text(size=10),
        legend.title=element_blank(),
        legend.key.size = unit(6,"mm"),
        legend.position=c(0.5,0.8),
        axis.text.y=element_text(size=10, color="black"),
        axis.title.y=element_text(size=10, color="black"),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=10,
                                 hjust=0.95, color='black'))
ggsave("GrainRY1x.png", width=7, height=3.5, units="in", path="/Users/junge037/Documents/Projects/orei_legume/figures/")

sdat2 %>% 
  group_by(yr, trt_code, trt, legtrt, timetrt) %>% 
  summarise(sd_ry = sd(g_ry2, na.rm=T), 
            n_ry = n(),
            g_ry2 = mean(g_ry2, na.rm=T)) %>% #order matters if you want to keep the g_ry column name, must do mean after sd
  mutate(se_ry=sd_ry/sqrt(n_ry)) %>%
  filter(trt!="K control"&trt!="K+manure 0.5x"&trt!="K+manure 1x"&trt!="K+manure 2x") %>% 
ggplot(aes(x=legtrt, y=g_ry2, color=timetrt)) + 
  geom_point(size=1.5, position = pps)+#, stat="identity", position = pps) + 
  facet_grid(~yr) +
  geom_hline(yintercept = 1, color="black")+
  geom_errorbar(aes(ymin=g_ry2-se_ry, ymax=g_ry2+se_ry), width=0.2, position = pps) +
  xlab("Treatment") +
  ylab("Grain yield relative to 2x fert")+
  ylim(0,3) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.y = element_text(size=10),
        strip.background =element_rect(color="black", fill="white"),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.text=element_text(size=10),
        legend.title=element_blank(),
        legend.key.size = unit(6,"mm"),
        legend.position=c(0.5,0.8),
        axis.text.y=element_text(size=10, color="black"),
        axis.title.y=element_text(size=10, color="black"),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=10,
                                 hjust=0.95, color='black'))
ggsave("GrainRY2x.png", width=7, height=3.5, units="in", path="/Users/junge037/Documents/Projects/orei_legume/figures/")

ggplot(data=subset(sdat2, trt!="K control"&trt!="K+manure 0.5x"&trt!="K+manure 1x"&trt!="K+manure 2x"),
       aes(x=legumeyld, y=g_ry))+
  geom_point(aes(shape=legtrt, color=timetrt))+
  facet_grid(~yr)+
  xlab("Legume biomass (kg/ha)") +
  ylab("Grain yield relative to 0 fert")+
  geom_hline(yintercept = 1, color="black")+
  geom_smooth(method='lm', formula=y ~ poly(x, 2), se=F)+
  ylim(0,3.5) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.y = element_text(size=10),
        strip.background =element_rect(color="black", fill="white"),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.text=element_text(size=10),
        legend.key.size = unit(6,"mm"),
        legend.position=c(0.5,0.8),
        legend.title=element_blank(),
        axis.text.y=element_text(size=10, color="black"),
        axis.title.y=element_text(size=10, color="black"),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=10, angle=45,
                                 hjust=0.95, color='black'))
ggsave("LegumebiomassAgainstRelativeGrain.png", width=7, height=3.5, units="in", path="/Users/junge037/Documents/Projects/orei_legume/figures/")

sdat2 %>% 
  group_by(yr, trt_code, trt, legtrt, timetrt) %>% 
  summarise(sd_ry = sd(weedyld, na.rm=T), 
            n_ry = n(),
            weedyld = mean(weedyld, na.rm=T)) %>% #order matters if you want to keep the g_ry column name, must do mean after sd
  mutate(se_ry=sd_ry/sqrt(n_ry)) %>%
  ggplot(aes(x=trt, y=weedyld)) + 
  geom_point(size=1.5, position = pps)+#, stat="identity", position = pps) + 
  facet_wrap(~yr, scales = "free_y", ncol=1) +
  geom_errorbar(aes(ymin=weedyld-se_ry, ymax=weedyld+se_ry), width=0.2, position = pps) +
  xlab("Treatment") +
  ylab("Weed biomass (kg/ha)")+
  #ylim(0,3) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.y = element_text(size=10),
        strip.background =element_rect(color="black", fill="white"),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.text=element_text(size=10),
        legend.title=element_blank(),
        legend.key.size = unit(6,"mm"),
        legend.position=c(0.5,0.8),
        axis.text.y=element_text(size=10, color="black"),
        axis.title.y=element_text(size=10, color="black"),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=10, angle = 45,
                                 hjust=0.95, color='black'))

sdat2 %>% 
  mutate(total_forage=rowSums(across(strawyld:legumeyld), na.rm=T)) %>% 
  group_by(yr, trt, legtrt) %>% 
  summarise(total_forage_yld = mean(total_forage, na.rm=T), 
            sd_yld = sd(total_forage, na.rm=T), 
            n_yld = n()) %>% 
  mutate(se_yld=sd_yld/sqrt(n_yld)) %>%
  mutate(trt=factor(trt, levels=c("K+A-fall", "K+A-spring","K+A-summer",
                                  "K+RC-fall", "K+RC-spring","K+RC-summer",
                                  "K control", "K+manure 0.5x",
                                  "K+manure 1x", "K+manure 2x"))) %>% 
  ggplot(aes(x=trt, y=total_forage_yld)) + 
  geom_point(size=1.5, position = pps, aes(color=legtrt), show.legend=F)+#, stat="identity", position = pps) + 
  facet_grid(~yr) +
  geom_errorbar(aes(ymin=total_forage_yld-se_yld, ymax=total_forage_yld+se_yld,
                    color=legtrt), width=0.2, position = pps, show.legend=F) +
  xlab("Treatment") +
  ylab("Total forage yield (kg/ha)")+
  #ylim(0,3) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.y = element_text(size=10),
        strip.background =element_rect(color="black", fill="white"),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.text=element_text(size=10),
        legend.title=element_blank(),
        legend.key.size = unit(6,"mm"),
        legend.position=c(0.5,0.8),
        axis.text.y=element_text(size=10, color="black"),
        axis.title.y=element_text(size=10, color="black"),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=10, angle = 45,
                                 hjust=0.95, color='black'))
ggsave("TotalForageYield.png", width=7, height=3.5, units="in", path="/Users/junge037/Documents/Projects/orei_legume/figures/")

sdat2 %>% 
  group_by(yr, trt, legtrt) %>% 
  summarise(mgrainyld = mean(grainyld, na.rm=T), 
            sd_yld = sd(grainyld, na.rm=T), 
            n_yld = n()) %>% 
  mutate(se_yld=sd_yld/sqrt(n_yld)) %>%
  mutate(trt=factor(trt, levels=c("K+A-fall", "K+A-spring","K+A-summer",
                                  "K+RC-fall", "K+RC-spring","K+RC-summer",
                                  "K control", "K+manure 0.5x",
                                  "K+manure 1x", "K+manure 2x"))) %>% 
  ggplot(aes(x=trt, y=mgrainyld)) + 
  geom_point(size=1.5, position = pps, aes(color=legtrt), show.legend=F)+#, stat="identity", position = pps) + 
  facet_grid(~yr) +
  geom_errorbar(aes(ymin=mgrainyld-se_yld, ymax=mgrainyld+se_yld,
                    color=legtrt), width=0.2, position = pps, show.legend=F) +
  xlab("Treatment") +
  ylab("Grain yield (kg/ha)")+
  #ylim(0,3) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.y = element_text(size=10),
        strip.background =element_rect(color="black", fill="white"),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.text=element_text(size=10),
        legend.title=element_blank(),
        legend.key.size = unit(6,"mm"),
        legend.position=c(0.5,0.8),
        axis.text.y=element_text(size=10, color="black"),
        axis.title.y=element_text(size=10, color="black"),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=10, angle = 45,
                                 hjust=0.95, color='black'))
ggsave("GrainYield.png", width=7, height=3.5, units="in", path="/Users/junge037/Documents/Projects/orei_legume/figures/")

