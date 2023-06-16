################################################################################################
# 1. Packages and path to project directory ######
################################################################################################
    set.seed(0.1)
  
    library(tidyverse)
    library(lme4)
    library(lmerTest)
    library(nlme)
    library(multcomp)
    library(emmeans)
    library(corrr)
    
    #take scientific notation off
    #options(scipen=999)
     library(sjPlot)
     library(ggpubr)
     library(patchwork)
    
    path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/EatingDisorder/Results"

################################################################################################
# 2. load data and make any additional data cleaning exclusions 
################################################################################################
  #Definite exclusions and mutations 
        #assign to an object; add in your own file structure from your PC
        ED<-readRDS("/Users/tonigibbs-dean/Documents/SpaceTaskProject/EatingDisorder/Intermediate_outputs/EatingDisorderTibble_Split1.RDS") %>%
                filter(level!="1") %>%
                mutate(BW=110-participantConfidence)  %>% 
                mutate(bw_pe=(BW/PE ) ) %>%
                mutate_if(is_character, as_factor) %>%
                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                mutate(correctedL6trialScore = case_when(level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                            level == 6 & trialScore>0  ~ trialScore/2,
                                            TRUE ~ as.numeric(trialScore))) %>%
                mutate(level=as.factor(level)) %>%
                mutate(newlevel=case_when(level == 2 ~ "2",
                                          between(TrialNumber, 34, 53) & (trialName == "spacetask02") ~ "3highnoise",
                                          between(TrialNumber, 14, 33) & (trialName == "spacetask02") ~ "3lownoise",
                                          between(TrialNumber, 34, 53) & (trialName == "spacetask02reversed") ~ "3lownoise",
                                          between(TrialNumber, 14, 33) & (trialName == "spacetask02reversed") ~ "3highnoise",
                                          between(TrialNumber, 34, 53) & (trialName == "spacetask005") ~ "3highnoise",
                                          between(TrialNumber, 14, 33) & (trialName == "spacetask005") ~ "3lownoise",
                                          between(TrialNumber, 34, 53) & (trialName == "spacetask005reversed") ~ "3lownoise",
                                          between(TrialNumber, 14, 33) & (trialName == "spacetask005reversed") ~ "3highnoise",
                                          level == 4 ~ "4",
                                          level == 5 ~ "5",
                                          level == 6 ~ "6"))

################################################################################################
#Recoding of demographics where needed
################################################################################################

        #recoding code - switch in the demos of interest 
        ED_recode<- ED %>%
                        dplyr::mutate(current_med=dplyr::recode(current_med, 
                                "<NA>" = "No")) %>%
                        mutate(edu=recode(edu,
                                "NA" = "Lower secondary education (i.e., GSCE/O levels)")) 

################################################################################################       
#create modelling df general
################################################################################################
    GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }


            EDtib <- ED_recode %>%
                            #filter(level %in% c(2, 3, 4, 5, 6))  %>%
                            filter(!level=="1") %>%
                            group_by(Participant.Private.ID, newlevel, group_recruit) %>%
                            mutate(beta=GetBeta(PE= signedPE, PC=signedPositionChange)) %>%
                            summarise(
                                    #lifetime_anx=first(lifetime_anx),
                                    betaLR=dplyr::first(beta),
                                    meanConf=mean(participantConfidence),
                                    meanscore=mean(trialScore),
                                    meancorrectedL6trialScore=mean(correctedL6trialScore),
                                    #sumsuccess=sum(success),
                                    sumsuccess_n=sum(success)/length(level),
                                    meanPE=mean(PE),
                                    age=dplyr::first(age),
                                    sex=dplyr::first(sex),
                                    Participant.Device.Type=dplyr::first(Participant.Device.Type),
                                    edu=dplyr::first(edu),
                                    #ED_Type=dplyr::first(ED_Type),
                                    current_med=dplyr::first(current_med),
                                    #group_recruit=dplyr::first(group_recruit),
                                    trialName=dplyr::first(trialName)
                                    )
                            #mutate_if(is.double, as.integer)

################################################################################################
#create modelling DF for LR differences
################################################################################################

    x<-     EDtib %>%
                #dplyr::select(Participant.Private.ID, gamers, gender, education, ethnicity, X3class, condition, 
                #                betaLR, masqCombined_anxiety, masqCombined_depression, spq_total, dsm_total, pdi_total,green_total) %>%
                dplyr::select(Participant.Private.ID, newlevel, group_recruit,
                                betaLR, sex, edu, age, current_med, trialName) %>%# ED_Type
                
               # mutate(ED_Type=as.factor(ED_Type))
                pivot_wider(names_from=newlevel, values_from=betaLR, names_glue = "{.value}_{newlevel}") %>%
                mutate(LRdiff_L2L3highnoise = betaLR_3highnoise - betaLR_2) %>%
                mutate(LRdiff_L3highnoiseL4 = betaLR_4 - betaLR_3highnoise) %>%
                mutate(LRdiff_L4L5 = betaLR_5 - betaLR_4) %>%
                mutate(LRdiff_L5L6 = betaLR_6 - betaLR_5) 

################################################################################################
#Bronwing line plots between levels 
################################################################################################
    browning_L3vsL4<-  x %>% 
                                mutate(slope = betaLR_4 > betaLR_3) %>%
                                pivot_longer(cols=betaLR_3:betaLR_6, names_to="level", values_to="betaLR") %>%
                                filter(level %in% c("betaLR_3", "betaLR_4")) %>%
                                ggplot(aes(x=level, y=betaLR, color=group_recruit)) +
                                geom_point(alpha=0.4) +
                                geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
                                facet_wrap(~slope)
    browning_L5vsL4<-  x %>% 
                                mutate(slope = betaLR_5 > betaLR_4) %>%
                                pivot_longer(cols=betaLR_3:betaLR_6, names_to="level", values_to="betaLR") %>%
                                filter(level %in% c("betaLR_4", "betaLR_5")) %>%
                                ggplot(aes(x=level, y=betaLR, color=group_recruit)) +
                                geom_point(alpha=0.4) +
                                geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
                                facet_wrap(~slope)

    browning_L5vsL6<-  x %>% 
                                mutate(slope = betaLR_6 > betaLR_5) %>%
                                pivot_longer(cols=betaLR_3:betaLR_6, names_to="level", values_to="betaLR") %>%
                                filter(level %in% c("betaLR_5", "betaLR_6")) %>%
                                ggplot(aes(x=level, y=betaLR, color=group_recruit)) +
                                geom_point(alpha=0.4) +
                                geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
                                facet_wrap(~slope)

            browningLRdiff_lineplots<-egg::ggarrange(browning_L3vsL4,browning_L5vsL4,browning_L5vsL6)
    
            ggsave(browningLRdiff_lineplots, file=file.path(path,"browningLRdiff_lineplots.pdf"))


########################################################################################################################
#modelling - is LR difference predicted? 
########################################################################################################################
        model<-   x %>%
                        pivot_longer(cols=betaLR_2:betaLR_6, names_to="newlevel", values_to="betaLR") %>%
                        mutate(directionforward = trialName %in% c("spacetask02", "spacetask005")) 
        model<- model %>% drop_na

        #l3/l2
                m0<-lme(LRdiff_L2L3highnoise ~ 1, random = ~1|Participant.Private.ID, data= model, method = "ML", 
                                na.action = na.exclude)
                m1_l3l2<-lme(LRdiff_L2L3highnoise ~ group_recruit + current_med + sex + age + directionforward, random = ~1|Participant.Private.ID, 
                                        data= model, method = "ML", na.action = na.exclude) #winner
                m2<-lme(LRdiff_L2L3highnoise ~ group_recruit*directionforward + current_med + sex + age, random = ~1|Participant.Private.ID, 
                                        data= model, method = "ML", na.action = na.exclude) 

                AIC(m0,m1, m2)
                anova(m0,m1, m2)

        #l4/l3
                m0<-lme(LRdiff_L3highnoiseL4 ~ 1, random = ~1|Participant.Private.ID, data= model, method = "ML", 
                                na.action = na.exclude)
                m1<-lme(LRdiff_L3highnoiseL4 ~ group_recruit + current_med + sex + age + directionforward, random = ~1|Participant.Private.ID, 
                                        data= model, method = "ML", na.action = na.exclude) #winner
                m2<-lme(LRdiff_L3highnoiseL4 ~ group_recruit*directionforward + current_med + sex + age, random = ~1|Participant.Private.ID, 
                                        data= model, method = "ML", na.action = na.exclude)

                AIC(m0,m1, m2)
                anova(m0,m1, m2)

                #p values for plot                                          
                        options(scipen=999)
                        m2_l4l3<-summary(m2)
                        m2_l4l3_table<-m2_l4l3$tTable[,5]
                        m2_l4l3_pval_grouprecruit<-m2_l4l3_table[2]               


        #l5/l4
                m0<-lme(LRdiff_L4L5 ~ 1, random = ~1|Participant.Private.ID, data= model, method = "ML", 
                                na.action = na.exclude)
                m1<-lme(LRdiff_L4L5 ~ group_recruit + current_med + sex + age + directionforward, random = ~1|Participant.Private.ID, 
                                        data= model, method = "ML", na.action = na.exclude) #winner
                m2<-lme(LRdiff_L4L5 ~ group_recruit*directionforward + current_med + sex + age, random = ~1|Participant.Private.ID, 
                                        data= model, method = "ML", na.action = na.exclude)

                AIC(m0,m1, m2)
                anova(m0,m1, m2)

        #l6/l5
                m0<-lme(LRdiff_L5L6 ~ 1, random = ~1|Participant.Private.ID, data= model, method = "ML", 
                                na.action = na.exclude)
                m1<-lme(LRdiff_L5L6 ~ group_recruit + current_med + sex + age + directionforward, random = ~1|Participant.Private.ID, 
                                        data= model, method = "ML", na.action = na.exclude) #winner
                m2<-lme(LRdiff_L5L6 ~ group_recruit*directionforward + current_med + sex + age, random = ~1|Participant.Private.ID, 
                                        data= model, method = "ML", na.action = na.exclude)

                AIC(m0,m1, m2)
                anova(m0,m1, m2)

########################################################################################################################
#plots - showing learning rate differences between levels - boxplots between ED and HC
########################################################################################################################

    lrdiff_L3highnoisevsL2_boxplot <-  x %>%
                                   # filter(trialName %in% c("spacetask005", "spacetask02")) %>%
                                    ggplot(aes(x=group_recruit, y=LRdiff_L2L3highnoise)) +
                                    #geom_point(aes(color=slope)) +
                                    geom_boxplot(aes(fill=group_recruit)) +
                                       theme_classic() +    
                                                theme(  axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank(),
                                                        axis.title.x = element_blank(),
                                                        legend.position = "none") +
                                                labs(y="Learning rate difference\n(block 3 - block 2)")+
                                                   #  x="Group") +
                                                     #colour= "Group") +
                                    geom_signif(comparisons = list(c("ED", "HC")),
                                                    map_signif_level=TRUE)  #+
                                        #facet_wrap(~trialName)
                 lrdiff_L3highnoisevsL2_boxplot<-lrdiff_L3highnoisevsL2_boxplot + scale_fill_manual(values=c("#7AC5CD", "#C1CDCD"))

    lrdiff_L4vsL3_boxplot <-  x %>%
                                   # filter(trialName %in% c("spacetask005", "spacetask02")) %>%
                                    ggplot(aes(x=group_recruit, y=LRdiff_L3highnoiseL4)) +
                                    #geom_point(aes(color=slope)) +
                                    geom_boxplot(aes(fill=group_recruit), position = "dodge") +
                                       theme_classic() +    
                                                theme(  axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank(),
                                                        axis.title.x = element_blank(),
                                                        legend.position = "none") +
                                                labs(y="Learning rate difference\n(block 4 - block 3)")+
                                                   #  x="Group") +
                                                     #colour= "Group") +
                                    geom_signif(map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.04),
                                            annotation = formatC(m2_l4l3_pval_grouprecruit, digits = 1),
                                                                     y_position = 0.85, xmin = 1, xmax = 2
                                    )
                                                 
                                                                     #tip_length = c(0.2, 0.04))
                                                                 #facet_wrap(~slope)
                 lrdiff_L4vsL3_boxplot<-lrdiff_L4vsL3_boxplot + scale_fill_manual(values=c("#7AC5CD", "#C1CDCD"))


    lrdiff_L5vsL4_boxplot <-  x %>%
                                   # filter(trialName %in% c("spacetask005", "spacetask02")) %>%
                                    ggplot(aes(x=group_recruit, y=LRdiff_L4L5)) +
                                    #geom_point(aes(color=slope)) +
                                    geom_boxplot(aes(fill=group_recruit)) +
                                        theme_classic() +    
                                                theme(  axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank(),
                                                        axis.title.x = element_blank(),
                                                        legend.position = "none") +
                                                labs(y="Learning rate difference\n(block 5 - block 4)")+
                                                     #x="Group") +
                                                     #colour= "Group") +
                                        geom_signif(comparisons = list(c("ED", "HC")),
                                                    map_signif_level=TRUE) 
                                    
                                            
                                            
                                            comparisons = list(c("ED", "HC")),
                                                    map_signif_level=TRUE) 
                                                                 #facet_wrap(~slo
                         lrdiff_L5vsL4_boxplot<-lrdiff_L5vsL4_boxplot + scale_fill_manual(values=c("#7AC5CD", "#C1CDCD"))


    lrdiff_L6vsL5_boxplot <-  x %>%
                                   # filter(trialName %in% c("spacetask005", "spacetask02")) %>%
                                    ggplot(aes(x=as.factor(group_recruit), y=LRdiff_L5L6)) +
                                    #geom_point(aes(color=slope)) +
                                    geom_boxplot(aes(fill=group_recruit)) +
                                   theme_classic() +    
                                                theme(  axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank(),
                                                        axis.title.x = element_blank(),
                                                        legend.position = "none") +
                                                labs(y="Learning rate difference\n(block 6 - block 5)")+
                                                     #x="Group") +
                                                     #colour= "Group") +
                                    geom_signif(comparisons = list(c("ED", "HC")),
                                                    map_signif_level=TRUE) 

                        lrdiff_L6vsL5_boxplot<-lrdiff_L6vsL5_boxplot + scale_fill_manual(values=c("#7AC5CD", "#C1CDCD"))
                                                                 #facet_wrap(~slo
            

    LRdiff_boxplots_xlevel<- (lrdiff_L3highnoisevsL2_boxplot / lrdiff_L4vsL3_boxplot) | (lrdiff_L5vsL4_boxplot / lrdiff_L6vsL5_boxplot)
    LRdiff_boxplots_xlevel_plot<- LRdiff_boxplots_xlevel + plot_annotation(tag_levels=c("A","B", "C", "D"))


    ggsave(LRdiff_boxplots_xlevel_plot, file=file.path(path,"LRdiff_boxplots_xlevel_finalplot_incl23.pdf"))