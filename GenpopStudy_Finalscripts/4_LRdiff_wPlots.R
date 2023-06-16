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

         path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop2/Results"

    greenColour<-"#6fb24b"
    MASQdepColour<-"#676ed4"
    MASQanxColour<-"#b74d86"
    PDIColour<-"#b6b638"
    SPQColour<-"#afab4f"
    
    Comp1<-"#8951a5"
    Comp2<-"#58bf7a"
    Comp3<-"#be4a5b"
    Comp4<-"#afab4f"
    Comp5<-"#a47e3c"
    Comp6<-"#43c8ac"
    Comp7<-""
    Comp8<-""
    Comp9<-""
    
    L2Col<-"#c75f34"
    L3Col<-"#648cd5"
    L4Col<-"#cc8c33"
    L5Col<-"#588234"
    L6Col<-"#ae4837"

################################################################################################
# 2. load data and make any additional data cleaning exclusions 
################################################################################################
  #Definite exclusions and mutations 
        #assign to an object; add in your own file structure from your PC
        genpop2tibble<-readRDS("/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop2/Intermediate_outputs/GenPop2class.RDS") %>%
                filter(level!="1") %>%
                filter(device_type!="tablet") %>% #only one tablet - so remove
                #filter(devdx.text %in% c("ADHD", "Chronic migraine")) %>% #exclude all expect ADHD and chronic migraine (n10 altogether)
                filter(devdx!="Yes (if you feel comfortable to do so please specify)") %>%
                mutate(BW=110-participantConfidence)  %>% 
                mutate(bw_pe=(BW/PE)) %>%
                mutate_if(is_character, as_factor) %>%
                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                mutate(level=as.factor(level)) %>%
                mutate(gamers=gamingtime.quant>2) %>%
                mutate(PEscaled=abs(scale(Result, center = c(-0.5), scale = c(0.01)) - participantPosition)) %>%
                mutate(PEsignedScaled=scale(Result, center = c(-0.5), scale = c(0.01)) - participantPosition) %>%
                mutate(meanchange=Mean != dplyr::lag(Mean)) %>%
                mutate(SDhigh = SD %in% 0.12) %>%
                mutate(condition= case_when((SD == 0.03 | SD == 0.06) & (as.numeric(level == 5) | as.numeric(level == 6)) ~ "combined_highV_lowN",
                                                (SD == 0.03 | SD == 0.06) & (as.numeric(level == 4)) ~ "isolated_highV_lowN",
                                                (SD == 0.12) & (as.numeric(level == 4) | as.numeric(level == 5) | as.numeric(level == 6)) ~ "combined_highV_highN", 
                                                (SD == 0.03 | SD == 0.06) & (as.numeric(level == 2) | as.numeric(level == 3))  ~ "stable_lowV_lowN",
                                                (SD == 0.12) & (as.numeric(level == 2) | as.numeric(level == 3)) ~ "isolated_lowV_highN"
                                                )) %>%
                mutate(correctedL6trialScore = case_when(level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                            level == 6 & trialScore>0  ~ trialScore/2,
                                            TRUE ~ as.numeric(trialScore))) 



        
################################################################################################
#Recoding of demographics where needed
################################################################################################
        #recoding demograhpic variables so the groups arent too small etc. :
        #Groups combined in some istances listed below:
                #gender: other specified they were female in the text option
                #Education A level with AS and other (as text says higher national diploma in both); PhD and postgrad
                #Ethnicity – issue with prefer not to say as 1 – mixed category shouild now be mixed or non specified 
                #Employment – other and prefer not to say combined 
        
        #recoding code - switch in the demos of interest 
    genpop2<- genpop2tibble %>%
                        mutate(gender=recode(gender, 
                                "Other (please specify)" ="Female")) %>%  
                        mutate(education=recode(education, 
                                "Other (please specify)" = "A Levels or equivalent diplomas (e.g. BTEC)",
                                "AS Levels"              = "A Levels or equivalent diplomas (e.g. BTEC)",
                                #"A Levels or equivalent diplomas (e.g. BTEC)" = "A Levels or equivalent diplomas (e.g. BTEC)",
                                "PhD or equivalent professional qualification" = "Postgraduate degree (MSc/MA/MRes)"))  %>% 
                        mutate(ethnicity=recode(ethnicity,
                                "Prefer not to say" = "Mixed or multiple ethnic groups, or if your specific ethnicity is not listed, please specify (box will appear when ticked): ")) %>% 
                        mutate(employment=recode(employment,
                                "Other (please specify)" = "Prefer not to say")) # pull(employment) %>% table           

###############################################################################################       
#create modelling df
################################################################################################
    GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }
                            
       
    df_mod<-genpop2 %>%
            group_by(Participant.Private.ID, level, X3class) %>%
            mutate(gamers=gamingtime.quant>2) %>% 
            mutate(c4class=as.factor(c4class)) %>%
            mutate(X3class=as.factor(X3class)) %>%
            mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
            mutate(beta=GetBeta(PE= signedPE, PC=signedPositionChange)) %>%
            summarise(score=mean(trialScore), 
                    meancorrectedL6trialScore=mean(correctedL6trialScore),
                    betaLR=dplyr::first(beta),
                    meanconf=mean(participantConfidence),
                    sumsuccess=sum(success/length(level)),
                    meanPE=mean(PE),
                    meanPEscaled=mean(PEscaled),
                    meanPEsignedScaled=mean(PEsignedScaled),
                    meanPerfE=mean(PerfE),
                    #meanchangeTF=dplyr::first(meanchange),
                    #SDhighcondit=dplyr::first(SDhigh),
                    masq_total_score=dplyr::first(masq_total_score), 
                    gdd_score=dplyr::first(gdd_score),
                    aa_score=dplyr::first(aa_score),
                    gda_score=dplyr::first(gda_score),
                    ad_score=dplyr::first(ad_score),
                    masqCombined_anxiety=sum(aa_score, gda_score),
                    masqCombined_depression=sum(ad_score, gdd_score),
                    gamingtime=dplyr::first(gamingtime),
                    pdi_total=dplyr::first(pdi_total),
                    dsm_total=dplyr::first(dsm_total),
                    green_total=dplyr::first(green_total),
                    gamers=dplyr::first(gamers),
                    education=dplyr::first(education),
                    ethnicity=dplyr::first(ethnicity),
                    employment=dplyr::first(employment),
                    psychep=dplyr::first(psychep), 
                    psychdx=dplyr::first(psychdx),
                    neurologicalImp=dplyr::first(devdx),
                    spq_total=dplyr::first(spq_total),
                    trialName=dplyr::first(trialName),
                    gender=dplyr::first(gender),
                    age=dplyr::first(age),
                    device_type=dplyr::first(device_type),
                    wideStartSD=dplyr::first(startSD)) #%>%
                    #filter(level %in% c(3, 4, 5, 6)) 

########################################################################################
#make LR difference metrics 
########################################################################################
    x<-     df_mod %>%
                #dplyr::select(Participant.Private.ID, gamers, gender, education, ethnicity, X3class, condition, 
                #                betaLR, masqCombined_anxiety, masqCombined_depression, spq_total, dsm_total, pdi_total,green_total) %>%
                dplyr::select(Participant.Private.ID, level,
                                betaLR, masqCombined_anxiety,wideStartSD, X3class,
                                spq_total, pdi_total, masqCombined_depression,
                                green_total, dsm_total, gender, education, ethnicity, age) %>%
                pivot_wider(names_from=level, values_from=betaLR, names_glue = "{.value}_{level}") %>%
                mutate(LRdiff_L3L4 = betaLR_4 - betaLR_3) %>%
                mutate(LRdiff_L4L5 = betaLR_5 - betaLR_4) %>%
                mutate(LRdiff_L5L6 = betaLR_6 - betaLR_5) 
   

########################################################################################
# Plots - Browning LR diff line plots
########################################################################################
    browning_L3vsL4<-  x %>% 
                                mutate(slopeIncrease = betaLR_4 > betaLR_3) %>%
                                pivot_longer(cols=betaLR_3:betaLR_6, names_to="level", values_to="betaLR") %>%
                                filter(level %in% c("betaLR_4", "betaLR_3")) 
    browning_L3vsL4_plot <-        browning_L3vsL4 %>%     
                                                mutate(test=case_when(level=="betaLR_4" ~ "4",
                                                                      level=="betaLR_3" ~ "3")) %>%
                                                ggplot(aes(x=test, y=betaLR, color=slopeIncrease)) +
                                                geom_point(alpha=0.4) +
                                                geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
                                                #facet_wrap(~slope)
                                                theme_classic() +    
                                                theme(  axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank())+
                                                labs(y="Beta LR",
                                                     x="Block",
                                                     colour= "Increase in\ LR")
 
    browning_L4vsL5<-  x %>% 
                                mutate(slopeIncrease = betaLR_5 > betaLR_4) %>%
                                pivot_longer(cols=betaLR_3:betaLR_6, names_to="level", values_to="betaLR") %>%
                                filter(level %in% c("betaLR_4", "betaLR_5")) 
    browning_L4vsL5_plot <-        browning_L4vsL5 %>%     
                                                mutate(test=case_when(level=="betaLR_4" ~ "4",
                                                                      level=="betaLR_5" ~ "5")) %>%
                                                ggplot(aes(x=test, y=betaLR, color=slopeIncrease)) +
                                                geom_point(alpha=0.4) +
                                                geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
                                                #facet_wrap(~slope)
                                                theme_classic() +    
                                                theme(  axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank())+
                                                labs(y="Beta LR",
                                                     x="Block",
                                                     colour= "Increase in\ LR")

    browning_L5vsL6<-  x %>% 
                                mutate(slopeIncrease = betaLR_6 > betaLR_5) %>%
                                pivot_longer(cols=betaLR_3:betaLR_6, names_to="level", values_to="betaLR") %>%
                                filter(level %in% c("betaLR_5", "betaLR_6")) 
    browning_L5vsL6_plot <-        browning_L5vsL6 %>%     
                                                mutate(test=case_when(level=="betaLR_5" ~ "5",
                                                                      level=="betaLR_6" ~ "6")) %>%
                                                ggplot(aes(x=test, y=betaLR, color=slopeIncrease)) +
                                                geom_point(alpha=0.4) +
                                                geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
                                                #facet_wrap(~slope)
                                                theme_classic() +    
                                                theme(  axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank())+
                                                labs(y="Beta LR",
                                                     x="Block",
                                                     colour= "Increase in\ LR")

    browinglineplots_xlevel<-egg::ggarrange(browning_L3vsL4_plot, browning_L4vsL5_plot, browning_L5vsL6_plot)
    ggsave(browinglineplots_xlevel, file=file.path(path, "browinglineplots_xlevel.pdf"))


########################################################################################
# Plots LR diff between levels + correlations with symptom scales + boxplots for clusters
########################################################################################
    ####Level 3 vs level 4
        lrdiff_L4vsL3_anx_cont  <-  x %>%
                                        mutate(slope = betaLR_4 > betaLR_3) %>%
                                        ggplot(aes(x=masqCombined_anxiety, y=LRdiff_L3L4)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_point(alpha=0.2, colour=MASQanxColour) +
                                        geom_smooth(method="lm", colour=MASQanxColour) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.9) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank())+
                                              #axis.title.x = element_blank())   +  
                                        labs(#fill="No. of times\na mean has\nbeen presented",
                                            #color=legend_colors,
                                            y = "LR Difference\nBlock 4 - Block 3",
                                            x = "Anxiety Score")
                         lrdiff_L4vsL3_anx_cont<-lrdiff_L4vsL3_anx_cont + scale_colour_manual(values=c(MASQanxColour))



        lrdiff_L4vsL3_dep_cont  <-  x %>%
                                        mutate(slope = betaLR_4 > betaLR_3) %>%
                                        ggplot(aes(x=masqCombined_depression, y=LRdiff_L3L4)) +
                                        geom_point(alpha=0.2, colour=MASQdepColour) +
                                        geom_smooth(method="lm", colour=MASQdepColour) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.9) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank()) +
                                              #axis.title.x = element_blank())   +  
                                        labs(#fill="No. of times\na mean has\nbeen presented",
                                            #color=legend_colors,
                                            y = "LR Difference\nBlock 4 - Block 3",
                                            x = "Depression Score")
                         lrdiff_L4vsL3_dep_cont<-lrdiff_L4vsL3_dep_cont + scale_colour_manual(values=c(MASQdepColour))

            lrdiff_L4vsL3_spq_cont  <-  x %>%
                                        mutate(slope = betaLR_4 > betaLR_3) %>%
                                        ggplot(aes(x=spq_total, y=LRdiff_L3L4)) +
                                        geom_point(alpha=0.2, colour=SPQColour) +
                                        geom_smooth(method="lm", colour=SPQColour) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.9) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank())+
                                              #axis.title.x = element_blank())   +  
                                        labs(#fill="No. of times\na mean has\nbeen presented",
                                            #color=legend_colors,
                                            y = "LR Difference\nBlock 4 - Block 3",
                                            x = "Schizotypy Score")
                         lrdiff_L4vsL3_spq_cont<-lrdiff_L4vsL3_spq_cont + scale_colour_manual(values=c(SPQColour))


            lrdiff_L4vsL3_green_cont  <-  x %>% #**
                                        mutate(slope = betaLR_4 > betaLR_3) %>%
                                        ggplot(aes(x=green_total, y=LRdiff_L3L4)) +
                                        geom_point(alpha=0.2, colour=greenColour) +
                                        geom_smooth(method="lm", colour=greenColour) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.9) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank()) +
                                              #axis.title.x = element_blank())   +  
                                        labs(#fill="No. of times\na mean has\nbeen presented",
                                            #color=legend_colors,
                                            y = "LR Difference\nBlock 4 - Block 3",
                                            x = "Paranoia Score")
                         lrdiff_L4vsL3_green_cont<-lrdiff_L4vsL3_green_cont + scale_colour_manual(values=c(greenColour))

            lrdiff_L4vsL3_pdi_cont  <-  x %>%
                                        mutate(slope = betaLR_4 > betaLR_3) %>%
                                        ggplot(aes(x=pdi_total, y=LRdiff_L3L4)) +
                                        geom_point(alpha=0.2, colour=PDIColour) +
                                        geom_smooth(method="lm", colour=PDIColour) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.9) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank()) +
                                              #axis.title.x = element_blank())   +  
                                        labs(#fill="No. of times\na mean has\nbeen presented",
                                            #color=legend_colors,
                                            y = "LR Difference\nBlock 4 - Block 3",
                                            x = "Delusionary Ideation Score")
                         lrdiff_L4vsL3_pdi_cont<-lrdiff_L4vsL3_pdi_cont + scale_colour_manual(values=c(PDIColour))

                                        
            lrdiff_L4vsL3_dsm_cont  <-  x %>%
                                        mutate(slope = betaLR_4 > betaLR_3) %>%
                                        ggplot(aes(x=dsm_total, y=LRdiff_L3L4)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                        #geom_smooth(aes(color=slope), method="lm") +
                                        stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01)

        sympscale_LRdiffLEVELS_plots_pearson<-egg::ggarrange(lrdiff_L4vsL3_anx_cont,
                                                    lrdiff_L4vsL3_dep_cont,
                                                    lrdiff_L4vsL3_spq_cont,
                                                    lrdiff_L4vsL3_green_cont,
                                                    lrdiff_L4vsL3_pdi_cont)
                                                    #lrdiff_L4vsL3_dsm_cont)


            ggsave(sympscale_LRdiffLEVELS_plots_pearson, file=file.path(path,"sympscale_LRdiffLEVELS_plots_pearson.pdf"))

        #clusters

           lrdiff_L4vsL3_clusters3  <-  x %>%
                                        mutate(slope = betaLR_4 > betaLR_3) %>%
                                        ggplot(aes(x=as.factor(X3class), y=LRdiff_L3L4)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_boxplot(aes(fill=X3class)) +
                                        geom_signif(comparisons = list(c("1", "2"), c("1","3"), c("2", "3")),
                                                    map_signif_level=TRUE)



    #####l4 vs l5

        LRdiff_L4L5_anx_cont  <-  x %>%
                                        mutate(slope = betaLR_5 > betaLR_4) %>%
                                        ggplot(aes(x=masqCombined_anxiety, y=LRdiff_L4L5)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_point(alpha=0.2, colour=MASQanxColour) +
                                        geom_smooth(method="lm", colour=MASQanxColour) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.9) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.x = element_blank())   +  
                                        labs(#fill="No. of times\na mean has\nbeen presented",
                                            #color=legend_colors,
                                            y = "LR Difference\nBlock 6 - Block 5",
                                            x = "Anxiety Score")
                         LRdiff_L5L6__anx_cont<-LRdiff_L5L6__anx_cont + scale_colour_manual(values=c(MASQanxColour))


        LRdiff_L4L5_dep_cont  <-  x %>%
                                        mutate(slope = betaLR_5 > betaLR_4) %>%
                                        ggplot(aes(x=masqCombined_depression, y=LRdiff_L4L5)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01)

            LRdiff_L4L5_spq_cont  <-  x %>%
                                        mutate(slope = betaLR_5 > betaLR_4) %>%
                                        ggplot(aes(x=spq_total, y=LRdiff_L4L5)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                    # geom_smooth(aes(color=slope), method="lm")+
                                        stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01)


            LRdiff_L4L5_green_cont  <-  x %>% #**
                                        mutate(slope = betaLR_5 > betaLR_4) %>%
                                        ggplot(aes(x=green_total, y=LRdiff_L4L5)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01)

            LRdiff_L4L5_pdi_cont  <-  x %>%
                                        mutate(slope = betaLR_5 > betaLR_4) %>%
                                        ggplot(aes(x=pdi_total, y=LRdiff_L4L5)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        
            LRdiff_L4L5_dsm_cont  <-  x %>%
                                        mutate(slope = betaLR_5 > betaLR_4) %>%
                                        ggplot(aes(x=dsm_total, y=LRdiff_L4L5)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                        #geom_smooth(aes(color=slope), method="lm") +
                                        stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01)

        sympscale_LRdiffL4_L5_plots_pearson<-egg::ggarrange(LRdiff_L4L5_anx_cont,
                                                            LRdiff_L4L5_dep_cont,
                                                            LRdiff_L4L5_spq_cont,
                                                            LRdiff_L4L5_green_cont,
                                                            LRdiff_L4L5_pdi_cont,
                                                            LRdiff_L4L5_dsm_cont)


            ggsave(sympscale_LRdiffL4_L5_plots_pearson, file=file.path(path,"sympscale_LRdiffL4_L5_plots_pearson.pdf"))

           lrdiff_L4vsL5_clusters  <-  x %>%
                                        #mutate(slope = betaLR_4 > betaLR_3) %>%
                                        ggplot(aes(x=X3class, y=LRdiff_L4L5)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_boxplot(aes(fill=X3class)) +
                                        geom_signif(comparisons = list(c("1", "2"), c("1", "3"), c("2", "3")), 
                                                    map_signif_level=TRUE)



    #####l5 vs l6

        LRdiff_L5L6__anx_cont  <-  x %>%
                                        mutate(slope = betaLR_6 > betaLR_5) %>%
                                        ggplot(aes(x=masqCombined_anxiety, y=LRdiff_L5L6)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_point(alpha=0.2, colour=MASQanxColour) +
                                        geom_smooth(method="lm", colour=MASQanxColour) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.9) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank()) +
                                        labs(x = "Anxiety Score")
                         LRdiff_L5L6__anx_cont<-LRdiff_L5L6__anx_cont + scale_colour_manual(values=c(MASQanxColour))

        LRdiff_L5L6__dep_cont  <-  x %>%
                                        mutate(slope = betaLR_6 > betaLR_5) %>%
                                        ggplot(aes(x=masqCombined_depression, y=LRdiff_L5L6)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01)

            LRdiff_L5L6__spq_cont  <-  x %>%
                                     mutate(slope = betaLR_6 > betaLR_5) %>%
                                        ggplot(aes(x=spq_total, y=LRdiff_L5L6)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                    # geom_smooth(aes(color=slope), method="lm")+
                                        stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01)


            LRdiff_L5L6__green_cont  <-  x %>% #**
                                        mutate(slope = betaLR_6 > betaLR_5) %>%
                                        ggplot(aes(x=green_total, y=LRdiff_L5L6)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01)

            LRdiff_L5L6__pdi_cont  <-  x %>%
                                        mutate(slope = betaLR_6 > betaLR_5) %>%
                                        ggplot(aes(x=pdi_total, y=LRdiff_L5L6)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        
            LRdiff_L5L6_dsm_cont  <-  x %>%
                                        mutate(slope = betaLR_6 > betaLR_5) %>%
                                        ggplot(aes(x=dsm_total, y=LRdiff_L5L6)) +
                                        geom_point() +
                                        geom_smooth(method="lm") +
                                        #geom_smooth(aes(color=slope), method="lm") +
                                        stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01)

        sympscale_LRdiffL5_L6_plots_pearson<-egg::ggarrange(LRdiff_L5L6__anx_cont,
                                                            LRdiff_L5L6__dep_cont,
                                                            LRdiff_L5L6__spq_cont,
                                                            LRdiff_L5L6__green_cont,
                                                            LRdiff_L5L6__pdi_cont,
                                                            LRdiff_L5L6_dsm_cont)


            ggsave(sympscale_LRdiffL5_L6_plots_pearson, file=file.path(path,"sympscale_LRdiffL5_L6_plots_pearson.pdf"))

           lrdiff_L6vsL5_clusters  <-  x %>%
                                        #mutate(slope = betaLR_4 > betaLR_3) %>%
                                        ggplot(aes(x=X3class, y=LRdiff_L5L6)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_boxplot(aes(fill=X3class)) +
                                        geom_signif(comparisons = list(c("1", "2"), c("1", "3"), c("2", "3")), 
                                                    map_signif_level=TRUE)


########################################################################################
# Final Plots 
########################################################################################
    #####plot arrangements

        anxiety_LRdiffplots<- lrdiff_L4vsL3_anx_cont | LRdiff_L4L5_anx_cont | LRdiff_L5L6__anx_cont
                                ggsave(anxiety_LRdiffplots, file=file.path(path,"anxiety_LRdiffplots.pdf"))
        dep_LRdiffplots<- lrdiff_L4vsL3_dep_cont | LRdiff_L4L5_dep_cont | LRdiff_L5L6__dep_cont
                                ggsave(dep_LRdiffplots, file=file.path(path, "dep_LRdiffplots.pdf"))
        paranoia_LRdiffplots<- lrdiff_L4vsL3_green_cont | LRdiff_L4L5_green_cont | LRdiff_L5L6__green_cont
                                ggsave(paranoia_LRdiffplots, file=file.path(path, "paranoia_LRdiffplots.pdf"))
        spq_LRdiffplots<- lrdiff_L4vsL3_spq_cont | LRdiff_L4L5_spq_cont | LRdiff_L5L6__spq_cont
                                ggsave(spq_LRdiffplots, file=file.path(path, "spq_LRdiffplots.pdf"))        
        delusions_LRdiffplots<- lrdiff_L4vsL3_pdi_cont | LRdiff_L4L5_pdi_cont | LRdiff_L5L6__pdi_cont
                                ggsave(delusions_LRdiffplots, file=file.path(path,"delusions_LRdiffplots.pdf"))      

    
    #clusters 3
      lrdiff_xlevel_clusters3_all<- lrdiff_L4vsL3_clusters3 / lrdiff_L6vsL5_clusters | lrdiff_L4vsL5_clusters
                                ggsave(lrdiff_xlevel_clusters3_all, file=file.path(path, "lrdiff_xlevel_clusters3_all.pdf"))
