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
            options(scipen=999)
            library(sjPlot)
            library(data.table)

                greenParaColour<-"#6fb24b"
            MASQdepColour<-"#676ed4"
            MASQanxColour<-"#b74d86"
            PDIColour<-"#b6b638"
            SPQColout<-""
            
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

        path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop2/Results"

################################################################################################
# 2. load data and make any additional data cleaning exclusions 
################################################################################################
  #Definite exclusions and mutations 
        #assign to an object; add in your own file structure from your PC
        genpop2tibble<-readRDS("/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop2/Intermediate_outputs/GenPop2Tibble.RDS") %>%
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
                
############################################################################################################################################
# eyeballing relationship between PE and position change 
############################################################################################################################################
    x<-   genpop2 %>%
            group_by(Participant.Private.ID, level) %>% 
            summarise(sPCmed=mean(signedPositionChange), sPEmed=mean(signedPE)) %>%

            ggplot(aes(x = sPCmed, y = sPEmed, color = level)) + #geom_point(size = 1, alpha = .5) +
                    geom_smooth(method = "lm", size = 1, se = FALSE, fullrange=TRUE, alpha = .2) + 
                    xlab("Signed Position Change") + 
                    ylab("Signed PE") + 
                    theme(text = element_text(size=12)) #+ 
                    #theme(legend.position = "none") +
                    #theme(plot.title = element_text(size = 14, face = "bold")) #+
                    #facet_wrap(~level)

############################################################################################################################################
#create df 1
############################################################################################################################################
                                                                                  
             GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }
                              
    genpop2_df<-genpop2 %>%
            group_by(Participant.Private.ID, level) %>%
            mutate(gamers=gamingtime.quant>2) %>% 
            mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
            mutate(beta=GetBeta(PE= signedPE, PC=signedPositionChange)) %>%
            summarise(score=mean(trialScore), 
                    beta=dplyr::first(beta),
                    signedPE=dplyr::first(signedPE),
                    signedPositionChange=dplyr::first(signedPositionChange),
                    meancorrectedL6trialScore=mean(correctedL6trialScore),
                    meanconf=mean(participantConfidence),
                    sumsuccess=sum(success/length(level)),
                    meanPE=mean(PE),
                    meanPEscaled=mean(PEscaled),
                    meanPEsignedScaled=mean(PEsignedScaled),
                    meanPerfE=mean(PerfE),
                    PerfE=dplyr::first(PerfE),
                    PE=dplyr::first(PE),
                    PEscaled=dplyr::first(PEscaled),
                    PEsignedScaled=dplyr::first(PEsignedScaled),
                    #meanchange=meanchange,
                    SDhighcondit=dplyr::first(SDhigh),
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
                    age=dplyr::first(age),
                    gender=dplyr::first(gender),
                    device_type=dplyr::first(device_type),
                    wideStartSD=dplyr::first(startSD)) %>%
                    filter(level %in% c(3, 4, 5, 6)) 
                     genpop2_df <- genpop2_df %>% drop_na
            # Get participant-specific LR by block

                        genpop2_df$level<-droplevels(genpop2_df$level)
                      
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                        c(0,-1,1,0), # 4 vs 5
                        c(0,0,-1,1)) #5vs 6
                contrasts(genpop2_df$level) <-cMat
                colnames(attr(genpop2_df$level, "contrasts")) <- c("3v4", "4v5", "5v6")


############################################################################################################################################
#Modelling
############################################################################################################################################

            mostsimple<-genpop2_df %>% 
                        lmer(beta ~ 1 +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(mostsimple)

    
            LR_full<-  genpop2_df %>%  
                                        #ggplot(aes(x=score, y=masqCombined_depression, color=level)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(beta ~ device_type + education + age + gamers*level + gender + 
                                        green_total*level +  pdi_total*level + spq_total*level + 
                                        masqCombined_anxiety*level + masqCombined_depression*level + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(LR_full)

            LR_reduced<-  genpop2_df %>%  
                                        #ggplot(aes(x=score, y=masqCombined_depression, color=level)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(beta ~ device_type  + age + gamers*level + 
                                             spq_total*level + 
                                             masqCombined_anxiety+ masqCombined_depression*level + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(LR_reduced)

            LR_reduced2<-  genpop2_df %>%  #winner 
                                        #ggplot(aes(x=score, y=masqCombined_depression, color=level)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(beta ~ device_type  + age + gamers*level + 
                                             spq_total*level + 
                                              masqCombined_depression + 
                                       (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
                                        summary(LR_reduced2)
                                        car::qqPlot(resid(LR_reduced2))
                                        scatter.smooth(residuals(LR_reduced2) ~ fitted(LR_reduced2))

                                        #nb - tried a glmer because aberage LR still skewed but shows same results
                                        #also tried using scaled and non scaled signed PE and position change for calculating the LR
                                        #the results were all the same EXCEPT SPQ overall also becomes signficant
                                        




                 AIC(mostsimple, LR_reduced, LR_full, LR_reduced2)            
                  anova(mostsimple, LR_reduced, LR_full, LR_reduced2, test="Chisq")
    
###################################################################################################################################################
#PLOTS
###################################################################################################################################################

    ###################################
    #general plots (not from modelling)
    ###################################
        ###################################
        #Looking at LRs in response to SD and mean change
        ###################################

             #need new df for this
                #LRs in response to mean changes by symtpom scale 
                    genpop2forLR <- genpop2tibble %>% 
                                mutate(masqCombined_anxiety=sum(aa_score, gda_score)) %>%
                                mutate(masqCombined_depression=sum(ad_score, gdd_score)) %>%
                                mutate(spq_total=dplyr::first(spq_total)) %>%
                                mutate(pdi_total=dplyr::first(pdi_total)) %>%
                                mutate(green_total=dplyr::first(green_total)) %>%
                                
                                drop_na(meanchange)
                                        
                    trialwiseLR_PEc = setDT(genpop2forLR)[, coef(lm(scale(signedPE)~scale(signedPositionChange)))[2], by = c("Participant.Private.ID", 
                                                                                                                                "level", 
                                                                                                                                "meanchange",
                                                                                                                                "spq_total",
                                                                                                                                "green_total",
                                                                                                                                "masqCombined_depression",
                                                                                                                                "masqCombined_anxiety",
                                                                                                                                "pdi_total",
                                                                                                                                "aa_score", 
                                                                                                                                "gda_score",
                                                                                                                                "ad_score",
                                                                                                                                "gdd_score")]

                                                                                                                                  

                    LR_SDchange_overall<-try %>%
                                        filter(!level==4) %>%
                                        ggplot(aes(y=V1, x=level, fill=SDhighcondit)) +
                                        geom_boxplot()+
                              theme_classic()+
                                                theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            legend.box.background = element_rect(),
                                                            legend.key = element_rect(fill = "white", colour = "black"),
                                                            legend.box.margin = margin(6, 6, 6, 6),
                                                            legend.title = element_text(size = 10, colour = "black"),
                                                            legend.text = element_text(size = 8, colour = "black"),
                                                            legend.justification = c("right", "center")) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "Block",
                                                        y= "Learning rates (beta coefficient)",
                                                        colour="Block",
                                                        fill= "High noise\ncondition"
                                                    )
                    LR_SDchange_overall<-LR_SDchange_overall + scale_fill_manual(values=c(L3Col, L4Col))
                    
                    LR_meanChange_overall<- trialwiseLR_PEc %>%
                                                    filter(!level %in% c(2,3)) %>%
                                                    ggplot(aes(y=V1, x=level, fill=meanchange)) +
                                                    geom_boxplot() +
                                                    ylim(0,1) +
                                                     theme_classic()+
                                                            theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            legend.box.background = element_rect(),
                                                            legend.key = element_rect(fill = "white", colour = "black"),
                                                            legend.box.margin = margin(6, 6, 6, 6),
                                                            legend.title = element_text(size = 10, colour = "black"),
                                                            legend.text = element_text(size = 8, colour = "black"),
                                                            legend.justification = c("right", "center")) +
                                                    labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "Block",
                                                        y= "Learning rates (beta coefficient)",
                                                        colour="Block" ,
                                                        fill="Mean change\ntrials"
                                                    )
                                                    
                    LR_meanChange_overall<-LR_meanChange_overall + scale_fill_manual(values=c(L3Col, L4Col))
                    LR_to_stats<- egg::ggarrange(LR_SDchange_overall,LR_meanChange_overall)

                    ggsave(LR_to_stats, file=file.path(path,"LRs_toMean_andSD_changes.pdf"))

                    LR_overall_level<- trialwiseLR_PEc %>%
                                                    filter(level %in% c(3:6)) %>%
                                                    ggplot(aes(y=V1, x=level, fill=level)) +
                                                    geom_boxplot() +
                                                    ylim(0,1) +
                                                     theme_classic()+
                                                            theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            legend.key = element_blank())+
                                                            
                                                    labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "Block",
                                                        y= "Learning rates (beta coefficient)",
                                                        #colour="Block" ,
                                                        #fill="Block"
                                                    )

                    LR_overall_level<-LR_overall_level + scale_fill_manual(values=c(L3Col, L4Col, L5Col, L6Col))
                    ggsave(LR_overall_level, file=file.path(path,"LRs_overXlevel.pdf"))


                    

        #########################################################################################################
        #Looking at LRs for different symtpom scales, in response to SD and mean change    
        #########################################################################################################
            ###################################
            #mean changes               
            ###################################
                a<- trialwiseLR_PEc %>%
                                    filter(level==4) %>%
                                    ggplot(aes(x=pdi_total, y=V1, color=meanchange)) +
                                    geom_point(alpha=0.2)+
                                    geom_smooth(method="lm",se=T, alpha=0.15) +
                                            ylim(0,1) +
                                            theme_classic()+
                                            theme(  axis.ticks.x = element_blank(),
                                                    axis.ticks.y = element_blank(),
                                                    axis.text.x = element_blank(),
                                                                        # legend.box.background = element_rect(),
                                                    legend.position = "none") +
                                                                            #legend.box.margin = margin(6, 6, 6, 6),
                                                                            ##legend.title = element_text(size = 10, colour = "black"),
                                                                            #legend.text = element_text(size = 8, colour = "black"),
                                                                            #legend.justification = c("right", "center")) +
                                                                #facet_wrap(~level) +
                                                                #theme(legend.position = c(0.1, 0.7)) +
                                                                #coord_cartesian(ylim = c(-1, 1)) +
                                            labs( 
                                                    x= "PDI score",
                                                    y= "Learning rate",
                                                    color="Mean Change" )

                a<-a + scale_color_manual(values=c(L3Col, L4Col))

                b<-         trialwiseLR_PEc %>%
                    filter(level==4) %>%
                    ggplot(aes(x=spq_total, y=V1, color=meanchange)) +
                    geom_point(alpha=0.2)+
                    geom_smooth(method="lm",se=T, alpha=0.15) +
                            ylim(0,1) +
                              theme_classic()+
                                        theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                           # legend.box.background = element_rect(),
                                                            legend.position = "none") +
                                                            #legend.box.margin = margin(6, 6, 6, 6),
                                                            ##legend.title = element_text(size = 10, colour = "black"),
                                                            #legend.text = element_text(size = 8, colour = "black"),
                                                            #legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "SPQ score",
                                                        y= "Learning rate",
                                                        #colour="Block" 
                                                    )

                                 b<-b + scale_color_manual(values=c(L3Col, L4Col))

                c<-          trialwiseLR_PEc %>%
                    filter(level==4) %>%
                    ggplot(aes(x=green_total, y=V1, color=meanchange)) +
                    geom_point(alpha=0.2)+
                    geom_smooth(method="lm",se=T, alpha=0.15) +
                            ylim(0,1) +
                       theme_classic()+
                                        theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                           # legend.box.background = element_rect(),
                                                            legend.position = "none") +
                                                            #legend.box.margin = margin(6, 6, 6, 6),
                                                            ##legend.title = element_text(size = 10, colour = "black"),
                                                            #legend.text = element_text(size = 8, colour = "black"),
                                                            #legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "Green Paranoia score",
                                                        y= "Learning rate",
                                                        ##colour="Block" 
                                                    )

                             c<-c + scale_color_manual(values=c(L3Col, L4Col))

                d<-          trialwiseLR_PEc %>%
                                            filter(level==4) %>%
                                            ggplot(aes(x=masqCombined_depression, y=V1, color=meanchange)) +
                                            geom_point(alpha=0.15)+
                                            geom_smooth(method="lm",se=T, alpha=0.2) +
                           # ylim(0,1) +
                              theme_classic()+
                                                theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            legend.box.background = element_rect(),
                                                            legend.key = element_rect(fill = "white", colour = "black"),
                                                            legend.box.margin = margin(6, 6, 6, 6),
                                                            legend.title = element_text(size = 10, colour = "black"),
                                                            legend.text = element_text(size = 8, colour = "black"),
                                                            legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "MASQ Depression score",
                                                        y= "Learning rate",
                                                        color="Mean Change"
                                                    )
                             d<-d + scale_color_manual(values=c(L3Col, L4Col))

                e<-          trialwiseLR_PEc %>%
                    filter(level==4) %>%
                    ggplot(aes(x=masqCombined_anxiety, y=V1, color=meanchange)) +
                    geom_point(alpha=0.2)+
                    geom_smooth(method="lm",se=T, alpha=0.15) +
                            ylim(0,1) +
                             theme_classic()+
                                        theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                           # legend.box.background = element_rect(),
                                                            legend.position = "none") +
                                                            #legend.box.margin = margin(6, 6, 6, 6),
                                                            ##legend.title = element_text(size = 10, colour = "black"),
                                                            #legend.text = element_text(size = 8, colour = "black"),
                                                            #legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "MASQ anxiety score",
                                                        y= "Learning rate",
                                                        #colour="Block" 
                                                    )

                             e<-e + scale_color_manual(values=c(L3Col, L4Col))

                            LR_MeanChange_plots<-egg::ggarrange(a,b,c,d,e)
                            ggsave(LR_MeanChange_plots, file=file.path(path,"LR_MeanChange_plots_XsymptomScale.pdf"))

            ##########################################
            #SD changes
            ##########################################
                f<- try %>%
                    filter(level==3) %>%
                    ggplot(aes(x=pdi_total, y=V1, color=SDhighcondit)) +
                    geom_point(alpha=0.2)+
                    geom_smooth(method="lm",se=T, alpha=0.2,) +
                            ylim(0,1) +
                             theme_classic()+
                                        theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                           # legend.box.background = element_rect(),
                                                            legend.position = "none") +
                                                            #legend.box.margin = margin(6, 6, 6, 6),
                                                            ##legend.title = element_text(size = 10, colour = "black"),
                                                            #legend.text = element_text(size = 8, colour = "black"),
                                                            #legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "PDI score",
                                                        y= "Learning rate",
                                                        #colour="Block" 
                                                    )

                    f<-f + scale_color_manual(values=c(L3Col, L4Col))

                g<-         try %>%
                    filter(level==3) %>%
                    ggplot(aes(x=spq_total, y=V1, color=SDhighcondit)) +
                    geom_point(alpha=0.2)+
                    geom_smooth(method="lm",se=T, alpha=0.2,) +
                            ylim(0,1) +
                               theme_classic()+
                                        theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                           # legend.box.background = element_rect(),
                                                            legend.position = "none") +
                                                            #legend.box.margin = margin(6, 6, 6, 6),
                                                            ##legend.title = element_text(size = 10, colour = "black"),
                                                            #legend.text = element_text(size = 8, colour = "black"),
                                                            #legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "SPQ score",
                                                        y= "Learning rate",
                                                       # colour="Block" 
                                                    )

                g<-g + scale_color_manual(values=c(L3Col, L4Col))

                h<-          try %>%
                    filter(level==3) %>%
                    ggplot(aes(x=green_total, y=V1, color=SDhighcondit)) +
                    geom_point(alpha=0.2)+
                    geom_smooth(method="lm",se=T, alpha=0.2,) +
                            ylim(0,1) +
                       theme_classic()+
                                        theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                           # legend.box.background = element_rect(),
                                                            legend.position = "none") +
                                                            #legend.box.margin = margin(6, 6, 6, 6),
                                                            ##legend.title = element_text(size = 10, colour = "black"),
                                                            #legend.text = element_text(size = 8, colour = "black"),
                                                            #legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "Green Paranoia score",
                                                        y= "Learning rate",
                                                       # colour="Block" 
                                                    )

                h<-h + scale_color_manual(values=c(L3Col, L4Col))

                i<-          try %>%
                    filter(level==3) %>%
                    ggplot(aes(x=masqCombined_depression, y=V1, color=SDhighcondit)) +
                    geom_point(alpha=0.2)+
                    geom_smooth(method="lm",se=T, alpha=0.2,) +
                            ylim(0,1) +
                              theme_classic()+
                                                theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            legend.box.background = element_rect(),
                                                            legend.key = element_rect(fill = "white", colour = "black"),
                                                            legend.box.margin = margin(6, 6, 6, 6),
                                                            legend.title = element_text(size = 10, colour = "black"),
                                                            legend.text = element_text(size = 8, colour = "black"),
                                                            legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "MASQ Depression score",
                                                        y= "Learning rate",
                                                        colour="High Noise Condition" 
                                                    )

                i<-i + scale_color_manual(values=c(L3Col, L4Col))

                j<-          try %>%
                    filter(level==3) %>%
                    ggplot(aes(x=masqCombined_anxiety, y=V1, color=SDhighcondit)) +
                    geom_point(alpha=0.2)+
                    geom_smooth(method="lm",se=T, alpha=0.2,) +
                            ylim(0,1) +
                               theme_classic()+
                                        theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                           # legend.box.background = element_rect(),
                                                            legend.position = "none") +
                                                            #legend.box.margin = margin(6, 6, 6, 6),
                                                            ##legend.title = element_text(size = 10, colour = "black"),
                                                            #legend.text = element_text(size = 8, colour = "black"),
                                                            #legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "MASQ Anxiety  scores",
                                                        y= "Learning rate",
                                                      #  colour="Block" 
                                                    )

                j<-j + scale_color_manual(values=c(L3Col, L4Col))

                SDChange_plots<-egg::ggarrange(f,g,h,i,j)
                ggsave(SDChange_plots, file=file.path(path,"LR_SDChange_plots_XsymptomScale.pdf"))



    ###################################
    #Residual plot for finding 1 
    ###################################

        #SPQ groups show interaction in LRs between L3 and 4
            #create residual plot
                residplot_LR_reduced2_spq<- try %>%  
                                                    #ggplot(aes(x=score, y=masqCombined_depression, color=level)) +
                                                    #geom_point() +
                                                    #geom_smooth(method="lm")
                                                    lmer(V1 ~ device_type  + age + gamers + SDhighcondit +
                                                      #*level + 
                                                    #  spq_total*level + 
                                                        masqCombined_depression + 
                                                    (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                    optCtrl=list(maxfun=2e7)) )
                
                try$residual_LR_spq<-resid(residplot_LR_reduced2_spq)

            #make plots using residuals 
                SPQ_LR_l34 <-    try %>%
                                    #filter(level %in% c(3,4)) %>%      
                                                ggplot(aes(x=spq_total, y=residual_LR_spq) ) +
                                                #geom_point(alpha=0.2)+
                                                geom_smooth(method=lm, se=T, alpha=0.1, aes(color=level))+ 
                                                theme_classic()+
                                                theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            legend.box.background = element_rect(),
                                                            legend.key = element_rect(fill = "white", colour = "black"),
                                                            legend.box.margin = margin(6, 6, 6, 6),
                                                            legend.title = element_text(size = 10, colour = "black"),
                                                            legend.text = element_text(size = 8, colour = "black"),
                                                            legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        x= "SPQ total scores",
                                                        y= "Learning rates (beta coefficient)\n(residual unexplained by covariates)",
                                                        colour="Block" 
                                                    )
                                                
                SPQ_LR_l34<-SPQ_LR_l34 + scale_color_manual(values=c(L3Col, L4Col, L5Col, L6Col))
                ggsave(SPQ_LR_l34, file=file.path(path,"LR_result1_SPQ_LR_l3+4.pdf"))

    ###################################
    #Residual plot for finding 2
    ###################################

        #dperession groups show overall higher LRs

            residplot_LR_reduced2_DEP<- try %>%  
                                        #ggplot(aes(x=score, y=masqCombined_depression, color=level)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(V1 ~ device_type  + age + gamers*level + SDhighcondit +
                                           spq_total*level + 
                                            #masqCombined_depression + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
    
            
            try$residual_LR_DEP<-resid(residplot_LR_reduced2_DEP)
    
            LR_result3_dep_overall <-    try %>%
                        #filter(level %in% c(3,4)) %>%      
                                    ggplot(aes(x=masqCombined_depression, y=residual_LR_DEP, color="L4Col")) +
                                    #geom_point()+
                                    geom_smooth(method=lm, se=T, alpha=0.1)+#, aes(color=level))+ 
                                    theme_classic()+
                                    theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.position = "none") +
                                                  #legend.box.background = element_rect(),
                                                  #legend.key = element_rect(fill = "white", colour = "black"),
                                                  #legend.box.margin = margin(6, 6, 6, 6),
                                                  #legend.title = element_text(size = 10, colour = "black"),
                                                  ##legend.text = element_text(size = 8, colour = "black"),
                                                  #legend.justification = c("right", "center")) +
                                    #facet_wrap(~level) +
                                    #theme(legend.position = c(0.1, 0.7)) +
                                    #coord_cartesian(ylim = c(-1, 1)) +
                                    labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                           # caption = "Figure made using residuals from mixed model",
                                            x= "MASQ depression score",
                                            y= "Learning rates (beta coefficient)\n(residual unexplained by covariates)",
                                            #colour="Block" 
                                        )
                                    
                ggsave(LR_result3_dep_overall, file=file.path(path,"LR_result3_dep_overall.pdf"))

            LR_result3b_depxlevel<- try %>% ggplot(aes(x=masqCombined_depression, y=residual_LR_DEP) ) +
                                    #geom_point(alpha=0.2)+
                                    geom_smooth(method=lm, se=T, alpha=0.1, aes(color=level))+ 
                                    theme_classic()+
                                    theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.box.background = element_rect(),
                                                  legend.key = element_rect(fill = "white", colour = "black"),
                                                  legend.box.margin = margin(6, 6, 6, 6),
                                                  legend.title = element_text(size = 10, colour = "black"),
                                                  legend.text = element_text(size = 8, colour = "black"),
                                                  legend.justification = c("right", "center")) +
                                    #facet_wrap(~level) +
                                    #theme(legend.position = c(0.1, 0.7)) +
                                    #coord_cartesian(ylim = c(-1, 1)) +
                                    labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                           # caption = "Figure made using residuals from mixed model",
                                            x= "MASQ depression score",
                                            y= "Learning rates (beta coefficient)\n(residual unexplained by covariates)",
                                            colour="Block" 
                                        )

                LR_result3b_depxlevel<-LR_result3b_depxlevel + scale_color_manual(values=c(L3Col, L4Col, L5Col, L6Col))
                ggsave(LR_result3b_depxlevel, file=file.path(path,"LR_result3b_depxlevel.pdf"))

   ###################################
    #Residual plot for finding 3
    ###################################

        #SD high condit has sig lower LRs

            residplot_LR_reduced2_SDCondit<- try %>%  
                                        #ggplot(aes(x=score, y=masqCombined_depression, color=level)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(V1 ~ device_type  + age + gamers*level + #SDhighcondit +
                                           spq_total*level + 
                                            masqCombined_depression + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

        try$residual_LR_SDhigh<-resid(residplot_LR_reduced2_SDCondit)

        basicSDchangeplot<- try %>%
                                ggplot(aes(y=residual_LR_SDhigh, fill=SDhighcondit)) +
                                geom_boxplot()+
                                #ylim(0,1) +
                               theme_classic()+
                                        theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            axis.text.x = element_blank(),
                                                           legend.box.background = element_rect(),
                                                            #legend.position = "none") +
                                                            legend.box.margin = margin(6, 6, 6, 6),
                                                            legend.title = element_text(size = 10, colour = "black"),
                                                            legend.text = element_text(size = 8, colour = "black"),
                                                            legend.justification = c("right", "center")) +
                                                #facet_wrap(~level) +
                                                #theme(legend.position = c(0.1, 0.7)) +
                                                #coord_cartesian(ylim = c(-1, 1)) +
                                                labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                                    # caption = "Figure made using residuals from mixed model",
                                                        #x= "MASQ Anxiety  scores",
                                                        y= "Learning rates (beta coefficient)\n(residual unexplained by covariates)",
                                                      fill="High noise\ncondition" 
                                                    )

        basicSDchangeplot<- basicSDchangeplot+scale_fill_manual(values=c(L3Col, L4Col))
        ggsave(basicSDchangeplot, file=file.path(path, "LR_basicSDchangeplot.pdf"))