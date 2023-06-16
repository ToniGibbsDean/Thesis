################################################################################################
# 1. Packages and path to project directory ######
################################################################################################
    set.seed(0.1)
  
    path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop2/Results"

    library(tidyverse)
    library(ggpubr)

    library(lme4)
    library(lmerTest)
    library(nlme)
    library(multcomp)
    library(emmeans)
    library(corrr)
    #take scientific notation off
    options(scipen=999)
     library(sjPlot)

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
    df_mod<-genpop2 %>%
            mutate(gamers=gamingtime.quant>2) %>% 
            mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
            group_by(Participant.Private.ID, level, trialName, gender, age, device_type) %>%
            summarise(score=mean(trialScore), 
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
                    wideStartSD=dplyr::first(startSD)) %>%
                    filter(level %in% c(3, 4, 5, 6)) 
        df_mod<- df_mod %>%
                drop_na

            #inc l6
                df_mod$level<-droplevels(df_mod$level)
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                        c(0,-1,1,0), # 4 vs 5
                        c(0,0,-1,1)) #5vs 6
                contrasts(df_mod$level) <-cMat
                colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5", "5v6")


    ##############################
    #data frame for models for beam
    ########################

    #difference data frame as taking into account  underlying SD conditions 
            df_mod_confidence<-genpop2 %>%
                    mutate(gamers=gamingtime.quant>2) %>% 
                    mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
                    group_by(Participant.Private.ID, level, SDhigh, trialName, gender, age, device_type) %>%
                    summarise(score=mean(trialScore), 
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
                        wideStartSD=dplyr::first(startSD)) %>%
                        filter(level %in% c(3, 4, 5, 6)) 
            df_mod_confidence<- df_mod_confidence %>%
                    drop_na

# ################################################################################################
# 1. Overall task performance 
# ################################################################################################

    # ###################
    # A. Learning mean: PEs/performance errors decrease after around 3 trials after the participant is exposed to a mean
    # ###################

        ##NB - IMPORTANT THERES AN ISSUE HERE WITH THERE BEING AN EERRONEOUS STARTING MEAN THAT NEEDS REMOVING
                #AND THEN CAN ONLY LOOK AT EACH DIRECTION SEPERATELY
                #no removal needed for forwards only for reversed 
                #possibly theres a way to recombine - havekept seperate for now
            genpop2_l4_FORWARD<-genpop2 %>% 
                            group_by(Participant.Private.ID) %>%
                            filter(level==4) %>%
                            filter(trialName %in% c("spacetask003", "spacetask012")) #%>%
                        # select(Mean) %>% print(n=100)

            genpop2_l4_REVERSE<-genpop2 %>% 
                            group_by(Participant.Private.ID) %>%
                            filter(level==4) %>%
                            filter(trialName %in% c("spacetask003reversed", "spacetask012reversed")) %>%
                            #select(Mean) %>% print(n=100) %>%
                            slice(-c(1))

            genpop2_l4<-rbind(genpop2_l4_REVERSE,genpop2_l4_FORWARD)
            
                        
                        #checking length
                        genpop2 %>% group_by(Participant.Private.ID) %>%
                                        summarise(n=n()) %>%
                                        select(n) %>%as.vector %>%  table


        # make mean counting columns
                meantimeseen<-c()
                meancount<-c()
                perps<-unique(genpop2_l4$Participant.Private.ID)
                for (i in 1:length(perps)) {
                        perpid<-perps[i]
                        df_perp<-genpop2_l4[genpop2_l4$Participant.Private.ID==perpid,]
                        lengthsofruns<-rle(as.character(df_perp$Mean))$lengths
                        perp_meancount<-sequence(lengthsofruns)
                                        
                        perpmeantimeseen<-c()
                        counter<-1
                        for (i in 1:length(lengthsofruns)) {
                            perpmeantimeseen_new<-rep(counter, lengthsofruns[i])
                            perpmeantimeseen<-c(perpmeantimeseen, perpmeantimeseen_new)
                            if((i %% 2) == 0) { counter<-counter+1}
                        }
                        meancount<-c(meancount, perp_meancount)
                        perpmeantimeseen[1]<-1 # accoutn for the very first mean havign alreay been seen in the set up for the level
                        meantimeseen<-c(meantimeseen, perpmeantimeseen)

                }

                genpop2_l4$meancount<-meancount
                #genpop2_l4$meanTimesSeenColour<-c("red", "blue", "pink")
                genpop2_l4[["meanTimesSeen"]]<-as.factor(meantimeseen)

        
            #make plots
                genpop2_l4 %>%  
                    #filter(level.x==4) %>%
                    select(Mean, meancount, meanTimesSeen, level) %>%
                # head()
                    print(n=100)

            PEplot<- genpop2_l4 %>% # select(meancount, meanTimesSeen)
                        ggplot(aes(x=as.factor(meancount), y=PE)) +
                        geom_boxplot(outlier.shape=NA, aes(fill=as.factor(meanTimesSeen)))  +
                        geom_smooth() +
                        theme_classic() +
                        theme(axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              legend.justification = c("right", "top"),
                              legend.position = c(.95, .95),
                              legend.box.background = element_rect(),
                              legend.key = element_rect(fill = "white", colour = "black"),
                              legend.box.margin = margin(6, 6, 6, 6),
                              legend.title = element_text(size = 10, colour = "black"),
                              legend.text = element_text(size = 8, colour = "black")) +
                        labs(fill="No. of times\na mean has\nbeen presented",
                             x = "Mean Count",
                             y = "Prediction Error") 
            PEFinalPLot<-PEplot+ scale_fill_manual(values=c(Comp3, Comp4, Comp2))
            ggsave(PEFinalPLot, file="TaskOutputs_learningtheMean_PE_Plot.png")

            PerfEplot<- genpop2_l4 %>% # select(meancount, meanTimesSeen)
                    ggplot(aes(x=as.factor(meancount), y=PerfE)) +
                       geom_boxplot(outlier.shape=NA, aes(fill=as.factor(meanTimesSeen)))  +
                        geom_smooth() +
                        theme_classic() +
                        theme(axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              legend.justification = c("right", "top"),
                              legend.position = c(.95, .95),
                              legend.box.background = element_rect(),
                              legend.key = element_rect(fill = "white", colour = "black"),
                              legend.box.margin = margin(6, 6, 6, 6),
                              legend.title = element_text(size = 10, colour = "black"),
                              legend.text = element_text(size = 8, colour = "black")) +
                        labs(fill="No. of times\na mean has\nbeen presented",
                             x = "Mean Count",
                             y = "Performance Error") 
            PerfEFinalPLot<-PerfEplot+ scale_fill_manual(values=c(Comp3, Comp4, Comp2))
            ggsave(PerfEFinalPLot, file="TaskOutputs_learningtheMean_PerfE_Plot.png")

            #egg::ggarrange(PEFinalPLot,PerfEFinalPLot)

                        
    # ###################
    # B. Learning SD: Beam - People use wider beam in wide distributions 
    # ###################

                beammod_noSDcondit<- #WINNING MODEL
                                df_mod_confidence %>%  
                                        #ggplot(aes(y=meanconf, x=pdi_total, color=SDhighcondit)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(meanconf ~ gender  + 
                                         masqCombined_depression + masqCombined_anxiety+ green_total +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                 df_mod_confidence$residual_beamxSDcondit<-resid(beammod_noSDcondit)

                LearningSDPlot<-df_mod_confidence%>%
                                    filter(!level=="4") %>%
                                    mutate(SD=case_when(SDhigh==FALSE ~ "High Noise Conditions", SDhigh==TRUE ~ "Low Noise Conditions")) %>%                
                                    ggplot(aes(y=residual_beamxSDcondit, x=as.factor(level), fill=as.factor(SD))) +
                                            geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                            theme_classic() +
                                            theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.box.background = element_rect(),
                                                  legend.key = element_rect(fill = "white", colour = "black"),
                                                  legend.box.margin = margin(6, 6, 6, 6),
                                                  legend.title = element_text(size = 10, colour = "black"),
                                                  legend.text = element_text(size = 8, colour = "black"),
                                                  legend.justification = c("right", "center"))+
                                                  #legend.position = c(.95, .95)) +
                                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    fill = "Noise Condition",
                                                    x= "Blocks",
                                                    y= "Space Task Beam Width\n(residual unexplained by covariates)")
                                    LearningSDPlot<-LearningSDPlot + scale_fill_manual(values=c(Comp1, Comp6))
                                    ggsave(LearningSDPlot, file="TaskOutputs_learningtheSD_Beam_Plot.png")
                                            


        
    # ###################
    # C. Score: decrease as the game increases in difficulty	
    # ###################
        #take rediuals of overall model, without removing? 

                overallscoreMod_nolevel_plots<- 
                        df_mod %>%  
                                        lmer(meancorrectedL6trialScore ~ device_type + gamers+ gender  +
                                        green_total +  
                                        masqCombined_depression + 
                                        (1|Participant.Private.ID) + (1|trialName), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                df_mod$residual_levelxscore<-resid(overallscoreMod_nolevel_plots)

        #NOT working because the scores are on different scales because of l6. Also, something not right because the levels
            #should be contrasts 
                overallscore<-df_mod %>%
                                mutate(Block=case_when(level==2 ~ "Block 2 (stable)", level==3 ~ "Block 3 (high noise)", level==4 ~ "Block 4 (high volatility)",
                                level==5 ~ "Block 5 (high noise and volatiility)", level==6 ~ "Block 6 (high noise and volatility (losses))")) %>%
                                  ggplot(aes(y=residual_levelxscore, x=level, fill=Block)) +
                                   geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                            theme_classic() +
                                            theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.box.background = element_rect(),
                                                  legend.position = c(.99, .99),
                                                  legend.key = element_rect(fill = "white", colour = "black"),
                                                  legend.box.margin = margin(6, 6, 6, 6),
                                                  #legend.title = element_text(size = 10, colour = "black"),
                                                  legend.title = element_blank(),
                                                  legend.text = element_text(size = 8, colour = "black"),
                                                  legend.justification = c("right", "top"))+
                                                  #legend.position = c(.95, .95)) +
                                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    fill = "Block",
                                                    x= "Blocks",
                                                    y= "Space Task Score (normalised)\n(residual unexplained by covariates)")
                                 
                overallscore<-overallscore + scale_fill_manual(values=c(L3Col, L4Col, L5Col, L6Col))
 

################################################################################################
# 2. Covariate findings: Gamers by gender 
################################################################################################

    #1. scores and gamers, by gender. Gamers get better scores. Males get back scores. 

         Plots_covariates_gamers<- 
                        df_mod %>%                                
                                lmer(score ~ device_type  + gender  +
                                        green_total*level +  
                                        masqCombined_depression*level + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
        
        df_mod$residual_gamers<-resid(Plots_covariates_gamers)

        #not working cant get gedner to plot seperately 
        p8<-ggplot(df_mod, aes(x=score, y=residual_gamers, colour=gamers))+   #, fill=gender)) +
                            #geom_boxplot()
                                    geom_point() +
                                    geom_smooth(method=lm) +
                                    facet_wrap(~gamers) +

                                    
                                    + 
                                    theme_classic()+
                                    #theme(legend.position = c(0.1, 0.7)) +
                                    coord_cartesian(ylim = c(-20, 20)) +
                                    facet_wrap(~gender) +
                                    labs(   title="Relationship between Scores on the MASQ Depression Scales, and Space Task Performance Error", 
                                            caption = "Figure made using residuals from mixed model",
                                            x= "MASQ Depression Scales",
                                            y= "Space Task Score (residuals)",
                                            colour="MASQ Depression Scales" )

                #havent got significance things to work yet. 
               GamersPlot_level<-df_mod%>%
                                    #mutate(SD=case_when(SDhigh==TRUE ~ "High Noise Conditions", SDhigh==FALSE ~ "Low Noise Conditions")) %>%                
                                    #mutate(levelxgamer4plot=factor(interaction(level, gamers),
                                    #                                levels = c("3.T", "3.F", "4.T", "4.F", "5.T", "5.F", "6.T", "6.F"))) %>%
                                    mutate(gamerstext=case_when(gamers==TRUE ~ "Gaming Over 5hrs/week", gamers==FALSE ~ "Gaming Under 5hrs/week")) %>%                
                                    ggplot(aes(y=residual_gamers, x=as.factor(level), fill=as.factor(gamerstext))) +
                                            geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                            #geom_signif(comparisons = list(c("5.T", "6.T"),  
                                             #                         map_signif_level=TRUE)
                                            theme_classic() +
                                            theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.box.background = element_rect(),
                                                  legend.key = element_rect(fill = "white", colour = "black"),
                                                  legend.box.margin = margin(6, 6, 6, 6),
                                                  legend.title = element_text(size = 10, colour = "black"),
                                                  legend.text = element_text(size = 8, colour = "black"),
                                                  legend.justification = c("right", "center"))+
                                                  #legend.position = c(.95, .95)) +
                                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    fill = "Gaming Status",
                                                    x= "Blocks",
                                                    y= "Space Task Score\n(residual unexplained by covariates)")
                                    GamersPlot_level<-GamersPlot_level + scale_fill_manual(values=c(Comp5, Comp6))
                                    ggsave(GamersPlot_level, file="Covariates_GamingxLevel_Score_Plot.png")
                                            

            GamersPlot_overallScore<-df_mod%>%
                                    mutate(gamerstext=case_when(gamers==TRUE ~ "Gaming Over 5hrs/week", gamers==FALSE ~ "Gaming Under 5hrs/week")) %>%                
                                    ggplot(aes(y=residual_gamers, x=gamerstext, fill=as.factor(gamerstext))) +
                                            geom_boxplot(outlier.size=1, outlier.color="darkgrey", show.legend=NULL) +
                                            theme_classic() +
                                            theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank())+
                                                  #legend.box.background = element_rect(),
                                                  #legend.key = element_rect(fill = "white", colour = "black"),
                                                  #legend.box.margin = margin(6, 6, 6, 6),
                                                  #legend.title = element_text(size = 10, colour = "black"),
                                                  #legend.text = element_text(size = 8, colour = "black"),
                                                  #legend.justification = c("right", "center"))+
                                                  #legend.position = c(.95, .95)) +
                                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    fill = "Gaming Status",
                                                    x= "Gaming Time",
                                                    y= "Space Task Score\n(residual unexplained by covariates)")
                                    GamersPlot_overallScore<-GamersPlot_overallScore + scale_fill_manual(values=c(Comp5, Comp6))
                                    ggsave(GamersPlot_overallScore, file="Covariates_GamingOverallScore_Plot.png")

# ################################################################################################
# 3Aa, b, c. Symptom finding Aa, Ab - main findings (MASQ-dep, Green)
################################################################################################
    ###########################
    #3Aa. Score
    ###########################   

            BestModel_missingGreenInteraction<- 
                                        df_mod %>%                            
                                                lmer(score ~ device_type + gamers*level + gender  +  
                                                        masqCombined_depression*level + 
                                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                optCtrl=list(maxfun=2e7)) )

                df_mod$residualGreenMeanCorrectedL6trialScore <-resid(BestModel_missingGreenInteraction)

            Aa1<-ggplot(df_mod, aes(x=green_total, y=residualGreenMeanCorrectedL6trialScore)) +
                                    geom_point(alpha=0.1,  color=greenParaColour)+
                                    geom_smooth(method=lm,  color=greenParaColour)+ 
                                    theme_classic()+
                                    coord_cartesian(ylim = c(-1, 1)) +
                                    labs(   #title="Relationship between Scores on the Green Paranoia Scale and Space Task Score", 
                                            #caption = "Figure made using residuals from mixed model",
                                            #x= "Green Paranoia Symptom Scores",
                                            #y= "Space Task Score (residuals)",
                                            #colour="Green Paranoia Scale"
                                            )+
                                    theme(legend.position="none")

            BestModel_missingMasqInteraction<- 
                                        df_mod %>%                            
                                                lmer(meancorrectedL6trialScore ~ device_type + gamers*level + gender  +  
                                                        green_total*level + 
                                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                optCtrl=list(maxfun=2e7)) )

                df_mod$residualMasqMeanCorrectedL6trialScore <-resid(BestModel_missingMasqInteraction)

            Aa2<-ggplot(df_mod, aes(x=masqCombined_depression, y=residualMasqMeanCorrectedL6trialScore) ) +
                                    geom_point(alpha=0.1,  color=MASQdepColour)+
                                    geom_smooth(method=lm,  color=MASQdepColour)+ 
                                    theme_classic()+
                                    #theme(legend.position = c(0.1, 0.7)) +
                                    coord_cartesian(ylim = c(-1, 1)) +
                                    labs(   #title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                            #caption = "Figure made using residuals from mixed model",
                                            #x= "MASQ Depression Scales",
                                            #y= "Space Task Score (residuals)",
                                            #colour="MASQ Depression Scales"                 
                        )+
                                    theme(legend.position="none",
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank(),
                                            axis.title.y=element_blank())
        
                
            #combined_scorefinding<-egg::ggarrange(p1, p2, ncol=2) 

            #combined_scorefinding_plot<-annotate_figure(combined_scorefinding_plot, top="Plot Showing Relationship Between High Symtpom Scorers on Symptom Scales and Scores on the Space Task")   

            Aa1<-Aa1+ xlab("Green Paranoia\nSymptom Score") +ylab("Space Task Score\n(residual unexplained by covariates)")
            Aa2<-Aa2 + xlab("MASQ Depression\nScale")

            N<-cowplot::ggdraw() +
                cowplot::draw_plot(Aa1, x=0, y=0, width=0.55, height=1) +
                cowplot::draw_plot(Aa2, x=0.5, y=0, width=0.45, height=1)


            Nplusstats<- N + annotate("text", x = 0.8, y = 0.85,
                         label = "paste(italic(p), \" < .001\")", 
                         parse = TRUE,
                         size=3.5)

            Nplusstats2<- Nplusstats + annotate("text", x = 0.4, y = 0.85,
                         label = "paste(italic(p), \" < .001\")", 
                         parse = TRUE,
                         size=3.5)

     


            ggsave(Nplusstats2, file="SymptomFinding1_Dep_Paranoia_Score.png")


    ######################
    #3Ab. Plots for PE
    ######################

            mod_plots_noGreen2<-df_mod %>% ###WINNING MODEL
                                mutate(logmeanPE=log(meanPE)) %>%
                                lmer(logmeanPE ~ device_type + gamers  + gender + 
                                masqCombined_depression  + level + 
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                df_mod$residual_green_PE <-resid(mod_plots_noGreen2)

            Ab1<-ggplot(df_mod, aes(x=green_total, y=residual_green_PE) ) +
                                    geom_point(alpha=0.1,  color=greenParaColour)+
                                    geom_smooth(method=lm,  color=greenParaColour)+ 
                                    theme_classic()+
                                    coord_cartesian(ylim = c(-0.25, 0.25)) +
                                    labs(  #title="Relationship between Scores on the Green Paranoia Scale and Space Task Prediction Error", 
                                            #caption = "Figure made using residuals from mixed model",
                                            #x= "Green Paranoia Symptom Scores",
                                            #y= "Space Task Score (residuals)",
                                            #colour="Green Paranoia Scale"
                                    theme(legend.position="none",
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank(),
                                            axis.title.y=element_blank()))

            mod_plots_noDep2<-df_mod %>% ###WINNING MODEL
                                mutate(logmeanPE=log(meanPE)) %>%
                                lmer(logmeanPE ~ device_type + gamers  + gender + 
                                green_total  +  
                                level + 
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                df_mod$residual_Dep_PE <-resid(mod_plots_noDep2)

            Ab2<-ggplot(df_mod, aes(x=masqCombined_depression, y=residual_Dep_PE) ) +
                                    geom_point(alpha=0.1,  color=MASQdepColour)+
                                    geom_smooth(method=lm,  color=MASQdepColour)+ 
                                    theme_classic()+
                                    #theme(legend.position = c(0.1, 0.7)) +
                                    coord_cartesian(ylim = c(-0.25, 0.25)) +
                                    labs( )+  #title="Relationship between Scores on the MASQ Depression Scales, and Space Task Prediction Error", 
                                            #caption = "Figure made using residuals from mixed model",
                                            #x= "MASQ Depression Scales",
                                            #y= "Space Task Prediction Error (residuals)",
                                            #colour="MASQ Depression Scales" ))
                                    theme(legend.position="none",
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank(),
                                            axis.title.y=element_blank()
                                            )
   
           # combined_PEfinding<-egg::ggarrange(p3, p4, ncol=2) 

           #combined_PEfinding_plot<-annotate_figure(combined_PEfinding, top="Plot Showing Relationship Between High Symtpom Scorers on Symptom Scales and Prediction errors in the Space Task")   

            Ab1<-Ab1+ xlab("Green Paranoia\nSymptom Score") + ylab("Space Task Prediction Error\n(residual unexplained by covariates)")
            Ab2<-Ab2 + xlab("MASQ Depression\nScale") 

            PEfordepandgreen<-cowplot::ggdraw() +
                                cowplot::draw_plot(Ab1, x=0, y=0, width=0.5, height=1) +
                                cowplot::draw_plot(Ab2, x=0.5, y=0, width=0.5, height=1)
            ggsave(PEfordepandgreen, file="SymptomFinding1_Dep_Paranoia_PE.png")

    ######################
    #3Ab.Plots for Performance error 
    ######################

                mod_plots_noGreen3<- ###WINNING MODEL
                                df_mod %>%
                                        mutate(logmeanPerfE=log(meanPerfE)) %>%
                                        #filter(level!="6") %>%  
                                        # ggplot(aes(y=meanPEsignedScaled, x=spq_total, color=level)) +
                                        # geom_point() +
                                        # geom_smooth(method="lm", se=F)
                                        lmer(logmeanPerfE ~ device_type + gender + gamers +
                                        pdi_total*level  +
                                        masqCombined_anxiety + masqCombined_depression  +

                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                df_mod$residual_green_PerfE<-resid(mod_plots_noGreen2)


            Ab1<-ggplot(df_mod, aes(x=green_total, y=residual_green_PerfE) ) +
                                    geom_point(alpha=0.1,  color=greenParaColour)+
                                    geom_smooth(method=lm,  color=greenParaColour)+ 
                                    theme_classic()+
                                    coord_cartesian(ylim = c(-0.25, 0.25)) +
                                    labs(  #title="Relationship between Scores on the Green Paranoia Scale and Space Task Prediction Error", 
                                            #caption = "Figure made using residuals from mixed model",
                                            #x= "Green Paranoia Symptom Scores",
                                            #y= "Space Task Score (residuals)",
                                            #colour="Green Paranoia Scale"
                                    theme(legend.position="none",
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank(),
                                            axis.title.y=element_blank()))

                                        
                mod_plots_noDep3<-
                             df_mod %>%
                                        mutate(logmeanPerfE=log(meanPerfE)) %>%
                                        #filter(level!="6") %>%  
                                        # ggplot(aes(y=meanPEsignedScaled, x=spq_total, color=level)) +
                                        # geom_point() +
                                        # geom_smooth(method="lm", se=F)
                                        lmer(logmeanPerfE ~ device_type + gender + gamers +
                                        pdi_total*level  +
                                        masqCombined_anxiety  + green_total +

                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )


                df_mod$residual_Dep_PerfE <-resid(mod_plots_noDep3)

                            Ab2<-ggplot(df_mod, aes(x=masqCombined_depression, y=residual_Dep_PerfE) ) +
                                    geom_point(alpha=0.1,  color=MASQdepColour)+
                                    geom_smooth(method=lm,  color=MASQdepColour)+ 
                                    theme_classic()+
                                    #theme(legend.position = c(0.1, 0.7)) +
                                    coord_cartesian(ylim = c(-0.25, 0.25)) +
                                    labs( )+  #title="Relationship between Scores on the MASQ Depression Scales, and Space Task Prediction Error", 
                                            #caption = "Figure made using residuals from mixed model",
                                            #x= "MASQ Depression Scales",
                                            #y= "Space Task Prediction Error (residuals)",
                                            #colour="MASQ Depression Scales" ))
                                    theme(legend.position="none",
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank(),
                                            axis.title.y=element_blank())
   
   
            Ab1<-Ab1+ xlab("Green Paranoia\nSymptom Score") + ylab("Space Task Performance Error\n(residual unexplained by covariates)")
            Ab2<-Ab2 + xlab("MASQ Depression\nScale") 

            PerfEfordepandgreen<-cowplot::ggdraw() +
                                cowplot::draw_plot(Ab1, x=0, y=0, width=0.5, height=1) +
                                cowplot::draw_plot(Ab2, x=0.5, y=0, width=0.5, height=1)
            
            plusstats<- PerfEfordepandgreen + annotate("text", x = 0.8, y = 0.85,
                         label = "paste(italic(p), \" < .001\")", 
                         parse = TRUE,
                         size=3.5)

            plusstats2<- plusstats + annotate("text", x = 0.4, y = 0.85,
                         label = "paste(italic(p), \" < .001\")", 
                         parse = TRUE,
                         size=3.5)

            ggsave(plusstats2, file="SymptomFinding1_Dep_Paranoia_PerformanceError.png")

    ##############
    #3Ab.Plots for Beam
    ##############

        mod_plots_noGreen4<- ###WINNING MODEL
                            df_mod_confidence %>%  
                                            #ggplot(aes(y=meanconf, x=pdi_total, color=SDhighcondit)) +
                                            #geom_point() +
                                            #geom_smooth(method="lm")
                                            lmer(meanconf ~ gender  + SDhigh:level +
                                            masqCombined_depression*SDhigh + masqCombined_anxiety*SDhigh +
                                            (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                            control=lme4::lmerControl(optimizer="bobyqa", 
                                                                            optCtrl=list(maxfun=2e7)) )

                    df_mod_confidence$residual_green_Beam<-resid(mod_plots_noGreen4)

                    Ab1<- df_mod_confidence %>%
                                mutate(SD=case_when(SDhigh==FALSE ~ "High Noise Conditions", SDhigh==TRUE ~ "Low Noise Conditions")) %>%                
                                ggplot(aes(x=green_total, y=residual_green_Beam) ) +
                                        geom_point(alpha=0.1, colour=greenParaColour)+
                                        geom_smooth(method=lm, color=greenParaColour)+ 
                                        theme_classic()+
                                        geom_hline(yintercept=0, linetype = "dashed")+
                                        coord_cartesian(ylim = c(-20, 20)) +
                                        facet_grid(SD~1) +
                                        theme(legend.position="none",
                                               strip.text.x=element_blank(),
                                               axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank())#+
                                        #+
                                        #labs(   title="Relationship between Scores on the Green Paranoia Scale and Space Task Beam Width", 
                                         #       #caption = "Figure made using residuals from mixed model",
                                          #      x= "Green Paranoia Symptom Scores",
                                           #     y= "Space Task Score (residuals)",
                                            #    colour="Green Paranoia Scale"
                                             #   )


                    mod_plots_noDep4<- ###WINNING MODEL
                            df_mod_confidence %>%  
                                            #ggplot(aes(y=meanconf, x=pdi_total, color=SDhighcondit)) +
                                            #geom_point() +
                                            #geom_smooth(method="lm")
                                            lmer(meanconf ~ gender  + SDhigh:level +
                                            masqCombined_anxiety*SDhigh + green_total*SDhigh  +
                                            (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                            control=lme4::lmerControl(optimizer="bobyqa", 
                                                                            optCtrl=list(maxfun=2e7)) )


                    df_mod_confidence$residual_Dep_Beam<-resid(mod_plots_noDep4)

                    Ab2<-df_mod_confidence%>%
                                mutate(SD=case_when(SDhigh==FALSE ~ "High Noise Conditions", SDhigh==TRUE ~ "Low Noise Conditions")) %>%       
                                ggplot(aes(x=masqCombined_depression, y=residual_Dep_Beam) ) +
                                        geom_point(alpha=0.1, colour=MASQdepColour)+
                                        geom_smooth(method=lm, color=MASQdepColour)+ 
                                        theme_classic()+
                                        geom_hline(yintercept=0, linetype = "dashed")+
                                        #theme(legend.position = c(0.1, 0.7)) +
                                        coord_cartesian(ylim = c(-20, 20)) +
                                        facet_grid(SD~1) +
                                        theme(legend.position="none",
                                            axis.text.y=element_blank(),
                                            axis.title.y=element_blank(),
                                            axis.ticks.y=element_blank(),
                                            strip.text.x=element_blank())#+

                        
                                        #labs(   title="Relationship between Scores on the MASQ Depression Scales, and Space Task Beam Width", 
                                         #       caption = "Figure made using residuals from mixed model",
                                          #      x= "MASQ Depression Scales",
                                           #     y= "Space Task Score (residuals)",
                                            #    colour="MASQ Depression Scales" )


            Ab1<-Ab1+ xlab("Green Paranoia\nSymptom Score") + ylab("Space Task Beam Width\n(residual unexplained by covariates)") #+ strip
          # Ab1<-Ab1 + annotate("text", x = 0.1, y = 0.1, label = "Some text")
                    
            Ab2<-Ab2 + xlab("MASQ Depression\nScale")
             #Ab2<-Ab2 + annotate("text", x = 4, y = 25, label = "Some text")

            N2<-cowplot::ggdraw() +
                cowplot::draw_plot(Ab1, x=0, y=0, width=0.55, height=1) +
                cowplot::draw_plot(Ab2, x=0.5, y=0, width=0.45, height=1)
            
            #N2<-N2 + annotate("text", x = 0.4, y = 0.85, label="italic(R) ^ 2 == 0.75", parse=TRUE)
            
           N3<-N2 + annotate("text", x = 0.8, y = 0.85,
                         label = "paste(italic(p), \" < .001\")", 
                         parse = TRUE,
                         size=3.5)

            N4<-N3 + annotate("text", x = 0.4, y = 0.85,
                         label = "paste(italic(p), \" < .017\")", 
                         parse = TRUE,
                         size=3.5)

            ggsave(N4, file="SymptomFinding1_Dep_Paranoia_Beam.png")


    ##############
    #3Ac.Interactions with level for masq depression 
    ##############
        #should see better scoing in level 6 vs 5
                  residplot_scoreXlevel<- 
                        df_mod %>% 
                                    lmer(score ~ device_type + gamers*level + gender  +
                                        green_total*level +  
                                         
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                    df_mod$residual_scoreforlevelplot<-resid(residplot_scoreXlevel)

                MASQdepscoreimprovementinL6<-
                    df_mod %>%
                        #filter(level %in% c(5,6)) %>%
                                    mutate(Block=case_when(level==2 ~ "Block 2 (stable)", level==3 ~ "Block 3 (high noise)", level==4 ~ "Block 4 (high volatility)", 
                                                           level==5 ~ "Block 5 (high noise and volatiility)", level==6 ~ "Block 6 (high noise and volatility (losses))")) %>%       
                                    ggplot(aes(x=masqCombined_depression, y=residual_scoreforlevelplot) ) +
                                    #geom_point(alpha=0.2)+
                                    geom_smooth(method=lm, se=T, alpha=0.1, aes(color=Block))+ 
                                    theme_classic()+
                                    theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.box.background = element_rect(),
                                                  legend.key = element_rect(fill = "white", colour = "black"),
                                                  legend.box.margin = margin(6, 6, 6, 6),
                                                  legend.title = element_text(size = 10, colour = "black"),
                                                  legend.text = element_text(size = 8, colour = "black"),
                                                  legend.justification = c("right", "center"))+
                                    #facet_wrap(~level) +
                                    #theme(legend.position = c(0.1, 0.7)) +
                                    #coord_cartesian(ylim = c(-1, 1)) +
                                    labs(  # title="Relationship between Scores on the MASQ Depression Scales, and Space Task Score", 
                                           # caption = "Figure made using residuals from mixed model",
                                            x= "MASQ Depression Score",
                                            y= "Space Task Score\n(residual unexplained by covariates)",
                                            colour="Block" 
                                        )
                                    
                                    MASQdepscoreimprovementinL6<-MASQdepscoreimprovementinL6 + scale_color_manual(values=c(L3Col, L4Col, L5Col, L6Col))
                                    
                                    MASQdepscoreimprovementinL6_withstats<-MASQdepscoreimprovementinL6 + annotate("text", x = 160, y = 0.35,
                                                label = "paste(italic(p), \" < .001\")", 
                                                parse = TRUE,
                                                size=3.5)


                                    ggsave(MASQdepscoreimprovementinL6_withstats, path="path", filename="SymptomFinding1b_DepxLevel_Score.png")
                                     ggsave(MASQdepscoreimprovementinL6_withstats, file="SymptomFinding1b_DepxLevel_Score.png")
        

################################################################################################
# Plots: Symptom finding B - Anxiety: lower success in catching; wrong beam use
################################################################################################
    #ANX less successfull at catching

                mod_plots_noAnx<-       
                    df_mod %>%  
                            #ggplot(aes(y=sumsuccess, x=spq_total, color=level)) +
                            #geom_point() +
                            #geom_smooth(method="lm")
                            lmer(sumsuccess ~ device_type + gamers*level + 
                             spq_total*level 
                              + masqCombined_depression +
                            (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                            control=lme4::lmerControl(optimizer="bobyqa", 
                                                            optCtrl=list(maxfun=2e7)) )

                
                        df_mod$residual_ANX <-resid(mod_plots_noAnx)

                MASQanx_success<-ggplot(df_mod, aes(x=masqCombined_anxiety, y=residual_ANX, color=masqCombined_anxiety)) +
                                    geom_point(alpha=0.1, color=MASQanxColour)+
                                    geom_smooth(color=MASQanxColour, method=lm, se=T, alpha=0.3)+ 
                                    theme_classic()+
                                    geom_hline(yintercept=0, linetype = "dashed")+
                                    theme(axis.ticks.x = element_blank(),
                                          axis.ticks.y = element_blank()) +
                                    coord_cartesian(ylim = c(-0.2, 0.2)) +
                                    labs(   #title="Relationship between Scores on the MASQ Anxiety Scales and Space Task Success", 
                                            #caption = "Figure made using residuals from mixed model",
                                            y= "Space Task Catch Success Rate\n(residual unexplained by covariates)",
                                            x= "MASQ Anxiety Score",
                                            #colour="MASQ Anxiety Scales"
                                            )
  
                MASQanx_success_withstats<-MASQanx_success + annotate("text", x = 99, y = 0.05,
                                                label = "paste(italic(p), \" = .005\")", 
                                                parse = TRUE,
                                                size=3.5)

                ggsave(MASQanx_success, file="SymptomFinding2a_Anxiety_Success.png")

                        
    
    #ANX use the beam weirdly, which may support findings of less overall success 

            mod_plots_noAnx2<- ###WINNING MODEL
                          df_mod_confidence %>%  
                                        #ggplot(aes(y=meanconf, x=pdi_total, color=SDhighcondit)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                       lmer(meanconf ~ gender  + SDhigh:level +
                                         masqCombined_depression*SDhigh + green_total*SDhigh  +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                df_mod_confidence$residual_Anx_Beam<-resid(mod_plots_noAnx2)

                AnxbeamWidth<-df_mod_confidence %>%
                        mutate(SD=case_when(SDhigh==FALSE ~ "High Noise Conditions", SDhigh==TRUE ~ "Low Noise Conditions")) %>%                
                        ggplot(aes(x=masqCombined_anxiety, y=residual_Anx_Beam, color=masqCombined_anxiety) ) +
                                    geom_point(alpha=0.1,color=MASQanxColour) +
                                    geom_smooth(method=lm, se=T, alpha=0.3, color=MASQanxColour)+ 
                                    geom_hline(yintercept=0, linetype = "dashed")+
                                    theme_classic()+
                                    theme(axis.ticks.x = element_blank(),
                                          axis.ticks.y = element_blank()) +
                                    facet_wrap(~SD) +
                                    coord_cartesian(ylim = c(-20, 20)) +
                                    labs(   #title="Relationship between Scores on the MASQ Anxiety Scales and Beam Width", 
                                            #caption = "Figure made using residuals from mixed model",
                                            x= "MASQ Anxiety Scales Symptom Scores",
                                            y= "Space Task Beam Width\n(residual unexplained by covariates)",
                                            #colour="MASQ Anxiety Scales"
                                            )

                        AnxbeamWidth_withstats<-AnxbeamWidth + annotate("text", x = 99, y = 0.65,
                                                label = "paste(italic(p), \" = .054\")", 
                                                parse = TRUE,
                                                size=3.5)


                               ggsave(AnxbeamWidth, file="SymptomFinding2b_Anxiety_Beam.png")          

################################################################################################
# Plots: Symptom finding 3: only occur once per symtpom scale 
################################################################################################

    #c1: PDI performance error is bigger under volatiltiy 
    #PDI - PerfE is different between levels 3 and 4

            mod_plots_noPDI<-
                             df_mod %>%
                                        mutate(logmeanPerfE=log(meanPerfE)) %>%
                                        #filter(level!="6") %>%  
                                        # ggplot(aes(y=meanPEsignedScaled, x=spq_total, color=level)) +
                                        # geom_point() +
                                        # geom_smooth(method="lm", se=F)
                                        lmer(logmeanPerfE ~ device_type + gender + gamers +
                                        masqCombined_anxiety  + green_total +

                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )


                df_mod$residual_PDI_PerfE <-resid(mod_plots_noPDI)

                PDIperfE<-df_mod %>%
                            filter(!level %in% c(2,5,6)) %>%
                            mutate(Block=case_when(level==2 ~ "Block 2 (stable)", level==3 ~ "Block 3 (high noise)", level==4 ~ "Block 4 (high volatility)", 
                                                    level==5 ~ "Block 5 (high noise and volatiility)", level==6 ~ "Block 6 (high noise and volatility (losses))")) %>%       
                            ggplot(aes(x=pdi_total, y=residual_PDI_PerfE, color=Block)) +
                                    geom_point(alpha=0.3)+
                                    geom_smooth(method=lm, se=T, alpha=0.1)+ 
                                    theme_classic()+
                                    theme( #legend.position="none",
                                                 #strip.text.x=element_blank(),       
                                                  axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.box.background = element_rect(),
                                                  legend.key = element_rect(fill = "white", colour = "black"),
                                                  legend.box.margin = margin(6, 6, 6, 6),
                                                  legend.title = element_text(size = 10, colour = "black"),
                                                  legend.text = element_text(size = 8, colour = "black")) +
                                                  #legend.justification = c("top", "right"))+
                                    #facet_wrap(~level) +
                                    #facet_wrap(~level) +
                                    coord_cartesian(ylim = c(-0.5, 0.5)) +
                                    labs(   #title="Relationship between Scores on the PDI and Space Task Success", 
                                            #caption = "Figure made using residuals from mixed model",
                                            x= "PDI Scores",
                                            y= "Space Task Performance Error\n(residual unexplained by covariates)",
                                            #colour="Block" 
                                            )
                        
                        PDIperfE<-PDIperfE + scale_color_manual(values=c(L3Col, L4Col))
                        PDIperfE_withstats<-PDIperfE + annotate("text", x = 139, y = 0,
                                                label = "paste(italic(p), \" = .007\")", 
                                                parse = TRUE,
                                                size=3.5)
                        ggsave(PDIperfE, file="SymptomFinding3_PDI_PerfE.png")
                    
                    
    #c2 success - SPQ

            SPQsuccessforplto<-       
                    df_mod %>%  
                            #ggplot(aes(y=sumsuccess, x=spq_total, color=level)) +
                            #geom_point() +
                            #geom_smooth(method="lm")
                            lmer(sumsuccess ~ device_type + gamers*level + 
                            
                            masqCombined_anxiety  + masqCombined_depression +
                            (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                            control=lme4::lmerControl(optimizer="bobyqa", 
                                                            optCtrl=list(maxfun=2e7)) )

    
    df_mod$residual_SPQ_Success <-resid(SPQsuccessforplto)

                 SPQsuccessplot<-ggplot(df_mod, aes(x=spq_total, y=residual_SPQ_Success, color=spq_total)) +
                                    geom_point(alpha=0.3, color=Comp6)+
                                    geom_smooth(color=Comp6, method=lm, se=T, alpha=0.1)+ 
                                    geom_hline(yintercept=0, linetype = "dashed")+
                                    theme_classic()+
                                    theme(axis.ticks.x = element_blank(),
                                          axis.ticks.y = element_blank()) +
                                    coord_cartesian(ylim = c(-0.2, 0.2)) +
                                    labs(   #title="Relationship between Scores on the MASQ Anxiety Scales and Space Task Success", 
                                            #caption = "Figure made using residuals from mixed model",
                                            y= "Space Task Catch Success Rate\n(residual unexplained by covariates)",
                                            x= "SPQ Score",
                                            #colour="MASQ Anxiety Scales"
                                            )
  
                ggsave(SPQsuccessplot, file="SymptomFinding3_SPQ_Success.png")





#saving
    ggsave(PEFinalPLot, file=file.path(path,"TaskOutputs_learningtheMean_PE_Plot.pdf")) 
    ggsave(PerfEFinalPLot, file=file.path(path,"TaskOutputs_learningtheMean_PerfE_Plot.pdf"))
    ggsave(LearningSDPlot, file=file.path(path,"TaskOutputs_learningtheSD_Beam_Plot.pdf"))
    ggsave(overallscore, file=file.path(path,"TaskOutputs_socrexlevel_NormlaisedMod_Plot.pdf"))
     
    ggsave(GamersPlot_overallScore, file=file.path(path,"Covariates_GamingOverallScore_Plot.pdf"))
    ggsave(GamersPlot_level, file=file.path(path,"Covariates_GamingxLevel_Score_Plot.pdf"))
    
    ggsave(Nplusstats2, file=file.path(path,"SymptomFinding1_Dep_Paranoia_Score.pdf"))
    ggsave(PEfordepandgreen, file=file.path(path,"SymptomFinding1_Dep_Paranoia_PE.pdf"))
    ggsave(plusstats2, file=file.path(path,"SymptomFinding1_Dep_Paranoia_PerformanceError.pdf"))
    ggsave(N4, file=file.path(path,"SymptomFinding1_Dep_Paranoia_Beam.pdf"))
    
    ggsave(MASQdepscoreimprovementinL6_withstats, file=file.path(path,"SymptomFinding1b_DepxLevel_Score.pdf"))
    
    ggsave(MASQanx_success_withstats, file=file.path(path,"SymptomFinding2a_Anxiety_Success.pdf"))
    ggsave(AnxbeamWidth, file=file.path(path,"SymptomFinding2b_Anxiety_Beam.pdf")   )       

    ggsave(PDIperfE_withstats, file=file.path(path,"SymptomFinding3_PDI_PerfE.pdf"))

    ggsave(SPQsuccessplot, file=file.path(path,"SymptomFinding3_SPQ_Success.pdf"))