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
                

################################################################################################       
#create modelling df
################################################################################################
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
                    FourClass=dplyr::first(c4class),
                    wideStartSD=dplyr::first(startSD)) %>%
                    filter(level %in% c(3, 4, 5, 6)) 
        df_mod<- df_mod %>%
                drop_na


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

    #contrasts
            #exlucding L6 - sensitivity analysis
            # df_mod$level<-droplevels(df_mod$level)
            #  cMat <- cbind(c(1,-1,0), # 3 vs 4
            #             c(0,1,-1)) # 4 vs 5
                #        contrasts(df_mod$level) <-cMat
                #        colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5")
            #inc l6
                df_mod$level<-droplevels(df_mod$level)
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                               c(0,-1,1,0), # 4 vs 5
                               c(0,0,-1,1)) #5vs 6
                contrasts(df_mod$level) <-cMat
                colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5", "5v6")

        #only run if you need constrasts for beam - currently, not using constrasts
                df_mod_confidence$level<-droplevels(df_mod_confidence$level)
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                        c(0,-1,1,0), # 4 vs 5
                        c(0,0,-1,1)) #5vs 6
                contrasts(df_mod_confidence$level) <-cMat
                colnames(attr(df_mod_confidence$level, "contrasts")) <- c("3v4", "4v5", "5v6")                        

#######################
        #score
        #######################
            #lowest AIC = MASQCombined_score_reduced

            mostsimple<-df_mod %>% 
                        lmer(score ~ 1 +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(mostsimple)

            nointeraction<-df_mod %>% 
                        lmer(score ~ device_type + gamers  + gender + 
                        green_total  +  
                        masqCombined_depression + masqCombined_anxiety + level + spq_total + pdi_total +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )

                        summary(nointeraction)
                        car::qqPlot(resid(nointeraction))
                        scatter.smooth(residuals(nointeraction) ~ fitted(nointeraction))

            nointeraction_oneint<-df_mod %>% 
                        lmer(score ~ device_type + gamers  + gender + 
                        green_total  +  
                        masqCombined_depression + masqCombined_anxiety*level + spq_total + pdi_total +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )

                        summary(nointeraction_oneint)
                        car::qqPlot(resid(nointeraction_oneint))
                        scatter.smooth(residuals(nointeraction_oneint) ~ fitted(nointeraction_oneint))
        


            MASQCombined_score_reduced<- 
                        df_mod %>%                                
                                lmer(score ~ device_type + gamers*level + gender  +
                                        green_total*level +  
                                        masqCombined_depression*level + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                                summary(MASQCombined_score_reduced)
                                car::qqPlot(resid(MASQCombined_score_reduced))
                                scatter.smooth(residuals(MASQCombined_score_reduced) ~ fitted(MASQCombined_score_reduced))
                                
                                
                                
                                #summary(glht(MASQCombined_score_reduced, linfct = mcp(level = "Tukey")), test = adjusted("holm"))
                                emm = emmeans(MASQCombined_score_reduced, ~ level * green_total)
                               # pairs(emm)
                                contrast(emm)

                                emm1 = emmeans(MASQCombined_score_reduced, ~ level * masqCombined_depression)
                                pairs(emm1)

                                emm = emmeans(MASQCombined_score_reduced, ~ level * gamers)
                                pairs(emm,simple = "each")

                               emm3<- emmeans(MASQCombined_score_reduced, "level")
                                ply <- contrast(pigs.emm.p, "poly")
                                ply

            
                                 r.squaredGLMM(MASQCombined_score_reduced)

                               

            MASQCombined_score_full<-  df_mod %>%  
                                        #ggplot(aes(x=score, y=masqCombined_depression, color=level)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(meancorrectedL6trialScore ~ device_type + gamers*level + gender + 
                                        green_total*level +  pdi_total*level + spq_total*level + 
                                        masqCombined_anxiety*level + masqCombined_depression*level + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(MASQCombined_score_full)
                                        car::qqPlot(resid(MASQCombined_score_full))
                                        scatter.smooth(residuals(MASQCombined_score_full) ~ fitted(MASQCombined_score_full))

            AICc(mostsimple, nointeraction, nointeraction_oneint, MASQCombined_score_full, MASQCombined_score_reduced)            

            anova(mostsimple, nointeraction, nointeraction_oneint, MASQCombined_score_full, MASQCombined_score_reduced, test="Chisq")

  #######################
        #Confidence
        #######################
            #winning AIC = MASQCombined_beam_reduced_IntwithSDcondit
                mostsimple_beam<-df_mod_confidence %>% 
                        lmer(meanconf ~ 1 +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(mostsimple_beam)

                nointeraction_beam<-df_mod_confidence %>% 
                        lmer(meanconf ~ device_type + gamers  + gender + SDhigh +
                        green_total  +  
                        masqCombined_depression + masqCombined_anxiety + level + spq_total + pdi_total +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )

                        summary(nointeraction_beam)
                        car::qqPlot(resid(nointeraction_beam))
                        scatter.smooth(residuals(nointeraction_beam) ~ fitted(nointeraction_beam))

                nointeraction_oneint_beam<-df_mod_confidence %>% 
                        lmer(meanconf ~ device_type + gamers  + gender + 
                        green_total  +  
                        masqCombined_depression + masqCombined_anxiety*level + spq_total + pdi_total +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )

                        summary(nointeraction_oneint_beam)
                        car::qqPlot(resid(nointeraction_oneint_beam))
                        scatter.smooth(residuals(nointeraction_oneint_beam) ~ fitted(nointeraction_oneint_beam))

            
                MASQCombined_beam_reduced<-  df_mod_confidence %>%  
                                        #ggplot(aes(x=score, y=masqCombined_depression, color=level)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(meanconf ~ device_type + gamers*level + gender + 
                                        spq_total*level +                                       
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(MASQCombined_beam_reduced)
                                        car::qqPlot(resid(MASQCombined_beam_reduced))
                                        scatter.smooth(residuals(MASQCombined_beam_reduced) ~ fitted(MASQCombined_beam_reduced))

                                #emm = emmeans(MASQCombined_beam_reduced, ~ level * spq_total)
                                #pairs(emm) #dont think its doing the contrasts right? 
                               # pairs(emm, simple="easy")#doesnt work 

                                #plot<-emmip(MASQCombined_beam_reduced, ~ level * spq_total)
                                #plot(emm, comparisons = TRUE)

    
                MASQCombined_beam_full<- 
                                df_mod_confidence %>%  
                                        #ggplot(aes(y=meanconf, x=spq_total, color=level)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(meanconf ~ device_type + gamers*level + gender + SDhigh +
                                        green_total*level +  pdi_total*level + spq_total*level + 
                                        masqCombined_anxiety*level  + masqCombined_depression*level +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(MASQCombined_beam_full)
                                        car::qqPlot(resid(MASQCombined_beam_full))
                                        scatter.smooth(residuals(MASQCombined_beam_full) ~ fitted(MASQCombined_beam_full))

                MASQCombined_beam_IntwithSDcondit<- 
                                df_mod_confidence %>%  
                                        #ggplot(aes(y=meanconf, x=spq_total, color=level)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(meanconf ~ device_type + gamers*level + gender  +
                                        green_total*SDhigh +  pdi_total*SDhigh + spq_total*SDhigh + 
                                        masqCombined_anxiety*SDhigh  + masqCombined_depression*SDhigh +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(MASQCombined_beam_IntwithSDcondit)
                                        car::qqPlot(resid(MASQCombined_beam_IntwithSDcondit))
                                        scatter.smooth(residuals(MASQCombined_beam_IntwithSDcondit) ~ fitted(MASQCombined_beam_IntwithSDcondit))

                MASQCombined_beam_reduced_IntwithSDcondit<- 
                                df_mod_confidence %>%  
                                        #ggplot(aes(y=meanconf, x=pdi_total, color=SDhighcondit)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(meanconf ~ gamers*level + gender  +
                                         masqCombined_depression*SDhigh + masqCombined_anxiety*SDhigh + green_total*SDhigh +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(MASQCombined_beam_reduced_IntwithSDcondit)
                                        car::qqPlot(resid(MASQCombined_beam_reduced_IntwithSDcondit))
                                        scatter.smooth(residuals(MASQCombined_beam_reduced_IntwithSDcondit) ~ fitted(MASQCombined_beam_reduced_IntwithSDcondit))

                MASQCombined_beam_reduced_IntwithSDcondit<- #WINNING MODEL
                                df_mod_confidence %>%  
                                        #ggplot(aes(y=meanconf, x=pdi_total, color=SDhighcondit)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(meanconf ~ gender  + SDhigh:level +
                                         masqCombined_depression*SDhigh + masqCombined_anxiety*SDhigh + green_total*SDhigh  +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(MASQCombined_beam_reduced_IntwithSDcondit)
                                        car::qqPlot(resid(MASQCombined_beam_reduced_IntwithSDcondit))
                                        scatter.smooth(residuals(MASQCombined_beam_reduced_IntwithSDcondit) ~ fitted(MASQCombined_beam_reduced_IntwithSDcondit))



                MASQCombined_beam_reduced_IntwithSDcondit_wbkcotrasts<- 
                                df_mod_confidence %>%  
                                        #ggplot(aes(y=meanconf, x=pdi_total, color=SDhighcondit)) +
                                        #geom_point() +
                                        #geom_smooth(method="lm")
                                        lmer(meanconf ~ gamers*level + gender +
                                         masqCombined_depression*SDhigh + masqCombined_anxiety*SDhigh + green_total*SDhigh  +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(MASQCombined_beam_reduced_IntwithSDcondit_wbkcotrasts)
                                        car::qqPlot(resid(MASQCombined_beam_reduced_IntwithSDcondit_wbkcotrasts))
                                        scatter.smooth(residuals(MASQCombined_beam_reduced_IntwithSDcondit_wbkcotrasts) ~ fitted(MASQCombined_beam_reduced_IntwithSDcondit_wbkcotrasts))
            AIC(
            mostsimple_beam,
            nointeraction_beam,
            nointeraction_oneint_beam,
            MASQCombined_beam_reduced,
            MASQCombined_beam_full, MASQCombined_beam_IntwithSDcondit, MASQCombined_beam_reduced_IntwithSDcondit)

                anova(    mostsimple_beam,
            nointeraction_beam,
            nointeraction_oneint_beam,
            MASQCombined_beam_reduced,
            MASQCombined_beam_full, MASQCombined_beam_IntwithSDcondit, MASQCombined_beam_reduced_IntwithSDcondit, test="Chisq")

        #######################
        #Success
        #######################
            #winning AIC = MASQCombined_success_reduced_2

            mostsimple_sumsuccess<-df_mod %>% 
                        lmer(sumsuccess ~ 1 +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(mostsimple_sumsuccess)

            nointeraction_sumsuccess<-df_mod %>% 
                        lmer(sumsuccess ~ device_type + gamers  + gender + 
                        green_total  +  
                        masqCombined_depression + masqCombined_anxiety + level + spq_total + pdi_total +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )

                        summary(nointeraction_sumsuccess)
                        car::qqPlot(resid(nointenointeraction_sumsuccessraction_beam))
                        scatter.smooth(residuals(nointeraction_sumsuccess) ~ fitted(nointeraction_sumsuccess))

            MASQCombined_success_reduced_1<- 
                    df_mod %>%  
                            ggplot(aes(y=sumsuccess, x=spq_total, color=level)) +
                            geom_point() +
                            geom_smooth(method="lm", se=F)
                            lmer(sumsuccess ~ device_type + gamers*level + 
                             spq_total*level + 
                            masqCombined_anxiety*level  + masqCombined_depression*level +
                            (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                            control=lme4::lmerControl(optimizer="bobyqa", 
                                                            optCtrl=list(maxfun=2e7)) )

                            summary(MASQCombined_success_reduced)
                            car::qqPlot(resid(MASQCombined_success_reduced))
                            scatter.smooth(residuals(MASQCombined_success_reduced) ~ fitted(MASQCombined_success_reduced))

            MASQCombined_success_reduced_2<-       
                    df_mod %>%  
                            #ggplot(aes(y=sumsuccess, x=spq_total, color=level)) +
                            #geom_point() +
                            #geom_smooth(method="lm")
                            lmer(sumsuccess ~ device_type + gamers*level + 
                             spq_total*level + 
                            masqCombined_anxiety  + masqCombined_depression +
                            (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                            control=lme4::lmerControl(optimizer="bobyqa", 
                                                            optCtrl=list(maxfun=2e7)) )

                            summary(MASQCombined_success_reduced_2)
                            car::qqPlot(resid(MASQCombined_success_reduced_2))
                            scatter.smooth(residuals(MASQCombined_success_reduced_2) ~ fitted(MASQCombined_success_reduced_2))

                                emm = emmeans(MASQCombined_success_reduced_2, ~ level * spq_total)
                                pairs(emm) #dont think its doing the contrasts right? 
                                pairs(emm, simple="easy")#doesnt work 

                                plot<-emmip(MASQCombined_beam_reduced, ~ level * spq_total)
                                plot(emm, comparisons = TRUE)



            MASQCombined_success_full<- 
                    df_mod %>%  
                            #ggplot(aes(y=sumsuccess, x=spq_total, color=level)) +
                            #geom_point() +
                            #geom_smooth(method="lm")
                            lmer(sumsuccess ~ device_type + gamers*level + gender + 
                            green_total*level +  pdi_total*level + spq_total*level + 
                            masqCombined_anxiety*level  + masqCombined_depression*level +
                            (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                            control=lme4::lmerControl(optimizer="bobyqa", 
                                                            optCtrl=list(maxfun=2e7)) )

                            summary(MASQCombined_success_full)
                            car::qqPlot(resid(MASQCombined_success_full))
                            scatter.smooth(residuals(MASQCombined_success_full) ~ fitted(MASQCombined_success_full))


                AIC(
                mostsimple_sumsuccess,
                nointeraction_sumsuccess,
                MASQCombined_success_reduced_1,
                MASQCombined_success_reduced_2,
                MASQCombined_success_full)



        #######################
        #PE
        #######################
            #winning AIC = 
            #PE resides too weird when using lme. Need gamma dist so using lme4. Some improvement but not loads. 
            #looking at outliers - possibly because of level 6?
        
                #taking the log improves the distribution (log of meanPE)

                genpop2 %>%
                         mutate(logPE=log(PE)) %>%
                         ggplot(aes(x=logPE)) +
                         geom_density()
                
                mostsimple_logmeanPE<-df_mod %>% 
                                mutate(logmeanPE=log(meanPE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPE ~ 1 +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                                summary(mostsimple_logmeanPE)

                nointeraction_meanPE<-df_mod %>% 
                                mutate(logmeanPE=log(meanPE)) %>%
                                lmer(logmeanPE ~ device_type + gamers  + gender + 
                                green_total  +  
                                masqCombined_depression + masqCombined_anxiety + level + spq_total + pdi_total  +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                                summary(nointeraction_meanPE)
                                car::qqPlot(resid(nointeraction_meanPE))
                                scatter.smooth(residuals(nointeraction_meanPE) ~ fitted(nointeraction_meanPE))

                nointeraction_meanPE_REDUCED<-df_mod %>% ###WINNING MODEL
                                mutate(logmeanPE=log(meanPE)) %>%
                                lmer(logmeanPE ~ device_type + gamers  + gender + 
                                green_total  +  
                                masqCombined_depression  + level + 
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                                summary(nointeraction_meanPE_REDUCED)
                                car::qqPlot(resid(nointeraction_meanPE_REDUCED))
                                scatter.smooth(residuals(nointeraction_meanPE_REDUCED) ~ fitted(nointeraction_meanPE_REDUCED))


                Int_forLevel_logmeanPE<- 
                        df_mod %>%
                                mutate(logmeanPE=log(meanPE)) %>%
                                #filter(level!="6") %>%  
                                # ggplot(aes(y=meanPEsignedScaled, x=spq_total, color=level)) +
                                # geom_point() +
                                # geom_smooth(method="lm", se=F)
                                lmer(logmeanPE ~ device_type + gamers*level + gender +
                                spq_total*level + pdi_total*level + green_total*level +
                                masqCombined_anxiety*level  + masqCombined_depression*level + wideStartSD +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                                summary(Int_forLevel_logmeanPE)
                                car::qqPlot(resid(Int_forLevel_logmeanPE))
                                scatter.smooth(residuals(Int_forLevel_logmeanPE) ~ fitted(Int_forLevel_logmeanPE))


                Int_excnonsigIntsforlevel_logmeanPE<-       
                        df_mod %>%  
                                mutate(logmeanPE=log(meanPE)) %>%
                                # ggplot(aes(y=meanPEsignedScaled, x=spq_total, color=level)) +
                                #geom_point() +
                                #geom_smooth(method="lm")
                                lmer(logmeanPE ~ device_type + gamers*level + gender +
                                spq_total*level + pdi_total*level + green_total +
                                masqCombined_anxiety  + masqCombined_depression   +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                                summary(Int_excnonsigIntsforlevel_logmeanPE)
                                car::qqPlot(resid(Int_excnonsigIntsforlevel_logmeanPE))
                                scatter.smooth(residuals(Int_excnonsigIntsforlevel_logmeanPE) ~ fitted(Int_excnonsigIntsforlevel_logmeanPE))

               Int_excnonsigIntsforlevel_logmeanPE_nogamers<-       
                        df_mod %>%  
                                mutate(logmeanPE=log(meanPE)) %>%
                                # ggplot(aes(y=meanPEsignedScaled, x=spq_total, color=level)) +
                                #geom_point() +
                                #geom_smooth(method="lm")
                                lmer(logmeanPE ~ device_type + gender +
                                spq_total*level + pdi_total*level + green_total +
                                 masqCombined_depression   +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                                summary(Int_excnonsigIntsforlevel_logmeanPE_nogamers)
                                car::qqPlot(resid(Int_excnonsigIntsforlevel_logmeanPE_nogamers))
                                scatter.smooth(residuals(Int_excnonsigIntsforlevel_logmeanPE_nogamers) ~ fitted(Int_excnonsigIntsforlevel_logmeanPE_nogamers))


                 AIC(
                                mostsimple_logmeanPE,
                                nointeraction_meanPE,
                                nointeraction_meanPE_REDUCED,
                                Int_forLevel_logmeanPE,
                                Int_excnonsigIntsforlevel_logmeanPE,
                                Int_excnonsigIntsforlevel_logmeanPE_nogamers)

                  anova(   mostsimple_logmeanPE,
                                nointeraction_meanPE,
                                nointeraction_meanPE_REDUCED,
                                Int_forLevel_logmeanPE,
                                Int_excnonsigIntsforlevel_logmeanPE,
                                Int_excnonsigIntsforlevel_logmeanPE_nogamers, test="Chisq")

     
        #######################
        #Performance Error
        #######################
                        c<-        df_mod %>%
                                        ggplot(aes(x=log(meanPerfE))) +
                                        geom_density() #+
                                        #facet_wrap(~level)

                        mostsimple_logmeanPerfE<-df_mod %>% 
                                        mutate(logmeanPerfE=log(meanPerfE)) %>%
                                        lmer(logmeanPerfE ~ 1 +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
                                                                        summary(mostsimple_logmeanPerfE)

                        nointeraction_logmeanPerfE<-df_mod %>% 
                                        mutate(logmeanPerfE=log(meanPerfE)) %>%
                                        lmer(logmeanPerfE ~ device_type + gamers  + gender +level +
                                        green_total  +  
                                        masqCombined_depression + masqCombined_anxiety + spq_total + pdi_total +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(nointeraction_logmeanPerfE)
                                        car::qqPlot(resid(nointeraction_logmeanPerfE))
                                        scatter.smooth(residuals(nointeraction_logmeanPerfE) ~ fitted(nointeraction_logmeanPerfE))

                        Int_forLevel_logmeanPerfE<- 
                                df_mod %>%
                                        mutate(logmeanPerfE=log(meanPerfE)) %>%
                                        #filter(level!="6") %>%  
                                        # ggplot(aes(y=meanPEsignedScaled, x=spq_total, color=level)) +
                                        # geom_point() +
                                        # geom_smooth(method="lm", se=F)
                                        lmer(logmeanPerfE ~ device_type + gamers*level + gender +
                                        spq_total*level + pdi_total*level + green_total*level +
                                        masqCombined_anxiety*level  + masqCombined_depression*level  + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(Int_forLevel_logmeanPerfE)
                                        car::qqPlot(resid(Int_forLevel_logmeanPerfE))
                                        scatter.smooth(residuals(Int_forLevel_logmeanPerfE) ~ fitted(Int_forLevel_logmeanPerfE))



                        Int_combinations_intLevel_logmeanPerfE_REDUCED<- 
                                df_mod %>%
                                        mutate(logmeanPerfE=log(meanPerfE)) %>%
                                        #filter(level!="6") %>%  
                                        # ggplot(aes(y=meanPEsignedScaled, x=spq_total, color=level)) +
                                        # geom_point() +
                                        # geom_smooth(method="lm", se=F)
                                        lmer(logmeanPerfE ~ device_type + gender + gamers +
                                        pdi_total*level  + spq_total*level +
                                        masqCombined_anxiety + masqCombined_depression + green_total +

                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(Int_combinations_intLevel_logmeanPerfE_REDUCED)
                                        car::qqPlot(resid(Int_combinations_intLevel_logmeanPerfE_REDUCED))
                                        scatter.smooth(residuals(Int_combinations_intLevel_logmeanPerfE_REDUCED) ~ fitted(Int_combinations_intLevel_logmeanPerfE_REDUCED))

                Int_combinations_intLevel_logmeanPerfE_REDUCED2<- ###WINNING MODEL
                                df_mod %>%
                                        mutate(logmeanPerfE=log(meanPerfE)) %>%
                                        #filter(level!="6") %>%  
                                        # ggplot(aes(y=meanPEsignedScaled, x=spq_total, color=level)) +
                                        # geom_point() +
                                        # geom_smooth(method="lm", se=F)
                                        lmer(logmeanPerfE ~ device_type + gender + gamers +
                                        pdi_total*level  +
                                        masqCombined_anxiety + masqCombined_depression + green_total +

                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(Int_combinations_intLevel_logmeanPerfE_REDUCED2)
                                        car::qqPlot(resid(Int_combinations_intLevel_logmeanPerfE_REDUCED2))
                                        scatter.smooth(residuals(Int_combinations_intLevel_logmeanPerfE_REDUCED2) ~ fitted(Int_combinations_intLevel_logmeanPerfE_REDUCED2))


                AIC(mostsimple_logmeanPerfE,
                        nointeraction_logmeanPerfE,
                        Int_forLevel_logmeanPerfE,
                        Int_combinations_intLevel_logmeanPerfE_REDUCED,
                        Int_combinations_intLevel_logmeanPerfE_REDUCED2)

                anova(mostsimple_logmeanPerfE,
                        nointeraction_logmeanPerfE,
                        Int_forLevel_logmeanPerfE,
                        Int_combinations_intLevel_logmeanPerfE_REDUCED,
                        Int_combinations_intLevel_logmeanPerfE_REDUCED2, test="Chisq")