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
               # mutate(BED = current_BED=="Binge-eating disorde

################################################################################################
#Model DF
################################################################################################


    GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }


            perform_avg <- ED %>%
                            filter(!level=="1") %>%
                            #filter(level %in% c(3, 4, 5, 6))  %>%
                            group_by(Participant.Private.ID, level, group_recruit) %>%
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
                                    current_med=dplyr::first(current_med),
                                    #group_recruit=dplyr::first(group_recruit),
                                    trialName=dplyr::first(trialName)
                                    )
                perform_avg <- perform_avg %>%
                                drop_na

                perform_avg$level<-droplevels(perform_avg$level)
                cMat <- cbind( c(-1,1,0,0,0),
                               c(0,-1,1,0,0), # 3 vs 4
                               c(0,0,-1,1,0), # 4 vs 5
                               c(0,0,0,-1,1)) #5vs 6
                contrasts(perform_avg$level) <-cMat
                colnames(attr(perform_avg$level, "contrasts")) <- c("2v3", "3v4", "4v5", "5v6")



################################################################################################
#Modelling 
################################################################################################

                conf_bl = lme(sumsuccess_n ~ 1, random = ~1|Participant.Private.ID, data=perform_avg_sub, method = "ML", 
                            na.action = na.exclude)
                summary(conf_bl)

                conf_ME = lme(sumsuccess_n ~ group_recruit + level + age + sex + current_med, random = ~1|Participant.Private.ID/level, 
                            data=perform_avg_sub, method = "ML", na.action = na.exclude)
                summary(conf_ME)
                car::qqPlot(resid(conf_ME)) #there is some lift - check for outliers

                conf_INT = lme(sumsuccess_n ~ group_recruit*level + age + sex + current_med, random = ~1|Participant.Private.ID/level, 
                            data=perform_avg_sub, method = "ML", na.action = na.exclude)
                summary(conf_INT)
                car::qqPlot(resid(conf_INT))

                anova(conf_bl, conf_ME, conf_INT)

    ###########################
    #LR
    ###########################


                LR_bl = lme(V1 ~ 1, random = ~1|Participant.Private.ID, data= try, method = "ML", 
                            na.action = na.exclude)
                summary(LR_bl)

                LR_ME = lme(V1 ~ group_recruit + level + sex + age + current_med, random = ~1|Participant.Private.ID/level, 
                            data= try, method = "ML", na.action = na.exclude)
                summary(LR_ME)
                car::qqPlot(resid(LR_ME)) #there is some lift - check for outliers
                hist(resid(LR_ME))

                LR_INT = lme(V1 ~ group_recruit*level + sex + age + current_med, random = ~1|Participant.Private.ID/level, 
                            data= try, method = "ML", na.action = na.exclude)
                summary(LR_INT)
                car::qqPlot(resid(LR_INT))

                anova(LR_bl, LR_ME, LR_INT)
                anova(LR_ME, LR_INT)

################################################################################################
#Plots
################################################################################################

        #Sumsuccess difference between groups 
            x<-ggplot(perform_avg_sub_sum, aes(y= sumsuccess_n, colour = group_recruit)) + 
                    #geom_hline(yintercept = 0, linetype = "dashed") +
                    #geom_flat_violin(aes(fill=group),position=position_nudge(x=.15,y=0),adjust=1.5,trim=FALSE,alpha=.5,colour=NA) +
                    geom_point(aes(x=group_recruit, y= sumsuccess_n, colour=group_recruit), position = position_jitterdodge(jitter.width = 0.2, dodge.width = .9), size=2, shape=20) +
                    geom_boxplot(aes(x=group_recruit, y= sumsuccess_n, fill=group_recruit), outlier.shape=NA, alpha=.5, width=.9, colour="black") +
                    ylab("Catch success") + 
                    xlab("Group")  + 
                    theme_classic() + 
                    theme(legend.position="none") + 
                    theme(axis.text = element_text(size = 10)) 

            sumSuccuessBoxplot<-x + scale_fill_manual(values=c("#7AC5CD", "#C1CDCD"))
            sumSuccuessBoxplot<- sumSuccuessBoxplot+ scale_colour_manual(values=c("#7AC5CD", "#C1CDCD"))

            ggsave(sumSuccuessBoxplot, file=file.path(path,"sumSuccuessBoxplot.pdf"))
