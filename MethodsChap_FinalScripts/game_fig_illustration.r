

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


    
    Comp1<-"#8951a5"
    Comp2<-"#58bf7a"

        path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Methods/Results/"

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
                
getBackgroundShading_noise<-function(direction){
                                    if(direction=="forward") {
                                                            backgroundshade<-data.frame( ymin = -Inf, 
                                                            ymax = Inf,
                                                            xmin = c(4,14,34,105, 115,  147, 178,199),
                                                            xmax = c(13,33,104,114, 146, 177, 198, Inf),
                                                            SD = c("Low SD","High SD", "Low SD", "High SD","Low SD", "High SD","Low SD", "High SD"),
                                                            fill=rep(c("Low noise", "High noise"),4))

                                    } else if(direction=="reverse") {
                                                            break
                                                            backgroundshade<-data.frame( ymin = -Inf, 
                                                            ymax = Inf,
                                                            xmin = c(4,14,34,105, 115,  147, 178,199),
                                                            xmax = c(13,33,104,114, 146, 177, 198, Inf),
                                                            SD = c("Low SD","High SD", "Low SD", "High SD","Low SD", "High SD","Low SD", "High SD"),
                                                            rep(c(Comp2, Comp1),4))
                                    } else {break}

                                    return(backgroundshade)
                                    }

                    gamefig<-genpop2 %>%
                                        filter(trialName=="spacetask012reversed") %>%
                                        mutate(SD=case_when(SDhigh==FALSE ~ "Low Noise", SDhigh==TRUE ~ "High Noise")) %>%      
                                        ggplot(aes(x=TrialNumber)) +
                                        geom_line(aes(y=Mean)) +
                                        geom_rect(data = getBackgroundShading_noise("forward"), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE, alpha = 0.15) +
                                       # geom_smooth(aes(colour=startSDHigh)) +
                                        #geom_bar(aes(fill=SDhigh), position="fill", stat="identity", width=1, alpha=0.1) +
                                        geom_vline(xintercept=c(14, 54, 94, 178), colour="black", linetype="dotted") +
                                        geom_point(aes(y=originalResult), colour="dark grey", size=0.5) +
                                        #geom_point(colour="red") +
                                         theme_classic() +
                                            theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank()) +
                                        labs(fill="Noise Condition")
                                                   gamefig<-gamefig + scale_fill_manual(values=c(Comp1, Comp2)) 

                            ggsave(gamefig, file=file.path(path,"gamefig_contingencies.pdf"))

                                       # facet_grid(~trialName)
