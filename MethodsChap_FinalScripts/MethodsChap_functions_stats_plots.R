########################################################################################################
#packages, path, colour scheme
########################################################################################################

    library(cumstats)
    library(zoo)
    library(ggpubr)
    library(tidyverse)
    library(lme4)
    library(lmerTest)
    library(nlme)
    library(multcomp)
    library(emmeans)
    library(corrr)

    options(scipen=999)
     library(sjPlot)
    library(patchwork)
    library(PerformanceAnalytics)

    path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Methods/Results/"


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

    Colcomb<-c("#c75f34","#648cd5")

########################################################################################################
#read in; make df
########################################################################################################
    #issue with the LRs (nassar i.e., created in datacleaning process - software updated and now seems to be making them in a matrix - needs fixing)
        spq<-readRDS("/Users/tonigibbs-dean/Documents/SpaceTaskProject/SPQ/Intermediate_Results/SPQTibble.RDS") %>% mutate(LR=as.vector(LR)) %>% 
                            filter(SPQsum != "n/a") %>% #removes 3
                            filter(level!="1") %>%
                            #filter(Participant.Device.Type %in% c("computer", "mobile")) %>% #removes 3
                            mutate(pid=as.factor(Participant.Private.ID)) %>%
                            mutate(spqH=SPQsum>=38)%>%
                            mutate(startSDHigh=trialName %in% c("spacetask012", "spacetask012reversed")) %>%
                            mutate(SDHigh=(SD==0.12)) %>%
                            mutate(level=as.factor(level)) %>%
                            mutate(meanchange=Mean != dplyr::lag(Mean)) %>%
                            mutate(PREmeanchange= lead(Mean != dplyr::lag(Mean))) %>%
                            mutate(POSTmeanchange= lag(Mean != dplyr::lag(Mean))) %>%
                            mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                            mutate(highVolatility=level %in% c(4:6)) %>%
                            mutate(correctedL6trialScore = case_when(   level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                                    level == 6 & trialScore>0  ~ trialScore/2,
                                                    TRUE ~ as.numeric(trialScore))) %>%
                            mutate(forward=trialName %in% c("spacetask003","spacetask012"), 
                                reverse=trialName %in% c("spacetask003reversed","spacetask012reversed")) %>%
                            mutate(correctedLR = case_when(LR <= 0 ~ 0, 
                                                        LR >= 1 ~ 1, 
                                                        TRUE ~ as.numeric(LR))) %>%
                            mutate(condition= case_when((SD == 0.03 | SD == 0.06) & (as.numeric(level == 5) | as.numeric(level == 6)) ~ "combined_highV_lowN",
                                                        (SD == 0.03 | SD == 0.06) & (as.numeric(level == 4)) ~ "isolated_highV_lowN",
                                                        (SD == 0.12) & (as.numeric(level == 4) | as.numeric(level == 5) | as.numeric(level == 6)) ~ "highV_highN", 
                                                        (SD == 0.03 | SD == 0.06) & (as.numeric(level == 2) | as.numeric(level == 3))  ~ "lowV_lowN",
                                                        (SD == 0.12) & (as.numeric(level == 2) | as.numeric(level == 3)) ~ "lowV_highN"
                                                        )) %>%
                            mutate(isolatedChange = as.factor(level %in% 2:4))   

        genpop1<-readRDS("/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop1/Intermediate_outputs/GenPop1Tibble_withdemos.RDS") %>% mutate(LR=as.vector(LR)) %>% 
                            filter(level!="1") %>%
                            #filter(Participant.Device.Type %in% c("computer", "mobile")) %>% #removes 3
                            mutate(pid=as.factor(Participant.Private.ID)) %>%
                            mutate(startSDHigh=trialName %in% c("spacetask012", "spacetask012reversed")) %>%
                            mutate(SDHigh=(SD==0.12)) %>%
                            mutate(level=as.factor(level)) %>%
                            mutate(meanchange=Mean != dplyr::lag(Mean)) %>%
                            mutate(PREmeanchange= lead(Mean != dplyr::lag(Mean))) %>%
                            mutate(POSTmeanchange= lag(Mean != dplyr::lag(Mean))) %>%
                            mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                            mutate(highVolatility=level %in% c(4:6)) %>%
                            mutate(correctedL6trialScore = case_when(   level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                                    level == 6 & trialScore>0  ~ trialScore/2,
                                                    TRUE ~ as.numeric(trialScore))) %>%
                            mutate(forward=trialName %in% c("spacetask003","spacetask012"), 
                                reverse=trialName %in% c("spacetask003reversed","spacetask012reversed")) %>%

                            mutate(correctedLR = case_when(LR <= 0 ~ 0, 
                                                        LR >= 1 ~ 1, TRUE ~ as.numeric(LR)))

########################################################################################################
#functions
########################################################################################################
getBackgroundShading_noise<-function(direction){
                                    if(direction=="forward") {
                                                            backgroundshade<-data.frame( ymin = -Inf, 
                                                            ymax = Inf,
                                                            xmin = c(4,14,34,105, 115,  147, 178,199),
                                                            xmax = c(13,33,104,114, 146, 177, 198, Inf),
                                                            SD = c("Low SD","High SD", "Low SD", "High SD","Low SD", "High SD","Low SD", "High SD"),
                                                            fill=rep(c(Comp2, Comp1),4))

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
                                    
Cumulative_MeanTrack<-function(dataset){

            cumulativemean_contingencyplot<-dataset %>%
                                                filter(forward==TRUE) %>%
                                                #filter(!level=="2") %>%
                                                group_by(TrialNumber) %>% 
                                                summarise(meanPP=mean(participantPosition), 
                                                          originalResult=mean(originalResult), 
                                                          Mean=dplyr::first(Mean)) %>%
                                                mutate(originalResult=(originalResult+0.5)*100) %>%
                                                mutate(Mean=(Mean+0.5)*100) %>%
                                                mutate(cumMean10 = zoo::rollapplyr(originalResult, width = 10, FUN = mean, partial = TRUE)) %>%
                                                mutate(cumMean15 = zoo::rollapplyr(originalResult, width = 15, FUN = mean, partial = TRUE)) %>%
                                                mutate(cumMean5 = zoo::rollapplyr(originalResult, width = 5, FUN = mean, partial = TRUE)) %>%
                                                mutate(cumMean3 = zoo::rollapplyr(originalResult, width = 3, FUN = mean, partial = TRUE)) %>%

                                    #longdf<-pivot_longer(cumulativemean_contingencyplot) %>% 
                                                ggplot(aes(x=TrialNumber)) +
                                                    #geom_line(aes( y= cumMean5), color="orange") + 
                                                    geom_line(aes( y= cumMean3), color=L2Col, alpha=0.4) +                                         
                                                    geom_line(aes(y=meanPP), color="grey") +  
                                                    geom_line(aes(y=Mean), linetype="dotted", color="black")+
                                                    #geom_line(data=plotdata_PPs, aes(x=TrialNumber, y=meanPP)) +  
                                                    #geom_smooth() + 
                                                    #geom_bar(aes(fill=highVolatility), position="fill", stat="identity", width=1, alpha=0.1) +
                                                    #geom_vline(xintercept=c(54, 94, 178), colour="grey") +
                                                    #geom_vline(xintercept=c(54, 64, 74, 82, 93, 94, 105, 115, 125, 135, 146, 167, 178, 188, 209), colour="grey") #+
                                                    #geom_vline(xintercept = meanchange==TRUE, colour="grey") #+
                                                         theme_classic() +    
                                                    theme(axis.ticks.x = element_blank(),
                                                          axis.ticks.y = element_blank(),
                                                          axis.title.x = element_blank())+
                                                          #legend.box.margin = margin(6, 6, 6, 6),
                                                          #legend.position='bottom')+
                                                    labs(#fill="No. of times\na mean has\nbeen presented",
                                                                    #color=legend_colors,
                                                                    y = "Rover Position")
                                                                   # x = "Trial Number")# +
                                                    #scale_color_manual(values=legend_colors) #+
                                                     #guides(color=guide_legend(nrow=2, byrow=TRUE)) 
                                               
                                                                                        
                   # ggplot() + 
                   # geom_line(data = mtcars_list[[1]], aes(x = wt, y = mpg, color = "4 Cyl")) +
                   # geom_line(data = mtcars_list[[2]], aes(x = wt, y = mpg, color = "6 Cyl")) + 
                  #  labs(color = "the legend") + 
                   # scale_color_manual(values = legend_colors) + 
                   # theme_bw()
                                                                                #LearningSDPlot<-LearningSDPlot + scale_fill_manual(values=c(Comp1, Comp6))
                        

                                return(cumulativemean_contingencyplot)
                    }


SDTrack<-function(dataset, direction){  

                if (direction == "forward") {
                    forward<-TRUE
                    }else if (direction == "reverse") {
                        forward<-FALSE
                        }else {break}

                            conf_noiseXwholegameXlevel<-dataset %>%
                                                    mutate(pid=as.factor(Participant.Private.ID)) %>%
                                                    filter(forward==forward ) %>% 
                                                    #filter(!level=="2") %>%
                                                    group_by(TrialNumber) %>%
                                                    summarise(meanBW=mean(110-participantConfidence),
                                                              originalResult=mean(originalResult),
                                                              SD=dplyr::first(SD),
                                                              SDHigh=dplyr::first(SDHigh)) %>%
                                                    mutate(cumnoise15 = zoo::rollapplyr(originalResult, width = 15, FUN = sd, partial = TRUE)) %>%
                                                    mutate(cumnoise5 = zoo::rollapplyr(originalResult, width = 10, FUN = sd, partial = TRUE)) %>%
                                                    #mutate(cumnoise20 = zoo::rollapplyr(originalResult, width = 20, FUN = sd, partial = TRUE)) %>%
                                                    ggplot(aes(x=TrialNumber, y=meanBW)) +
                                                        geom_line(aes( y= cumnoise15*100), color=L6Col, alpha=0.8) +
                                                        geom_line(aes( y= cumnoise5*100), color=L4Col, alpha=0.8) +
                                                        geom_line(aes(y=meanBW), colour="black", alpha=0.9) + 
                                                        geom_rect(data = getBackgroundShading_noise(direction), 
                                                                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
                                                                  inherit.aes = FALSE, 
                                                                  alpha = 0.2)+
                                                        scale_fill_manual(values = unique(getBackgroundShading_noise(direction)$fill), 
                                                            labels = unique(getBackgroundShading_noise(direction)$SD))+
                                                        geom_vline(xintercept=c(54, 94, 178), colour="grey") +
                                                    theme_classic() +    
                                                    theme(  axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            legend.position = "none") +
                                                            #axis.title.x=element_blank(),
                                                            #axis.text.x = element_blank(),
                                                            #axis.text.y = element_blank(),
                                                            #axis.title.y=element_blank()) +
                                                    labs(y="Beam Width",
                                                         x="Trial Number")
                                                         #fill= "Noise\nCondition" )


        return(conf_noiseXwholegameXlevel) 
        
        }


residplot_SDconditions_beamwidths<-function(dataset){
        

        modelshowingBW_SDcondi<-
                                dataset %>%  
                                        group_by(Participant.Private.ID, level, SDHigh) %>%
                                        summarise(meanBW=mean(110-participantConfidence),
                                                  wideSDstart=dplyr::first(startSDHigh),
                                                  trialName=dplyr::first(trialName)) %>%
                                        
                                        lmer(meanBW ~  SDHigh + wideSDstart + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
        BW_SDcondit_modresult<-summary(modelshowingBW_SDcondi)
        
        beammod_noSDcondit<- 
                                dataset %>%  
                                        group_by(Participant.Private.ID, level, SDHigh) %>%
                                        summarise(meanBW=mean(110-participantConfidence),
                                                  wideSDstart=dplyr::first(startSDHigh),
                                                  trialName=dplyr::first(trialName)) %>%
                                        lmer(meanBW ~  wideSDstart + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
        dfforplot<-dataset %>%  
                                        group_by(Participant.Private.ID, level, SDHigh) %>%
                                        summarise(meanBW=mean(110-participantConfidence),
                                                  wideSDstart=dplyr::first(startSDHigh),
                                                  trialName=dplyr::first(trialName)) 
        
        dfforplot$residual_beamxSDcondit<-resid(beammod_noSDcondit)

          LearningSDPlot<-   dfforplot %>%
                                   # group_by(SDHigh) %>%
                                    filter(!level %in% c(2,4)) %>%
                                    mutate(SD=case_when(SDHigh==FALSE ~ "Low Noise", SDHigh==TRUE ~ "High Noise")) %>%                
                                    ggplot(aes(y=residual_beamxSDcondit, x=SD,fill=as.factor(SD))) +
                                            geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                            theme_classic() +
                                            theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.position="none") +
                                                  #legend.box.background = element_rect(),
                                                  #legend.key = element_rect(fill = "white", colour = "black"),
                                                  #legend.box.margin = margin(6, 6, 6, 6),
                                                  #legend.title = element_text(size = 10, colour = "black"),
                                                  #legend.text = element_text(size = 8, colour = "black"),
                                                  #legend.justification = c("right", "center"))+
                                                  #legend.position = c(.95, .95)) +
                                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    #fill = "Noise Condition",
                                                    x= "Blocks",
                                                    y= "Space Task Beam Width\n(residual unexplained by covariates)")
                                    LearningSDPlot<-LearningSDPlot + scale_fill_manual(values=c(Comp1, Comp6))

            return(list("modoutput"=BW_SDcondit_modresult,"plot"=LearningSDPlot))
    }

meanCountPlot<-function(dataset) { #level,
 # make mean counting columns
        meantimeseen<-c()
        meancount<-c()
        perps<-unique(dataset$Participant.Private.ID)
        for (i in 1:length(perps)) {
                perpid<-perps[i]
                df_perp<-dataset[dataset$Participant.Private.ID==perpid,]
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

        dataset$meancount<-meancount
        dataset[["meanTimesSeen"]]<-as.factor(meantimeseen)

       

                #make plots

                PEplot<- dataset %>% # select(meancount, meanTimesSeen)
                        ggplot(aes(x=as.factor(meancount), y=PE)) +
                                    geom_boxplot(outlier.shape=NA, aes(fill=as.factor(meanTimesSeen)))  +
                                    geom_smooth() +
                                    theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.text.x = element_blank(),
                                        axis.title.x=element_blank(),
                                        legend.justification = c("right", "top"),
                                        legend.position = c(.99, .99),
                                        legend.box.background = element_rect(),
                                        legend.key = element_rect(fill = "white", colour = "black"),
                                        legend.box.margin = margin(6, 6, 6, 6),
                                        legend.title = element_text(size = 10, colour = "black"),
                                        legend.text = element_text(size = 8, colour = "black")) +
                                    labs(fill="No. Times\nMean Seen",
                                        # x = "No. times mean seen",
                                        y = "Average Prediction Error") 
                        PEFinalPLot<-PEplot+ scale_fill_manual(values=c(Comp3, Comp4, Comp2))
                            

                PerfEplot<- dataset %>% # select(meancount, meanTimesSeen)
                                    ggplot(aes(x=as.factor(meancount), y=PerfE)) +
                                    geom_boxplot(outlier.shape=NA, aes(fill=as.factor(meanTimesSeen)))  +
                                    geom_smooth() +
                                    theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        legend.justification = c("right", "top"),
                                        legend.position = "none") +
                                        #legend.box.background = element_rect(),
                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                        #legend.box.margin = margin(6, 6, 6, 6),
                                        #legend.title = element_text(size = 10, colour = "black"),
                                        #legend.text = element_text(size = 8, colour = "black")) +
                                    labs(#fill="No. Times Mean was Seen",
                                        x = "No. Trials From Mean Change",
                                        y = "Average Performance Error") 
                        PEFinalPLot<-PEplot+ scale_fill_manual(values=c(Comp3, Comp4, Comp2)


                meancountplots_PE_perfE<-egg::ggarrange(PEplot,PerfEplot)
                #meancountplots_LR_LRvio<-egg::ggarrange(LRcoefsplot,correctedLR_violin)

            #return(meancountplots_PE_perfE, meancountplots_LR_LRvio)
            return(meancountplots_PE_perfE)

            }

meancount_LR_violins<-function(dataset, LRtype) {

                    if (LRtype == "Nassar") {
                                        #do nothing
                    }else if (LRtype == "Corrected") {
                                        dataset$LR = dataset$correctedLR 
                    }else if (LRtype == "Excluded") {
                                        dataset <- dataset %>% filter(LR<1 & LR>0)  #%>%dplyr::select(LR) %>% summary
                    }else {break}

        
        meantimeseen<-c()
        meancount<-c()
        perps<-unique(dataset$Participant.Private.ID)
        for (i in 1:length(perps)) {
                perpid<-perps[i]
                df_perp<-dataset[dataset$Participant.Private.ID==perpid,]
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

        dataset$meancount<-meancount
        dataset[["meanTimesSeen"]]<-as.factor(meantimeseen)

        LRcoefsplot<- dataset %>% # select(meancount, meanTimesSeen)
                        filter(level==4) %>%
                        filter(meanTimesSeen==1) %>%
                        ggplot(aes(x=as.factor(meancount), y=LR)) +
                        geom_boxplot(outlier.shape=NA, aes(fill=as.factor(meanTimesSeen)))  +
                        geom_smooth() +
                        theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        #axis.text.x = element_blank(),
                                        axis.title.x = element_blank(),
                                        #legend.justification = c("right", "top"),
                                        legend.position = "none")+
                                        #legend.box.background = element_rect(),
                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                        #legend.box.margin = margin(6, 6, 6, 6),
                                        #legend.title = element_text(size = 10, colour = "black"),
                                        #legend.text = element_text(size = 8, colour = "black")) +
                                    labs(#fill="Mean label",
                                        # x = "No. times mean seen",
                                        y = paste0(LRtype, " LR")) 
                        LRcoefsplot<-LRcoefsplot+ scale_fill_manual(values=c(Comp2))


                correctedLR_violin<-dataset %>% # select(meancount, meanTimesSeen)
                                    filter(level==4) %>%
                                    filter(meanTimesSeen==1) %>%
                                    mutate(as.factor(meancount)) %>%
                                    ggplot(aes(factor(meancount), LR)) +
                                    geom_violin(aes(fill=as.factor(meanTimesSeen)))  +
                                    theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          #axis.text.x = element_blank(),
                                        #legend.justification = c("right", "top"),
                                        legend.position = "none") +
                                        #legend.box.background = element_rect(),
                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                        #legend.box.margin = margin(6, 6, 6, 6),
                                        #legend.title = element_text(size = 10, colour = "black"),
                                        #legend.text = element_text(size = 8, colour = "black")) +
                                    labs(#fill="Mean label",
                                        x = "No. trials after a mean change",
                                        y = paste0(LRtype, " LR")) 
                        correctedLR_violin<-correctedLR_violin+ scale_fill_manual(values=c(Comp4))

    return(list("meancountplot"=LRcoefsplot, "violinplot"=correctedLR_violin))
}

LR_meanchange<-function(dataset,LRtype) { #direction can be "forward" or "reverse"

                    if (LRtype == "Nassar") {
                                        #do nothing
                    }else if (LRtype == "Corrected") {
                                        dataset$LR = dataset$correctedLR 
                    }else if (LRtype == "Excluded") {
                                        dataset <- dataset %>% filter(LR<1 & LR>0)  #%>%dplyr::select(LR) %>% summary
                    }else {break}


        meanchange_LR_vol<-dataset %>%
                                            group_by(TrialNumber) %>%
                                            filter(forward==TRUE ) %>% 
                                            summarise(LRmeanTrialwise=mean(LR)) %>%
                                            #filter(c(LR<1 & LR>0)) %>%
                                            #filter(!level %in% c(2,3)) %>%
                                            #filter(!level=="2") %>%
                                            ggplot(aes(x=TrialNumber, y=(LRmeanTrialwise))) +    
                                            geom_line(color="orange") +  
                                            theme_classic() +                
                                            theme(axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank(),
                                                axis.title.x=element_blank())+
                                                #axis.text.x = element_blank(),
                                                #axis.text.y = element_blank(),
                                                #axis.title.y=element_blank()) +
                                                #geom_bar(aes(fill=highVolatility), position="fill", stat="identity", width=1, alpha=0.1) +
                                                geom_vline(xintercept=c(94, 178), colour="dark grey") +
                                                geom_vline(xintercept=c(54, 64, 74, 82, 93, 94, 105, 115, 125, 135, 146, 167, 178, 188, 209), colour="dark grey", linetype='dotted') +
                                                geom_rect(data = getBackgroundShading_noise(direction), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE, alpha = 0.15) +
                                                scale_fill_manual(values = unique(getBackgroundShading_noise(direction)$fill), 
                                                                  labels = unique(getBackgroundShading_noise(direction)$SD)) +
                                                labs( y = paste0(LRtype, " LR"),
                                                    fill="Noise Condition")


        return(meanchange_LR_vol)
}
                  
BernikerHyp1_2<-function(dataset, taskversion) {

                    if (taskversion == "1") {
                                        dataset$Hint = scale(dataset$Hint) 
                    }else if (taskversion == "2") {
                                        dataset$Hint = lag(dataset$Result)
                    }else {break}


    BernikerHyp1<- dataset %>%
                #mutate(Hint=taskversion) %>%
                group_by(SDHigh) %>%
                filter(!participantPosition<0) %>%
                filter(level==3) %>%
                ggplot(aes(x=scale(participantPosition), y=Hint, colour=SDHigh)) +
                geom_point(alpha=0.1)+
                geom_smooth(method="lm")+
                geom_line(aes(y=Mean), colour="blue", linetype="dotted", alpha=0.5) +
                #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                #                                label.x.npc = 0.5, label.y.npc = 0.9) +
                #geom_smooth(aes(y=scale(meanHintlagResult)), colour="blue", method="lm", linetype="dotted") +
                #geom_smooth(aes(y=scale(lagResult)), colour="purple", method="lm") +
                #geom_smooth(aes(y=scale(Mean), colour="pink", method="lm"))+
                #facet_wrap(~level) +
                theme_classic() +
                theme(axis.ticks.x = element_blank(),
                      axis.ticks.y = element_blank()) +
                labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    #fill = "Noise Condition",
                    x= "Participant Position",
                    y= "Input A (cue)")
    BernikerHyp1<-BernikerHyp1 + scale_color_manual(values=c(Comp6, Comp1))

        corr_lownoise_Dat<-   dataset %>%   group_by(SDHigh)%>%
                            filter(SDHigh==FALSE) %>%
                            filter(!participantPosition<0) %>%
                            filter(level==3) 
          corr_lownoise<-                  cor.test(corr_lownoise_Dat$participantPosition, corr_lownoise_Dat$Hint)

         corr_highnoise_Dat<-   dataset %>%   group_by(SDHigh)%>%
                            filter(SDHigh==TRUE) %>%
                            filter(level==3) 
         corr_highnoise<-                   cor.test(corr_highnoise_Dat$participantPosition, corr_highnoise_Dat$Hint)




  


    BHyp2mod<-
                                dataset %>%  
                                        group_by(Participant.Private.ID, level, SDHigh) %>%
                                        summarise(meanPerfE=mean(PerfE),
                                                  wideSDstart=dplyr::first(startSDHigh),
                                                  trialName=dplyr::first(trialName)) %>%
                                        
                                        lmer(meanPerfE ~  SDHigh +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
        Hyp2modelshowingPerfE_SDcondi<-summary(BHyp2mod)
        
        BernikerHyp2_SDxperfE<-   dataset %>%
                                   # group_by(SDHigh) %>%
                                    #filter(!level %in% c(2,4)) %>%
                                       group_by(Participant.Private.ID, SDHigh) %>%
                                       mutate(SD=case_when(SDHigh==FALSE ~ "Low Noise", SDHigh==TRUE ~ "High Noise")) %>%   
                                       summarise(meanPerfE=mean(PerfE), SD=dplyr::first(SD)) %>%     
                                    ggplot(aes(y=meanPerfE, x=SD,fill=as.factor(SD))) +
                                            geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                            theme_classic() +
                                            theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.position = "none") +
                                                  #legend.box.background = element_rect(),
                                                  #legend.box.margin = margin(6, 6, 6, 6),
                                                  #legend.key = element_rect(fill = "white", colour = "black"),
                                                  #legend.title = element_text(size = 10, colour = "black"),
                                                  #legend.text = element_text(size = 8, colour = "black"),
                                                  #legend.justification = c("right", "center"))+
                                                  #legend.position = c(.95, .95)) +
                                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    fill = "Noise Condition",
                                                    x= "Noise Condition",
                                                    y= "Average Performance Error")
        BernikerHyp2_SDxperfE<-BernikerHyp2_SDxperfE + scale_fill_manual(values=c(Comp1, Comp6))


        return(list("plot1"=BernikerHyp1, "modoutput"=Hyp2modelshowingPerfE_SDcondi,"plot2"=BernikerHyp2_SDxperfE, "corr_lownoise"=corr_lownoise, "corr_highnoise"=corr_highnoise))    
          
  }

PE_PerfE_MeanChange<-function(dataset) {

                                conf_volXwholegame<-dataset %>%
                                                            group_by(TrialNumber) %>%
                                                            filter(forward==TRUE ) %>% 
                                                            summarise(meanPerfE=mean(PerfE), meanPE=mean(PE)) %>%
                                                            #filter(!level=="2") %>%
                                                            ggplot(aes(x=TrialNumber, y=PE)) +    
                                                                geom_line(aes(x=TrialNumber, y=meanPerfE), color=Comp4) +  
                                                                geom_line(aes(x=TrialNumber, y=meanPE), color=Comp1) +  
                                                                ylim(0,80) +
                                                                #geom_smooth() + 
                                                                #geom_bar(aes(fill=highVolatility), position="fill", stat="identity", width=1, alpha=0.1) +
                                                                geom_vline(xintercept=c(14, 54, 94, 178), colour="dark grey") +
                                                                geom_vline(xintercept=c(54, 64, 74, 82, 93, 94, 105, 115, 125, 135, 146, 167, 178, 188, 209), colour="grey", linetype='dotted') +
                                                                theme_classic() +
                                                                theme(axis.ticks.x = element_blank(),
                                                                              axis.ticks.y = element_blank())+
                                                                              #axis.title.x=element_blank(),
                                                                              #axis.text.x = element_blank(),
                                                                              #axis.text.y = element_blank(),
                                                                              #axis.title.y=element_blank()) +
                                                                labs(x="Trial Number",
                                                                     y="Prediction Error")

            return(conf_volXwholegame)                                                
            
            }

########################################################################################################
#Data clearning/exclusion plots
########################################################################################################
########################################################################################################
#Analysis of Basic Metrics 
########################################################################################################

    #########################################
    #1. Cumulative mean and SD tracking plots 
    #########################################
        
        cummean_genpop1<-Cumulative_MeanTrack(genpop1)
        cummean_spq<-Cumulative_MeanTrack(spq)

        cumSD_beam_genpop1<-SDTrack(genpop1, "forward")
        cumSD_beam_spq<-SDTrack(spq, "forward")

    #########################################
    #2. Beam wider in high noise 
    #########################################
        #produces model results (meanconf ~ SDhigh) + plot using residuals from model
            model_and_plots_genpop1<-residplot_SDconditions_beamwidths(genpop1)
            model_and_plots_spq<-residplot_SDconditions_beamwidths(spq)


    #########################################
    #3. Hint and cue  issues 
    #########################################

            hint_cueXlevel<-  genpop1 %>%
                                    filter(!participantPosition<0) %>%
                                    mutate(lagResult=lag(Result)) %>%
                                    filter(forward==TRUE) %>%
                                    #filter(!level=="2") %>%
                                    rowwise() %>%
                                    mutate(meanHintlagResult=median(c(Hint, lagResult))) %>%
                                    ggplot(aes(x=scale(participantPosition), y=scale(Hint))) +
                                    geom_smooth(aes(y=scale(Hint)), colour=Comp5, method="lm") +
                                    #geom_smooth(aes(y=scale(meanHintlagResult)), colour=Comp6, method="lm", linetype="dotted") +
                                    geom_smooth(aes(y=scale(lagResult)), colour=Comp3, method="lm") +
                                    geom_smooth(aes(y=scale(Mean)), method="lm") +
                                    geom_abline(intercept = 0, slope = 1, color="black", linetype="dotted") +
                                    #geom_smooth(aes(y=scale(Mean)), colour="pink", method="lm") +
                                    #facet_grid(~level) +
                                    theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank()) +
                                    labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                                        #caption = "Figure made using residuals from mixed model",
                                                                        #fill = "Noise Condition",
                                        x= "Participant Position (scaled)",
                                        y= "Input A: cue (scaled)") +
                                    annotate("text", x = 1, y = 1.5, label = "italic(R) ^ 2 == 0.94",
                                              parse = TRUE) +
                                    annotate("text", x = 1, y = 0.35, label = "italic(R) ^ 2 == 0.75",
                                              parse = TRUE) +

                                              facet_wrap(~level)
                                    

            hint_cue<-  genpop1 %>%
                                    filter(!participantPosition<0) %>%
                                    mutate(lagResult=lag(Result)) %>%
                                    filter(forward=TRUE) %>%
                                    #filter(!level=="2") %>%
                                    dplyr::select(participantPosition, lagResult, Hint) 
            dat<-hint_cue[-1]
            corr_plot_hintResult<-chart.Correlation(dat, histogram = TRUE, method = "pearson")

            cor.test(hint_cue$lagResult, hint_cue$participantPosition)

        
        #attempting the ttrial wise plots corect pp for hint and then plotting result

            cl.data$hint_screen<- (cl.data$result+0.5)*100
            cl.data$participantPosition_hintcorrected<-cl.data$participantPosition-cl.data$hint_screen

            genpop1 %>%
                    group_by(TrialNumber) %>%
                    mutate(cue=(Hint+0.5)*100) %>%
                    mutate(result=(Result+0.5)*100) %>%
                    mutate(hintscreen=participantPosition-result) %>%
                    summarise(meanpp=mean(participantPosition), 
                              cue=dplyr::first(cue), 
                              result=dplyr::first(result),
                              hintscreen=mean(hintscreen)) %>%
                    ggplot(aes(x=TrialNumber, y=hintscreen)) +
                    geom_line(aes(y=cue)) +
                    geom_line(aes(y=hintscreen)) 


            genpop1 %>%
                    group_by(TrialNumber) %>%
                    mutate(cue=(Hint+0.5)*100) %>%
                    mutate(hint_screen=(Result+0.5)*100) %>%
                    mutate(participantPosition_hintcorrected=participantPosition-cue) %>%
                    summarise(meanpp=mean(participantPosition), 
                              cue=dplyr::first(cue), 
                              mean_participantPosition_hintcorrected = mean(participantPosition_hintcorrected)) %>%
                    ggplot(aes(x=TrialNumber, y=participantPosition_hintcorrected)) +
                    geom_line(aes(y=meanpp), color="blue") +
                    geom_line(aes(y=mean_participantPosition_hintcorrected), color="red") 

        #comparison between the cue-mean and rover-mean (ie., how much people are copying the cue when they should be updateing about
        #the mean)
        genpop1hint_result_plot<-  genpop1 %>%
                filter(!participantPosition<0) %>%
                group_by(Participant.Private.ID) %>% # dplyr::select(Hint, Mean, participantPosition) %>% summary
                    mutate(Hint= (Hint+0.5)*100,
                           Mean= (Mean+0.5)*100) %>% #dplyr::select(Hint, Mean, participantPosition) #%>% summary
                    mutate(cue_mean_genpop1= abs(Hint-Mean)) %>% #dplyr::select(Hint, Mean, participantPosition,cue_mean) 
                    mutate(rover_mean_genpop1=abs(participantPosition-Mean))# %>%
                   # ggplot(aes(x=cue_mean, y=rover_mean)) +
                   # geom_point() +
                   # geom_smooth(method=lm)  +
                   # geom_abline(intercept = 0, slope = 1, color="red")  +
                   # facet_wrap(~level)

        recreateoldplots_overall<- spq %>%
                            #filter(!level=2) %>%
                            group_by(Participant.Private.ID) %>%
                            mutate(Result= (Result+0.5)*100,
                                    Mean= (Mean+0.5)*100) %>% #dplyr::select(Result, Mean, participantPosition) %>% summary
                            mutate(cue_mean=abs(lag(Result)-lag(Mean))) %>%
                            mutate(rover_mean=abs(participantPosition-lag(Mean))) %>%
                            ggplot(aes(x=cue_mean, y=rover_mean)) +
                            geom_smooth(method="lm", se=TRUE, colour=Comp5) +
                            geom_smooth(data=genpop1hint_result_plot, aes(x=cue_mean_genpop1, y=rover_mean_genpop1), method="lm", colour=Comp4) +
                            geom_abline(intercept = 0, slope = 1, color="black", linetype="dotted") +
                            ylim(0,30) +
                            theme_classic() +
                            theme(axis.ticks.x = element_blank(),
                                  axis.ticks.y = element_blank()) +
                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                                        #caption = "Figure made using residuals from mixed model",
                                                                        #fill = "Noise Condition",
                                x= "Distance between the\ncue and the mean",
                                y= "Distance between the\nrover position and the mean") 
                                     #specifically not the performance error - Perfe would be without the lag
                                     #i.e..,not taking into account when means change - whereas this calc needs to
                 
        recreateoldplots_level<- spq %>%
                            #filter(!level=2) %>%
                            group_by(Participant.Private.ID) %>%
                            mutate(Result= (Result+0.5)*100,
                                    Mean= (Mean+0.5)*100) %>% #dplyr::select(Result, Mean, participantPosition) %>% summary
                            mutate(cue_mean=abs(lag(Result)-lag(Mean))) %>%
                            mutate(rover_mean=abs(participantPosition-lag(Mean))) %>%
                            ggplot(aes(x=cue_mean, y=rover_mean)) +
                            geom_smooth(method="lm", colour=Comp5) +
                            geom_smooth(data=genpop1hint_result_plot, aes(x=cue_mean_genpop1, y=rover_mean_genpop1), method="lm", colour=Comp4, se=T) +
                            geom_abline(intercept = 0, slope = 1, color="black", linetype="dotted") +
                            ylim(0,30) +
                                 theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank()) +
                                    labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                                        #caption = "Figure made using residuals from mixed model",
                                                                        #fill = "Noise Condition",
                                        x= "Distance between the\ncue and the mean",
                                        y= "Distance between the\nrover position and the mean") +
                            facet_wrap(~level)

    ########################################
    #4. Mean count plots (PE, PerfE, LR)
    ########################################
        #something not quite right with these plots - genpop2 mean times seen in 5 and only 3 in spq
        #also spq includes plots for LR 
            meancountspq<-spq %>%
                            #filter(c(LR<=1 & LR>=0)) %>%
                            filter(level==4) %>%
                            filter(forward==TRUE) #%>%
                            #ggplot(aes(x=TrialNumber, y=Mean)) +
                           # geom_line()


            meancountgenpop1<-genpop1 %>%
                            #mutate(LR=case_when(LR < 0 ~ 0, 
                            #                    LR > 1 ~ 1, TRUE ~ as.numeric(LR))) %>%
                            #filter(c(LR<1 & LR>0)) %>%
                            filter(level==4)  %>%
                            filter(forward==TRUE) 
                            
            meanCountPlot_spq<-meanCountPlot(meancountspq)
            
            meanCountPlot_genpop1<-meanCountPlot(meancountgenpop1)


    ##############################################
    #4. PE and PerfE trialwise plots - mean change
    ##############################################
       p3<-PE_PerfE_MeanChange(spq)
       p4<-PE_PerfE_MeanChange(genpop1)

       PE_perfE_withMidpoint<-genpop1 %>% 
                                        mutate(lagResult=lag(Result)) %>%
                                            #filter(reverse=TRUE) %>%
                                            rowwise() %>%
                                            mutate(meanHintlagResult=median(c(Hint, lagResult))) %>%
                                            mutate(PE=abs(scale(meanHintlagResult, center = c(-0.5), scale = c(0.01)) - participantPosition))
        p5<-PE_PerfE_MeanChange(PE_perfE_withMidpoint)

########################################################################################################
#Berniker validation
########################################################################################################

    #currently only includ hyp 1 and 2 because hyp 3 is more complicated and Im not sure anyone will miss it

    #function needs dataset and version of task - just so the script would work with the hint/result differences 
    #across the versions. Only relevent for Hyp 1. 

    #function prints [1] hyp1 - cue reliance under high noise; [2] model for hyp 2; [3] plots for hyp 3 = boxplot perfo under noise 
    #return(list("plot1"=BernikerHyp1, "modoutput"=Hyp2modelshowingPerfE_SDcondi,"plot2"=BernikerHyp2_SDxperfE, "corr_lownoise"=corr_lownoise, "corr_highnoise"=corr_highnoise))    
    output_genpop1<-BernikerHyp1_2(genpop1, "1")
    output_spq<-BernikerHyp1_2(spq, "2")


########################################################################################################
#LRs
########################################################################################################
        #Learning rate mean count plots and violns
            #function needs dataset (spq) and type of LR 
            #will prodce [1] mean count plot, and [2] violin plot 
                meancount_LR_violins_Nassar<- meancount_LR_violins(spq, "Nassar") #standard LRs 
                meancount_LR_violins_Corrected<- meancount_LR_violins(spq, "Corrected") #all above and below 1 and 0 are now 1 and 0
                meancount_LR_violins_Excluded<- meancount_LR_violins(spq, "Excluded") #excludes anythig outside of 1 and 0

        #learning rate trial wise at mean change points 
            #function needs dataset and LRtype 
              meanchange_Nassar<-  LR_meanchange(spq, "Nassar")
              meanchange_Corrected<-  LR_meanchange(spq, "Corrected")
              meanchange_Excluded<-  LR_meanchange(spq, "Excluded")

########################################
#Printing plots
########################################

    #1. Figure 1. Basic metrics 
        #genpop1
        grouped_plots_genpop1<-cummean_genpop1/cumSD_beam_genpop1 | model_and_plots_genpop1[[2]] 
        basic_mets_genpop1<-grouped_plots_genpop1 + plot_annotation(tag_levels = c("A", "B", "C"))
            ggsave(basic_mets_genpop1, file=file.path(path,"Fig1_groupedBasicMets_genpop1.pdf"))

        #spq
        grouped_plots_spq<-cummean_spq/cumSD_beam_spq | model_and_plots_spq[[2]] 
        basic_mets_spq<-grouped_plots_spq + plot_annotation(tag_levels = c("A", "B", "C"))
            ggsave(basic_mets_spq, file=file.path(path,"Fig1_groupedBasicMets_spq.pdf"))


    # Figure 2. Hint/result correlation
        #cant assign the corr plot for some reason 
        #genpop1 only
        hint_cue<-  genpop1 %>%
                            filter(!participantPosition<0) %>%
                            mutate(lagResult=lag(Result)) %>%
                            filter(forward=TRUE) %>%
                            filter(!level=="2") %>%
                            dplyr::select(participantPosition, lagResult, Hint) 
        dat<-hint_cue[-1]
        corr_plot_hintResult<-chart.Correlation(dat, histogram = TRUE, method = "pearson")
        
        #calls the level wise plot - not sure if going to use
        hint_cue_overall<-hint_cueXlevel
        ggsave(hint_cue_overall, file=file.path(path, "hint_cue_overall.pdf"))


        #comparison between SPQ and genpop2
        hint_result_comp<-recreateoldplots_overall/recreateoldplots_level
        hint_SPQvsGenpo1<- hint_result_comp + plot_annotation(tag_level = c("A", "B"))
        ggsave(hint_SPQvsGenpo1, file=file.path(path,"hint_SPQvsGenpo1.pdf"))

    #Figure 3. Mean count for PE and perfE
        #genpop1 - needs looking at agian
            meancount_genpop1<- meancountplot + plot_annotation(tag_levels = c("A", "B"))
        #spq plot need to update function to include LRs when SPQ
            meancountspq<- meancountspq + plot_annotation(tag_levels = c("A", "B"))
    
    
    #fig 4. meanchange and pe and perfe
        fig2<-p3/(p4|p5)
        PE_PerfE_MeanChange<-fig2 + plot_annotation(tag_levels = c("A", "B", "C"))
         ggsave(PE_PerfE_MeanChange, file=file.path(path,"PE_PerfE_MeanChange.pdf"))

    #Figure 5. Berniker validation plots 
        Hyp1_Bern_genpop1<- output_genpop1[[1]]|output_genpop1[[3]]
        Hyp1adn2_Bern_genpop1<-Hyp1_Bern_genpop1 + plot_annotation(tag_levels = c("A", "B"))
         ggsave(Hyp1adn2_Bern_genpop1, file=file.path(path,"Hyp1adn2_Bern_genpop1.pdf"))

        Hyp1_Bern_spq<- output_spq[[1]]|output_spq[[3]]
        Hyp1and2_Bern_spq<-Hyp1_Bern_spq + plot_annotation(tag_levels = c("A", "B"))
         ggsave(Hyp1and2_Bern_spq, file=file.path(path,"Hyp1and2_Bern_spq.pdf"))


        #super combined plots for SPQ findings 
        megacomb_spq<- (cummean_spq/model_and_plots_spq[[2]]/cumSD_beam_spq) | (recreateoldplots_overall/recreateoldplots_level)
         megacomb_spq<-megacomb_spq + plot_annotation(tag_levels = c("A", "B", "C", "D", "E"))
        basicmets_hintMean_combined<- ggsave(megacomb_spq, file=file.path(path,"basicmets_hintMean_combined.pdf"))

        #super combined plots for SPQ - PE at mean change points and PE count plots 
        PEperfE_meanchange_meancount<-p3/(p4|p5) | meanCountPlot_spq
        PEperfE_meanchange_meancount<-PEperfE_meanchange_meancount + plot_annotation(tag_levels = c("A", "B", "C", "D", "E"))
        PEperfE_meanchange_meancount<- ggsave(PEperfE_meanchange_meancount, file=file.path(path,"PEperfE_meanchange_meancount.pdf"))
        #plot better in the r print out

    #Fig 6 - learning rates
        #meancount_LR_violins_Nassar[[1]]/meancount_LR_violins_Nassar[[2]] 
        meancount_LR_violins_CorrectedandExcluded<-meancount_LR_violins_Corrected[[1]]/meancount_LR_violins_Corrected[[2]] | meancount_LR_violins_Excluded[[1]]/meancount_LR_violins_Excluded[[2]]
        meancount_LR_violins_CorrectedandExcluded_plots<-meancount_LR_violins_CorrectedandExcluded + plot_annotation(tag_levels = c("A", "B", "C", "D"))
        meancount_LR_violins_CorrectedandExcluded_plots<- ggsave(meancount_LR_violins_CorrectedandExcluded_plots, file=file.path(path,"meancount_LR_violins_CorrectedandExcluded_plots.pdf"))

        #mean change plots - all LRs
        meanchange_LRplots<-meanchange_Nassar/meanchange_Corrected/meanchange_Excluded
        meanchange_LRplots<-meanchange_LRplots + plot_annotation(tag_levels = c("A", "B", "C"))
        meanchange_LRplots<- ggsave(meanchange_LRplots, file=file.path(path,"meanchange_LRplots.pdf"))
