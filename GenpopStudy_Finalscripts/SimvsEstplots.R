################################################################################################################################################
# 1. Packages and path to project directory ######
################################################################################################################################################
        
    #set seed and load pacakges 
        set.seed(0.1)
    
        library(tidyverse)
        library(ggplot2)
        #library(ggplotify)
        library(nlme)
        library(lme4)
        library(lmerTest)
        library(sjPlot)
        library(patchwork)

        options(scipen=999)

        path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop2/Results"

            #read in main data frame  

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
                

                #data re mean estimates 
            L3_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_meanEsts_GENPOP.csv")
            L3_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_meanEsts_GENPOP.csv")
            
            L4_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_meanEsts_GENPOP.csv")
            L4_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_meanEsts_GENPOP.csv")
            
            L5_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_meanEsts_GENPOP.csv")
            L5_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_meanEsts_GENPOP.csv")
            
            L6_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_meanEsts_GENPOP.csv")
            L6_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_meanEsts_GENPOP.csv")


            L3_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3reverse_REAL_meanEsts_GENPOP.csv")
            L3_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3reverse_SIM_meanEsts_GENPOP.csv")
            
            L4_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4reverse_REAL_meanEsts_GENPOP.csv")
            L4_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4reverse_SIM_meanEsts_GENPOP.csv")
            
            L5_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5reverse_REAL_meanEsts_GENPOP.csv")
            L5_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5reverse_SIM_meanEsts_GENPOP.csv")
            
            L6_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6reverse_REAL_meanEsts_GENPOP.csv")
            L6_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6reverse_SIM_meanEsts_GENPOP.csv")


    #data re BW estimates 
            L3_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_BWests_GENPOP.csv")
            L3_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_BWest_GENPOPs.csv")
            
            L4_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_BWests_GENPOP.csv")
            L4_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_BWest_GENPOPs.csv")
            
            L5_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_BWests_GENPOP.csv")
            L5_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_BWest_GENPOPs.csv")
            
            L6_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_BWests_GENPOP.csv")
            L6_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_BWest_GENPOPs.csv")


            L3_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3reverse_REAL_BWests_GENPOP.csv")
            L3_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3reverse_SIM_BWest_GENPOPs.csv")
            
            L4_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4reverse_REAL_BWests_GENPOP.csv")
            L4_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4reverse_SIM_BWest_GENPOPs.csv")
            
            L5_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5reverse_REAL_BWests_GENPOP.csv")
            L5_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5reverse_SIM_BWest_GENPOPs.csv")
            
            L6_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6reverse_REAL_BWests_GENPOP.csv")
            L6_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6reverse_SIM_BWest_GENPOPs.csv")

################################################################################################################################################
#functions
################################################################################################################################################

            plotting.SIMvsEST.sd<-function(ParticipantRoverPos_df, SimRoverPos_df, REAL_EST, SIM_EST, levels=c(3,4,5,6), direction) { 

                        if (direction == "forward") {
                                forward<-TRUE
                                }else if (direction == "reverse") {
                                    forward<-FALSE
                                    }else {break}
                
                # get df of trial means and sds from underlying contingencies 
                    mean_sd_df<-genpop2 %>%
                                                    mutate(forward= trialName %in% c("spacetask003","spacetask012" )) %>%
                                                    dplyr::filter(level %in% levels) %>%
                                                    filter(forward) %>%
                                                    mutate(PID=as.factor(Participant.Private.ID)) %>%
                                                    filter(as.numeric(PID)==1) %>%
                                                    group_by(Mean) %>%
                                                    dplyr::select(Mean, SD) %>%
                                                    rowid_to_column("ID") %>% 
                                                    mutate(SDmax=(Mean+SD/2)) %>%
                                                    mutate(SDmin=(Mean-SD/2))

                # get participant rover positions from ParticipantRoverPos_df
                    mean_sd_prp_df<-ParticipantRoverPos_df %>%
                                            rowwise() %>%
                                            summarise(median_partEsts=median(c_across(1:length(REAL_EST)))) %>%
                                            rowid_to_column("ID") %>%
                                            left_join(., mean_sd_df, "ID") %>%
                                            mutate(medianParticipantPos=(median_partEsts/100)-0.5) %>%
                                            select(!median_partEsts)

                # get sim rover positions from roverPos_df
                    mean_sd_prp_srp_df<-SimRoverPos_df %>%
                                            rowwise() %>%
                                            summarise(median_partEsts=median(c_across(1:length(REAL_EST)))) %>%
                                            rowid_to_column("ID") %>%
                                            left_join(., mean_sd_prp_df, "ID") %>%
                                            mutate(medianSimulationPos=(median_partEsts/100)-0.5) %>%
                                            select(!median_partEsts)

                # get df of median perp bws and add to previous df
                    mean_sd_prp_srp_pbw_df<-REAL_EST %>%
                                            rowwise() %>%
                                            summarise(median_partEsts=median(c_across(1:length(REAL_EST)))) %>%
                                            rowid_to_column("ID") %>%
                                            left_join(., mean_sd_prp_srp_df, "ID")

                # get df of median sim bws and add to previous df
                    preplot_df<-SIM_EST %>%
                                rowwise() %>%
                                summarise(median_simEsts=median(c_across(1:length(SIM_EST)))) %>% 
                                rowid_to_column("ID") %>%
                                left_join(., mean_sd_prp_srp_pbw_df, "ID")

                # create max and min beam positions for participants and sims
                    plot_df<-preplot_df%>%
                                mutate(partBeamMax=medianParticipantPos+median_partEsts/2,
                                       partBeamMin=medianParticipantPos-median_partEsts/2,
                                       simBeamMax=medianSimulationPos+median_simEsts/2,
                                       simBeamMin=medianSimulationPos-median_simEsts/2)



                # make plot
                     plot<-ggplot(plot_df, aes(x=ID)) +
                                                        geom_ribbon(aes(ymin=partBeamMin, ymax=partBeamMax), fill="grey", alpha=0.5) +
                                                        geom_line(aes(y=medianParticipantPos), alpha=0.8) +
                                                        geom_ribbon(aes(ymin=simBeamMin, ymax=simBeamMax), fill="orange", alpha=0.2) +
                                                        geom_line(aes(y=medianSimulationPos), alpha=0.8, colour="orange") +
                                                        geom_line(aes(y=Mean), colour="blue", linetype=2, alpha=0.5) +
                                                        geom_line(aes(y=SDmax), colour="blue", linetype="dotted", alpha=0.5) +
                                                        geom_line( aes(y=SDmin), colour="blue", linetype="dotted", alpha=0.5) +
                                                        theme_classic() +
                                                        theme( axis.ticks.x = element_blank(),
                                                                axis.ticks.y = element_blank(),
                                                                axis.title.x = element_blank(),
                                                                axis.title.y = element_blank()) +
                                                        coord_cartesian(ylim=c(-0.6,0.6)) 
                                
                    return(plot)

            }


#########################################################################################################
# Print plots sim vs ests####################################################
########################################################################################################

        L3plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L3_realEsts, SimRoverPos_df=L3_simEsts, L3_realEsts_BW, SIM_EST=L3_simEsts_BW, 3, "forward")
        L4plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L4_realEsts, SimRoverPos_df=L4_simEsts, L4_realEsts_BW, L4_simEsts_BW, 4, "forward")
        L5plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L5_realEsts, SimRoverPos_df=L5_simEsts, L5_realEsts_BW, L5_simEsts_BW, 5, "forward")
        L6plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L6_realEsts, SimRoverPos_df=L6_simEsts, L6_realEsts_BW, L6_simEsts_BW, 6, "forward")

                    simvsests_plots<-L3plots_rev/L4plots_rev/L5plots_rev/L6plots_rev
                    simVSest_plot<-simvsests_plots + plot_annotation(tag_levels = c("A", "B", "C", "D"))

                    ggsave(simVSest_plot, file=file.path(path,"simVSest_plots_SDandMean.pdf"))
