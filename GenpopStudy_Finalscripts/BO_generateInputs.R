
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
                
    Forward<-genpop2%>%
                    filter(trialName %in% c('spacetask003', 'spacetask012')) 

    Reverse<-genpop2 %>%
            filter(trialName %in% c('spacetask003reversed', 'spacetask012reversed')) 


###################
#INPUTS
###################

        ##################
        #junk positions 
        ##################
                    forwardinputs<- Forward %>%
                                        group_by(Participant.Private.ID) %>%
                                        filter(level %in% c(3:6)) %>%
                                        filter(Participant.Private.ID=="6633517") %>%
                                        mutate(scaledResult=scale(originalResult, center = c(-0.5), scale = c(0.01))) %>%
                                        dplyr::select(scaledResult) %>% 
                                        print(n=219) 
                                        #y$trialnumc<-1:219
                                        forwardinputs=forwardinputs[-1]
                                        write.table(forwardinputs,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/forwardinputs_genpop2.txt",sep="\t",row.names=FALSE, col.names=FALSE)

                    reversedinputs<- Reverse %>%
                                        group_by(Participant.Private.ID) %>%
                                        filter(level %in% c(3:6)) %>%
                                        filter(Participant.Private.ID=="6629313") %>%
                                        mutate(scaledResult=scale(originalResult, center = c(-0.5), scale = c(0.01))) %>%
                                        dplyr::select(scaledResult) #%>% 
                                        #print(n=219) 
                                        #y$trialnumc<-1:219
                                        reversedinputs=reversedinputs[-1]
                                        write.table(reversedinputs, "/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/reversedinputs_genpop2.txt", sep="\t",row.names=FALSE, col.names=FALSE)                    

###################
#RESPONSES
###################             

            #########################
            #trialscores
            ########################

                    trialscoreL3_6<- Forward %>%
                                                group_by(Participant.Private.ID) %>%
                                                filter(level %in% c(3:6)) %>%
                                                dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                pull(4) 

                    trialscoreL3_6_reverse<- Reverse %>%
                                                group_by(Participant.Private.ID) %>%
                                                filter(level %in% c(3:6)) %>%
                                                dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                pull(4) 
                    

                    trialscoreL3_6_mat <- matrix(data = trialscoreL3_6, ncol = 297)

                    trialscoreL3_6_reverse_mat <- matrix(data = trialscoreL3_6_reverse, ncol = 283)

                    write.table(trialscoreL3_6_reverse_mat,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/trialscoreL3_6_reverse.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                    write.table(trialscoreL3_6_mat,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/trialscoreL3_6.txt",sep="\t",row.names=FALSE, col.names=FALSE)


            #######################
            #Participant positions
            #######################

                        forwardresponse_level3<- Forward %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="3") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(2)
                                                    forwardresponse_level3 <- matrix(data = forwardresponse_level3, ncol = 297)
                                                    write.table(forwardresponse_level3,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/forwardresponse_level3.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        forwardresponse_level4<- Forward %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="4") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(2)
                                                    forwardresponse_level4 <- matrix(data = forwardresponse_level4, ncol = 297)
                                                    write.table(forwardresponse_level4,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/forwardresponse_level4.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        forwardresponse_level5<- Forward %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="5") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(2)
                                                    forwardresponse_level5 <- matrix(data = forwardresponse_level5, ncol = 297)
                                                    write.table(forwardresponse_level5,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/forwardresponse_level5.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        forwardresponse_level6<- Forward %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="6") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(2)
                                                    forwardresponse_level6 <- matrix(data = forwardresponse_level6, ncol = 297)
                                                    write.table(forwardresponse_level6, "/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/forwardresponse_level6.txt",sep="\t",row.names=FALSE, col.names=FALSE)


                        reverse_response_level3<- Reverse %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="3") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(2)
                                                    reverse_response_level3 <- matrix(data = reverse_response_level3, ncol = 283)
                                                    write.table(reverse_response_level3,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/reverse_response_level3.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        reverse_response_level4<- Reverse %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="4") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(2)
                                                    reverse_response_level4 <- matrix(data = reverse_response_level4, ncol = 283)
                                                    write.table(reverse_response_level4,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/reverse_response_level4.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        reverse_response_level5<- Reverse %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="5") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(2)
                                                    reverse_response_level5 <- matrix(data = reverse_response_level5, ncol = 283)
                                                    write.table(reverse_response_level5,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/reverse_response_level5.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        reverse_response_level6<- Reverse %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="6") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(2)
                                                    reverse_response_level6 <- matrix(data = reverse_response_level6, ncol = 283)
                                                    write.table(reverse_response_level6, "/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/reverse_response_level6.txt",sep="\t",row.names=FALSE, col.names=FALSE)
            #######################
            #beamwidth
            #######################
                        forwardresponseBEAM_level3<- Forward %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="3") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(3)
                                                    forwardresponseBEAM_level3 <- matrix(data = forwardresponseBEAM_level3, ncol = 297)
                                                    write.table(forwardresponseBEAM_level3, "/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/forwardresponseBEAM_level3.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        forwardresponseBEAM_level4<- Forward %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="4") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(3)
                                                    forwardresponseBEAM_level4 <- matrix(data = forwardresponseBEAM_level4, ncol = 297)
                                                    write.table(forwardresponseBEAM_level4,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/forwardresponseBEAM_level4.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        forwardresponseBEAM_level5<- Forward %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="5") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(3)
                                                    forwardresponseBEAM_level5 <- matrix(data = forwardresponseBEAM_level5, ncol = 297)
                                                    write.table(forwardresponseBEAM_level5,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/forwardresponseBEAM_level5.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        forwardresponseBEAM_level6<- Forward %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="6") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(3)
                                                    forwardresponseBEAM_level6 <- matrix(data = forwardresponseBEAM_level6, ncol = 297)
                                                    write.table(forwardresponseBEAM_level6,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/forwardresponseBEAM_level6.txt",sep="\t",row.names=FALSE, col.names=FALSE)



                        reverse_responseBEAM_level3<- Reverse %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="3") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(3)
                                                    reverse_responseBEAM_level3_csv <- matrix(data = reverse_responseBEAM_level3, ncol = 283)
                                                    write.table(reverse_responseBEAM_level3_csv,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/reverse_responseBEAM_level3.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        reverse_responseBEAM_level4<- Reverse %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="4") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(3)
                                                    reverse_responseBEAM_level4_csv <- matrix(data = reverse_responseBEAM_level4, ncol = 283)
                                                    write.table(reverse_responseBEAM_level4_csv,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/reverse_responseBEAM_level4.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        reverse_responseBEAM_level5<- Reverse %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="5") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(3)
                                                    reverse_responseBEAM_level5_csv <- matrix(data = reverse_responseBEAM_level5, ncol = 283)
                                                    write.table(reverse_responseBEAM_level5_csv,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/reverse_responseBEAM_level5.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                        reverse_responseBEAM_level6<- Reverse %>%
                                                    group_by(Participant.Private.ID) %>%
                                                    filter(level=="6") %>%
                                                    dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                    pull(3)
                                                    reverse_responseBEAM_level6_csv <- matrix(data = reverse_responseBEAM_level6, ncol = 283)
                                                    write.table(reverse_responseBEAM_level6_csv,"/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/INPUT_RESPONSES_GENPOP2/reverse_responseBEAM_level6.txt",sep="\t",row.names=FALSE, col.names=FALSE)
