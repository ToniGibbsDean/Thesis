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

        path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/SPQ/Results"

    #load colours
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


    #read in parameters - L5 MAY CHANGE and l6 as s=now doing the error message iterations thing
    #these parameters were created using the new SPQ_HGF-creatingInputs_Responses file so I could ensure the ordering 

        L3_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_3.csv") 
        L4_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_4.csv")
        L5_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_5.csv")
        L6_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_6.csv")

        L3_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_3.csv")
        L4_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_4.csv")
        L5_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_5.csv")
        L6_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_6.csv")


    #read in main data frame  

            spqall<-readRDS("/Users/tonigibbs-dean/Documents/SpaceTaskProject/SPQ/Intermediate_Results/SPQTibble.RDS" ) 
            
            spqForward<-spqall %>%
                        filter(trialName %in% c('spacetask003', 'spacetask012')) 

            spqReversed<-spqall %>%
                        filter(trialName %in% c('spacetask003reversed', 'spacetask012reversed')) 

    
    #data re mean estimates 
            L3_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_meanEsts.csv")
            L3_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_meanEsts.csv")
            
            L4_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_meanEsts.csv")
            L4_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_meanEsts.csv")
            
            L5_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_meanEsts.csv")
            L5_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_meanEsts.csv")
            
            L6_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_meanEsts.csv")
            L6_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_meanEsts.csv")


            L3_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_meanEsts_reversed.csv")
            L3_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_meanEsts_reversed.csv")
            
            L4_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_meanEsts_reversed.csv")
            L4_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_meanEsts_reversed.csv")
            
            L5_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_meanEsts_reversed.csv")
            L5_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_meanEsts_reversed.csv")
            
            L6_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_meanEsts_reversed.csv")
            L6_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_meanEsts_reversed.csv")


    #data re BW estimates 
            L3_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_BWests.csv")
            L3_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_BWests.csv")
            
            L4_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_BWests.csv")
            L4_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_BWests.csv")
            
            L5_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_BWests.csv")
            L5_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_BWests.csv")
            
            L6_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_BWests.csv")
            L6_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_BWests.csv")


            L3_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_BWests_reversed.csv")
            L3_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_BWests_reversed.csv")
            
            L4_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_BWests_reversed.csv")
            L4_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_BWests_reversed.csv")
            
            L5_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_BWests_reversed.csv")
            L5_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_BWests_reversed.csv")
            
            L6_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_BWests_reversed.csv")
            L6_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_BWests_reversed.csv")




################################################################################################################################################
#joining datasets, making dataframe
################################################################################################################################################

        #get list of IDs in the order they were fed into the HGF 
        #bind ID to params
            Participant.Private.ID<-spqForward %>%
                                                group_by(Participant.Private.ID) %>%
                                                filter(level %in% c(3:6))  %>%
                                                select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                pull(1) %>%
                                                unique

            ParamsandIDs<-cbind(L3_params_F, L4_params_F, L5_params_F, L6_params_F, Participant.Private.ID)

            #forwardIDstoExc<-unique(ParamsandIDs$Participant.Private.ID[14, 3, 8, 12, 49, 48])
            
            L3fIDexc<-unique(ParamsandIDs$Participant.Private.ID[14])
            L4fIDexc<-unique(ParamsandIDs$Participant.Private.ID[c(3,8,12, 49)])
            L5fIDexc<-unique(ParamsandIDs$Participant.Private.ID[48])


            Participant.Private.ID<-spqReversed %>%
                                                group_by(Participant.Private.ID) %>%
                                                filter(level %in% c(3:6))  %>%
                                                select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                pull(1) %>%
                                                unique
            ParamsandIDsReversed<-cbind(L3_params_R, L4_params_R, L5_params_R, L6_params_R, Participant.Private.ID)

            L4rIDexc<-unique(ParamsandIDs$Participant.Private.ID[73])
            L5rIDexc<-unique(ParamsandIDs$Participant.Private.ID[1])
            L6rIDexc<-unique(ParamsandIDs$Participant.Private.ID[c(11,39)])

            #L4r<-73
            #L5r<-1
            #L6r<-11, 39

        #left join to main data
            temp <- spqall %>%
                                        group_by(Participant.Private.ID) %>%
                                        left_join(x=spqall, y=ParamsandIDs, by="Participant.Private.ID")
                                    

            SPQwithParams <- temp %>%
                                        group_by(Participant.Private.ID) %>%
                                        left_join(x=temp, y=ParamsandIDsReversed, by="Participant.Private.ID") 
                                        
     
################################################################################################################################################
#creating modelling df
################################################################################################################################################

        #plots of parameters that arent fixed 
        
        plotting<- SPQwithParams %>%
                                group_by(Participant.Private.ID) %>%
                                #filter(level %in% c(3,4)) %>%
                                filter(SPQsum!="NA") %>%
                                #filter(Participant.Device.Type %in% c("computer", "mobile")) %>% #removes 3
                                #filter(level=="3") %>%
                                mutate(spqH=SPQsum>=30) %>%
                                mutate(startSDHigh=trialName %in% c("spacetask012", "spacetask012reversed")) %>%
                                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                                summarise(  
                                            trialName=dplyr::first(trialName),
                                            gender=dplyr::first(Gender),
                                            age=dplyr::first(Age),
                                            device_type=dplyr::first(Participant.Device.Type),
                                            education=dplyr::first(Education),
                                            spqH=dplyr::first(spqH),
                                            startSDHigh=dplyr::first(startSDHigh),                                  
                                            L3_mux_F=dplyr::first(L3_mux_F), 
                                            L3_mux_R=dplyr::first(L3_mux_R),
                                            L3_saa_0_F=dplyr::first(L3_saa_0_F), 
                                            L3_saa_0_R=dplyr::first(L3_saa_0_R), 
                                            L3_kax_F=dplyr::first(L3_kax_F),
                                            L3_kax_R=dplyr::first(L3_kax_R),
                                            L3_kaa_F=dplyr::first(L3_kaa_F), 
                                            L3_kaa_R=dplyr::first(L3_kaa_R), 
                                            L3_be1_F=dplyr::first(L3_be1_F),
                                            L3_be1_R=dplyr::first(L3_be1_R),
                                            L3_zem_F=dplyr::first(L3_zem_F), 
                                            L3_zem_R=dplyr::first(L3_zem_R), 
                                            L3_zes_F=dplyr::first(L3_zes_F), 
                                            L3_zes_R=dplyr::first(L3_zes_R), 
                                            L4_mux_F=dplyr::first(L4_mux_F), 
                                            L4_mux_R=dplyr::first(L4_mux_R), 
                                            L4_saa_0_F=dplyr::first(L4_saa_0_F),
                                            L4_saa_0_R=dplyr::first(L4_saa_0_R),
                                            L4_kax_F=dplyr::first(L4_kax_F),
                                            L4_kax_R=dplyr::first(L4_kax_R),
                                            L4_kaa_F=dplyr::first(L4_kaa_F), # fixed
                                            L4_kaa_R=dplyr::first(L4_kaa_R), # fixed
                                            L4_be1_F=dplyr::first(L4_be1_F),
                                            L4_be1_R=dplyr::first(L4_be1_R),
                                            L4_zem_F=dplyr::first(L4_zem_F), 
                                            L4_zem_R=dplyr::first(L4_zem_R), 
                                            L4_zes_F=dplyr::first(L4_zes_F), 
                                            L4_zes_R=dplyr::first(L4_zes_R),
                                            L5_mux_F=dplyr::first(L5_mux_F), 
                                            L5_mux_R=dplyr::first(L5_mux_R), 
                                            L5_saa_0_F=dplyr::first(L5_saa_0_F),
                                            L5_saa_0_R=dplyr::first(L5_saa_0_R),
                                            L5_kax_F=dplyr::first(L5_kax_F),
                                            L5_kax_R=dplyr::first(L5_kax_R),
                                            L5_kaa_F=dplyr::first(L5_kaa_F), 
                                            L5_kaa_R=dplyr::first(L5_kaa_R), 
                                            L5_be1_F=dplyr::first(L5_be1_F),
                                            L5_be1_R=dplyr::first(L5_be1_R),
                                            L5_zem_F=dplyr::first(L5_zem_F), 
                                            L5_zem_R=dplyr::first(L5_zem_R), 
                                            L5_zes_F=dplyr::first(L5_zes_F), 
                                            L5_zes_R=dplyr::first(L5_zes_R),
                                            L6_mux_F=dplyr::first(L6_mux_F), 
                                            L6_mux_R=dplyr::first(L6_mux_R), 
                                            L6_saa_0_F=dplyr::first(L6_saa_0_F),
                                            L6_saa_0_R=dplyr::first(L6_saa_0_R),
                                            L6_kax_F=dplyr::first(L6_kax_F),
                                            L6_kax_R=dplyr::first(L6_kax_R),
                                            L6_kaa_F=dplyr::first(L6_kaa_F), 
                                            L6_kaa_R=dplyr::first(L6_kaa_R), 
                                            L6_be1_F=dplyr::first(L6_be1_F),
                                            L6_be1_R=dplyr::first(L6_be1_R),
                                            L6_zem_F=dplyr::first(L6_zem_F), 
                                            L6_zem_R=dplyr::first(L6_zem_R), 
                                            L6_zes_F=dplyr::first(L6_zes_F), 
                                            L6_zes_R=dplyr::first(L6_zes_R)) %>%                                                                                   
                                unite("L3_mux", L3_mux_F:L3_mux_R, na.rm = TRUE, remove = TRUE) %>% #SET TO FLASE if you want to see the OG cols and vals
                                unite("L3_saa", L3_saa_0_F:L3_saa_0_R, na.rm = TRUE, remove = TRUE) %>%
                                unite("L3_kax", L3_kax_F:L3_kax_R, na.rm = TRUE, remove = TRUE) %>%
                                unite("L3_kaa", L3_kaa_F:L3_kaa_R, na.rm = TRUE, remove = TRUE) %>%          
                                unite("L3_be1", L3_be1_F:L3_be1_R, na.rm = TRUE, remove = TRUE) %>%            
                                unite("L3_zem", L3_zem_F:L3_zem_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L3_zes", L3_zes_F:L3_zes_R, na.rm = TRUE, remove = TRUE) %>% 
                                unite("L4_mux", L4_mux_F:L4_mux_R, na.rm = TRUE, remove = TRUE) %>%                  
                                unite("L4_saa_0", L4_saa_0_F:L4_saa_0_R, na.rm = TRUE, remove = TRUE) %>%     
                                unite("L4_kax", L4_kax_F:L4_kax_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L4_kaa", L4_kaa_F:L4_kaa_R, na.rm = TRUE, remove = TRUE) %>% 
                                unite("L4_be1", L4_be1_F:L4_be1_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L4_zem", L4_zem_F:L4_zem_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L4_zes", L4_zes_F:L4_zes_R, na.rm = TRUE, remove = TRUE) %>%
                                unite("L5_mux", L5_mux_F:L5_mux_R, na.rm = TRUE, remove = TRUE) %>%                  
                                unite("L5_saa_0", L5_saa_0_F:L5_saa_0_R, na.rm = TRUE, remove = TRUE) %>%     
                                unite("L5_kax", L5_kax_F:L5_kax_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L5_kaa", L5_kaa_F:L5_kaa_R, na.rm = TRUE, remove = TRUE) %>% 
                                unite("L5_be1", L5_be1_F:L5_be1_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L5_zem", L5_zem_F:L5_zem_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L5_zes", L5_zes_F:L5_zes_R, na.rm = TRUE, remove = TRUE) %>%
                                unite("L6_mux", L6_mux_F:L6_mux_R, na.rm = TRUE, remove = TRUE) %>%                  
                                unite("L6_saa_0", L6_saa_0_F:L6_saa_0_R, na.rm = TRUE, remove = TRUE) %>%     
                                unite("L6_kax", L6_kax_F:L6_kax_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L6_kaa", L6_kaa_F:L6_kaa_R, na.rm = TRUE, remove = TRUE) %>% 
                                unite("L6_be1", L6_be1_F:L6_be1_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L6_zem", L6_zem_F:L6_zem_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L6_zes", L6_zes_F:L6_zes_R, na.rm = TRUE, remove = TRUE) %>%
                                mutate( trialName=as.factor(trialName), 
                                        device_type=as.factor(device_type)) %>%
                                mutate_if(is_character, as.numeric)


################################################################################################################################################
#plotting parameteres 
################################################################################################################################################
                                                            
        ####plotting###
        #must always filter by level 

                a<- plotting %>%
                                    filter(Participant.Private.ID!=c("4104878")) %>%
                                    ggplot(aes(y=L3_mux, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
                b<- plotting %>%
                                    filter(Participant.Private.ID!=c("4104878")) %>%
                                    ggplot(aes(y=L3_kaa, fill=spqH)) +
                                    ylim(0.5,1) +
                                    geom_boxplot(alpha=0.5)
                c<- plotting %>%
                                    filter(Participant.Private.ID!=c("4104878")) %>%
                                    ggplot(aes(y=L3_be1, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
                d<- plotting %>%
                                    filter(Participant.Private.ID!=c("4104878")) %>%
                                    ggplot(aes(y=L3_zem, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)                
                e<- plotting %>%
                                    filter(Participant.Private.ID!=c("4104878")) %>%
                                    ggplot(aes(y=L3_zes, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
            egg::ggarrange(a,b,c,d, e)

                f<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                                    ggplot(aes(y=L4_mux, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)

                g<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                                    ggplot(aes(y=L4_saa_0, fill=spqH)) +
                                    geom_boxplot(alpha=0.5) 
                                    #ylim(0,0.25)
                h<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                                    ggplot(aes(y=L4_kax, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
                i<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                                    ggplot(aes(y=L4_be1, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
                j<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                                    ggplot(aes(y=L4_zem, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
                k<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                                    ggplot(aes(y=L4_zes, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
            egg::ggarrange(f,g,h,i,j,k)



                l<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                                    ggplot(aes(y=L5_mux, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)

                m<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                                    ggplot(aes(y=L5_kax, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)

                n<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                                    ggplot(aes(y=L5_kaa, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)

                o<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                                    ggplot(aes(y=L5_be1, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
                p<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                                    ggplot(aes(y=L5_zem, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
                q<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                                    ggplot(aes(y=L5_zes, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
            egg::ggarrange(l,m,n,o,p,q)


            

                r<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                                    ggplot(aes(y=L6_mux, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)

                s<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                                    ggplot(aes(y=L6_kax, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)

                t<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                                    ggplot(aes(y=L6_kaa, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)

                u<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                                    ggplot(aes(y=L6_be1, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
                v<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                                    ggplot(aes(y=L6_zem, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
                w<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                                    ggplot(aes(y=L6_zes, fill=spqH)) +
                                    geom_boxplot(alpha=0.5)
            egg::ggarrange(r,s,t,u,v,w)


################################################################################################################################################
#modellng L3
################################################################################################################################################


        L3aovdf<- plotting %>%
                                filter(!Participant.Private.ID %in% c("4104878")) %>%
                                mutate(spqH=as.factor(spqH)) 
                        
                                L3_mux_aov<-t.test(L3_mux ~ spqH, data=L3aovdf)
                            # L3_mux_aov
                                L3_kaa_aov<-t.test(L3_kaa ~ spqH, data=L3aovdf)
                                #L3_kaa_aov
                                L3_be1_aov<-t.test(L3_be1 ~ spqH, data=L3aovdf)
                            # L3_be1_aov
                                L3_zem_aov<-t.test(L3_zem ~ spqH, data=L3aovdf)
                            # L3_zem_aov
                                L3_zes_aov<-t.test(L3_zes ~ spqH, data=L3aovdf)
                            # L3_zes_aov

                            #  test<- rbind(L3_mux_aov,L3_kaa_aov)
                            tab_model(L3_mux_aov, L3_kaa_aov, L3_be1_aov, L3_zem_aov, L3_zes_aov)

                            mancova(data = L3aovdf, 
                                    deps = vars(L3_mux, L3_kaa, L3_be1, L3_zem, L3_zes),
                                    factors = spqH)
################################################################################################################################################
#modellng L4
################################################################################################################################################

        L4aovdf<- plotting %>%
                                filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                                mutate(spqH=as.factor(spqH)) 
                        
                                L4_mux_aov<-t.test(L4_mux ~ spqH, data=L4aovdf)
                                L4_mux_aov
                                L4_kax_aov<-t.test(L4_kax ~ spqH, data=L4aovdf)
                                L4_kax_aov
                                L4_be1_aov<-t.test(L4_be1 ~ spqH , data=L4aovdf) ##only one sig - adding in other variables 
                                L4_be1_aov
                                L4_zem_aov<-t.test(L4_zem ~ spqH, data=L4aovdf)
                                L4_zem_aov
                                L4_zes_aov<-t.test(L4_zes ~ spqH, data=L4aovdf)
                                L4_zes_aov
                                
        L4_be1_aov<-lm(L4_be1 ~ spqH + education + gender + age + device_type + startSDHigh + trialName, data=L4aovdf) ##only one sig - adding in other variables 
                                summary(L4_be1_aov)
            L4be1_reduced<-lm(L4_be1 ~ spqH , data=L4aovdf) ##best model 
                                summary(L4be1_reduced)

                MuMIn::AICc(L4_be1_aov,L4be1_reduced)


                    mancova(data = L4aovdf, 
                                    deps = vars(L4_mux, L4_kax, L4_be1, L4_zes, L4_zem),
                                    factors = spqH)

                #######################
                #plot
                #######################

                     L4_be1_aov<-lm(L4_be1 ~  education + gender + age + device_type + startSDHigh + trialName, data=L4aovdf) ##only one sig - adding in other variables 
                    x<- plotting %>%
                        filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234", )) %>%
                        mutate(spqH=as.factor(spqH)) 
                     x$be1L4_plot<-resid(L4_be1_aov)

                        be1L4_plots<-x %>%
                                mutate(spgroup=case_when(spqH==TRUE ~ "High Schizoptypy", spqH==FALSE ~ "Low Schizoptypy")) %>%
                                  ggplot(aes(y=be1L4_plot, x=spgroup, fill=spgroup)) +
                                   geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                            theme_classic() +
                                            ylim(-0.1,0.1) +
                                            theme(axis.ticks.x = element_blank(),
                                                  #axis.text.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  #legend.box.background = element_rect(),
                                                  legend.position = "none")+
                                                  #legend.key = element_rect(fill = "white", colour = "black"),
                                                  #legend.box.margin = margin(6, 6, 6, 6),
                                                  #legend.title = element_text(size = 10, colour = "black"),
                                                  #legend.text = element_text(size = 8, colour = "black"),
                                                  #legend.justification = c("right", "top"))+
                                                  #legend.position = c(.95, .95)) +
                                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    fill = "SPQ High Scorers",
                                                    x= "SPQ Group",
                                                    y= "Noise Belief (normalised)\n(residual unexplained by covariates)")
                                 
                                    SPQ_be1L4_boxplot<-be1L4_plots + scale_fill_manual(values=c(L3Col, L4Col))
                                      ggsave(SPQ_be1L4_boxplot, file=file.path(path,"SPQ_be1L4_boxplot.pdf"))

 
 L4_be1_aov<-aov(L4_be1 ~ spqH*startSDHigh + education + gender + age + device_type  + trialName, data=L4aovdf) ##only one sig - adding in other variables 
                         summary(L4_be1_aov)

 tab_model(L4_mux_aov, L4_kax_aov, L4_be1_aov, L4_zem_aov, L4_zes_aov)


##############################################################################################################
#level 5 and 6 - INDIVIDUAL
##############################################################################################################
                L5aovdf<- plotting %>%
                                        filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                                        mutate(spqH=as.factor(spqH)) 
                                
                                        L5_mux_aov<-t.test(L5_mux ~ spqH, data=L5aovdf)
                                        L5_mux_aov
                                        L5_kaa_aov<-t.test(L5_kaa ~ spqH, data=L5aovdf)
                                        L5_kaa_aov
                                        L5_kax_aov<-t.test(L5_kax ~ spqH, data=L5aovdf)
                                        L5_kax_aov
                                        L5_be1_aov<-t.test(L5_be1 ~ spqH, data=L5aovdf)
                                        L5_be1_aov
                                        L5_zem_aov<-t.test(L5_zem ~ spqH, data=L5aovdf)
                                        L5_zem_aov
                                        L5_zes_aov<-t.test(L5_zes ~ spqH, data=L5aovdf)
                                        L5_zes_aov

                tab_model(L5_mux_aov, L5_kaa_aov, L5_kax_aov, L5_be1_aov, L5_zem_aov, L5_zes_aov)

                L6aovdf<- plotting %>%
                                        filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                                        mutate(spqH=as.factor(spqH)) 
                                
                                        L6_mux_aov<-t.test(L6_mux ~ spqH, data=L6aovdf)
                                        L6_mux_aov
                                        L6_kaa_aov<-t.test(L6_kaa ~ spqH, data=L6aovdf)
                                        L6_kaa_aov
                                        L6_kax_aov<-t.test(L6_kax ~ spqH, data=L6aovdf)
                                        L6_kax_aov
                                        L6_be1_aov<-t.test(L6_be1 ~ spqH, data=L6aovdf)
                                        L6_be1_aov
                                        L6_zem_aov<-t.test(L6_zem ~ spqH, data=L6aovdf)
                                        L6_zem_aov
                                        L6_zes_aov<-t.test(L6_zes ~ spqH, data=L6aovdf)
                                        L6_zes_aov

                                        #L6_zes_aov<-aov(L6_zes ~ spqH + education, data=L6aovdf)
                                    # summary(L6_zes_aov)

                tab_model(L6_mux_aov, L6_kaa_aov, L6_kax_aov, L6_be1_aov, L6_zem_aov, L6_zes_aov)

##############################################################################################################
#level 5 and 6 - COMBINED 5 and 6
##############################################################################################################
            L5and6Mux<- plotting %>% 
                                    #select(c("L5_mux", "L6_mux")) %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656"))  %>% 
                                    select(c("L5_mux", "L6_mux", Participant.Private.ID, spqH, age, gender, education, device_type)) %>% # just 5 and 6 cols
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "mux"), names_pattern = c("(.)_(.)")) %>% # each param is in acolumn and level is acollumn 
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) 
                                    #ggplot(aes(y=value, x=level, fill=spqH)) +
                                # geom_boxplot()
                     Mux_int<-      lm(value ~ spqH*level + age + gender + education + device_type, data=L5and6Mux)
                     Mux_noint<-    lm(value ~ spqH + level + age + gender + education + device_type, data=L5and6Mux)
                     mux_simple<-   lm(value ~ 1,data=L5and6Mux)
                                    
                                    MuMIn::AICc(mux_simple, Mux_noint, Mux_int)
                                    MuMIn::model.avg(Mux_noint, Mux_int) %>% summary
                                    anova(mux_simple, L5and6Zes_int, L5and6Zes_noInt, test="Chisq")

            L5and6Kax<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656")) %>% 
                                    select(c("L5_kax", "L6_kax", Participant.Private.ID, spqH, age, gender, education, device_type)) %>% # just 5 and 6 cols
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "kax"), names_pattern = c("(.)_(.)")) %>% # each param is in acolumn and level is acollumn 
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) 
                                # ggplot(aes(y=value, x=level, fill=spqH)) +
                                # geom_boxplot()
                     Kax_int<-      lm(value ~ spqH*level + age + gender + education + device_type, data=L5and6Kax)
                     Kax_noint<-    lm(value ~ spqH + level + age + gender + education + device_type, data=L5and6Kax)
                     Kax_simple<-   lm(value ~ 1,data=L5and6Kax)

                                    MuMIn::AICc(Kax_simple, Kax_noint, Kax_int)
                                    MuMIn::model.avg(Kax_noint, Kax_int) %>% summary
                                    anova(Kax_simple, Kax_int, Kax_noint, test="Chisq")


            L5and6Kaa<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656")) %>% #needs updating
                                    select(c("L5_kaa", "L6_kaa", Participant.Private.ID, spqH, age, gender, education, device_type)) %>% # just 5 and 6 cols
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "kaa"), names_pattern = c("(.)_(.)")) %>% # each param is in acolumn and level is acollumn 
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) #%>%
                                    #ggplot(aes(y=value, x=level, fill=spqH)) +
                                    #geom_boxplot()
                     Kaa_int<-      lm(value ~ spqH*level + age + gender + education + device_type, data=L5and6Kaa)
                     Kaa_noint<-    lm(value ~ spqH + level + age + gender + education + device_type, data=L5and6Kaa)
                     Kaa_simple<-   lm(value ~ 1,data=L5and6Kaa)

                                    MuMIn::AICc(Kaa_simple, Kaa_noint, Kaa_int)
                                    MuMIn::model.avg(Kaa_noint, Kaa_int) %>% summary
                                    anova(Kaa_simple, Kaa_int, Kaa_noint, test="Chisq")


            L5and6Be1<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656")) %>% #needs updating
                                    select(c("L5_be1", "L6_be1", Participant.Private.ID, spqH, age, gender, education, device_type)) %>% # just 5 and 6 cols
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "be1"), names_pattern = c("(.)_(.)")) %>% # each param is in acolumn and level is acollumn 
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) #%>%
                                    #ggplot(aes(y=value, x=level, fill=spqH)) +
                                # geom_boxplot()
                     Be1_int<-      lm(value ~ spqH*level + age + gender + education + device_type, data=L5and6Be1)
                     Be1_noint<-    lm(value ~ spqH + level + age + gender + education + device_type, data=L5and6Be1)
                     Be1_simple<-   lm(value ~ 1, data=L5and6Be1)

                                    MuMIn::AICc(Be1_simple, Be1_noint, Be1_int)
                                    MuMIn::model.avg(Be1_noint, Be1_int) %>% summary
                                    anova(Be1_simple, Be1_int, Be1_noint, test="Chisq")
                                    
            L5and6Zes_int<- plotting %>% #0.060
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656")) %>% #needs updating
                                    select(c("L5_zes", "L6_zes", Participant.Private.ID, spqH, age, gender, education, device_type)) %>% # just 5 and 6 cols
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "zes"), names_pattern = c("(.)_(.)")) %>% # each param is in acolumn and level is acollumn 
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level))# %>%
                                    #ggplot(aes(y=value, x=level, fill=spqH)) +
                                #geom_boxplot()
                     Zes_int<-      lm(value ~ spqH*level + age + gender + education + device_type, data=L5and6Zes_int)
                     Zes_noint<-    lm(value ~ spqH + level + age + gender + education + device_type, data=L5and6Zes_int)
                     Zes_simple<-   lm(value ~ 1,data=L5and6Zes_int)

                                    MuMIn::AICc(Zes_simple, Zes_noint, Zes_int)
                                    MuMIn::model.avg(Zes_noint, Zes_int) %>% summary
                                    anova(Zes_simple, Zes_int, Zes_noint, test="Chisq")

            L5and6Zem<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656")) %>% #needs updating
                                    select(c("L5_zem", "L6_zem", Participant.Private.ID, spqH, age, gender, education, device_type)) %>% # just 5 and 6 cols
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "zem"), names_pattern = c("(.)_(.)")) %>% # each param is in acolumn and level is acollumn 
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) #%>%
                                    # ggplot(aes(y=value, x=level, fill=spqH)) +
                                # geom_boxplot()
                     Zem_int<-      lm(value ~ spqH*level + age + gender + education + device_type, data=L5and6Zem)
                     Zem_noint<-    lm(value ~ spqH + level + age + gender + education + device_type, data=L5and6Zem)
                     Zem_simple<-   lm(value ~ 1,data=L5and6Zem)

                                    MuMIn::AICc(Zem_simple, Zem_noint, Zem_int)
                                    MuMIn::model.avg(Zem_noint, Zem_int) %>% summary
                                    anova(Zem_simple, Zem_int, Zem_noint, test="Chisq")
                    
    
                    egg::ggarrange( L5and6Mux,
                                    L5and6Kax,
                                    L5and6Kaa,
                                    L5and6Be1,
                                    L5and6Zes,
                                    L5and6Zem)

                #######################
                #plot
                #######################

                    Mux_resids_level<-          lm(value ~ spqH + age + gender + education + device_type, data=L5and6Mux)
                                                L5and6Mux$Mux_resids_level<-resid(Mux_resids_level)
                                
                                p1 <- L5and6Mux %>%
                                                        ggplot(aes(y=Mux_resids_level, x=level, fill=spqH)) +
                                                        geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                                                    theme_classic() +
                                                                    theme(axis.ticks.x = element_blank(),
                                                                        axis.ticks.y = element_blank(),
                                                                        #axis.text.x = element_blank(),
                                                                        #legend.box.background = element_rect(),
                                                                        legend.position = "none") +
                                                                         ggpubr::stat_compare_means(test = 'wilcox.test', label = 'p.signif', show.legend = F)
                                                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                                                        #legend.box.margin = margin(6, 6, 6, 6),
                                                                        #legend.title = element_text(size = 10, colour = "black"),
                                                                        #legend.title = element_blank(),
                                                                        #legend.text = element_text(size = 8, colour = "black"),
                                                                        #legend.justification = c("right", "top"))+
                                                                        #legend.position = c(.95, .95)) +
                                                                    labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                                            #caption = "Figure made using residuals from mixed model",
                                                                          #  fill = "SPQ High",
                                                                           x= "Blocks",
                                                                            y= "Mux")
                                                        
                                        Mux_residsLevel_boxplot<-p1 + scale_fill_manual(values=c(L3Col, L4Col))
 
                    Kax_resids_level<-      lm(value ~ spqH + age + gender + education + device_type, data=L5and6Kax)
                                            L5and6Kax$Kax_resids_level<-resid(Kax_resids_level)
                                
                                p2 <- L5and6Kax %>%
                                                        ggplot(aes(y=Kax_resids_level, x=level, fill=spqH)) +
                                                        geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                                                    theme_classic() +
                                                                    theme(axis.ticks.x = element_blank(),
                                                                        axis.ticks.y = element_blank(),
                                                                        legend.box.background = element_rect(),
                                                                        legend.position = c(.99, .99),
                                                                        legend.key = element_rect(fill = "white", colour = "black"),
                                                                        legend.box.margin = margin(6, 6, 6, 6),
                                                                        legend.title = element_text(size = 10, colour = "black"),
                                                                        #legend.title = element_blank(),
                                                                        legend.text = element_text(size = 8, colour = "black"),
                                                                        legend.justification = c("right", "top"))+
                                                                        #legend.position = c(.95, .95)) +
                                                                    labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                                            #caption = "Figure made using residuals from mixed model",
                                                                            fill = "SPQ High",
                                                                            x= "Blocks",
                                                                            y= "Kax")
                                                        
                                       Kax_resids_level_boxplot<-p2 + scale_fill_manual(values=c(L3Col, L4Col))


                    Kaa_resids_levelt<-      lm(value ~ spqH + age + gender + education + device_type, data=L5and6Kaa)
                                            L5and6Kaa$Kaa_resids_levelt<-resid(Kaa_resids_levelt)
                                
                                p3 <- L5and6Kaa %>%
                                                        ggplot(aes(y=Kaa_resids_levelt, x=level, fill=spqH)) +
                                                        geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                                                    theme_classic() +
                                                                    coord_cartesian(ylim = c(-10,30)) +
                                                                    theme(axis.ticks.x = element_blank(),
                                                                        axis.ticks.y = element_blank(),
                                                                        #legend.box.background = element_rect(),
                                                                        legend.position = "none") +
                                                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                                                        #legend.box.margin = margin(6, 6, 6, 6),
                                                                        #legend.title = element_text(size = 10, colour = "black"),
                                                                        #legend.title = element_blank(),
                                                                        #legend.text = element_text(size = 8, colour = "black"),
                                                                        #legend.justification = c("right", "top"))+
                                                                        #legend.position = c(.95, .95)) +
                                                                    labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                                            #caption = "Figure made using residuals from mixed model",
                                                                          #  fill = "SPQ High",
                                                                            x= "Blocks",
                                                                            y= "Kaa")
                                                        
                                       Kaa_resids_levelt_boxplot<-p3 + scale_fill_manual(values=c(L3Col, L4Col))


                    Be1__resids_level<-      lm(value ~ spqH + age + gender + education + device_type, data=L5and6Be1)
                                             L5and6Be1$Be1__resids_level<-resid(Be1__resids_level)
                                
                                p4 <- L5and6Be1 %>%
                                                        ggplot(aes(y=Be1__resids_level, x=level, fill=spqH)) +
                                                        geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                                                    theme_classic() +
                                                                    theme(axis.ticks.x = element_blank(),
                                                                        axis.ticks.y = element_blank(),
                                                                        #legend.box.background = element_rect(),
                                                                        legend.position = "none") +
                                                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                                                        #legend.box.margin = margin(6, 6, 6, 6),
                                                                        #legend.title = element_text(size = 10, colour = "black"),
                                                                        #legend.title = element_blank(),
                                                                        #legend.text = element_text(size = 8, colour = "black"),
                                                                        #legend.justification = c("right", "top"))+
                                                                        #legend.position = c(.95, .95)) +
                                                                    labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                                            #caption = "Figure made using residuals from mixed model",
                                                                          #  fill = "SPQ High",
                                                                            x= "Blocks",
                                                                            y= "Be1")
                                                        
                                       Be1__resids_level_boxplot<-p4 + scale_fill_manual(values=c(L3Col, L4Col))

                    Zes_resids_level<-      lm(value ~ spqH + age + gender + education + device_type, data=L5and6Zes_int)
                                             L5and6Zes_int$Zes_resids_level<-resid(Zes_resids_level)
                                
                                p5 <- L5and6Zes_int %>%
                                                        ggplot(aes(y=Zes_resids_level, x=level, fill=spqH)) +
                                                        geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                                                    theme_classic() +
                                                                    theme(axis.ticks.x = element_blank(),
                                                                        axis.ticks.y = element_blank(),
                                                                        #legend.box.background = element_rect(),
                                                                        legend.position = "none") +
                                                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                                                        #legend.box.margin = margin(6, 6, 6, 6),
                                                                        #legend.title = element_text(size = 10, colour = "black"),
                                                                        #legend.title = element_blank(),
                                                                        #legend.text = element_text(size = 8, colour = "black"),
                                                                        #legend.justification = c("right", "top"))+
                                                                        #legend.position = c(.95, .95)) +
                                                                    labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                                            #caption = "Figure made using residuals from mixed model",
                                                                          #  fill = "SPQ High",
                                                                            x= "Blocks",
                                                                            y= "Zes")
                                                        
                                      Zes_resids_level_boxplot<-p5 + scale_fill_manual(values=c(L3Col, L4Col))


                    Zem_resids_level<-      lm(value ~ spqH + age + gender + education + device_type, data=L5and6Zem)
                                             L5and6Zem$Zem_resids_level<-resid(Zem_resids_level)
                                
                                p6 <- L5and6Zem %>%
                                                        ggplot(aes(y=Zem_resids_level, x=level, fill=spqH)) +
                                                        geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                                                    theme_classic() +
                                                                    theme(axis.ticks.x = element_blank(),
                                                                        axis.ticks.y = element_blank(),
                                                                        #legend.box.background = element_rect(),
                                                                        legend.position = "none") +
                                                                        ggpubr::stat_compare_means(test = 'wilcox.test', label = 'p.signif', show.legend = F)
                                                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                                                        #legend.box.margin = margin(6, 6, 6, 6),
                                                                        #legend.title = element_text(size = 10, colour = "black"),
                                                                        #legend.title = element_blank(),
                                                                        #legend.text = element_text(size = 8, colour = "black"),
                                                                        #legend.justification = c("right", "top"))+
                                                                        #legend.position = c(.95, .95)) +
                                                                    labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                                            #caption = "Figure made using residuals from mixed model",
                                                                          #  fill = "SPQ High",
                                                                            x= "Blocks",
                                                                            y= "Zem")
                                                        
                                      Zem_resids_level_boxplot<-p6 + scale_fill_manual(values=c(L3Col, L4Col))

                egg::ggarrange(  Mux_residsLevel_boxplot,
                                Kax_resids_level_boxplot,
                                Kaa_resids_levelt_boxplot,
                                Be1__resids_level_boxplot,
                                Zes_resids_level_boxplot,
                                Zem_resids_level_boxplot)



##############################################################################################################
#One model attempt
##############################################################################################################

            all<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
                                    #select(c(Participant.Private.ID, spqH)) %>% # just 5 and 6 cols
                                    mutate(increaseTRUE = L4_be1 > L3_be1) %>%
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>% # each param is in acolumn and level is acollumn 
                                    #filter(param=="zes") %>%
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) %>%
                                    mutate(param=as.factor(param)) %>%
                                    mutate(startSDHigh=as.numeric(startSDHigh)) %>%
                                    filter(param=="be1") %>%
                                    mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                    spqH==FALSE ~ "Low SPQ" )) %>%
                                    #filter(!device_type=="tablet") %>%
                                    filter(level %in% c(3,4))


                                    ##plot 
                                        ggplot(all, aes(y=value, x=level)) +
                                            #geom_point(aes(colour=increaseTRUE)) +
                                            geom_point(alpha=0.2)+
                                            #geom_point(col="red",  
                                            #                           size = 3,
                                            #                          shape = 24,
                                            #                         fill = "red") +
                                            theme_classic()+
                                                theme(axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank()) +
                                            #geom_line(aes(group=Participant.Private.ID, colour=increaseTRUE)) +
                                            geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
                                            #scale_color_manual(values=c("darkgrey", "pink")) +
                                            #facet_wrap(spqH~trialName) 
                                            facet_grid(~spqH)

                                            ggsave(all, file=file.path(path,"SPQ_be1_L3and4_nocolour.pdf"))

                #simple models
                     Be1_int_allLev<-      lm(value ~ spqH*level + age + gender + education + device_type + trialName, data=all)
                     Be1_noint_allLev<-    lm(value ~ spqH + level + age + gender + education + device_type + trialName, data=all)
                     Be1_simple_allLev<-   lm(value ~ 1, data=all)

                            MuMIn::AICc(Be1_int_allLev, Be1_noint_allLev, Be1_simple_allLev)
                            #MuMIn::model.avg(Be1_noint, Be1_int) %>% summary
                            anova(Be1_int_allLev, Be1_noint_allLev, Be1_simple_allLev, test="Chisq")

                #MANCOVA for l 3 and 4 combined

                  c<-      mancova(data = plotting,
                            deps = vars(L3_mux, L3_kaa, L3_be1, L3_zem, L3_zes, 
                                        L4_mux, L4_kax, L4_be1, L4_zes, L4_zem,
                                        L5_mux, L5_kaa, L5_kax, L5_be1, L5_zes, L5_zem,
                                        L6_mux, L6_kaa, L6_kax, L6_be1, L6_zes, L6_zem),
                            factors = spqH)

                        c<-      mancova(data = plotting,
                            deps = vars(L3_kaa, L3_be1, L3_zem,  
                                        L4_kax, L4_be1, L4_zes, 
                                        L5_kaa, L5_kax, L5_be1,
                                        L6_kaa, L6_kax, L6_be1 ),
                            factors = spqH)

                    mantest<-    manova(cbind(L3_mux, L3_kaa, L3_be1, L3_zem, L3_zes,
                                        L4_mux, L4_kax, L4_be1, L4_zes, L4_zem, L4_saa_0,
                                        L5_mux, L5_kaa, L5_kax, L5_be1, L5_zes, L5_zem, L5_saa_0,
                                        L6_mux, L6_kaa, L6_kax, L6_be1, L6_zes, L6_zem, L6_saa_0) ~ spqH, data=plotting)

                        #tablemean_sd
                            plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>% # each param is in acolumn and level is acollumn 
                                    #filter(param=="zes") %>%
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) %>%
                                    mutate(param=as.factor(param)) %>%
                                        filter(level==3) %>%
                                        #filter(param== "be1") %>%
                                        group_by(spqH, param) %>%
                                        summarise(mean(value), sd(value))

                    plotting %>%
                            ggplot()

                #mixed models - no constrasting

                     Be1_int_allLev<- lmer(value ~ spqH*level + age + gender + education + device_type + trialName +
                                            (1|Participant.Private.ID) + (1|trialName/level), data=all, REML=F,
                                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                        optCtrl=list(maxfun=2e7)) )


                #mixed models with constrasts 

                        temp <- plotting %>%
                                        pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>%
                                        mutate(level=as.factor(level))  # each param is in acolumn and level is acollumn 

                        cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                        c(0,-1,1,0), # 4 vs 5
                        c(0,0,-1,1)) #5vs 6
                        contrasts(temp$level) <-cMat
                        colnames(attr(temp$level, "contrasts")) <- c("3v4", "4v5", "5v6")                
                                            
                        mixedmod_allLevs<-  temp %>% 
                                                filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656")) %>% #needs updating
                                                            mutate(spqH=as.factor(spqH)) %>%
                                                            mutate(level=as.factor(level)) %>%
                                                            mutate(param=as.factor(param)) %>%
                                                            filter(param=="be1") #%>%
                                                            #filter(level %in% c(3,4)) %>%

                                                   mixedmod_allLevs<-     lmer(value ~ spqH*level + age + gender + education + device_type + 
                                                                (1|Participant.Private.ID) + (1|trialName/level), data=mixedmod_allLevs, REML=F,
                                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                        optCtrl=list(maxfun=2e7)) )

                                                        test<-lm(value ~ spqH*level, data=mixedmod_allLevs)
                                                        test<-lm(value ~ spqH*level + age + gender + education + device_type +trialName, data=mixedmod_allLevs)

                                                        summary(mixedmod_allLevs)
                                                        car::qqPlot(resid( reduced_full))
                                                        scatter.smooth(residuals(reduced_full) ~ fitted(reduced_full))


########################################
#plotting the simulations vs estimates from matlab
########################################

    ##############################
    #functions for creating plots 
    ##############################

            std.error <- function(x) 
                                    sd(x)/sqrt(length(x))


            plotting.SIMvsEST.mean<-function(REAL_EST, SIM_EST, levels=c(3,4,5,6), direction) { 

                        if (direction == "forward") {
                                forward<-TRUE
                                }else if (direction == "reverse") {
                                    forward<-FALSE
                                    }else {break}
                
                    level4_mean_sd_dat<-spqall %>%
                                                    mutate(forward= trialName %in% c("spacetask003","spacetask012" )) %>%
                                                    dplyr::filter(level %in% levels) %>%
                                                    filter(forward) %>%
                                                    mutate(PID=as.factor(Participant.Private.ID)) %>%
                                                    filter(as.numeric(PID)==1) %>%
                                                    group_by(Mean) %>%
                                                    dplyr::select(Mean, SD) %>%
                                                    rowid_to_column("ID") 

                    L4realest_forplot<-REAL_EST %>%
                                            rowwise() %>%
                                            summarise(median_partEsts=median(c_across(1:length(REAL_EST)))) %>%
                                            rowid_to_column("ID") %>%
                                            left_join(., level4_mean_sd_dat, "ID") %>%
                                            mutate(median_partEsts=(median_partEsts/100)-0.5) %>% 
                                            #mutate(median_partEsts=(median_partEsts)-0.5) %>% 
                                            mutate(SDmax=(Mean+SD/2)) %>%
                                            mutate(SDmin=(Mean-SD/2))
                        
                    L4_simVSest_plot<-SIM_EST %>%
                                rowwise() %>%
                                summarise(median_simEsts=median(c_across(1:length(SIM_EST))), 
                                        mean_simEsts=mean(c_across(1:length(SIM_EST))),
                                        #mean_real=mean_se(c(mean_real_1:mean_real_77))) #produces like df within df
                                        sd_simEsts=sd(c_across(1:length(SIM_EST))),
                                        #se= sd_real/sq.root(n())) %>%
                                        se_simEsts=std.error(c_across(1:length(SIM_EST)))) %>% 
                                mutate(median_simEsts=(median_simEsts/100)-0.5) %>%
                                #mutate(sd_simEsts=(sd_simEsts/100)-0.5) %>%
                                #mutate(se_simEsts=(se_simEsts/100)-0.5) %>%
                                mutate(minSD=median_simEsts-sd_simEsts/2) %>%
                                mutate(maxSD=median_simEsts+sd_simEsts/2) %>%
                                mutate(minSE=median_simEsts-se_simEsts/2) %>%
                                mutate(maxSE=median_simEsts+se_simEsts/2) %>%
                                rowid_to_column("ID") %>%
                                                    ggplot(aes(x=ID)) +
                                                        geom_ribbon(aes(ymin=minSE, ymax=maxSE), fill="grey", alpha=0.2) +
                                                        geom_line(aes(y=median_simEsts), alpha=0.8) +
                                                        geom_line(data=L4realest_forplot, aes(y=median_partEsts), colour="green") +
                                                        geom_line(data=L4realest_forplot, aes(y=Mean), colour="blue", linetype=2, alpha=0.5) +
                                                        geom_line(data=L4realest_forplot, aes(y=SDmax), colour="brown", linetype="dotted", alpha=0.5) +
                                                        geom_line(data=L4realest_forplot, aes(y=SDmin), colour="brown", linetype="dotted", alpha=0.5) +
                                                        theme_classic() +
                                                        theme( axis.ticks.x = element_blank(),
                                                                axis.ticks.y = element_blank(),
                                                                axis.title.x = element_blank(),
                                                                axis.title.y = element_blank()) #+
                                                        #coord_cartesian(ylim=c(-1.5,1.5))
                                
                    return(L4_simVSest_plot)

            }

           plotting.SIMvsEST.SD<-function(REAL_EST, SIM_EST, levels=c(3,4,5,6), direction) { 

                        if (direction == "forward") {
                                forward<-TRUE
                                }else if (direction == "reverse") {
                                    forward<-FALSE
                                    }else {break}
                
                    level4_mean_sd_dat<-spqall %>%
                                                    mutate(forward= trialName %in% c("spacetask003","spacetask012" )) %>%
                                                    dplyr::filter(level %in% levels) %>%
                                                    filter(forward) %>%
                                                    mutate(PID=as.factor(Participant.Private.ID)) %>%
                                                    filter(as.numeric(PID)==1) %>%
                                                    group_by(Mean) %>%
                                                    dplyr::select(Mean, SD) %>%
                                                    rowid_to_column("ID") 

                    L4realest_forplot<-REAL_EST %>%
                                            rowwise() %>%
                                            summarise(median_partEsts=median(c_across(1:length(REAL_EST)))) %>%
                                            rowid_to_column("ID") %>%
                                            left_join(., level4_mean_sd_dat, "ID") %>%
                                            #mutate(median_partEsts=(median_partEsts/100)-0.5) %>% 
                                            mutate(median_partEsts=(median_partEsts)-0.5) %>% 
                                            mutate()
                                            mutate(SDmax=(Mean+SD/2)) %>%
                                            mutate(SDmin=(Mean-SD/2))
                        
                    L4_simVSest_plot_Sd<-SIM_EST %>%
                                rowwise() %>%
                                summarise(median_simEsts=median(c_across(1:length(SIM_EST))), 
                                        mean_simEsts=mean(c_across(1:length(SIM_EST))),
                                        #mean_real=mean_se(c(mean_real_1:mean_real_77))) #produces like df within df
                                        sd_simEsts=sd(c_across(1:length(SIM_EST))),
                                        #se= sd_real/sq.root(n())) %>%
                                        se_simEsts=std.error(c_across(1:length(SIM_EST)))) %>% 
                                mutate(median_simEsts=(median_simEsts)-0.5) %>%
                                #mutate(sd_simEsts=(sd_simEsts/100)-0.5) %>%
                                #mutate(se_simEsts=(se_simEsts/100)-0.5) %>%
                                mutate(minSD=median_simEsts-sd_simEsts/2) %>%
                                mutate(maxSD=median_simEsts+sd_simEsts/2) %>%
                                mutate(minSE=median_simEsts-se_simEsts/2) %>%
                                mutate(maxSE=median_simEsts+se_simEsts/2) %>%
                                mutate()
                                rowid_to_column("ID") %>%
                                                    ggplot(aes(x=ID)) +
                                                        geom_ribbon(aes(ymin=minSE, ymax=maxSE), fill="grey", alpha=0.2) +
                                                        geom_line(aes(y=median_simEsts), alpha=0.8) +
                                                        geom_line(data=L4realest_forplot, aes(y=median_partEsts), colour="green") +
                                                        geom_line(data=L4realest_forplot, aes(y=Mean), colour="blue", linetype=2, alpha=0.5) +
                                                        geom_line(data=L4realest_forplot, aes(y=SDmax), colour="brown", linetype="dotted", alpha=0.5) +
                                                        geom_line(data=L4realest_forplot, aes(y=SDmin), colour="brown", linetype="dotted", alpha=0.5) +
                                                        theme_classic() +
                                                        theme( axis.ticks.x = element_blank(),
                                                                axis.ticks.y = element_blank(),
                                                                axis.title.x = element_blank(),
                                                                axis.title.y = element_blank()) #+
                                                        #coord_cartesian(ylim=c(-1.5,1.5))
                                
                    return(L4_simVSest_plot_Sd)

            }

            ##### simpler plots 

                            simVSests_plots_BW<-function(REAL_EST, SIM_EST) { ####NB - shortcut - diff n in summarise function (76 vs 77) - didnt both to write into function
                                                                                    #so needs changing depending on whether looking at rev of for


                                    L4realest_forplot<-REAL_EST %>%
                                                            rowwise() %>%
                                                            summarise(median_partEsts=median(c(bw_real_1:bw_real_76))) %>%
                                                            rowid_to_column("ID") 
                                        
                                    L4_simVSest_plot<-SIM_EST %>%
                                                rowwise() %>%
                                                summarise(median_simEsts=median(c(bw_sim_1:bw_sim_76)), 
                                                        mean_simEsts=mean(c(bw_sim_1:bw_sim_76)),
                                                        #mean_real=mean_se(c(mean_real_1:mean_real_77))) #produces like df within df
                                                        sd_simEsts=sd(c(bw_sim_1:bw_sim_76)),
                                                        #se= sd_real/sq.root(n())) %>%
                                                        se_simEsts=std.error(c(bw_sim_1:bw_sim_76))) %>%
                                                mutate(min=median_simEsts-sd_simEsts) %>%
                                                mutate(max=median_simEsts+sd_simEsts) %>%
                                                mutate(minSE=median_simEsts-se_simEsts) %>%
                                                mutate(maxSE=median_simEsts+se_simEsts) %>%
                                                rowid_to_column("ID") %>%
                                                                    ggplot(aes(x=ID)) +
                                                                        geom_ribbon(aes(ymin=minSE, ymax=maxSE), fill="grey", alpha=0.5) +
                                                                        geom_line(aes(y=median_simEsts)) +
                                                                        geom_line(data=L4realest_forplot, aes(y=median_partEsts), colour="purple", linetype="dotted") +
                                                                        #geom_smooth(data=L4realest_forplot, aes(y=median_partEsts), colour="purple", alpha=0.1) +
                                                                        #geom_line(data=level4_mean_sd_dat, aes(y=Mean*100))
                                                                        theme_classic() +
                                                                        theme( axis.ticks.x = element_blank(),
                                                                                axis.ticks.y = element_blank(),
                                                                                axis.title.x = element_blank(),
                                                                                axis.title.y = element_blank()) #+
                                                                    # ylim(0,150)
                                                
                                    return(L4_simVSest_plot)

                            }




 std.error <- function(x) 
                                    sd(x)/sqrt(length(x))


            plotting.SIMvsEST.mean<-function(REAL_EST, SIM_EST, levels=c(3,4,5,6), direction) { 

                        if (direction == "forward") {
                                forward<-TRUE
                                }else if (direction == "reverse") {
                                    forward<-FALSE
                                    }else {break}
                # get df of trial means and sds from underlying contingencies 
                    level4_mean_sd_dat<-spqall %>%
                                                    mutate(forward= trialName %in% c("spacetask003","spacetask012" )) %>%
                                                    dplyr::filter(level %in% levels) %>%
                                                    filter(forward) %>%
                                                    mutate(PID=as.factor(Participant.Private.ID)) %>%
                                                    filter(as.numeric(PID)==1) %>%
                                                    group_by(Mean) %>%
                                                    dplyr::select(Mean, SD) %>%
                                                    rowid_to_column("ID") 
                
                # get df of median perp estimates and add to previous df
                    L4realest_forplot<-REAL_EST %>%
                                            rowwise() %>%
                                            summarise(median_partEsts=median(c_across(1:length(REAL_EST)))) %>%
                                            rowid_to_column("ID") %>%
                                            left_join(., level4_mean_sd_dat, "ID") %>%
                                            mutate(median_partEsts_rover=(median_partEsts/100)-0.5) %>% 
                                            mutate(median_partEsts_beam=(median_partEsts)-0.5) %>%
                                            #mutate(median_partEsts=(median_partEsts)-0.5) %>% 
                                            mutate(SDmax=(Mean+SD/2)) %>%
                                            mutate(SDmin=(Mean-SD/2))
                
                # get df of median perp estimates and add to previous df
                    L4_simVSest_plot<-SIM_EST %>%
                                rowwise() %>%
                                summarise(median_simEsts=median(c_across(1:length(SIM_EST))), 
                                        mean_simEsts=mean(c_across(1:length(SIM_EST))),
                                        #mean_real=mean_se(c(mean_real_1:mean_real_77))) #produces like df within df
                                        sd_simEsts=sd(c_across(1:length(SIM_EST))),
                                        #se= sd_real/sq.root(n())) %>%
                                        se_simEsts=std.error(c_across(1:length(SIM_EST)))) %>% 
                                mutate(median_simEsts=(median_simEsts/100)-0.5) %>%
                                #mutate(sd_simEsts=(sd_simEsts/100)-0.5) %>%
                                #mutate(se_simEsts=(se_simEsts/100)-0.5) %>%
                                mutate(minSD=median_simEsts-sd_simEsts/2) %>%
                                mutate(maxSD=median_simEsts+sd_simEsts/2) %>%
                                mutate(minSE=median_simEsts-se_simEsts/2) %>%
                                mutate(maxSE=median_simEsts+se_simEsts/2) %>%
                                rowid_to_column("ID") %>%
                                                    ggplot(aes(x=ID)) +
                                                        geom_ribbon(aes(ymin=minSE, ymax=maxSE), fill="grey", alpha=0.2) +
                                                        geom_line(aes(y=median_simEsts), alpha=0.8) +
                                                        geom_line(data=L4realest_forplot, aes(y=median_partEsts), colour="green") +
                                                        geom_line(data=L4realest_forplot, aes(y=Mean), colour="blue", linetype=2, alpha=0.5) +
                                                        geom_line(data=L4realest_forplot, aes(y=SDmax), colour="brown", linetype="dotted", alpha=0.5) +
                                                        geom_line(data=L4realest_forplot, aes(y=SDmin), colour="brown", linetype="dotted", alpha=0.5) +
                                                        theme_classic() +
                                                        theme( axis.ticks.x = element_blank(),
                                                                axis.ticks.y = element_blank(),
                                                                axis.title.x = element_blank(),
                                                                axis.title.y = element_blank()) +
                                                        coord_cartesian(ylim=c(-0.5,0.5))
                                
                    return(L4_simVSest_plot)

            }





#####################################################
# Print plots sim vs ests
####################################################

    #mean plots 
            L3plots<-plotting.SIMvsEST.mean(L3_realEsts, L3_simEsts, 3, "forward")
            L4plots<-plotting.SIMvsEST.mean(L4_realEsts, L4_simEsts, 4, "forward")
            L5plots<-plotting.SIMvsEST.mean(L5_realEsts, L5_simEsts, 5, "forward")
            L6plots<-plotting.SIMvsEST.mean(L6_realEsts, L6_simEsts, 6, "forward")

            simvsests_plots_mean<-L3plots/L4plots/L5plots/L6plots
            simvsests_plots2<-(L3plots|L4plots)/(L5plots|L6plots)

            simVSest_FOR_plot<-simvsests_plots + plot_annotation(tag_levels = c("A", "B", "C", "D"))

            ggsave(simVSest_FOR_plot, file=file.path(path,"simVSest_FOR_plot.pdf"))


            L3plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L3_realEsts, SimRoverPos_df=L3_simEsts, L3_realEsts_BW, SIM_EST=L3_simEsts_BW, 3, "forward")
            L4plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L4_realEsts, SimRoverPos_df=L4_simEsts, L4_realEsts_BW, L4_simEsts_BW, 4, "forward")
            L5plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L5_realEsts, SimRoverPos_df=L5_simEsts, L5_realEsts_BW, L5_simEsts_BW, 5, "forward")
            L6plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L6_realEsts, SimRoverPos_df=L6_simEsts, L6_realEsts_BW, L6_simEsts_BW, 6, "forward")

            simvsests_plots_sd<-L3plots_rev/L4plots_rev/L5plots_rev/L6plots_rev
            simvsests_plots2<-(L3plots_rev|L4plots_rev)/(L5plots_rev/L6plots_rev)
            simVSest_REV_plot<-simvsests_plots + plot_annotation(tag_levels = c("A", "B", "C", "D"))

            ggsave(simVSest_REV_plot, file=file.path(path,"simVSest_REV_plot.pdf"))
simvsests_plots_mean|simvsests_plots_sd


    #BW plots 

            L3plots_BW<-simVSests_plots_mean(L3_realEsts_BW, L3_simEsts_BW, 3, "forward")
            L4plots_BW<-simVSests_plots_BW(L4_realEsts_BW, L4_simEsts_BW)
            L5plots_BW<-simVSests_plots_BW(L5_realEsts_BW, L5_simEsts_BW)
            L6plots_BW<-simVSests_plots_BW(L6_realEsts_BW, L6_simEsts_BW)

            simvsests_plots_BW<-L3plots_BW/L4plots_BW/L5plots_BW/L6plots_BW
            simVSest_FOR_plot_BW<-simvsests_plots_BW + plot_annotation(tag_levels = c("A", "B", "C", "D"))

            ggsave(simVSest_FOR_plot_BW, file=file.path(path,"simVSest_FOR_plot_BW.pdf"))


            L3plots_rev_BW<-simVSests_plots_BW(L3_realEsts_rev_BW, L3_simEsts_rev_BW)
            L4plots_rev_BW<-simVSests_plots_BW(L4_realEsts_rev_BW, L4_simEsts_rev_BW)
            L5plots_rev_BW<-simVSests_plots_BW(L5_realEsts_rev_BW, L5_simEsts_rev_BW)
            L6plots_rev_BW<-simVSests_plots_BW(L6_realEsts_rev_BW, L6_simEsts_rev_BW)

            simvsests_plots_BW<-L3plots_rev_BW/L4plots_rev_BW/L5plots_rev_BW/L6plots_rev_BW
            simVSest_REV_plot_BW<-simvsests_plots_BW + plot_annotation(tag_levels = c("A", "B", "C", "D"))

            ggsave(simVSest_REV_plot_BW, file=file.path(path,"simVSest_REV_plot_BW.pdf"))


    #combined plots
        comb_BWplots<-simVSest_REV_plot_BW | simVSest_FOR_plot_BW
        comb_Meanplots<-simVSest_REV_plot | simVSest_FOR_plot



        comb_reversedplots<-simVSest_REV_plot_BW | simVSest_REV_plot
        comb_reversedplots<- comb_reversedplots + plot_annotation(tag_levels = c("A", "B", "C", "D", "A1", "B1", "C1", "D1"))
        ggsave(comb_reversedplots, file=file.path(path,"comb_reversedplots_dots.pdf"))

        comb_forwardplots<-simVSest_FOR_plot_BW | simVSest_FOR_plot
        comb_forwardplots<- comb_forwardplots + plot_annotation(tag_levels = c("A", "B", "C", "D", "A", "B", "C", "D"))
        ggsave(comb_forwardplots, file=file.path(path,"comb_forwardplots.pdf"))

        comb_BWplots | comb_Meanplots








L4aov<- plotting %>%
                        filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                        t.test(L4_zes~spqH, data=.)
                        L4aov
                        plot(L4aov)

                    L4aov_zes<- plotting %>%
                        filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                        lm(spqH ~   L4_zes, data=.)
                        summary(L4aov_zes)

                    L4aov_zes_age<- plotting %>%
                        filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                        lm(spqH ~   L4_zes +age, data=.)
                        summary(L4aov_zes_age)

                    L4aov_zes_full<- plotting %>%
                        filter(!Participant.Private.ID %in% c("4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>%
                        lm(spqH ~   L4_zes +age +gender + education + device_type + startSDHigh, data=.)
                        summary(L4aov_zes_full)


                        AIC(L4aov_zes,L4aov,L4aov_zes_age,L4aov_zes_full)

        L5aov<- plotting %>%
                        filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                        lm(spqH ~  L5_mux  +L5_kax + L5_kaa + L5_be1  + L5_zem + L5_zes, data=.)
                        summary(L5aov)

                    L5aov_zes<- plotting %>%
                        filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                        t.test(L5_zes ~ spqH , data=.)
                        summary(L5aov_zes)

                      L5aov_zes_full<- plotting %>%
                        filter(!Participant.Private.ID %in% c("3981816","3932656" )) %>%
                        lm(spqH ~   L5_zes +age  + education  , data=.)
                        summary(L5aov_zes_full)

                    AIC(L5aov,L5aov_zes,L5aov_zes_full)

        L6aov<- plotting %>%
                         filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                        lm(spqH ~  L6_mux  +L6_kax + L6_kaa + L6_be1  + L6_zem + L6_zes, data=.)
                        summary(L6aov)

                    L6aov_zes<- plotting %>%
                        filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                        t.test(L6_zes ~ spqH , data=.)
                        L6aov_zes

                      L6aov_zes_full<- plotting %>%
                        filter(!Participant.Private.ID %in% c("4025943","3984606" )) %>%
                        lm(spqH ~   L6_zes +age  + education  , data=.)
                        summary(L6aov_zes_full)

                    AIC(L6aov,L6aov_zes,L6aov_zes_full)
                        

L4aov<- plotting %>%
                        lm(spqH ~  L4_be1_F*startSDHigh, data=.)
                        summary(L4aov)


a<-plotting %>%
lm(L4_be1_F ~ spqH+startSDHigh , data=.)
 summary(a)

x<-plotting %>%
lm(L4_be1_F ~ spqH*startSDHigh , data=.)
 summary(x)

 y<-plotting %>%
lm(spqH ~ L4_be1_F + startSDHigh , data=.)
 summary(y)

b<- plotting %>%
            lmer( spqH ~ L4_be1_F + startSDHigh + gender + (1|trialName), data=.)

                    summary(b)
                    car::qqPlot(resid(b))
                    scatter.smooth(residuals(b) ~ fitted(b))




