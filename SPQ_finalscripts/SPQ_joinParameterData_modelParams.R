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
            library(corrr)
               library(multcomp)




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
                                    mutate(increaseTRUE = L4_be1 >= L3_be1) %>%
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
                                            #scale_color_manual(values=c("darkgrey", "red")) +
                                            stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1))+
                                            facet_wrap(~spqH) 
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

                            mantest2<-    manova(cbind(L3_kaa, L3_be1,
                                        L4_kax, L4_be1,
                                        L5_kaa, L5_kax, L5_be1, 
                                        L6_kaa, L6_kax, L6_be1) ~ spqH, data=plotting)

                                    mantestL3<-    manova(cbind(L3_mux, L3_kaa, L3_be1, L3_zem, L3_zes) ~ spqH, data=plotting)

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
                                        group_by(Participant.Private.ID) %>%
                                        filter(param %in% c("be1", "kax", "kaa")) %>%
                                        mutate(level=as.factor(level)) %>%
                                        filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656")) %>% #needs updating
                                        mutate(spqH=as.factor(spqH)) %>%
                                        mutate(level=as.factor(level)) %>%
                                        mutate(param=as.factor(param)) %>% # each param is in acolumn and level is acollumn 
                                        filter(param=="kaa") #%>%
                                       

                        cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                        c(0,-1,1,0), # 4 vs 5
                        c(0,0,-1,1)) #5vs 6
                        contrasts(temp$level) <-cMat
                        colnames(attr(temp$level, "contrasts")) <- c("3v4", "4v5", "5v6")       

        cMat <-     cbind( c(1,-1, 0,0), # 3 vs 4
                           c(0,0,0,0), # 4 vs 5
                           c(0,0,1,-1)) #5vs 6
                        contrasts(temp$level) <-cMat
                        colnames(attr(temp$level, "contrasts")) <- c("3v4","noconstrast", "5v6")                
                                                     
                                            
                       # mixedmod_allLevs<-  temp %>% 
                        #                        filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656")) %>% #needs updating
                            #                                mutate(level=as.factor(level)) %>%
                         ##                                   mutate(spqH=as.factor(spqH)) %>%
                           #                                 mutate(param=as.factor(param)) #%>%
                                                            #filter(param=="be1") #%>%
                                                            #filter(level %in% c(3,4)) %>%

                                                   simple<-   
                                                                            lmer(value ~ 1 +
                                                                            (1|Participant.Private.ID) + (1|level), data=temp, REML=F,
                                                                                                            control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                                            optCtrl=list(maxfun=2e7)) )
                                                                                                                

                                                   mixedmod_allLevsmod<-     lmer(value ~ spqH + level +
                                                                (1|Participant.Private.ID) + (1|level), data=temp, REML=F,
                                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                        optCtrl=list(maxfun=2e7)) )


                                                   mixedmod_allLevsmod_interaction<-     lmer(value ~ spqH*level +age + gender + education + device_type + 
                                                                (1|Participant.Private.ID) + (1|level), data=temp, REML=F,
                                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                        optCtrl=list(maxfun=2e7)) )


                                                    Kappamodel
                                                                                        
                                                                                        
                                                                                        AICcmodavg::AICc(simple, mixedmod_allLevsmod, mixedmod_allLevsmod_interaction)            
                                                                                       anova(simple, mixedmod_allLevsmod, mixedmod_allLevsmod_interaction, test="Chisq")
                                                                                        r.squaredGLMM(mixedmod_allLevsmod_interaction)

                                             mixedmod_allLevsmod_oneL<- temp %>%
                                                                 filter(level %in% c(3)) %>%
                                                                lmer(value ~ spqH +age + gender + education + device_type + 
                                                                (1|trialName), data=., REML=F,
                                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                        optCtrl=list(maxfun=2e7)) )



                                        mixedmod_allLevsmod<-     lmer(value ~ level*param*spqH + age + gender + education + device_type + 
                                                                (1|Participant.Private.ID) + (1|trialName/level), data=temp, REML=F,
                                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                        optCtrl=list(maxfun=2e7)) )

                                                        test<-lm(value ~ spqH*level, data=mixedmod_allLevs)
                                                        test<-lm(value ~ spqH*level + age + gender + education + device_type +trialName, data=mixedmod_allLevs)

                                                        summary(mixedmod_allLevs)
                                                        car::qqPlot(resid( reduced_full))
                                                        scatter.smooth(residuals(reduced_full) ~ fitted(reduced_full))





############################################################################################################################################################
# correlation with be1 and performance metrics 
############################################################################################################################################################

            
make_correlation_paramAndPerf_df<-function(parameter) {     
            
        performmets<-     
            spqall %>%
            group_by(Participant.Private.ID, level) %>%
            filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
            filter(level %in% c(3,4,5,6)) %>%
             summarise(meanscore=mean(trialScore), meanPE=mean(PE), meanPerfE=mean(PerfE))  %>%
              group_by(Participant.Private.ID, level)  %>%
                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                mutate(level=as.factor(level))
       

            corr_params<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
                                    #select(c(Participant.Private.ID, spqH)) %>% # just 5 and 6 cols
                                    mutate(increaseTRUE = L4_be1 >= L3_be1) %>%
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>% # each param is in acolumn and level is acollumn 
                                    filter(param==parameter) %>%
                                     group_by(Participant.Private.ID, level) %>%
                                    mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                                    #filter(param=="zes") %>%
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) %>%
                                    mutate(param=as.factor(param)) #%>%

        df_params_perf_corr <- corr_params %>% left_join(performmets)

    return(df_params_perf_corr)


}


makeCorrPlots_bylevel_andOVerall_SCORE<-function(parameter, performMetric) {

        be1_levelwise_spqwise  <-  make_correlation_paramAndPerf_df(parameter) %>%
                                        mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                    spqH==FALSE ~ "Low SPQ" )) %>%
                                        ggplot(aes(x=meanscore, y=value, fill=spqNamedGroups)) +
                                        geom_point(aes(colour=spqNamedGroups),alpha=0.2) +
                                        geom_smooth(aes(fill=spqNamedGroups, colour=spqNamedGroups), method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.5) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank(),
                                              legend.title=element_blank()) +
                                        facet_wrap(~level) +
                                        
                                        #facet_wrap(~level) 
                                        #ylim(0,15)
                                        labs(y= "Noise Belief")
                                        #ylim(0,15)
                                        #labs(x = "Anxiety Score")
                         be1_levelwise_spqwise<-be1_levelwise_spqwise + scale_fill_manual(values=c(Comp1, Comp2))
                         be1_levelwise_spqwise<-be1_levelwise_spqwise + scale_colour_manual(values=c(Comp1, Comp2))
         
                 be1_overall <-  make_correlation_paramAndPerf_df(parameter) %>%
                                        mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                    spqH==FALSE ~ "Low SPQ" )) %>%
                                        ggplot(aes(x=meanscore, y=value, fill=spqNamedGroups)) +
                                        geom_point(aes(colour=spqNamedGroups),alpha=0.2) +
                                        geom_smooth(aes(fill=spqNamedGroups, colour=spqNamedGroups), method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.5) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank(),
                                              legend.title=element_blank()) +
                                        #facet_wrap(~level) 
                                        #ylim(0,15)
                                        labs(y = "Noise Belief")
                                        #facet_wrap(~level) 
                                        #ylim(0,15)
                                        #labs(x = "Anxiety Score")
                         be1_overall<-be1_overall + scale_fill_manual(values=c(Comp1, Comp2))
                         be1_overall<-be1_overall + scale_colour_manual(values=c(Comp1, Comp2))

                         final<-be1_overall / be1_levelwise_spqwise
              
              return(final)

    }



makeCorrPlots_bylevel_andOVerall_PE<-function(parameter, performMetric) {

        be1_levelwise_spqwise  <-  make_correlation_paramAndPerf_df(parameter) %>%
                                        mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                    spqH==FALSE ~ "Low SPQ" )) %>%
                                        ggplot(aes(x=meanPE, y=value, fill=spqNamedGroups)) +
                                        geom_point(aes(colour=spqNamedGroups),alpha=0.2) +
                                        geom_smooth(aes(fill=spqNamedGroups, colour=spqNamedGroups), method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.5) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank(),
                                              legend.title=element_blank()) +
                                        facet_wrap(~level) +
                                        #ylim(0,15)
                                        #labs(x = "Anxiety Score")+
                                        #facet_wrap(~level) 
                                        #ylim(0,15)
                                        labs(y = "Noise Belief")
                         be1_levelwise_spqwise<-be1_levelwise_spqwise + scale_fill_manual(values=c(Comp1, Comp2))
                         be1_levelwise_spqwise<-be1_levelwise_spqwise + scale_colour_manual(values=c(Comp1, Comp2))
         
                 be1_overall <-  make_correlation_paramAndPerf_df(parameter) %>%
                                        mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                    spqH==FALSE ~ "Low SPQ" )) %>%
                                        ggplot(aes(x=meanPE, y=value, fill=spqNamedGroups)) +
                                        geom_point(aes(colour=spqNamedGroups),alpha=0.2) +
                                        geom_smooth(aes(fill=spqNamedGroups, colour=spqNamedGroups), method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.5) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank(),
                                              legend.title=element_blank())+
                                        #facet_wrap(~level) 
                                        #ylim(0,15)
                                        labs(y = "Noise Belief")
                         be1_overall<-be1_overall + scale_fill_manual(values=c(Comp1, Comp2))
                         be1_overall<-be1_overall + scale_colour_manual(values=c(Comp1, Comp2))

                         final<-be1_overall / be1_levelwise_spqwise
              
              return(final)

    }

corr_PE_be1<-makeCorrPlots_bylevel_andOVerall_PE("be1") 
            corr_PE_be1<- corr_PE_be1 + plot_annotation(tag_levels = c("A", "B"))
            ggsave(corr_PE_be1, file=file.path(path,"corr_PE_be1.pdf"))
corr_PE_kax<-makeCorrPlots_bylevel_andOVerall_PE("kax")
            corr_PE_kax<- corr_PE_kax + plot_annotation(tag_levels = c("A", "B"))
            ggsave(corr_PE_kax, file=file.path(path,"corr_PE_kax.pdf"))
corr_PE_kaa<-makeCorrPlots_bylevel_andOVerall_PE("kaa")
            corr_PE_kaa<- corr_PE_kaa + plot_annotation(tag_levels = c("A", "B"))
            ggsave(corr_PE_kaa, file=file.path(path,"corr_PE_kaa.pdf"))

corr_score_be1<-makeCorrPlots_bylevel_andOVerall_SCORE("be1")
                corr_score_be1<- corr_score_be1 + plot_annotation(tag_levels = c("A", "B"))
                ggsave(corr_score_be1, file=file.path(path,"corr_score_be1.pdf"))
corr_score_kax<-makeCorrPlots_bylevel_andOVerall_SCORE("kax")
                corr_score_kax<- corr_score_kax + plot_annotation(tag_levels = c("A", "B"))
                ggsave(corr_score_kax, file=file.path(path,"corr_score_kax.pdf"))
corr_score_kaa<-makeCorrPlots_bylevel_andOVerall_SCORE("kaa")
                corr_score_kaa<- corr_score_kaa + plot_annotation(tag_levels = c("A", "B"))
                ggsave(corr_score_kaa, file=file.path(path,"corr_score_kaa.pdf"))




###chaecking/reporting  the corelations 
    performmets<-     
            spqall %>%
            group_by(Participant.Private.ID, level) %>%
            filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
            filter(level %in% c(3,4,5,6)) %>%
             summarise(meanscore=mean(trialScore), meanPE=mean(PE), meanPerfE=mean(PerfE))  %>%
              group_by(Participant.Private.ID, level)  %>%
                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                mutate(level=as.factor(level))

            corr_params<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
                                    #select(c(Participant.Private.ID, spqH)) %>% # just 5 and 6 cols
                                    mutate(increaseTRUE = L4_be1 >= L3_be1) %>%
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>% # each param is in acolumn and level is acollumn 
                                    filter(param=="be1") %>%
                                     group_by(Participant.Private.ID, level) %>%
                                    mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                                    #filter(param=="zes") %>%
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) %>%
                                    mutate(param=as.factor(param)) %>%
                                    filter(spqH==FALSE) %>%
                                    filter(level==5)

        df_params_perf_corr <- corr_params %>% left_join(performmets)
        
        cor.test(df_params_perf_corr$meanPE, df_params_perf_corr$value)







        overall_kax  <-  make_correlation_paramAndPerf_df("kax") %>%
                                        #mutate(slope = betaLR_6 > betaLR_5) %>%
                                        #filter(level==4) %>%
                                        ggplot(aes(x=meanscore, y=value)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_point(alpha=0.2, colour=Comp3) +
                                        geom_smooth(method="lm", colour=Comp3) +
                                       # facet_wrap(~level) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.3) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank()) #+
                                        #facet_wrap(~level) 
                                        #ylim(0,15)
                                        #labs(x = "Anxiety Score")
                         overall_kax<-overall_kax + scale_colour_manual(values=c(Comp3))


        overall_kaa  <-  make_correlation_paramAndPerf_df("kaa") %>%
                                        #mutate(slope = betaLR_6 > betaLR_5) %>%
                                        #filter(level==4) %>%
                                        ggplot(aes(x=meanscore, y=value)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_point(alpha=0.2, colour=Comp2) +
                                        geom_smooth(method="lm", colour=Comp2) +
                                       # facet_wrap(~level) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.3) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank()) #+
                                        #facet_wrap(~level) 
                                        #ylim(0,15)
                                        #labs(x = "Anxiety Score")
                         overall_kaa<-overall_kaa + scale_colour_manual(values=c(Comp2))

