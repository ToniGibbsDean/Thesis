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
        library(ggpubr)

        options(scipen=999)

        path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop2/Results"

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

################################################################################################################################################
#data
################################################################################################################################################
    #read in parameters 
        L3_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_3_GENPOP.csv") 
        L4_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_4_GENPOP.csv")
        L5_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_5_GENPOP.csv")
        L6_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_6_GENPOP.csv")

        L3_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_3_GENPOP.csv")
        L4_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_4_GENPOP.csv")
        L5_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_5_GENPOP.csv")
        L6_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_6_GENPOP.csv")


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
                

        Forward<-genpop2 %>%
                        filter(trialName %in% c('spacetask003', 'spacetask012')) 

        Reversed<-genpop2 %>%
                        filter(trialName %in% c('spacetask003reversed', 'spacetask012reversed')) 

    

################################################################################################################################################
#joining datasets, making dataframes 
################################################################################################################################################

        #get list of IDs in the order they were fed into the HGF 
        #bind ID to params
            Participant.Private.ID<-Forward %>%
                                                group_by(Participant.Private.ID) %>%
                                                filter(level %in% c(3:6))  %>%
                                                dplyr::select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                pull(1) %>%
                                                unique

            ParamsandIDs<-cbind(L3_params_F, L4_params_F, L5_params_F, L6_params_F, Participant.Private.ID)

            #get IDs of individuals where the model showed a warning 
            #L3fIDexc<-unique(ParamsandIDs$Participant.Private.ID[14])
            L4fIDexc<-unique(ParamsandIDs$Participant.Private.ID[c(53, 110, 163, 185, 195, 215, 221, 240, 278, 296)])
            L5fIDexc<-unique(ParamsandIDs$Participant.Private.ID[c(28, 33, 65, 90, 147, 148, 149, 224, 253, 272)])
            L6fIDexc<-unique(ParamsandIDs$Participant.Private.ID[c(65, 118, 159, 223, 267)])


            Participant.Private.ID<-Reversed %>%
                                                group_by(Participant.Private.ID) %>%
                                                filter(level %in% c(3:6))  %>%
                                                select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                pull(1) %>%
                                                unique
            ParamsandIDsReversed<-cbind(L3_params_R, L4_params_R, L5_params_R, L6_params_R, Participant.Private.ID)

            L3fIDexc_REV<-unique(ParamsandIDs$Participant.Private.ID[c(25)])
            L4fIDexc_REV<-unique(ParamsandIDs$Participant.Private.ID[c(75, 128, 235, 246)])
            L5fIDexc_REV<-unique(ParamsandIDs$Participant.Private.ID[c(61)])
            L6fIDexc_REV<-unique(ParamsandIDs$Participant.Private.ID[c(26, 69, 152, 185, 188, 214, 242, 275)])

        #left join to main data
            temp <- genpop2 %>%
                                        group_by(Participant.Private.ID) %>%
                                        left_join(x=genpop2, y=ParamsandIDs, by="Participant.Private.ID") %>%
                                        mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                               masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) #%>% dplyr::select(masqCombined_depression, masqCombined_anxiety)
                                    

            GenpopwithParams <- temp %>%
                                        group_by(Participant.Private.ID) %>%
                                        left_join(x=temp, y=ParamsandIDsReversed, by="Participant.Private.ID") %>%
                                        filter(Participant.Private.ID %in% c(L6fIDexc_REV, L5fIDexc_REV, L4fIDexc_REV, L3fIDexc_REV, L6fIDexc, L5fIDexc, L4fIDexc)==FALSE)


            ################################################################################################################################################
            #creating modelling df
            ################################################################################################################################################

                                #plots of parameters that arent fixed 
                                
                                wide_df<- GenpopwithParams %>%
                                                        group_by(Participant.Private.ID) %>%
                                                        mutate_if(is_character, as_factor) %>%
                                                        #filter(level %in% c(3,4)) %>%
                                                        #filter(SPQsum!="NA") %>%
                                                        #filter(Participant.Device.Type %in% c("computer", "mobile")) %>% #removes 3
                                                        #filter(level=="3") %>%
                                                        #mutate(spqH=SPQsum>=30) %>%
                                                        #mutate(startSDHigh=trialName %in% c("spacetask012", "spacetask012reversed")) %>%
                                                        mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                                                        summarise(
                                                                trialName=dplyr::first(trialName),
                                                                gender=dplyr::first(gender),
                                                                age=dplyr::first(age),
                                                                device_type=dplyr::first(Participant.Device.Type),
                                                                education=dplyr::first(education),
                                                                green_total=dplyr::first(green_total),
                                                                spq_total=dplyr::first(spq_total),
                                                                pdi_total=dplyr::first(pdi_total),
                                                                masqCombined_anxiety=dplyr::first(masqCombined_anxiety),
                                                                masqCombined_depression=dplyr::first(masqCombined_depression),
                                                                #spqH=dplyr::first(spqH),
                                                                #startSDHigh=dplyr::first(startSDHigh),                                  
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

                                        
                            genpop2_averaged <- genpop2 %>%
                                                        filter(level %in% c(3:6)) %>%
                                                        group_by(Participant.Private.ID, level) %>%
                                                        summarise(meanscore=mean(trialScore)) 
                            long_df <- wide_df %>%
                                            pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>%
                                            left_join(x=., y=genpop2_averaged, by=c("Participant.Private.ID", "level")) %>%
                                            mutate_if(is_character, as_factor) 


################################################################################################################################################
#Functions 
################################################################################################################################################

    generateScoreParameterPlot<-function(data, l, p) {

               # parameter<-enquo(param)

                        scorePlot <- 
                                        data %>%
                                                group_by(Participant.Private.ID) %>%
                                                mutate_if(is_character, as_factor) %>%
                                                filter(level==l) %>%
                                                filter(param==p) %>%
                                               # summarise(across({{param}}, first, .names = "test"), #_{.col}" but cant figure how to only call part of the name for the plot
                                                 #         meanscore=mean(trialScore)) %>%
                                                ggplot(aes(y=value, x=meanscore)) +
                                                    geom_point(colour="grey", alpha=0.4) +
                                                    geom_smooth(method="lm") +
                                                    coord_cartesian() +
                                                    stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                       label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                    theme_minimal() +
                                                    ylab(p)

                return(scorePlot)

        }
        
    generateSymtpomscoreParameterPlot<-function(data, parameter, symptomScale){

                        symtpomPlot <- data %>%
                                                group_by(Participant.Private.ID) %>%
                                               # summarise(across({{symptomScale}}, first), #symtpom_{.col}"),
                                               #           across({{parameter}}, first)) %>% #"parameter_{.col}")) %>%
                                                ggplot(aes(y=.data[[parameter]], x= .data[[symptomScale]])) +
                                                geom_point(colour="grey", alpha=0.4) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal() 

                        #plot<-scorePlot/symtpomPlot

                return(symtpomPlot)

                                             
        } 

        modellingSelectParameter<-function(p) {

                if (p == "be1") {
                                        "be1"
                    }else if (p == "kaa") {
                                        "kaa"  #%>%dplyr::select(LR) %>% summary
                    }else {break}
        }

################################################################################################################################################
#plots 
################################################################################################################################################

    #########################################################################################################
    #plots by level
    #########################################################################################################
        #Level 3 plots score and symtpoms for all free parameters 
                        L3NBScore<-generateScoreParameterPlot(long_df, "3", "be1")
                        L3nb_green<-generateSymtpomscoreParameterPlot(wide_df, "L3_be1", "green_total")
                        L3nb_spq<-generateSymtpomscoreParameterPlot(wide_df, "L3_be1", "spq_total")
                        L3nb_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L3_be1", "pdi_total")
                        L3nb_dep<-generateSymtpomscoreParameterPlot(wide_df, "L3_be1", "masqCombined_depression")
                        L3nb_anx<-generateSymtpomscoreParameterPlot(wide_df, "L3_be1", "masqCombined_anxiety")

                        plot1<- L3NBScore / (L3nb_green | L3nb_spq | L3nb_pdi) / (L3nb_dep | L3nb_anx)
                        L3be1<-plot1 + plot_annotation(title = "L3 Noise Belief")
                        ggsave(L3be1, file=file.path(path, "L3 Noise Belief and Symptom Scales.pdf"))
                    

                        L3KaaScore<-generateScoreParameterPlot(long_df, 3, "kaa")
                        L3kaa_green<-generateSymtpomscoreParameterPlot(wide_df,"L3_kaa", "green_total")
                        L3kaa_spq<-generateSymtpomscoreParameterPlot(wide_df, "L3_kaa", "spq_total")
                        L3kaa_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L3_kaa", "pdi_total")
                        L3kaa_dep<-generateSymtpomscoreParameterPlot(wide_df, "L3_kaa", "masqCombined_depression")
                        L3kaa_anx<-generateSymtpomscoreParameterPlot(wide_df, "L3_kaa", "masqCombined_anxiety")

                        plot2<-L3KaaScore / (L3kaa_green | L3kaa_spq | L3kaa_pdi) / (L3kaa_dep | L3kaa_anx)
                        L3kaa<-plot2 + plot_annotation(title = "L3 Kappa (SD)")
                        ggsave(L3kaa, file=file.path(path, "L3 Kappa (SD) and Symtpom Scales.pdf"))


        #Level 4 plots score and symtpoms for all free parameters 
                        L4NBScore<-generateScoreParameterPlot(long_df, 4, "be1")
                        L4nb_green<-generateSymtpomscoreParameterPlot(wide_df, "L4_be1", "green_total")
                        L4nb_spq<-generateSymtpomscoreParameterPlot(wide_df, "L4_be1", "spq_total")
                        L4nb_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L4_be1", "pdi_total")
                        L4nb_dep<-generateSymtpomscoreParameterPlot(wide_df, "L4_be1", "masqCombined_depression")
                        L4nb_anx<-generateSymtpomscoreParameterPlot(wide_df, "L4_be1", "masqCombined_anxiety")

                        plot3<-L4NBScore / (L4nb_green | L4nb_spq | L4nb_pdi) / (L4nb_dep | L4nb_anx)
                        L4be1<-plot3 + plot_annotation(title = "L4 Noise Belief")
                        ggsave(L4be1, file=file.path(path, "L4 Noise Belief and Symptom Scales.pdf"))

                        L4KaxScore<-generateScoreParameterPlot(long_df, 4, "kax")
                        L4kax_green<-generateSymtpomscoreParameterPlot(wide_df, "L4_kax", "green_total")
                        L4kax_spq<-generateSymtpomscoreParameterPlot(wide_df, "L4_kax", "spq_total")
                        L4kax_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L4_kax", "pdi_total")
                        L4kax_dep<-generateSymtpomscoreParameterPlot(wide_df, "L4_kax", "masqCombined_depression")
                        L4kax_anx<-generateSymtpomscoreParameterPlot(wide_df, "L4_kax", "masqCombined_anxiety")

                        plot4<-L4KaxScore / (L4kax_green | L4kax_spq | L4kax_pdi) / (L4kax_dep | L4kax_anx)
                        L4kax<-plot4 + plot_annotation(title = "L4 Kappa (mean)")
                        ggsave(L4kax, file=file.path(path, "L4 Kappa (mean) and Symptom Scales.pdf"))

        #Level 5 plots score and symtpoms for all free parameters 
                        L5NBScore<-generateScoreParameterPlot(long_df, 5, "be1")
                        L5nb_green<-generateSymtpomscoreParameterPlot(wide_df, "L5_be1", "green_total")
                        L5nb_spq<-generateSymtpomscoreParameterPlot(wide_df, "L5_be1", "spq_total")
                        L5nb_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L5_be1", "pdi_total")
                        L5nb_dep<-generateSymtpomscoreParameterPlot(wide_df, "L5_be1", "masqCombined_depression")
                        L5nb_anx<-generateSymtpomscoreParameterPlot(wide_df, "L5_be1", "masqCombined_anxiety")

                        plot5<-L5NBScore / (L5nb_green | L5nb_spq | L5nb_pdi) / (L5nb_dep | L5nb_anx)
                        L5be1<-plot5 + plot_annotation(title = "L5 Noise Belief")
                        ggsave(L5be1, file=file.path(path, "L5 Noise Belief and Symptom Scales.pdf"))


                        L5KaaScore<-generateScoreParameterPlot(long_df, 5, "kaa")
                        L5kaa_green<-generateSymtpomscoreParameterPlot(wide_df, "L5_kaa", "green_total")
                        L5kaa_spq<-generateSymtpomscoreParameterPlot(wide_df, "L5_kaa", "spq_total")
                        L5kaa_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L5_kaa", "pdi_total")
                        L5kaa_dep<-generateSymtpomscoreParameterPlot(wide_df, "L5_kaa", "masqCombined_depression")
                        L5kaa_anx<-generateSymtpomscoreParameterPlot(wide_df, "L5_kaa", "masqCombined_anxiety")

                        plot6<-L5KaaScore / (L5kaa_green | L5kaa_spq | L5kaa_pdi) / (L5kaa_dep | L5kaa_anx)
                        L5kaa<-plot6 + plot_annotation(title = "L5 Kappa (SD)")
                        ggsave(L5kaa, file=file.path(path, "L5 Kappa (SD) and Symptom Scales.pdf"))

                        L5KaxScore<-generateScoreParameterPlot(long_df, 5, "kax")
                        L5kax_green<-generateSymtpomscoreParameterPlot(wide_df, "L5_kax", "green_total")
                        L5kax_spq<-generateSymtpomscoreParameterPlot(wide_df, "L5_kax", "spq_total")
                        L5kax_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L5_kax", "pdi_total")
                        L5kax_dep<-generateSymtpomscoreParameterPlot(wide_df, "L5_kax", "masqCombined_depression")
                        L5kax_anx<-generateSymtpomscoreParameterPlot(wide_df, "L5_kax", "masqCombined_anxiety")

                        plot7<-L5KaxScore / (L5kax_green | L5kax_spq | L5kax_pdi) / (L5kax_dep | L5kax_anx)
                        L5kax<-plot7 + plot_annotation(title = "L5 Kappa (mean)")
                        ggsave(L5kax, file=file.path(path, "L5 Kappa (mean) and Symptom Scales.pdf"))


        #Level 6 plots score and symtpoms for all free parameters 
                        L6NBScore<-generateScoreParameterPlot(long_df, 6, "be1")
                        L6nb_green<-generateSymtpomscoreParameterPlot(wide_df, "L6_be1", "green_total")
                        L6nb_spq<-generateSymtpomscoreParameterPlot(wide_df, "L6_be1", "spq_total")
                        L6nb_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L6_be1", "pdi_total")
                        L6nb_dep<-generateSymtpomscoreParameterPlot(wide_df, "L6_be1", "masqCombined_depression")
                        L6nb_anx<-generateSymtpomscoreParameterPlot(wide_df, "L6_be1", "masqCombined_anxiety")

                        plot8<-L6NBScore / (L6nb_green | L6nb_spq | L6nb_pdi) / (L6nb_dep | L6nb_anx)
                        L6be1<-plot8 + plot_annotation(title = "L6 Noise Belief")
                        ggsave(L6be1, file=file.path(path, "L6 Noise Belief and Symptom Scales.pdf"))

                        L6KaaScore<-generateScoreParameterPlot(long_df, 6, "kaa")
                        L6kaa_green<-generateSymtpomscoreParameterPlot(wide_df, "L6_kaa", "green_total")
                        L6kaa_spq<-generateSymtpomscoreParameterPlot(wide_df, "L6_kaa", "spq_total")
                        L6kaa_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L6_kaa", "pdi_total")
                        L6kaa_dep<-generateSymtpomscoreParameterPlot(wide_df, "L6_kaa", "masqCombined_depression")
                        L6kaa_anx<-generateSymtpomscoreParameterPlot(wide_df, "L6_kaa", "masqCombined_anxiety")

                        plot9<-L6KaaScore / (L6kaa_green | L6kaa_spq | L6kaa_pdi) / (L6kaa_dep | L6kaa_anx)
                        L6kaa<-plot9 + plot_annotation(title = "L6 Kappa (SD)")
                        ggsave(L6kaa, file=file.path(path, "L6 Kappa (SD) and Symptom Scales.pdf"))

                        L6KaxScore<-generateScoreParameterPlot(long_df, 6, "kax")
                        L6kax_green<-generateSymtpomscoreParameterPlot(wide_df,"L6_kax", "green_total")
                        L6kax_spq<-generateSymtpomscoreParameterPlot(wide_df, "L6_kax", "spq_total")
                        L6kax_pdi<-generateSymtpomscoreParameterPlot(wide_df, "L6_kax", "pdi_total")
                        L6kax_dep<-generateSymtpomscoreParameterPlot(wide_df, "L6_kax", "masqCombined_depression")
                        L6kax_anx<-generateSymtpomscoreParameterPlot(wide_df, "L6_kax", "masqCombined_anxiety")

                        plot10<-L6KaxScore / (L6kax_green | L6kax_spq | L6kax_pdi) / (L6kax_dep | L6kax_anx)
                        L6kax<-plot10 + plot_annotation(title = "L6 Kappa (mean)")
                        ggsave(L6kax, file=file.path(path, "L6 Kappa (mean) and Symptom Scales.pdf"))


    ##################################################################################################
    #Plots by parameter
    ##################################################################################################

                    score_nb_l3 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                filter(level==3) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L3_be1_F=dplyr::first(L3_be1_F)) %>%
                                                ggplot(aes(y=L3_be1_F,x=meanscore)) +                              
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()

                        score_nb_l4 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                filter(level==4) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L4_be1_F=dplyr::first(L4_be1_F)) %>%
                                                ggplot(aes(y=L4_be1_F,x=meanscore)) +                              
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()

                        score_nb_l5 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                filter(level==5) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L5_be1_F=dplyr::first(L5_be1_F)) %>%
                                                ggplot(aes(y=L5_be1_F,x=meanscore)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()

                        score_nb_l6 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                filter(level==6) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L6_be1_F=dplyr::first(L6_be1_F)) %>%
                                                ggplot(aes(y=L6_be1_F,x=meanscore)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()

                

                        
                        spq_nb_l3 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                reframe(spq=dplyr::first(spq_total), 
                                                        L3_be1_F=dplyr::first(L3_be1_F)) %>%
                                                ggplot(aes(y=L3_be1_F,x=spq)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()

                        spq_nb_l4 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                reframe(spq=dplyr::first(spq_total), 
                                                        #green=dplyr::first()
                                                        L4_be1_F=dplyr::first(L4_be1_F)) %>%
                                                ggplot(aes(y=L4_be1_F,x=spq)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()
                        spq_nb_l5 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                reframe(spq=dplyr::first(spq_total), 
                                                        L5_be1_F=dplyr::first(L5_be1_F)) %>%
                                                ggplot(aes(y=L5_be1_F,x=spq)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()

                        spq_nb_l6 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                reframe(spq=dplyr::first(spq_total), 
                                                        L6_be1_F=dplyr::first(L6_be1_F)) %>%
                                                ggplot(aes(y=L6_be1_F,x=spq)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()


                        green_nb_l3 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                reframe(green=dplyr::first(green_total), 
                                                        L3_be1_F=dplyr::first(L3_be1_F)) %>%
                                                ggplot(aes(y=L3_be1_F,x=green)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()

                        green_nb_l4 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                reframe(green=dplyr::first(green_total), 
                                                        L4_be1_F=dplyr::first(L4_be1_F)) %>%
                                                ggplot(aes(y=L4_be1_F,x=green)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()

                        green_nb_l5 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                reframe(green=dplyr::first(green_total), 
                                                        L5_be1_F=dplyr::first(L5_be1_F)) %>%
                                                ggplot(aes(y=L5_be1_F,x=green)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()


                        green_nb_l6 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                reframe(green=dplyr::first(green_total), 
                                                        L6_be1_F=dplyr::first(L6_be1_F)) %>%
                                                ggplot(aes(y=L6_be1_F,x=green)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                theme_minimal()





                        dep_nb_l3 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L3_be1_F=dplyr::first(L3_be1_F), 
                                                        symptom=dplyr::first(masqCombined_depression)) %>%
                                                ggplot(aes(y=L3_be1_F,x=symptom)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                xlab("dep")+
                                                theme_minimal()

                        dep_nb_l4 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L4_be1_F=dplyr::first(L4_be1_F), 
                                                        symptom=dplyr::first(masqCombined_depression)) %>%
                                                ggplot(aes(y=L4_be1_F,x=symptom)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                xlab("dep")+
                                                theme_minimal()

                        dep_nb_l5 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L5_be1_F=dplyr::first(L5_be1_F), 
                                                        symptom=dplyr::first(masqCombined_depression)) %>%
                                                ggplot(aes(y=L5_be1_F,x=symptom)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                xlab("dep")+
                                                theme_minimal()


                        dep_nb_l6 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L6_be1_F=dplyr::first(L6_be1_F), 
                                                        symptom=dplyr::first(masqCombined_depression)) %>%
                                                ggplot(aes(y=L6_be1_F,x=symptom)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                xlab("dep")+
                                                theme_minimal()



                        anx_nb_l3 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L3_be1_F=dplyr::first(L3_be1_F), 
                                                        symptom=dplyr::first(masqCombined_anxiety)) %>%
                                                ggplot(aes(y=L3_be1_F,x=symptom)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                xlab("anx")+
                                                theme_minimal()

                        anx_nb_l4 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L4_be1_F=dplyr::first(L4_be1_F), 
                                                        symptom=dplyr::first(masqCombined_anxiety)) %>%
                                                ggplot(aes(y=L4_be1_F,x=symptom)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                xlab("anx")+
                                                theme_minimal()

                        anx_nb_l5 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L5_be1_F=dplyr::first(L5_be1_F), 
                                                        symptom=dplyr::first(masqCombined_anxiety)) %>%
                                                ggplot(aes(y=L5_be1_F,x=symptom)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                xlab("anx")+
                                                theme_minimal()


                        anx_nb_l6 <- GenpopwithParams %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L6_be1_F=dplyr::first(L6_be1_F), 
                                                        symptom=dplyr::first(masqCombined_anxiety)) %>%
                                                ggplot(aes(y=L6_be1_F,x=symptom)) +
                                                geom_point(colour="grey", alpha=0.2) +
                                                geom_smooth(method="lm") +
                                                coord_cartesian() +
                                                stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                                label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                                                xlab("anx") +
                                                theme_minimal()


                                anx_nb<- anx_nb_l3 | anx_nb_l4 | anx_nb_l5 | anx_nb_l6
                                green_nb<-green_nb_l3 | green_nb_l4 | green_nb_l5 | green_nb_l6
                                dep_nb<- dep_nb_l3 | dep_nb_l4 | dep_nb_l5 | dep_nb_l6
                                spq_nb<-spq_nb_l3 | spq_nb_l4 | spq_nb_l5 | spq_nb_l6
                                score_nb<-score_nb_l3 | score_nb_l4 | score_nb_l5 | score_nb_l6

                                all<-score_nb / spq_nb / green_nb / dep_nb / anx_nb

                                ggsave(all, file=file.path(path, "nb_scores_symtpoms.pdf"))


##########################################################################################################################################################################################
#Modelling 
##########################################################################################################################################################################################
        ################################################################################################################################
        #psychsis models
        ################################################################################################################################
              
              
                p<-"mux"

                        modNull<-     
                                long_df %>%
                                        filter(param==p) %>%
                                        drop_na %>%
                                        lmer(value ~  1 + 
                                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                        modNullish<-     
                                long_df %>%
                                        filter(param==p) %>%
                                        drop_na %>%
                                        lmer(value ~  1 + gender + age + device_type + education +
                                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )


                        modPsychosis<-     
                                long_df %>%
                                        filter(param==p) %>%
                                        drop_na %>%
                                        lmer(value ~  level:green_total + level:spq_total + level:pdi_total + 
                                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                        modSPQ<-     
                                long_df %>%
                                        filter(param==p) %>%
                                        drop_na %>%
                                        lmer(value ~  level:spq_total + 
                                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                        modPara<-     
                                long_df %>%
                                        filter(param==p) %>%
                                        drop_na %>%
                                        lmer(value ~  level:green_total + 
                                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                        modDelusions<-     
                                long_df %>%
                                        filter(param==p) %>%
                                        drop_na %>%
                                        lmer(value ~  level:pdi_total + 
                                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )

                                        
                                        MuMIn::model.sel(modNull, modPsychosis, modSPQ, modPara, modDelusions, modNullish)


                        plotdf<-long_df %>%         
                                        filter(param==p) %>%
                                        drop_na 
                                                                
                        plotdf$resids_be1psyc<-resid(modPara)
                        plotdf$resids_kaxpsyc<-resid(modPsychosis)
                               
                                        
                    modBe1_plot<- plotdf  %>% #non residual plot i.e., using vale of be1 is a better plot visually ... 
                                                ggplot(aes(x=green_total, y=value), colour=level) +
                                                geom_point(alpha=0.2) +
                                                geom_smooth(aes(colour=level),method="lm") +
                                                theme_classic() +
                                                theme(axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank()) +
                                                ylab("Noise belief (be1)") +
                                                xlab("GPTS Questionnaire\nScores")

                                                ggsave(modBe1_plot, file=file.path(path, "Noise Belief Modelling Findings: Paranoia Model.pdf"))

                modKax_plot_para<- plotdf  %>% #non residual plot i.e., using vale of be1 is a better plot visually ... 
                                                filter(!level==3) %>%
                                                ggplot(aes(x=green_total, y=value), colour=level) +
                                                geom_point(alpha=0.2) +
                                                geom_smooth(aes(colour=level),method="lm") +
                                                theme_classic() +
                                                theme(axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank()) +
                                                ylab("Kappa (mean)") +
                                                xlab("GPTS Questionnaire\nScores") +
                                                coord_cartesian(ylim = c(0, 10)) 

                                                ggsave(modKax_plot, file=file.path(path, "Kax Modelling Findings: Paranoia.pdf"))

                 modKax_plot_spq<- plotdf  %>% #non residual plot i.e., using vale of be1 is a better plot visually ... 
                                                filter(!level==3) %>%
                                                ggplot(aes(x=spq_total, y=value), colour=level) +
                                                geom_point(alpha=0.2) +
                                                geom_smooth(aes(colour=level),method="lm") +
                                                theme_classic() +
                                                theme(axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank()) +
                                                ylab("Kappa (mean)") +
                                                xlab("SPQ Questionnaire\nScores") +
                                                coord_cartesian(ylim = c(0, 10)) 

                                                ggsave(modKax_plot_spq, file=file.path(path, "Kax Modelling Findings: SPQ .pdf"))



        ################################################################################################################################
        #affect models
        ################################################################################################################################
             
        p<-"kaa"

              modNullish2<-     
                    long_df %>%
                        filter(param==p) %>%
                        drop_na %>%
                        lmer(value ~  1 + gender + age + device_type + education +
                                (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                          optCtrl=list(maxfun=2e7)))

                modNull2<-     
                    long_df %>%
                        filter(param==p) %>%
                        drop_na %>%
                        lmer(value ~  1 +
                                (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                          optCtrl=list(maxfun=2e7)))
                                      
            mod_dep<-
                    long_df %>%
                        filter(param==p) %>%
                        drop_na %>%
                        lmer(value ~ level:masqCombined_depression +device_type + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )               

            mod_anx<-
                    long_df %>%
                        filter(param==p) %>%
                        drop_na %>%
                        lmer(value ~  level:masqCombined_anxiety + device_type + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )

            mod_affect<-
                    long_df %>%
                        filter(param==p) %>%
                        drop_na %>%
                        lmer(value ~  level:masqCombined_anxiety +level:masqCombined_depression +device_type +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
                           
                           
         MuMIn::model.sel(modNull2, mod_affect, mod_anx, mod_dep, modNullish2)




                modBe1_plot_affective1<-  long_df %>%                                              
                                                filter(param==p) %>%
                                                drop_na %>%#non residual plot i.e., using vale of be1 is a better plot visually ... 
                                                #filter(!level==3) %>%
                                                ggplot(aes(x=masqCombined_depression, y=value), colour=level) +
                                                geom_point(alpha=0.2) +
                                                geom_smooth(aes(colour=level),method="lm") +
                                                theme_classic() +
                                                theme(axis.ticks.x = element_blank(),
                                                     axis.ticks.y = element_blank(),
                                                     legend.position="none") +
                                                ylab("Noise Belief") +
                                                xlab("Depression Questionnaire\nScores") #+
                                                #coord_cartesian(ylim = c(0, 10)) 

                modBe1_plot_affective2<-  long_df %>%                                              
                                                filter(param==p) %>%
                                                drop_na %>%#
                                                ggplot(aes(x=masqCombined_anxiety, y=value), colour=level) +
                                                geom_point(alpha=0.2) +
                                                geom_smooth(aes(colour=level),method="lm") +
                                                theme_classic() +
                                                theme(axis.ticks.x = element_blank(),
                                                     axis.ticks.y = element_blank(),
                                                     axis.title.y = element_blank()) +
                                                #ylab("Noise Belief") +
                                                xlab("Anxiety Questionnaire\nScores") #+
                                               # coord_cartesian(ylim = c(0, 10)) 

                modBe1_plot_affective <- modBe1_plot_affective1 | modBe1_plot_affective2

                ggsave(modBe1_plot_affective, file=file.path(path, "Be1 Modelling Findings: Anxiety and Depression.pdf"))




                modKaa_plot_affective1<-  long_df %>%                                              
                                                filter(param==p) %>%
                                                drop_na %>%#non residual plot i.e., using vale of be1 is a better plot visually ... 
                                                #filter(!level==3) %>%
                                                ggplot(aes(x=masqCombined_depression, y=value), colour=level) +
                                                geom_point(alpha=0.2) +
                                                geom_smooth(aes(colour=level),method="lm") +
                                                theme_classic() +
                                                theme(axis.ticks.x = element_blank(),
                                                     axis.ticks.y = element_blank(),
                                                     legend.position="none") +
                                                ylab("Kappa (SD)") +
                                                xlab("Depression Questionnaire\nScores") +
                                        coord_cartesian(ylim = c(0, 40)) 















            mod_paranoia<-##
                    long_df %>%
                        filter(param=="be1") %>%
                        drop_na %>%
                        lmer(value ~  level:green_total +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )

            mod_delusions<-
                    long_df %>%
                        filter(param=="be1") %>%
                        drop_na %>%
                        lmer(value ~  level:pdi_total + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
            mod_spq<-
                    long_df %>%
                        filter(param=="be1") %>%
                        drop_na %>%
                        lmer(value ~  level:spq_total + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
 
                                                

            modNull<-     
                    long_df %>%
                        filter(param=="kaa") %>%
                        drop_na %>%
                        lmer(value ~  1+
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )

            mod_paranoia<-
                    long_df %>%
                        filter(param=="kaa") %>%
                        drop_na %>%
                        lmer(value ~  level:green_total +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )

            mod_delusions<-
                    long_df %>%
                        filter(param=="kaa") %>%
                        drop_na %>%
                        lmer(value ~  level:pdi_total + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
            mod_spq<-
                    long_df %>%
                        filter(param=="kaa") %>%
                        drop_na %>%
                        lmer(value ~  level:spq_total + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
                           
            mod_dep<-##
                    long_df %>%
                        filter(param=="kaa") %>%
                        drop_na %>%
                        lmer(value ~ level:masqCombined_depression +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )               

            mod_anx<-
                    long_df %>%
                        filter(param=="kaa") %>%
                        drop_na %>%
                        lmer(value ~  level:masqCombined_anxiety +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
                           
                           
            modkaa<- MuMIn::model.sel(modNull, mod_paranoia, mod_delusions, mod_spq,mod_dep,mod_anx)


             modNull<-     
                    long_df %>%
                        filter(param=="kax") %>%
                        drop_na %>%
                        lmer(value ~  1+
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )

            mod_paranoia<-
                    long_df %>%
                        filter(param=="kax") %>%
                        drop_na %>%
                        lmer(value ~  level:green_total +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )

            mod_delusions<-
                    long_df %>%
                        filter(param=="kax") %>%
                        drop_na %>%
                        lmer(value ~  level:pdi_total + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
            mod_spq<-
                    long_df %>%
                        filter(param=="kax") %>%
                        drop_na %>%
                        lmer(value ~  level:spq_total + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
                           
            mod_dep<-##
                    long_df %>%
                        filter(param=="kax") %>%
                        drop_na %>%
                        lmer(value ~ level:masqCombined_depression +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )               

            mod_anx<-
                    long_df %>%
                        filter(param=="kax") %>%
                        drop_na %>%
                        lmer(value ~  level:masqCombined_anxiety +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
                           
                           
            modkax<- MuMIn::model.sel(modNull, mod_paranoia, mod_delusions, mod_spq,mod_dep,mod_anx)
                           
      


        modNull<-     
                    long_df %>%
                        filter(param=="zes") %>%
                        drop_na %>%
                        lmer(value ~  1+
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )

            mod_paranoia<-
                    long_df %>%
                        filter(param=="zes") %>%
                        drop_na %>%
                        lmer(value ~  level:green_total +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )

            mod_delusions<-
                    long_df %>%
                        filter(param=="zes") %>%
                        drop_na %>%
                        lmer(value ~  level:pdi_total + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
            mod_spq<-
                    long_df %>%
                        filter(param=="zes") %>%
                        drop_na %>%
                        lmer(value ~  level:spq_total + 
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
                           
            mod_dep<-##
                    long_df %>%
                        filter(param=="zes") %>%
                        drop_na %>%
                        lmer(value ~ level:masqCombined_depression +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )               

            mod_anx<-
                    long_df %>%
                        filter(param=="zes") %>%
                        drop_na %>%
                        lmer(value ~  level:masqCombined_anxiety +
                        (1|Participant.Private.ID) + (1|level), data=., REML=F,
                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e7)) )
                           
                           
            modzes<- MuMIn::model.sel(modNull, mod_paranoia, mod_delusions, mod_spq,mod_dep,mod_anx)

























                                #plotScoreAndSymptoms<-function(symptomscale){

                                L4nb_score_spq<-    temp %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                filter(level==4) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                summarise(meanscore=mean(trialScore), 
                                                        L4_be1_F=dplyr::first(L4_be1_F), 
                                                        symptom=dplyr::first(masqCombined_anxiety)) %>%
                                                ggplot(aes(x=L4_be1_F, y=symptom)) +
                                                        geom_smooth(method="lm") +
                                                        geom_smooth(aes(y=meanscore), colour="red", method="lm") +
                                                                coord_cartesian() +
                                                                ylab("green=red, mean score for the level=blue") 
                                

                                L3nb_score_spq<-   temp %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                filter(level==3) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L3_be1_F=dplyr::first(L3_be1_F), 
                                                        symptom=dplyr::first(masqCombined_anxiety)) %>%
                                                ggplot(aes(x=L3_be1_F)) +
                                                                geom_smooth(aes(y=meanscore), method="lm") +
                                                                geom_smooth(aes(y=symptom), colour="red", method="lm") +
                                                                coord_cartesian() +
                                                                ylab("green=red, mean score for the level=blue")

                                L5nb_score_spq<-   temp %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                filter(level==5) %>%
                                                                        mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L5_be1_F=dplyr::first(L5_be1_F), 
                                                        symptom=dplyr::first(masqCombined_anxiety)) %>%
                                                ggplot(aes(x=L5_be1_F)) +
                                                                geom_smooth(aes(y=meanscore), method="lm") +
                                                                geom_smooth(aes(y=symptom), colour="red", method="lm") +
                                                                coord_cartesian() +
                                                                ylab("green=red, mean score for the level=blue")

                                L6nb_score_spq<-   temp %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                filter(level==6) %>%
                                                mutate(masqCombined_anxiety=sum(dplyr::first(aa_score), dplyr::first(gda_score)) ,
                                                        masqCombined_depression=sum(dplyr::first(ad_score), dplyr::first(gdd_score))) %>%
                                                reframe(meanscore=mean(trialScore), 
                                                        L6_be1_F=dplyr::first(L6_be1_F), 
                                                        symptom=dplyr::first(masqCombined_anxiety)) %>%
                                                ggplot(aes(x=L6_be1_F)) +
                                                                geom_smooth(aes(y=meanscore), method="lm") +
                                                                geom_smooth(aes(y=symptom), colour="red", method="lm") +
                                                                coord_cartesian() +
                                                                ylab("symptom=red, mean score for the level=blue")

                                plot<-egg::ggarrange(L3nb_score_spq, L4nb_score_spq, L5nb_score_spq, L6nb_score_spq)


                                #return(plot)

                                #}



                        spq_nb<-egg::ggarrange(spq_nb_l3, spq_nb_l4, spq_nb_l5, spq_nb_l6, nrow=1)
                        plotScoreAndSymptoms(temp$spq_total)


                                









                                l4mu_score<-    temp %>%
                                                filter(level==4) %>%
                                                reframe(meanscore=mean(trialScore), L4_mux_F=L4_mux_F) %>%
                                                ggplot(aes(x=meanscore, y=L4_mux_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                l3mu_score<-    temp %>%
                                                filter(level==3) %>%
                                                reframe(meanscore=mean(trialScore), L3_mux_F=L3_mux_F) %>%
                                                ggplot(aes(x=meanscore, y=L3_mux_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()


                                                egg::ggarrange(L4nb_score,L3nb_score, l4mu_score, l3mu_score)





                                L4nb_score<-    temp %>%

                                                reframe(spq=first(spq_total), L4_be1_F=L4_be1_F) %>%
                                                ggplot(aes(x=spq, y=L4_be1_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                L3nb_score<-    temp %>%

                                                reframe(spq=first(spq_total), L3_be1_F=L3_be1_F) %>%
                                                ggplot(aes(x=spq, y=L3_be1_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                L5nb_score<-    temp %>%

                                                reframe(spq=first(spq_total), L5_be1_F=L5_be1_F) %>%
                                                ggplot(aes(x=spq, y=L5_be1_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                L6nb_score<-    temp %>%

                                                reframe(spq=first(spq_total), L6_be1_F=L6_be1_F) %>%
                                                ggplot(aes(x=spq, y=L6_be1_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                                egg::ggarrange(L4nb_score,L3nb_score, L5nb_score, L6nb_score)



                                L4nb_score<-    temp %>%
                                                filter(level==4) %>%
                                                reframe(green_total=first(green_total), L4_be1_F=L4_be1_F) %>%
                                                ggplot(aes(x=green_total, y=L4_be1_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                L3nb_score<-    temp %>%
                                                filter(level==3) %>%
                                                reframe(green_total=first(green_total), L3_be1_F=L3_be1_F) %>%
                                                ggplot(aes(x=green_total, y=L3_be1_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                L5nb_score<-    temp %>%
                                                filter(level==5) %>%
                                                reframe(green_total=first(green_total), L5_be1_F=L5_be1_F) %>%
                                                ggplot(aes(x=green_total, y=L5_be1_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                L6nb_score<-    temp %>%
                                                filter(level==6) %>%
                                                reframe(green_total=first(green_total), L6_be1_F=L6_be1_F) %>%
                                                ggplot(aes(x=green_total, y=L6_be1_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                                egg::ggarrange(L4nb_score,L3nb_score, L5nb_score, L6nb_score)





                                L4nb_score<-    temp %>%
                                                reframe(green_total=first(green_total), L3_kax_F=L3_kax_F) %>%
                                                ggplot(aes(x=green_total, y=L3_kax_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                L3nb_score<-    temp %>%
                                                reframe(green_total=first(green_total), L4_kax_F=L4_kax_F) %>%
                                                ggplot(aes(x=green_total, y=L4_kax_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                L5nb_score<-    temp %>%
                                                reframe(green_total=first(green_total), L5_kax_F=L5_kax_F) %>%
                                                ggplot(aes(x=green_total, y=L5_kax_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                L6nb_score<-    temp %>%
                                                reframe(green_total=first(green_total), L6_kax_F=L6_kax_F) %>%
                                                ggplot(aes(x=green_total, y=L6_kax_F)) +
                                                geom_point() +
                                                geom_smooth(method="lm") +
                                                coord_cartesian()

                                                egg::ggarrange(L4nb_score,L3nb_score, L5nb_score, L6nb_score)




                        
