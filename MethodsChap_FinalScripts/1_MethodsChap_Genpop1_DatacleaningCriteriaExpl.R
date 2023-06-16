#########################################
#GenPop1 Data: make simple tibble
#########################################
# Notes: 

    library(tidyverse)
    library(gridExtra)
    path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop1"
    path_results<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Methods/Results"


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

CorrectGameLength<-219
    
    path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop1"

    pfiles<-list.files(file.path(path, "Data", "Parameters"))
    tfiles<-list.files(file.path(path, "Data", "TaskData"))
    dfiles<-list.files(file.path(path, "Data", "Symptom_DemoData"))
    afiles<-list.files(file.path(path, "Data", "AttentionChecks"))

    #p<-lapply(file.path(path, "Data", "Parameters", pfiles), read.csv, header=TRUE)
    #names(p)<-substr(pfiles, start=1, stop=nchar(pfiles)-4)

    p<-lapply(file.path(path, "Data", "Parameters", pfiles), read.csv, header=TRUE)#, col.names=c("Cue", "Target", "Level"))
    names(p)<-substr(pfiles, start=1, stop=nchar(pfiles)-4)

    t<-lapply(file.path(path, "Data", "TaskData", tfiles), read.csv)
    names(t)<-tfiles

    d<-lapply(file.path(path, "Data",  "Symptom_DemoData", dfiles), read.csv)
    names(d)<-dfiles

    a<-lapply(file.path(path, "Data", "AttentionChecks", afiles), read.csv)
    names(a)<-afiles

#combine data

    #merge task datas
    tbound<-rbind(t[[1]], t[[2]])
    t<-tbound[,-c(27:115)]


    #for all poeple with correct game length append columns with their parameter data
    IDs<-unique(t$Participant.Private.ID)
    df<-c()
    for (ID in IDs) {
        PersonsTaskData<-t[t$Participant.Private.ID==ID,]
        gameLength<-dim(PersonsTaskData)[1]
        if (gameLength==CorrectGameLength)
            if(length(unique(PersonsTaskData$trialName))==1) {
                PersonParameters<-p[names(p)==unique(PersonsTaskData$trialName)]
                PersonFullData<-cbind(PersonsTaskData, PersonParameters)
                names(PersonFullData)<-c(names(PersonsTaskData), c("Mean", "SD", "Hint", "Result", "Level", "TrialNumber")) #rename columns to avoid renaming by spreadsheet
            } else {print("ERROR - multiple spreadsheets used by one person")}
        df<-rbind(df,PersonFullData)
    }

    #append demos
    dbound<-dplyr::bind_rows(d[[1]], d[[2]])
    df_withdemos<-left_join(df, dbound, by="Participant.Private.ID")
    df_withdemos<-df_withdemos[,-c(46:159)]
    #append attention check datas
        #merge it first
            a<-rbind(a[[1]], a[[2]])
        # and append
        full_df<-left_join(df_withdemos, a, by=c("Participant.Private.ID" = "private_id"))

    tibble<-as_tibble(full_df)

# tibble processing - data cleaning and column tidying

    #exclusions
        #record exclusions based on incomplete or repeated games
            excludeRepeatOrIncomplete <- length(IDs) -length(unique(tibble$Participant.Private.ID))


############################################################################################################################################################
#Creating tibbles for different cut off points 
############################################################################################################################################################
    ##########################################
    #mixed
    #########################################
        #exclude based on trial performance and symptom scale attention
            #get table of all reasons for exclusion for each participant
                ExclusionBasis<-tibble %>%
                    group_by(Participant.Private.ID) %>%
                    summarise(  nomoveX=sum(distanceMovedX==0), 
                                nomoveY=sum(distanceMovedY==0), 
                                nomoveXY=sum(distanceMovedX==0 & distanceMovedY==0), 
                                overtime=sum(timeToComplete>5000), 
                                undertime=sum(timeToComplete<1000), 
                                zerotime=sum(timeToComplete==0),
                                attentionCheck1= sum(first(at1_correct)!="yes" | is.na(first(at1_correct))) ,
                                attentionCheck2= sum(first(at2_correct)!="yes" | is.na(first(at2_correct))) , 
                                attentionCheck3= sum(first(at3_correct)!="yes" | is.na(first(at3_correct))) ) %>%
                    summarise(  Participant.Private.ID=Participant.Private.ID, 
                                excludeX= nomoveX>108, 
                                excludeY=nomoveY>108, 
                                excludeXY= nomoveXY >21, 
                                excludeOvertime= overtime >21, 
                                excludeUndertime= undertime >108,
                                excludeAttention = attentionCheck1+attentionCheck2+attentionCheck3>2) #, excludeZerotime= zerotime >0)
            #extract indices of participants to exclude
                IndicesToExclude<-ExclusionBasis %>%
                                select(c("excludeXY","excludeUndertime", "excludeAttention")) %>%
                                rowSums()>0
                IdsToExclude<-ExclusionBasis$Participant.Private.ID[IndicesToExclude]

        ############################################################################################################################################################
        #10%
        ############################################################################################################################################################

            ExclusionBasis_21<-tibble %>%
                    group_by(Participant.Private.ID) %>%
                    summarise(  nomoveX=sum(distanceMovedX==0), 
                                nomoveY=sum(distanceMovedY==0), 
                                nomoveXY=sum(distanceMovedX==0 & distanceMovedY==0), 
                                overtime=sum(timeToComplete>5000), 
                                undertime=sum(timeToComplete<1000), 
                                zerotime=sum(timeToComplete==0),
                                attentionCheck1= sum(first(at1_correct)!="yes" | is.na(first(at1_correct))) ,
                                attentionCheck2= sum(first(at2_correct)!="yes" | is.na(first(at2_correct))) , 
                                attentionCheck3= sum(first(at3_correct)!="yes" | is.na(first(at3_correct))) ) %>%
                    summarise(  Participant.Private.ID=Participant.Private.ID, 
                                excludeX= nomoveX>21, 
                                excludeY=nomoveY>21, 
                                excludeXY= nomoveXY >21, 
                                excludeOvertime= overtime >21, 
                                excludeUndertime= undertime >21,
                                excludeAttention = attentionCheck1+attentionCheck2+attentionCheck3>2) #, excludeZerotime= zerotime >0)
            #extract indices of participants to exclude
                IndicesToExclude<-ExclusionBasis %>%
                                select(c("excludeXY","excludeUndertime", "excludeAttention")) %>%
                                rowSums()>0
                IdsToExclude<-ExclusionBasis$Participant.Private.ID[IndicesToExclude]

        ############################################################################################################################################################
        #50%
        ############################################################################################################################################################

            ExclusionBasis_50<-tibble %>%
                    group_by(Participant.Private.ID) %>%
                    summarise(  nomoveX=sum(distanceMovedX==0), 
                                nomoveY=sum(distanceMovedY==0), 
                                nomoveXY=sum(distanceMovedX==0 & distanceMovedY==0), 
                                overtime=sum(timeToComplete>5000), 
                                undertime=sum(timeToComplete<1000), 
                                zerotime=sum(timeToComplete==0),
                                attentionCheck1= sum(first(at1_correct)!="yes" | is.na(first(at1_correct))) ,
                                attentionCheck2= sum(first(at2_correct)!="yes" | is.na(first(at2_correct))) , 
                                attentionCheck3= sum(first(at3_correct)!="yes" | is.na(first(at3_correct))) ) %>%
                    summarise(  Participant.Private.ID=Participant.Private.ID, 
                                excludeX= nomoveX>108, 
                                excludeY=nomoveY>108, 
                                excludeXY= nomoveXY >108, 
                                excludeOvertime= overtime >108, 
                                excludeUndertime= undertime >108,
                                excludeAttention = attentionCheck1+attentionCheck2+attentionCheck3>2) #, excludeZerotime= zerotime >0)
            #extract indices of participants to exclude
                IndicesToExclude<-ExclusionBasis %>%
                                select(c("excludeXY","excludeUndertime", "excludeAttention")) %>%
                                rowSums()>0
                IdsToExclude<-ExclusionBasis$Participant.Private.ID[IndicesToExclude]

        ############################################################################################################################################################
        #90%
        ############################################################################################################################################################

            ExclusionBasis_90<-tibble %>%
                    group_by(Participant.Private.ID) %>%
                    summarise(  nomoveX=sum(distanceMovedX==0), 
                                nomoveY=sum(distanceMovedY==0), 
                                nomoveXY=sum(distanceMovedX==0 & distanceMovedY==0), 
                                overtime=sum(timeToComplete>5000), 
                                undertime=sum(timeToComplete<1000), 
                                zerotime=sum(timeToComplete==0),
                                attentionCheck1= sum(first(at1_correct)!="yes" | is.na(first(at1_correct))) ,
                                attentionCheck2= sum(first(at2_correct)!="yes" | is.na(first(at2_correct))) , 
                                attentionCheck3= sum(first(at3_correct)!="yes" | is.na(first(at3_correct))) ) %>%
                    summarise(  Participant.Private.ID=Participant.Private.ID, 
                                excludeX= nomoveX>194, 
                                excludeY=nomoveY>194, 
                                excludeXY= nomoveXY >194, 
                                excludeOvertime= overtime >194, 
                                excludeUndertime= undertime >194,
                                excludeAttention = attentionCheck1+attentionCheck2+attentionCheck3>2) #, excludeZerotime= zerotime >0)
            #extract indices of participants to exclude
                IndicesToExclude<-ExclusionBasis %>%
                                select(c("excludeXY","excludeUndertime", "excludeAttention")) %>%
                                rowSums()>0
                IdsToExclude<-ExclusionBasis$Participant.Private.ID[IndicesToExclude]


################################################################################################################################################
#plots
################################################################################################################################################
        ##################################################################
        #Plot 1. Showing all possible criteria for 50% cut off 
        ##################################################################
            ###########################################################################
            #Looking at trial scores between potential exclusion criteria: 50% setting
            ###########################################################################

                    Xy_50<-tibble %>%	
                                                        group_by(Participant.Private.ID, level) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))	%>%	
                                                        mutate(ExclXY=case_when(excludeXY==FALSE ~ "Movement", excludeXY==TRUE ~ "No movement")) %>%                
                                                        ggplot(aes(x=score, fill=ExclXY)) +
                                                        geom_density(alpha=0.3) +
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
                                                        labs(fill="Rover\n& Beam",
                                                            y = "Participants",
                                                            x = NULL) 

                    Xy_50<- Xy_50 + scale_fill_manual(values=c(Comp2, Comp3))

                    X_50<-tibble %>%	
                                                        group_by(Participant.Private.ID, level ) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))	%>%	
                                                        mutate(ExclX=case_when(excludeX==FALSE ~ "Movement", excludeX==TRUE ~ "No movement")) %>%                
                                                        ggplot(aes(x=score, fill=ExclX)) +
                                                        geom_density(alpha=0.3) +
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
                                                        labs(fill="Rover",
                                                            #y = "Density (participants)",
                                                            y=NULL,
                                                            x = NULL) 

                    X_50<-  X_50 + scale_fill_manual(values=c(Comp2, Comp3))


                    
                    Y_50<-tibble%>%
                                                        group_by(Participant.Private.ID, level ) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))	%>%	
                                                        mutate(ExclY=case_when(excludeY==FALSE ~ "Movement", excludeY==TRUE ~ "No movement")) %>%                
                                                        ggplot(aes(x=score, fill=ExclY)) +
                                                        geom_density(alpha=0.3) +
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
                                                        labs(fill="Beam",
                                                        y = "Participants",
                                                            x = NULL) 
                                                    # ggtitle("No Beam Movement (Y axis)")

                    Y_50<-  Y_50 + scale_fill_manual(values=c(Comp2, Comp3))
                                
                                                                        
                    UT_50<-tibble%>%
                                                        group_by(Participant.Private.ID, level ) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))	%>%	
                                                        mutate(ExclUT=case_when(excludeUndertime==FALSE ~ "Over 1000m/s", excludeUndertime==TRUE ~ "Under 1000m/s")) %>%                
                                                        ggplot(aes(x=score, fill=ExclUT)) +
                                                        geom_density(alpha=0.3) +
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
                                                        labs(fill="Trial Time",
                                                            #y = "Density (participants)",
                                                            y=NULL,
                                                            x = NULL) 
 
                    UT_50<- UT_50+ scale_fill_manual(values=c(Comp2, Comp3))


                    OT_50<-tibble%>%
                                                        group_by(Participant.Private.ID, level ) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))	%>%	
                                                        mutate(ExclOT=case_when(excludeOvertime==FALSE ~ "Under 5000m/s", excludeOvertime==TRUE ~ "Over 5000m/s")) %>%                
                                                        ggplot(aes(x=score, fill=ExclOT)) +
                                                        geom_density(alpha=0.3) +
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
                                                        labs(fill="Trial Time",
                                                            y = "Participants",
                                                            #y=NULL,
                                                            x = "Score") 
 
                    OT_50<- OT_50+ scale_fill_manual(values=c(Comp2, Comp3))

                    
                    ATT_50<-tibble%>%
                                                        group_by(Participant.Private.ID, level ) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))	%>%	
                                                        mutate(ExclATT=case_when(excludeAttention==FALSE ~ "Passed", excludeAttention==TRUE ~ "Failed")) %>%                
                                                        ggplot(aes(x=score, fill=ExclATT)) +
                                                        geom_density(alpha=0.3) +
                                                        theme_classic() +
                                                        theme(axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            legend.justification = c("right", "top"),
                                                            legend.position = c(.95, .95),
                                                            legend.box.background = element_rect(),
                                                            legend.key = element_rect(fill = "white", colour = "black"),
                                                            legend.box.margin = margin(6, 6, 6, 6),
                                                            legend.title = element_text(size = 10, colour = "black"),
                                                            #legend.title = element_blank(),
                                                            legend.text = element_text(size = 8, colour = "black")) +
                                                        labs(fill="Attention Check",
                                                            #y = "Participants",
                                                            y=NULL,
                                                            x = "Score") 
 
                    ATT_50<- ATT_50+ scale_fill_manual(values=c(Comp2, Comp3))

                #arrange and print plots 
                    Methodschap_Genpop1_plot1_allcriteria50<-egg::ggarrange(Xy_50, X_50, Y_50, UT_50, OT_50, ATT_50, labels = c("A","B","C","D", "E", "F"))
                    ggsave(Methodschap_Genpop1_plot1_allcriteria50, file=file.path(path_results,"Methodschap_Genpop1_plot1_allcriteria50.pdf")) 

        ########################################################################################
        #Plot 2: Showing an example critiera (XY) across multiple cut off points 
        ########################################################################################
                 XY_10<-tibble %>%	
                                            group_by(Participant.Private.ID, level) %>%
                                            summarise(score=sum(trialScore)) %>%
                                            left_join(ExclusionBasis_21, by=c("Participant.Private.ID"))	%>%	
                                            mutate(ExclXY=case_when(excludeXY==FALSE ~ "Movement in at\nleast 10% of\ntrials", excludeXY==TRUE ~ "No movement\nin at least\n10% of\ntrials")) %>%                
                                            ggplot(aes(x=score, fill=ExclXY)) +
                                            geom_density(alpha=0.3) +
                                            theme_classic() +
                                            ylim(0,0.02) +
                                            theme(axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank(),
                                                legend.justification = c("right", "top"),
                                                legend.position = c(.95, .95),
                                                legend.box.background = element_rect(),
                                                legend.key = element_rect(fill = "white", colour = "black"),
                                                legend.box.margin = margin(6, 6, 6, 6),
                                                legend.title = element_text(size = 10, colour = "black"),
                                                #legend.title= element_blank(),
                                                legend.text = element_text(size = 8, colour = "black")) +
                                            labs(fill="Rover\n& Beam",
                                                y = "Participants",
                                                #x = "Score"
                                                x=NULL) 
                             
                XY_10<- XY_10 + scale_fill_manual(values=c(Comp1, Comp4))
                
                
                XY_50<-tibble %>%	
                                                        group_by(Participant.Private.ID, level) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))	%>%	
                                                        mutate(ExclXY=case_when(excludeXY==FALSE ~ "Movement", excludeXY==TRUE ~ "No movement")) %>%                
                                                        ggplot(aes(x=score, fill=ExclXY)) +
                                                        geom_density(alpha=0.3) +
                                                        ylim(0,0.02) +
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
                                                        labs(fill="Rover\n& Beam (50%)",
                                                            y = "Participants",
                                                            x = NULL) 

                    XY_50<- XY_50 + scale_fill_manual(values=c(Comp2, Comp3))

                    XY_90<-tibble %>%	
                                            group_by(Participant.Private.ID, level) %>%
                                            summarise(score=sum(trialScore)) %>%
                                            left_join(ExclusionBasis_90, by=c("Participant.Private.ID"))	%>%	
                                            mutate(ExclXY=case_when(excludeXY==FALSE ~ "Movement", excludeXY==TRUE ~ "No movement")) %>%                
                                            ggplot(aes(x=score, fill=ExclXY)) +
                                            geom_density(alpha=0.3) +
                                            theme_classic() +
                                            ylim(0,0.02) +
                                            theme(axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank(),
                                                legend.justification = c("right", "top"),
                                                legend.position = c(.95, .95),
                                                legend.box.background = element_rect(),
                                                legend.key = element_rect(fill = "white", colour = "black"),
                                                legend.box.margin = margin(6, 6, 6, 6),
                                                legend.title = element_text(size = 10, colour = "black"),
                                                legend.text = element_text(size = 8, colour = "black")) +
                                            labs(fill="Rover\n& Beam\n(90% trials)",
                                                y = "Participants",
                                                #x = "Score"
                                                x=NULL) 
                          
                    XY_90<- XY_90 + scale_fill_manual(values=c(Comp6, Comp5))

                    #arrange and print plots 
                    Methodschap_plot2_XY10_50_90<-egg::ggarrange(XY_10, XY_50, XY_90, labels = c("G","H","I"))
                    ggsave(Methodschap_plot2_XY10_50_90, file=file.path(path_results,"Methodschap_plot2_XY10_50_90.pdf")) 

        ####################################################################################################################################
        #table showing cumulative exclusions across data cateogries and criteria
        ####################################################################################################################################
                #make main table    
                    track_21<-c(excludeRepeatOrIncomplete=excludeRepeatOrIncomplete, 
                            ExclusionBasis_21 %>% select(excludeXY, excludeUndertime, excludeAttention, excludeOvertime) %>%summarise_if( is.logical, sum))  

                    track_50<-c(excludeRepeatOrIncomplete=excludeRepeatOrIncomplete, 
                            ExclusionBasis_50 %>% select(excludeXY, excludeUndertime, excludeAttention, excludeOvertime) %>%summarise_if( is.logical, sum))   

                    track_90<-c(excludeRepeatOrIncomplete=excludeRepeatOrIncomplete, 
                            ExclusionBasis_90 %>% select(excludeXY, excludeUndertime, excludeAttention, excludeOvertime) %>%summarise_if( is.logical, sum)) 

                #calculate cumulative exclusions
                CumulativeExclusionsTibble_21<-ExclusionBasis_21 %>%
                    group_by(Participant.Private.ID)%>%
                        summarise(  excludex=excludeX>0,
                                    excludex_y=sum(across(excludeX: excludeY))>0, # underscore means across from column a to column c with all coolumns inbetween included
                                    excluexy=excludeXY>0,
                                    excludex_over=sum(across(excludeX:excludeOvertime))>0,
                                    excludexy_under=sum(across(excludeXY:excludeUndertime))>0,
                                    excludexy_attent=sum(across(excludeXY:excludeAttention))>0,
                                    excludexyANDunder=sum(across(c(excludeXY,excludeUndertime)))>0,
                                    excludexyANDunderANDattent=sum(across(c(excludeXY,excludeUndertime,excludeAttention)))>0,
                                    excludexyANDunderANDattentANDovertime=sum(across(c(excludeXY,excludeUndertime,excludeAttention,excludeOvertime)))>0) %>%
                        select(excluexy, excludexyANDunder, excludexyANDunderANDattent,excludexyANDunderANDattentANDovertime)
                        
                    CumulativeExclusions_21<-CumulativeExclusionsTibble_21 %>%
                                            summarise_if( is.logical, sum)    

                CumulativeExclusionsTibble_50<-ExclusionBasis_50 %>%
                    group_by(Participant.Private.ID)%>%
                        summarise(  excludex=excludeX>0,
                                    excludex_y=sum(across(excludeX: excludeY))>0, # underscore means across from column a to column c with all coolumns inbetween included
                                    excluexy=excludeXY>0,
                                    excludex_over=sum(across(excludeX:excludeOvertime))>0,
                                    excludexy_under=sum(across(excludeXY:excludeUndertime))>0,
                                    excludexy_attent=sum(across(excludeXY:excludeAttention))>0,
                                    excludexyANDunder=sum(across(c(excludeXY,excludeUndertime)))>0,
                                    excludexyANDunderANDattent=sum(across(c(excludeXY,excludeUndertime,excludeAttention)))>0,
                                    excludexyANDunderANDattentANDovertime=sum(across(c(excludeXY,excludeUndertime,excludeAttention,excludeOvertime)))>0) %>%
                        select(excluexy, excludexyANDunder, excludexyANDunderANDattent,excludexyANDunderANDattentANDovertime) 
                        
                    CumulativeExclusions_50<-CumulativeExclusionsTibble_50 %>%
                                            summarise_if( is.logical, sum)    

                    CumulativeExclusionsTibble_90<-ExclusionBasis_90 %>%
                    group_by(Participant.Private.ID)%>%
                        summarise(  excludex=excludeX>0,
                                    excludex_y=sum(across(excludeX: excludeY))>0, # underscore means across from column a to column c with all coolumns inbetween included
                                    excluexy=excludeXY>0,
                                    excludex_over=sum(across(excludeX:excludeOvertime))>0,
                                    excludexy_under=sum(across(excludeXY:excludeUndertime))>0,
                                    excludexy_attent=sum(across(excludeXY:excludeAttention))>0,
                                    excludexyANDunder=sum(across(c(excludeXY,excludeUndertime)))>0,
                                    excludexyANDunderANDattent=sum(across(c(excludeXY,excludeUndertime,excludeAttention)))>0,
                                    excludexyANDunderANDattentANDovertime=sum(across(c(excludeXY,excludeUndertime,excludeAttention,excludeOvertime)))>0) %>%
                        select(excluexy, excludexyANDunder, excludexyANDunderANDattent,excludexyANDunderANDattentANDovertime) 
                    
                        
                    CumulativeExclusions_90<-CumulativeExclusionsTibble_90 %>%
                                            summarise_if( is.logical, sum)     


                    #make final tracking table
                    track_21<-rbind( track_21,
                            c(track_21["excludeRepeatOrIncomplete"], CumulativeExclusions_21+track_21$excludeRepeatOrIncomplete))
                    rownames(track_21)<-c("independent", "cumulative")

                    track_50<-rbind(  track_50,
                            c(track_50["excludeRepeatOrIncomplete"], CumulativeExclusions_50+track_50$excludeRepeatOrIncomplete))
                    rownames(track_50)<-c("independent", "cumulative")   

                    track_90<-rbind(  track_90,
                            c(track_90["excludeRepeatOrIncomplete"], CumulativeExclusions_90+track_90$excludeRepeatOrIncomplete))
                    rownames(track_90)<-c("independent", "cumulative")


                
        
        #Stats: Model comparison on 50% and 10% criteria 
            #10
                    aov1<- tibble %>%	
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_21, by=c("Participant.Private.ID"))	
                                aov1<-aov(score~excludeXY, aov1)

                    aov2<- tibble %>%	
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_21, by=c("Participant.Private.ID"))	
                                aov2<-aov(score~excludeXY + excludeUndertime, aov2)

                    #model looking at XY together
                    aov5<- tibble %>%	####winning model
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_21, by=c("Participant.Private.ID"))
                                aov5<-aov(score~excludeXY + excludeUndertime + excludeAttention + excludeOvertime, aov5)
                                summary(aov5)
                                
                    #model looking at XY seperately 
                    aov6<- tibble %>%	
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_21, by=c("Participant.Private.ID"))
                                aov6<-aov(score~excludeX + excludeY + excludeUndertime + excludeAttention + excludeOvertime, aov6)


                aov_10s<-anova(aov1, aov2, aov5, aov6, test="Chisq")

        #50
                    aov7<- tibble %>%	
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))	
                                aov7<-aov(score~excludeXY, aov7)

                    aov8<- tibble %>%	
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))	
                                aov8<-aov(score~excludeXY + excludeUndertime, aov8)

                    #model looking at XY together
                    aov9_tib<- tibble %>%	####winning model
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))
                                aov9<-lm(score~excludeXY + excludeUndertime + excludeAttention + excludeOvertime, aov9_tib)
                                summary(aov9)

                    x<-    aov9_tib %>% pivot_longer(
                                                        cols = excludeX:excludeAttention,
                                                        #names_to = c("diagnosis", "gender", "age"),
                                                        #names_pattern = "new_?(.*)_(.)(.*)",
                                                        values_to = "count"
                                                        ) %>%
                                                        mutate_if(is.logical, as.numeric) %>%
                                                        group_by(name) %>%
                                                        summarise(counttot=sum(count), scoresum=sum(score))
                                    ggplot(aes(x=score, y=count, fill=name)) +
                                    geom_density()

                            aov95<- tibble %>%	####winning model
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))
                                aov95<-aov(score~excludeXY*excludeUndertime + excludeAttention + excludeOvertime, aov95)
                                summary(aov95)
                    
                    #model looking at XY seperately 
                    aov10<- tibble %>%	
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_50, by=c("Participant.Private.ID"))
                                aov10<-aov(score~excludeX + excludeY + excludeUndertime + excludeAttention + excludeOvertime, aov10)


                aov_50s<-anova(aov7, aov8, aov9, aov10, aov95, test="Chisq")

                    
                    
                #90
                    aov11<- tibble %>%	
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_90, by=c("Participant.Private.ID"))	
                                aov11<-aov(score~excludeXY, aov11)

                    aov12<- tibble %>%	
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_90, by=c("Participant.Private.ID"))	
                                aov12<-aov(score~excludeXY + excludeUndertime, aov12)

                    #model looking at XY together
                    aov13<- tibble %>%	####winning model
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_90, by=c("Participant.Private.ID"))
                                aov13<-aov(score~excludeXY + excludeUndertime + excludeAttention + excludeOvertime, aov13)
                                summary(aov13)
                                
                    #model looking at XY seperately 
                    aov14<- tibble %>%	
                                group_by(Participant.Private.ID) %>%
                                summarise(score=sum(trialScore)) %>%
                                left_join(ExclusionBasis_90, by=c("Participant.Private.ID"))
                                aov14<-aov(score~excludeX + excludeY + excludeUndertime + excludeAttention + excludeOvertime, aov14)
                                summary(aov14)

                aov_90s<-anova(aov11, aov12, aov13, aov14, test="Chisq")
                    
                    #Plot 3: Showing outcomes of best model 




