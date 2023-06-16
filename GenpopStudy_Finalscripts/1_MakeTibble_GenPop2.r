

#########################################
#GenPop2 Data: make simple tibble
#########################################
# Notes: 

library(tidyverse)
path<-"~/Dropbox/SpaceTask_Project"

#parameters
CorrectGameLength<-216 # no level 1

# load data
    pfiles<-list.files(file.path(path, "Data", "GenPop2", "Parameters"))
    tfiles<-list.files(file.path(path, "Data", "GenPop2", "TaskData"))
    dfiles<-list.files(file.path(path, "Data", "GenPop2", "Symptom_DemoData"))
    afiles<-list.files(file.path(path, "Data", "GenPop2", "AttentionChecks"))

    p<-lapply(file.path(path, "Data", "GenPop2", "Parameters", pfiles), read.csv, header=TRUE)
    names(p)<-substr(pfiles, start=1, stop=nchar(pfiles)-4)

    t<-lapply(file.path(path, "Data", "GenPop2", "TaskData", tfiles), read.csv)
    names(t)<-tfiles

    d<-lapply(file.path(path, "Data", "GenPop2", "Symptom_DemoData", dfiles), read.csv)
    names(d)<-dfiles

    a<-lapply(file.path(path, "Data", "GenPop2", "AttentionChecks", afiles), read.csv)
    names(a)<-afiles

#combine data

    #merge task datas
        t<-rbind(t[[1]])

    #for all poeple with correct game length append columns with their parameter data
        IDs<-unique(t$Participant.Private.ID)
        df<-c()
        for (ID in IDs) {
            PersonsTaskData<-t[t$Participant.Private.ID==ID,] %>%
                filter(!is.na(level))

            gameLength<-dim(PersonsTaskData)[1]
            if (gameLength==CorrectGameLength) {
                if(length(unique(PersonsTaskData$trialName))==1) {
                    PersonParameters<-p[names(p)==unique(PersonsTaskData$trialName)] [[1]]%>% filter(level!=1)
                    PersonFullData<-cbind(PersonsTaskData, PersonParameters)
                    names(PersonFullData)<-c(names(PersonsTaskData), c("Mean", "SD", "Hint", "Result", "Level", "TrialNumber")) #rename columns to avoid renaming by spreadsheet
                } else {print("ERROR - multiple spreadsheets used by one person")}
                df<-rbind(df,PersonFullData)
            }
        }

    #append demos
        df_withdemos<-left_join(df, d[[1]], by="Participant.Private.ID")
        d_combined<-full_join(d[[1]], d[[2]], by=c("Participant.Private.ID" = "p_id")) %>%
                full_join( d[[3]], by="Participant.Private.ID") %>%
                full_join( d[[4]], by="Participant.Private.ID") %>%
                full_join( d[[5]], by="Participant.Private.ID") %>%
                full_join( d[[6]], by="Participant.Private.ID") %>%
                full_join( d[[7]], by="Participant.Private.ID") 


        df_withdemos<-left_join(df, d_combined, by="Participant.Private.ID")


    #append attention check datas
        #merge it first
            full_df<-a[[1]] %>%
                        left_join(df_withdemos, ., by="Participant.Private.ID")

        tibble<-as_tibble(full_df)

# tibble processing - data cleaning and column tidying

    #exclusions
        #record exclusions based on incomplete or repeated games
            excludeRepeatOrIncomplete <- length(IDs) -length(unique(tibble$Participant.Private.ID))

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
                                excludeUndertime= undertime >21,
                                excludeAttention1 = attentionCheck1, 
                                excludeAttention2 = attentionCheck2,
                                excludeAttention3 = attentionCheck3) #, excludeZerotime= zerotime >0)
            #extract indices of participants to exclude
                IndicesToExclude<-ExclusionBasis %>%
                                select(starts_with("exclude")) %>%
                                rowSums()>0
                IdsToExclude<-ExclusionBasis$Participant.Private.ID[IndicesToExclude]
                            
        #create exclusions tracking table      
            track<-c(excludeRepeatOrIncomplete=excludeRepeatOrIncomplete,
                colSums(ExclusionBasis[,2:9]), 
                TotalExclusions=sum(IndicesToExclude) +  excludeRepeatOrIncomplete )

        #apply exclusions
            tibble<-tibble[!tibble$Participant.Private.ID %in% IdsToExclude,]

#column tidying
    # column removal and naming
    tibble<-tibble[,c(1:26, 116:130,134:176)]

    # column na management
    tibble$success[is.na(tibble$success)]<-0
    tibble$tutorialRepeated[is.na(tibble$tutorialRepeated)]<-0

    #column additons
    tibble$PE<-abs(tibble$originalResult - tibble$participantPosition)
    tibble$signedPE<-tibble$originalResult - tibble$participantPosition
    tibble$PerfE<-abs(tibble$Mean - tibble$participantPosition)
    tibble$signedPerfE<-tibble$Mean - tibble$participantPosition



#save output
    write.csv(file=file.path(path, "Results", "TrackingTableGenPop1.csv"), track)
    saveRDS(file=file.path(path, "IntermediateOutputs", "GenPop1Tibble.RDS"), tibble)
