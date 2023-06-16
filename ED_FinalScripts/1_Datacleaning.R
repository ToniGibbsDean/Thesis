
#########################################
#Eating Disorder Data: make simple tibble
#########################################
    	rm(list=ls())
	    set.seed(0.1)

################################################################################################################
#read in packages and files 
################################################################################################################
            # Notes: currently parameter data doesn't contain means or standard deviations, fix the input to address.
                library(ggplot2)
                    #install.packages("gridExtra")
                    library(gridExtra)
                    #install.packages("stats")
                    library(stats)
                    library(Hmisc)
                    library(tidyverse)

            path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/EatingDisorder/"

            #parameters
            CorrectGameLength<-219
            ReducedCriteria<-TRUE
            exclusiontype<-"Split1_incCovariates_" #the name of the exclusion set you are using - will be used to prefix filenames

            # load data
                pfiles<-list.files(file.path(path, "Data", "Parameters"))
                tfiles<-list.files(file.path(path, "Data", "Task_SymptomData"))
                cfiles<-list.files(file.path(path, "Data", "Covariates"))

                p<-lapply(file.path(path,  "Data", "Parameters", pfiles), read.csv, header=FALSE, col.names=c("Cue", "Target", "Level"))
                names(p)<-substr(pfiles, start=1, stop=nchar(pfiles)-4)

                t<-lapply(file.path(path, "Data", "Task_SymptomData", tfiles), read.csv)
                names(t)<-tfiles

                #when read in like this you cant get the names for some reason so for now using the other
                #c<-lapply(file.path(path, "Data", "Covariates", cfiles), read.csv, header=TRUE)
                c<-read.csv("/Users/tonigibbs-dean/Documents/SpaceTaskProject/EatingDisorder/Data/Covariates/EDCOVID19_CRISIS_DSM5_data_010822.csv")

            #combine data
                #filter task data to parameter sheets which match those we have
                t_matched<-t[[1]][t[[1]]$trialName %in% names(p),]

                #for all poeple with correct game length append columns with their parameter data
                IDs<-unique(t_matched$Participant.Private.ID)
                df<-c()
                for (ID in IDs) {
                    PersonsTaskData<-t_matched[t_matched$Participant.Private.ID==ID,]
                    gameLength<-dim(PersonsTaskData)[1]
                    if (gameLength==CorrectGameLength) {
                        if(length(unique(PersonsTaskData$trialName))==1) {
                            PersonParameters<-p[names(p)==unique(PersonsTaskData$trialName)]
                            PersonFullData<-cbind(PersonsTaskData, PersonParameters, c(1:CorrectGameLength))
                            names(PersonFullData)<-c(names(PersonsTaskData), c("Hint", "Result", "Level", "TrialNumber")) #rename columns to avoid renaming by spreadsheet
                            df<-rbind(df,PersonFullData)
                        } else {print("ERROR - multiple spreadsheets used by one person")}
                    }
                }

                tibbletemp<-as_tibble(df)
                
                tibble <- tibbletemp %>%
                                    left_join(c[c(1, 109:153, 157:192)], by=c('Participant.Private.ID'), copy=TRUE)

################################################################################################################
#testing and applying exclusion critiera 
################################################################################################################
            # tibble processing - data cleaning and column tidying

                #exclusions
                    #record exclusions based on incomplete or repeated games
                        excludeRepeatOrIncomplete <- length(IDs) -length(unique(tibble$Participant.Private.ID))
                        RepeatOrIncompleteIds<-IDs[!IDs %in% unique(tibble$Participant.Private.ID)]
                        GroupsOfRepeatOrExcluded<-t_matched %>% group_by(Participant.Private.ID) %>%
                            summarise(first(group_recruit)) %>%
                            filter(Participant.Private.ID %in% RepeatOrIncompleteIds) %>%
                            table() %>%
                            colSums()
                        

                    #exclude based on trial performance and symptom scale attention
                        #get table of all reasons for exclusion for each participant
                    ExclusionBasis<-tibble %>%
                                group_by(Participant.Private.ID) %>%
                                reframe(  nomoveX=sum(distanceMovedX==0), 
                                            nomoveY=sum(distanceMovedY==0), 
                                            nomoveXY=sum(distanceMovedX==0 & distanceMovedY==0), 
                                            overtime=sum(timeToComplete>5000), 
                                            undertime=sum(timeToComplete<1000),
                                            zerotime=sum(timeToComplete==0),
                                            att_check=na.omit(sum(att_check_1!="2")),
                                            group=first(group_recruit) ) %>%
                                reframe(  Participant.Private.ID=Participant.Private.ID,
                                            excludeX= nomoveX>108, 
                                            excludeY=nomoveY>108, 
                                            excludeXY= nomoveXY >21, 
                                            excludeOvertime= overtime >21, 
                                            excludeUndertime= undertime >108,
                                            att_check=att_check>0,
                                            group=group ) 
                        #extract indices of participants to exclude
                            IndicesToExclude<-ExclusionBasis %>%
                                            select(c("excludeXY","excludeUndertime", "att_check")) %>%
                                            rowSums()>0
                            IdsToExclude<-ExclusionBasis$Participant.Private.ID[IndicesToExclude]
                                        
                
                            
                        if(ReducedCriteria) {

                            #create exclusions tracking table  
                            #make main table    
                            track<-c(excludeRepeatOrIncomplete=excludeRepeatOrIncomplete, 
                                        ExclusionBasis %>% 
                                                    select(excludeXY, excludeUndertime, att_check, group) %>%
                                                    summarise_if( is.logical, sum)) 
                            #calculate cumulative exclusions
                                CumulativeExclusionsTibble<-ExclusionBasis %>%
                                group_by(Participant.Private.ID)%>%
                                    summarise(  excludex=excludeX>0,
                                                excludex_y=sum(across(excludeX: excludeY))>0, # underscore means across from column a to column c with all coolumns inbetween included
                                                excluexy=excludeXY>0,
                                                excludex_over=sum(across(excludeX:excludeOvertime))>0,
                                                excludexy_under=sum(across(excludeXY:excludeUndertime))>0,
                                                excludexyANDunder=sum(across(c(excludeXY,excludeUndertime)))>0,
                                                excludexy_underANDattchk=sum(across(c(excludeXY, excludeUndertime, att_check)))>0,
                                                group=group) %>%
                                    select(excluexy, excludexyANDunder, excludexy_underANDattchk, group)
                                    
                                CumulativeExclusions<-CumulativeExclusionsTibble %>%
                                                        summarise_if( is.logical, sum) 


                                #use cumulative exclusions to calculate number of exclusions in each group
                                    groupwiseExclusionNumbers<-CumulativeExclusionsTibble %>%
                                        group_by(group) %>%
                                        summarise(xy=sum(excluexy), xyANDunder=sum(excludexyANDunder), excludexy_underANDattchk=sum(excludexy_underANDattchk))  %>%
                                        select(-group)
                        } else {

                            #create exclusions tracking table  
                            #make main table    
                            track<-c(excludeRepeatOrIncomplete=excludeRepeatOrIncomplete, 
                                        summarise_if(ExclusionBasis, is.logical, sum))  
                            #calculate cumulative exclusions
                                CumulativeExclusionsTibble<-ExclusionBasis %>%
                                group_by(Participant.Private.ID)%>%
                                    summarise(  excludex=excludeX>0,
                                                excludex_y=sum(across(excludeX: excludeY))>0, # underscore means across from column a to column c with all coolumns inbetween included
                                                excluex_xy=sum(across(excludeX: excludeXY))>0,
                                                excludex_over=sum(across(excludeX:excludeOvertime))>0,
                                                excludex_under=sum(across(excludeX:excludeUndertime))>0,
                                                group=group) 
                                    
                                CumulativeExclusions<-CumulativeExclusionsTibble %>%
                                                        summarise_if( is.logical, sum) 
                                                        
                                #use cumulative exclusions to calculate number of exclusions in each group
                                    groupwiseExclusionNumbers<-CumulativeExclusionsTibble %>%
                                        group_by(group) %>%
                                        summarise_if( is.logical, sum)  %>%
                                        select(-group)
                        }


                                
                        #append cumulative exclusions
                        groupwiseExclusionNumbersWithRepeatsAndIncompletes<-groupwiseExclusionNumbers+GroupsOfRepeatOrExcluded
                        cumulativeGroupwise<-cbind(GroupsOfRepeatOrExcluded,groupwiseExclusionNumbersWithRepeatsAndIncompletes)
                        names(cumulativeGroupwise)<-names(track)
                        names(CumulativeExclusions)<-names(track)[-1]

                        track<-rbind(  track,
                                c(track["excludeRepeatOrIncomplete"], CumulativeExclusions+track$excludeRepeatOrIncomplete),
                                cumulativeGroupwise
                                )
                        rownames(track)<-c("independent", "cumulative", "ED_cumulative", "HC_cumulative")


############################################################################################################################################################################################
#plots
############################################################################################################################################################################################


                #Looking at trial scores between potential exclusion criteria 
                #strict thresholds, and all criteria as a starting point. 
                StrictXYwholegameAllmetrics<-list()
                pdf(file="/Users/tonigibbs-dean/Documents/SpaceTask_Project/Results/DataCleaningFigs_StrictXYwholegameAllmetrics.pdf")

                    StrictXYwholegameAllmetrics[["l2strictXY"]]<-tibble %>%	
                                                        group_by(Participant.Private.ID, Level) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                        drop_na %>%
                                                        ggplot(aes(x=score, fill=excludeXY, alpha=0.1)) +
                                                        geom_density() +
                                                        ggtitle("A. No beam/rover movement") +
                                                        theme_classic() +
                                                        theme(legend.position="none") +
                                                        labs(x="Total Score", y="Density") +

                                                        #aov<-aov(score~excludeXY, x)
                    StrictXYwholegameAllmetrics[["l3strictXY"]]<-tibble %>%	
                                                        group_by(Participant.Private.ID, Level ) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                        drop_na %>%
                                                        ggplot(aes(x=score, fill=excludeX, alpha=0.1)) +
                                                        geom_density() +
                                                        ggtitle("No Rover Movement (X axis)") +
                                                        theme_classic() +
                                                        theme(legend.position="none") +
                                                        labs(x="Total Score", y="Density") +

                                                        #aov<-aov(score~excludeXY, x)
                    StrictXYwholegameAllmetrics[["l4strictXY"]] <-tibble%>%
                                                        group_by(Participant.Private.ID, Level ) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                        drop_na %>%
                                                        ggplot(aes(x=score, fill=excludeY, alpha=0.1)) +
                                                        geom_density() +
                                                        ggtitle("No Beam Movement (Y axis)") +
                                                        theme_classic() +
                                                        theme(legend.position="none") +
                                                        labs(x="Total Score", y="Density") +

                                                        #aov<-aov(score~excludeXY, x)
                    StrictXYwholegameAllmetrics[["l5strictXY"]]<-tibble%>%
                                                        group_by(Participant.Private.ID, Level ) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                        drop_na %>%
                                                        ggplot(aes(x=score, fill=excludeOvertime, alpha=0.1)) +
                                                        geom_density() +
                                                        ggtitle("Overtime (>5000ms)") +
                                                        theme_classic() +
                                                        theme(legend.position="none") +
                                                        labs(x="Total Score", y="Density") +

                                                        #aov<-aov(score~excludeXY, x)
                    StrictXYwholegameAllmetrics[["l6strictXY"]]<-tibble%>%
                                                        group_by(Participant.Private.ID, Level ) %>%
                                                        summarise(score=sum(trialScore)) %>%
                                                        left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                        drop_na %>%
                                                        ggplot(aes(x=score, fill=excludeUndertime, alpha=0.1)) +
                                                        geom_density() +
                                                        ggtitle("Undertime (<1000ms)") +
                                                        theme_classic() +
                                                        theme(legend.position="none") +
                                                        labs(x="Total Score", y="Density") +

                                                        #aov<-aov(score~excludeXY, x)

                plots<-grid.arrange(grobs=StrictXYwholegameAllmetrics, colnums=3)
                path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/EatingDisorder/Results"

                            ggsave(plots, file=file.path(path,"datacleaning_allexccrit.pdf"))

                #plots <-plots + scale_fill_manual(values=c("Comp1", "Comp2"))

                dev.off()


                #trialscore x level and nonmovers and undertimers - plot to show the similarities between undertimers and nonmovers
                    #evdience that they are the same group

                        #non movers 
                        l2<-tibble %>%	
                                                    group_by(Participant.Private.ID, Level) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                    drop_na %>%
                                                    filter(Level=="2") 	%>%		
                                                    ggplot(aes(x=score, fill=excludeXY)) +
                                                    geom_density(alpha=0.5) +
                                                    ggtitle("Block 2") +
                                                    theme_classic() +
                                                        theme(legend.position="none") +
                                                        theme(axis.title.x=element_blank())+
                                                        labs(x="Total Score", y="Density excluded\nfor <no movement in 10% of trials",fill="Exclude") +
                                                        ylim(0,0.035) +
                                                        xlim(0,200)
                        l3<-tibble %>%	
                                                    group_by(Participant.Private.ID, Level ) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%
                                                    drop_na %>%	
                                                    filter(Level=="3")  %>%
                                                    ggplot(aes(x=score, fill=excludeXY, alpha=0.1)) +
                                                    geom_density() +
                                                    ggtitle("Block 3")+
                                                    theme_classic() +
                                                        theme(legend.position="none") +
                                                        #theme(axis.title.y=element_blank())+
                                                        theme(axis.title.x=element_blank())+
                                                        labs(x="Total Score", y="Density excluded\nfor <no movement in 10% of trials",fill="Exclude") +
                                                        ylim(0,0.035) +
                                                        xlim(-200,200)
                        l4 <-tibble%>%
                                                    group_by(Participant.Private.ID, Level ) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                    drop_na %>%
                                                    filter(Level=="4")  %>%
                                                    ggplot(aes(x=score, fill=excludeXY)) +
                                                    geom_density(alpha=0.5) +
                                                    ggtitle("Block 4")+
                                                    theme_classic() +
                                                    theme(axis.title.y=element_blank())+
                                                    theme(axis.title.x=element_blank())+
                                                #        theme(legend.position="none") +
                                                        labs(x="Total Score",fill="Exclude") +
                                                        ylim(0,0.035) +
                                                        xlim(-200,200)
                        l5<-tibble%>%
                                                    group_by(Participant.Private.ID, Level ) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                    drop_na %>%
                                                    filter(Level=="5")  %>%
                                                    ggplot(aes(x=score, fill=excludeXY, alpha=0.1)) +
                                                    geom_density() +
                                                    ggtitle("Block 5") +
                                                    theme_classic() +
                                                    theme(axis.title.y=element_blank())+
                                                        theme(legend.position="none") +
                                                        theme(axis.title.x=element_blank())+
                                                        labs(x="Total Score") +
                                                        ylim(0,0.035) +
                                                        xlim(-200,200)
                        l6<-tibble%>%
                                                    group_by(Participant.Private.ID, Level ) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                    drop_na %>%
                                                    filter(Level=="6")  %>%
                                                    ggplot(aes(x=score, fill=excludeXY, alpha=0.1)) +
                                                    geom_density() +
                                                    ggtitle("Block 6") +
                                                    theme_classic() +
                                                        theme(legend.position="none") +
                                                        theme(axis.title.y=element_blank())+
                                                        theme(axis.title.x=element_blank())+
                                                        labs(x="Total Score") +
                                                        ylim(0,0.035) +
                                                        xlim(-200,200)


                
                ## undertimers 
                        l2under<-tibble %>%	
                                                    group_by(Participant.Private.ID, Level) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                    drop_na %>%
                                                    filter(Level=="2") 	%>%		
                                                    ggplot(aes(x=score, fill=excludeUndertime)) +
                                                    geom_density(alpha=0.5) +
                                                    #ggtitle("Block 2") +
                                                    theme_classic() +
                                                        theme(legend.position="none") +
                                                        theme(axis.title.x=element_blank())+
                                                        labs(x="Total Score", y="Density excluded\nfor <1000ms in 10% of trials",fill="Exclude") +
                                                        ylim(0,0.035) +
                                                        xlim(0,200)
                        l3under<-tibble %>%	
                                                    group_by(Participant.Private.ID, Level ) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%
                                                    drop_na %>%	
                                                    filter(Level=="3")  %>%
                                                    ggplot(aes(x=score, fill=excludeUndertime, alpha=0.1)) +
                                                    geom_density() +
                                                    #ggtitle("Block 3")+
                                                    theme_classic() +
                                                        theme(legend.position="none") +
                                                        #theme(axis.title.y=element_blank())+
                                                        theme(axis.title.x=element_blank())+
                                                        labs(x="Total Score", y="Density excluded\nfor <1000ms in 10% of trials",fill="Exclude") +
                                                        ylim(0,0.035) +
                                                        xlim(-200,200)
                        l4under <-tibble%>%
                                                    group_by(Participant.Private.ID, Level ) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                    drop_na %>%
                                                    filter(Level=="4")  %>%
                                                    ggplot(aes(x=score, fill=excludeUndertime)) +
                                                    geom_density(alpha=0.5) +
                                                    #ggtitle("Block 4")+
                                                    theme_classic() +
                                                    theme(axis.title.y=element_blank())+
                                                    theme(axis.title.x=element_blank())+
                                                #        theme(legend.position="none") +
                                                        labs(x="Total Score",  fill="Exclude") +
                                                        ylim(0,0.035) +
                                                        xlim(-200,200)
                        l5under<-tibble%>%
                                                    group_by(Participant.Private.ID, Level ) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                    drop_na %>%
                                                    filter(Level=="5")  %>%
                                                    ggplot(aes(x=score, fill=excludeUndertime, alpha=0.1)) +
                                                    geom_density() +
                                                    #ggtitle("Block 5") +
                                                    theme_classic() +
                                                        theme(legend.position="none") +
                                                        theme(axis.title.y=element_blank())+
                                                        theme(axis.title.x=element_blank())+
                                                        labs(x="Total Score") +
                                                        ylim(0,0.035) +
                                                        xlim(-200,200)
                        l6under<-tibble%>%
                                                    group_by(Participant.Private.ID, Level ) %>%
                                                    summarise(score=sum(trialScore)) %>%
                                                    left_join(ExclusionBasis, by=c("Participant.Private.ID"))	%>%	
                                                    drop_na %>%
                                                    filter(Level=="6")  %>%
                                                    ggplot(aes(x=score, fill=excludeUndertime, alpha=0.1)) +
                                                    geom_density() +
                                                    #ggtitle("Block 6") +
                                                    theme_classic() +
                                                        theme(legend.position="none") +
                                                        theme(axis.title.y=element_blank())+
                                                        theme(axis.title.x=element_blank())+
                                                        labs(x="Total Score") +
                                                        ylim(0,0.035) +
                                                        xlim(-200,200)

                
                x<-(l3 | l4 | l5 | l6) +  plot_annotation(title = 'Density of scores for group excluded and not excluded based on the non-mover criteria') 
                y<-(l3under| l4under| l5under| l6under) + plot_annotation(title = 'Density of scores for group excluded and not excluded based on <1000ms criteria')
                
                a<-x/y+ plot_layout(guides = 'collect')

                ggsave(a, file=file.path(path,"Results/datacleaning_comparingNonMovers_Undertimers.pdf"))



############################################################################################################################################################################################
#applying exclusion critiera and maknig a tibble
############################################################################################################################################################################################

            #apply exclusions
                tibble<-tibble[!tibble$Participant.Private.ID %in% IdsToExclude,]

            #column tidying
                # column removal and naming
                #tibble<-tibble[,c(1:26, 31:45,49)]

                # column na management
                tibble$success[is.na(tibble$success)]<-0

                #column additons
                tibble$PE<-abs(scale(tibble$originalResult, center = c(-0.5), scale = c(0.01)) - tibble$participantPosition)
                tibble$signedPE<-scale(tibble$originalResult, center = c(-0.5), scale = c(0.01)) - tibble$participantPosition
                tibble<-tibble %>%
                    group_by(Participant.Private.ID) %>%
                    mutate( signedPositionChange=lead(participantPosition)- participantPosition,
                            positionChange=abs(lead(participantPosition)- participantPosition),
                            LR = signedPositionChange/signedPE) 
                


                #tibble$PerfE<-abs(tibble$Mean - tibble$participantPosition)
                #tibble$signedPerfE<-tibble$Mean - tibble$participantPosition



            #save output
                write.csv(file=file.path(path, "Results", paste0(exclusiontype,"_EatingDisorderTrackingTableEatingDisorder.csv")), track)
                saveRDS(file=file.path(path, "Intermediate_outputs", paste0(exclusiontype,"_EatingDisorderTibble.RDS")), tibble)
