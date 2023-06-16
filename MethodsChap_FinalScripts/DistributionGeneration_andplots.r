
        set.seed(0.2)

        source("/Users/tonigibbs-dean/Documents/SpaceTaskProject/Methods/Scripts/find.dist().r")


#producing the data for the trials
###################################
###################################
###################################



            #sd list

            sdcoedfficent###
            a<-0.6

            sd_a = 0.05*a
            sd_b = 0.1*a
            sd_c = 0.2*a

            #sd for practice round
            sd_d = 0.075*a

            #meanlist

            ###mean_a = 0
            ###mean_b = 0.05
            ###mean_c = -0.05

            #for rounds 5+6
            ###mean_d = +0.020
            ###mean_e = -0.030

            #for practice
            ###mean_f = -0.075

            #for round 6
            ###mean_g = -0.01
            ###mean_h = 0.035

            ######MEANLISTACTUAL

            #mean coefficient
            b<-0.7


            #for practice
            mean_f = -0.1*b

            ##L2 AND 3 
            mean_a = 0*b

            #L4 AND 5
            mean_b = 0.5*b
            mean_c = -0.5*b

            #L5 AND 6
            mean_d = 0.2*b
            mean_e = -0.3*b

            #L6
            mean_h = 0.35*b


            #set all stages to have a given number of trials
            numtrials<-40

            numtrialsx2<-numtrials*2

            numtrials_prac<-3*2

            numtrials_L2 <-10*2

########    1     --     training round#####

            #3 trials sd= 0.075 m=-0.1

            ##priormean<-rnorm(1, mean_f, 0.1)
            priormean<-mean_f
            priorsd<-sd_d


            trainingdata0.075<-find.dist(numtrials_prac, priormean, priorsd)

            ##mean/SD indo for later analysis
            x<-rep(priormean, numtrials_prac/2)
            y<-rep(priorsd, numtrials_prac/2)
            parameterdataL1<-cbind(x,y)


            ########    2     --     training round 2 #####


            #10 trials sd=0.03
            #priormean<-rnorm(1, mean_a, 0.1)
            priormean<-mean_a
            priorsd<-c(sd_a)
            trainingdata0.03<-find.dist(numtrials_L2, priormean, priorsd)

            trainingdata_L2<-c(trainingdata0.03)

            ### sd=0.12
            #priormean<-mean_a
            #priorsd<-c(sd_c)
            #trainingdata0.12<-find.dist(numtrials_L2, priormean, priorsd)

            #trainingdata_L2<-c(trainingdata0.12)



            ##mean/SD indo for later analysis
            x<-rep(priormean, numtrials_L2/2)
            y<-rep(priorsd, numtrials_L2/2)
            parameterdataL2<-cbind(x,y)

            #10 trials sd=0.12
            #priormean<-rnorm(1, mean_a, 0.1)
            #priormean<-mean_a
            #priorsd<-c(sd_a)
            #trainingdata0.03<-find.dist(numtrials_L2, priormean, priorsd)
            #trainingdata_L2<-c(trainingdata0.03)


########3###########

            #stable mean and changing sd. This is done as per Berinker et al. 2010 
            #- i.e. sd changes to fixed figures halfway through. could be done with 
            # distributions and random change points as for #3
            # fixed values and fixed change points

            #priormean<-rnorm(1, mean_a, 0.1)
            priormean<-mean_a
            priorsd<-c(sd_a, sd_c)
            part1<-find.dist(numtrialsx2/2, priormean, priorsd[1])
            part2<-find.dist(numtrialsx2/2, priormean, priorsd[2])
            datasdchange<-c(part1, part2)

            ##mean/SD indo for later analysis
            x<-rep(priormean, numtrialsx2/2)
            y<-c(rep(priorsd[1], numtrialsx2/2/2),rep(priorsd[2], numtrialsx2/2/2))
            parameterdataL3<-cbind(x,y)



#######4#######
            #changing mean and stable sd
            #fixed values and random change points


            ####first we need to generate the list of changing means

            #create an empty vector to hold the prior means
            empty_vec <- rep(NA, numtrials)

            # the two means to alternate between
            priormean_initial<-c(mean_b, mean_c)

            # a set of variables to hold the changing means which will be assigned
            currentmean<-priormean_initial[1]
            unusedmean<-priormean_initial[2]
            holdingmean<-priormean_initial[1]

            #a variable to count the number of times a mean has been used
            count <-0
            #length of minimum run
            minrun<-4


            # a loop which fills in the vector with the values of the means we need
            for (i in 1:length(empty_vec)) {
            # if mean has not yet come up the minimum number of times on this run
            if (count<minrun) {
            ###set this item on vector to the current mean
            empty_vec[i]<-currentmean
            # add 1 to the count
            count<-count+1
            }
            
            #if mean has come up minimum number of times
            if (count>=minrun) {
            #get a random value to test whether the mean will flip 
            #(in this case 1 to 4 and test if 1, so 25%)
            flip<-floor(runif(1, min=1, max=5))
                if (flip==1) {
                    #adjust the means!!
                    currentmean<-unusedmean
                    unusedmean<-holdingmean
                    holdingmean<-currentmean
                    empty_vec[i]<-currentmean
                    #reset the count for this new mean
                    count<-0
                }
                # if 2-4 comes up then carry on as normal!
                else {
                empty_vec[i]<-currentmean
                count<-count+1
                }
            }
            }

            #duplicator to double the vector to get both signals and trials
            duplicator <- rep(2, numtrials)


            ###then we do as normal and seed the mean and sd to generate our data
            priormean<-rep.int(empty_vec, duplicator)
            priorsd<-c(sd_b)


                    ##mean/SD indo for later analysis
                    x<-empty_vec # this is in fact the non-duplicated list of means
                    y<-rep(priorsd, numtrials/2)
                    parameterdataL4<-cbind(x,y)


            ##carrying on after interlude to get parameter data

            firstmean_indices<-which(priormean==priormean_initial[1])
            secondmean_indices<-which(priormean==priormean_initial[2])

            priormean<- replace(priormean, firstmean_indices, find.dist(length(firstmean_indices), priormean_initial[1], priorsd))
            priormean<-replace(priormean, secondmean_indices, find.dist(length(secondmean_indices), priormean_initial[2], priorsd))

            datameanchange<-priormean




##############   5     ################

            # the means and sds to alternate between
            ###priormean<-c(rnorm(1, mean_a, 0.1), rnorm(1, mean_b, 0.1), rnorm(1, mean_c, 0.1), rnorm(1, mean_d, 0.1), rnorm(1, mean_e, 0.1))
            ###priorsd<-c(sd_a, sd_b, sd_c)

            ###length of each run (multiplied by 2 to ensure even number fo trials per value)
            ###runlength<-(floor(rnorm(15,6,0.3)))*2



            # number of trials in total = num trials in this case
            #part1<-find.dist(runlength[1], priormean[1], priorsd[1])
            #part2<-find.dist(runlength[2], priormean[1], priorsd[2])
            #part3<-find.dist(runlength[3], priormean[1], priorsd[3])
            #part4<-find.dist(runlength[4], priormean[2], priorsd[1])
            #part5<-find.dist(runlength[5], priormean[2], priorsd[2])
            #part6<-find.dist(runlength[6], priormean[2], priorsd[3])
            #part7<-find.dist(runlength[7], priormean[3], priorsd[1])
            #part8<-find.dist(runlength[8], priormean[3], priorsd[2])
            #part9<-find.dist(runlength[9], priormean[3], priorsd[3])
            #part10<-find.dist(runlength[10], priormean[4], priorsd[1])
            #part11<-find.dist(runlength[11], priormean[4], priorsd[2])
            #part12<-find.dist(runlength[12], priormean[4], priorsd[3])
            #part13<-find.dist(runlength[13], priormean[5], priorsd[1])
            #part14<-find.dist(runlength[14], priormean[5], priorsd[2])
            #part15<-find.dist(runlength[15], priormean[5], priorsd[3])

            #dataallchange<-c(part1, part2, part3, part4, part5, part6, part7, part8, part9, part10, part11, part12, part13, part14, part15)




###### level 5 shorter version ######

            # the means and sds to alternate between
            #priormean<-c( rnorm(1, mean_b, 0.1), rnorm(1, mean_c, 0.1), rnorm(1, mean_d, 0.1), rnorm(1, mean_e, 0.1))
            priormean<-c(mean_b, mean_c, mean_d, mean_e)
            priorsd<-c(sd_a, sd_c)

            #length of each run (multiplied by 2 to ensure even number fo trials per value)
            runlength<-(floor(rnorm(15,11,0.3)))*2



            # number of trials in total = num trials in this case

            L5part8<-find.dist(runlength[8], priormean[4], priorsd[2])
            L5part2<-find.dist(runlength[2], priormean[1], priorsd[2])
            L5part4<-find.dist(runlength[4], priormean[2], priorsd[2])
            L5part7<-find.dist(runlength[7], priormean[4], priorsd[1])
            L5part3<-find.dist(runlength[3], priormean[2], priorsd[1])
            L5part5<-find.dist(runlength[5], priormean[3], priorsd[1])
            L5part6<-find.dist(runlength[6], priormean[3], priorsd[2])
            L5part1<-find.dist(runlength[1], priormean[1], priorsd[1])


            dataallchange<-c(L5part8, L5part2, L5part4, L5part7, L5part3, L5part5, L5part6, L5part1)


            ##mean/SD indo for later analysis
            L5part8_M_SD<-cbind(rep(priormean[4],runlength[8]/2), rep(priorsd[2],runlength[8]/2))
            L5part2_M_SD<-cbind(rep(priormean[1],runlength[2]/2), rep(priorsd[2],runlength[2]/2))
            L5part4_M_SD<-cbind(rep(priormean[2],runlength[4]/2), rep(priorsd[2],runlength[4]/2))
            L5part7_M_SD<-cbind(rep(priormean[4],runlength[7]/2), rep(priorsd[1],runlength[7]/2))
            L5part3_M_SD<-cbind(rep(priormean[2],runlength[3]/2), rep(priorsd[1],runlength[3]/2))
            L5part5_M_SD<-cbind(rep(priormean[3],runlength[5]/2), rep(priorsd[1],runlength[5]/2))
            L5part6_M_SD<-cbind(rep(priormean[3],runlength[6]/2), rep(priorsd[2],runlength[6]/2))
            L5part1_M_SD<-cbind(rep(priormean[1],runlength[1]/2), rep(priorsd[1],runlength[1]/2))


            dataallchange<-c(L5part8, L5part2, L5part4, L5part7, L5part3, L5part5, L5part6, L5part1)

            parameterdataL5<-rbind(L5part8_M_SD, L5part2_M_SD, L5part4_M_SD, L5part7_M_SD, L5part3_M_SD, L5part5_M_SD, L5part6_M_SD, L5part1_M_SD)

                ##############   6   ################

                # the means and sds to alternate between
                #priormean<-c(rnorm(1, mean_e, 0.1), rnorm(1, mean_h, 0.1))
                priormean<-c(mean_e, mean_h)
                priorsd<-c(sd_a, sd_c)

                #length of each run (multiplied by 2 to ensure even number fo trials per value)
                runlength<-(floor(rnorm(9,11,0.3)))*2




                # number of trials in total = num trials in this case
                L6part1<-find.dist(runlength[1], priormean[1], priorsd[1])   #m=e; sd=a   4
                L6part2<-find.dist(runlength[2], priormean[1], priorsd[2])   #m=e; sd=c   1
                L6part3<-find.dist(runlength[3], priormean[2], priorsd[1])   #m=h; sd=a   3
                L6part4<-find.dist(runlength[4], priormean[2], priorsd[2])   #m=h; sd=c   2



                data_loss_round<-c(L6part2, L6part4, L6part3, L6part1)

                    ###mean sd analysis
                    L6part1_M_SD<-cbind(rep(priormean[1],runlength[1]/2), rep(priorsd[1],runlength[1]/2))
                    L6part2_M_SD<-cbind(rep(priormean[1],runlength[2]/2), rep(priorsd[2],runlength[2]/2))
                    L6part3_M_SD<-cbind(rep(priormean[2],runlength[3]/2), rep(priorsd[1],runlength[3]/2))
                    L6part4_M_SD<-cbind(rep(priormean[2],runlength[4]/2), rep(priorsd[2],runlength[4]/2))

                    parameterdataL6<-rbind(L6part2_M_SD, L6part4_M_SD, L6part3_M_SD, L6part1_M_SD)

                #####bit of data validation ######################
                ylimits <-c(-1,1)
                xlimits <-c(-0.5,0.5)

                pdf(file="trialdatasummarygraphs_pg1.pdf")  

                par(mfrow=c(2,3))
                boxplot(trainingdata0.075, ylim=ylimits)
                boxplot(trainingdata_L2, ylim=ylimits)
                boxplot(datasdchange, ylim=ylimits)

                plot(density(trainingdata0.075), xlim=xlimits)
                plot(density(trainingdata_L2), xlim=xlimits)
                plot(density(datasdchange), xlim=xlimits)

                dev.off() 



                pdf(file="trialdatasummarygraphs_pg2.pdf")  

                par(mfrow=c(2,3))
                boxplot(datameanchange, ylim=ylimits)
                boxplot(dataallchange, ylim=ylimits)
                boxplot(data_loss_round, ylim=ylimits)
                plot(density(datameanchange), xlim=xlimits)
                plot(density(dataallchange), xlim=xlimits)
                plot(density(data_loss_round), xlim=xlimits)

                dev.off() 











                # new plots


#################################################################
######DIST PLOTS AT 0.1 

xlimits <-c(-1,1)

pdf(file="Dists 0.1 L1,2,3,4.pdf")

par(mfrow=c(2,3))

 plot(density(trainingdata0.075),main= "L1 m=-0.075 sd=0.075", xlim=xlimits)

 plot(density(trainingdata0.12), main="L2 sd=0.12 M=0", xlim=xlimits)
 plot(density(trainingdata0.03), main="L2 sd=0.03 M=0", xlim=xlimits)

 plot(density(part1), main="L3 sd=0.05 M=0", xlim=xlimits)
 plot(density(part2), main="L3 sd=0.2 M=0", xlim=xlimits)

 plot(density(datameanchange),main="L4 2xmeans 0.05,-0.05 SD=0.1", xlim=xlimits)

dev.off() 

    
    setwd("/Users/tonigibbs-dean/Documents/SpaceTaskProject/Methods/Results")
    pdf(file="Dist 0.1 L5.pdf")

        par(mfrow=c(3,3))

                plot(density(L5part1),main="L5 M=0.05 SD=0.05", xlim=xlimits)
                plot(density(L5part2),main="L5 M=0.05 SD=0.2", xlim=xlimits)
                plot(density(L5part3),main="L5 M=-0.05 SD=0.05", xlim=xlimits)
                plot(density(L5part4),main="L5 M=-0.05 SD=0.2", xlim=xlimits)
                plot(density(L5part5),main="L5 M=0.02 SD=0.05", xlim=xlimits)
                plot(density(L5part6),main="L5 M=0.02 SD=0.2", xlim=xlimits)
                plot(density(L5part7),main="L5 M=-0.03 SD=0.05", xlim=xlimits)
                plot(density(L5part8),main="L5 M=-0.03 SD=0.2", xlim=xlimits)

    dev.off() 


        pdf(file="Dist 0.1 L6.pdf")

        par(mfrow=c(2,3))

        plot(density(L6part1),main="L6 M=-0.03 SD=0.05", xlim=xlimits)
        plot(density(L6part2),main="L6 M=-0.03 SD=0.2", xlim=xlimits)
        plot(density(L6part3),main="L6 M=0.35 SD=0.05", xlim=xlimits)
        plot(density(L6part4),main="L6 M=0.35 SD=0.2", xlim=xlimits)

        dev.off() 


#############################








summarydata<-data.frame(
	# descr of training data 0.05
	sd(trainingdata0.075),
	mean(trainingdata0.075),
	# descr of training data 0.2
	sd(trainingdata_L2),
	mean(trainingdata_L2),

	# descr of training data 0.2
	sd(datasdchange),
	mean(datasdchange),

	# descr of training data 0.2
	sd(datameanchange),
	mean(datameanchange),

	# descr of training data 0.2
	sd(dataallchange),
	mean(dataallchange),

    # descr of training data 0.2
  sd(data_loss_round),
  mean(data_loss_round)
)






#
#
#
#
#
 odd <- seq_along(1:1000) %% 2 == 1


######get task data into two columns
x<-trainingdata0.075
    o <- x[odd]
    e <- x[!odd]
    length(e) <- length(x)/2
    length(o) <- length(x)/2
trainingdata0.075<-data.frame(o, e)

x<-trainingdata_L2
    o <- x[odd]
    e <- x[!odd]
    length(e) <- length(x)/2
    length(o) <- length(x)/2
trainingdata_L2<-data.frame(o, e)

x<-datasdchange
    o <- x[odd]
    e <- x[!odd]
    length(e) <- length(x)/2
    length(o) <- length(x)/2
datasdchange<-data.frame(o, e)

x<-datameanchange
    o <- x[odd]
    e <- x[!odd]
    length(e) <- length(x)/2
    length(o) <- length(x)/2
datameanchange<-data.frame(o, e)

x<-dataallchange
    o <- x[odd]
    e <- x[!odd]
    length(e) <- length(x)/2
    length(o) <- length(x)/2
dataallchange<-data.frame(o, e)

x<-data_loss_round
    o <- x[odd]
    e <- x[!odd]
    length(e) <- length(x)/2
    length(o) <- length(x)/2
data_loss_round<-data.frame(o, e)



#give data round names in a column
trainingdata0.075$round<-1
trainingdata_L2$round<-2
datasdchange$round<-3
datameanchange$round<-4
dataallchange$round<-5
data_loss_round$round<-6

#reverse data order within each round
trainingdata0.075_rev<- trainingdata0.075[seq(dim(trainingdata0.075)[1],1),]
trainingdata_L2_rev<- trainingdata_L2[seq(dim(trainingdata_L2)[1],1),]
datasdchange_rev<- datasdchange[seq(dim(datasdchange)[1],1),]
datameanchange_rev<- datameanchange[seq(dim(datameanchange)[1],1),]
dataallchange_rev<- dataallchange[seq(dim(dataallchange)[1],1),]
data_loss_round_rev<- data_loss_round[seq(dim(data_loss_round)[1],1),]

alldata<-rbind(trainingdata0.075, trainingdata_L2, datasdchange, datameanchange, dataallchange, data_loss_round)

alldata_rev<-rbind(trainingdata0.075_rev, trainingdata_L2_rev, datasdchange_rev, datameanchange_rev, dataallchange_rev, data_loss_round_rev)


#reverse parameter data order within each round
parameterdataL1_rev<- parameterdataL1[seq(dim(parameterdataL1)[1],1),]
parameterdataL2_rev<- parameterdataL2[seq(dim(parameterdataL2)[1],1),]
parameterdataL3_rev<- parameterdataL3[seq(dim(parameterdataL3)[1],1),]
parameterdataL4_rev<- parameterdataL4[seq(dim(parameterdataL4)[1],1),]
parameterdataL5_rev<- parameterdataL5[seq(dim(parameterdataL5)[1],1),]
parameterdataL6_rev<- parameterdataL6[seq(dim(parameterdataL6)[1],1),]

parameterdata<-rbind(parameterdataL1, parameterdataL2, parameterdataL3, parameterdataL4, parameterdataL5, parameterdataL6)
parameterdata_rev<-rbind(parameterdataL1_rev, parameterdataL2_rev, parameterdataL3_rev, parameterdataL4_rev, parameterdataL5_rev, parameterdataL6_rev)

parameterdata<-cbind(cbind(parameterdata, alldata), seq(1:dim(parameterdata)[1]))
parameterdata_rev<-cbind(cbind(parameterdata_rev, alldata_rev), seq(1:dim(parameterdata_rev)[1]))


#remove names for printing
names(alldata)<-NULL
names(alldata_rev)<-NULL

#add col names for parameters for printing
names(parameterdata)<-c("mean", "sd", "hint", "result", "level", "trialnum")
names(parameterdata_rev)<-c("mean", "sd", "hint", "result", "level", "trialnum")

###### unused dataframe
#make dataframe (slightly complicated.. could be improved, but couldn't get it to write.csv without forcing to dataframe in this fashion.)

      #df <- data.frame(blank = rep(NA, max(sapply(list(trainingdata0.075, trainingdata_L2, datasdchange, datameanchange, dataallchange, data_loss_round), length))))
      #df[1:nrow(trainingdata0.075), 1:2] <- trainingdata0.075
      #df[1:nrow(trainingdata_L2), 3:4] <- trainingdata_L2
      #df[1:nrow(datasdchange), 5:6] <- datasdchange
      #df[1:nrow(datameanchange), 7:8] <- datameanchange
      #df[1:nrow(dataallchange), 9:10] <- dataallchange
      #df[1:nrow(data_loss_round), 11:12] <- data_loss_round
      #names(df)<-c("trainingdata0.075_signal","trainingdata0.075_test", "trainingdata_L2_signal","trainingdata_L2_test", "datasdchange_signal", 
      #	"datasdchange_test", "datameanchange_signal","datameanchange_test", "dataallchange_signal", "dataallchange_test", "data_loss_round_signal", "data_loss_round_test")

#print csv of summary stats for trial data
write.csv(summarydata, file = 'trialdatasummary.csv', row.names = FALSE, na = '')
#print csv of trial data
write.csv(alldata, file = 'spacetask003.csv', row.names = FALSE, na = '')
write.csv(alldata_rev, file = 'spacetask003reversed.csv', row.names = FALSE, na = '')

#print csv of parameter data
write.csv(parameterdata, file = 'parameters003.csv', row.names = FALSE, na = '')
write.csv(parameterdata_rev, file = 'parameters003reversed_t.csv', row.names = FALSE, na = '')

