
########################################
#plotting the simulations vs estimates from matlab - now outdated - find in its own script.
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


################################################################################################################################################################
##### simpler plots 
################################################################################################################################################################

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




