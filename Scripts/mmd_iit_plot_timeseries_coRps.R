#Purpose: Create visualization using ggplot comparing performance in MMD and IIT

df_filepath <- "C:/Users/jstephens/Documents/MSD/MER_Structured_Datasets_PSNU_IM_FY20-22_20220211_v1_1_FY22Q1i.txt"
df<- read_msd(df_filepath)

df<- resolve_knownissues(df, remove_cs = TRUE, store_excl = FALSE)

#Munge MMD data for Multiple Quarters
df1 <- df %>% 
  filter(indicator %in% (c("TX_CURR")),
         standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator"),
         fiscal_year %in% (c( "2021", "2022"))) %>% 
  mutate(
    otherdisaggregate = str_remove(
      otherdisaggregate, "ARV Dispensing Quantity - "),
    otherdisaggregate = case_when(
      otherdisaggregate == "Less than 3 months" ~ "nonmmd",
      otherdisaggregate == "3 to 5 months" ~ "threem",
      otherdisaggregate == "6 or more months" ~ "sixm",
      is.na(otherdisaggregate) ~ "tn",
      TRUE ~ otherdisaggregate)) %>% 
  group_by(operatingunit, otherdisaggregate, fiscal_year) %>% 
  summarise(across(c(qtr1, qtr2, qtr3, qtr4), sum, na.rm=TRUE)) %>% 
  filter(otherdisaggregate!="<3") %>% 
  pivot_longer(c(qtr1, qtr2, qtr3, qtr4), names_to="quarter") %>%
  pivot_wider(names_from=otherdisaggregate, names_sep="_", values_from=value) %>% 
  ungroup() %>% 
  mutate(Three_Months_Share=threem/tn,
         Six_Months_Share=sixm/tn) %>% 
  pivot_longer(c(Three_Months_Share, Six_Months_Share), names_to="MMD_Prop") %>% 
  select(operatingunit, MMD_Prop, value, quarter, fiscal_year) %>% 
  group_by(operatingunit) %>% 
  drop_na(value) %>% 
  mutate(mmd_color=case_when(
    MMD_Prop=="Six_Months_Share" ~si_palettes$moody_blues[3],
    MMD_Prop=="Three_Months_Share" ~si_palettes$moody_blues[5],
    TRUE ~ grey40k))

#Munge TX_CURR total numerator
df_tx_curr<-df %>% 
  filter(indicator=="TX_CURR", 
         standardizeddisaggregate=="Total Numerator",
         fiscal_year %in% (c( "2021", "2022"))) %>% 
  group_by(operatingunit, otherdisaggregate, fiscal_year) %>%
  summarise(across(c(qtr1, qtr2, qtr3, qtr4), sum, na.rm=TRUE)) %>%
  pivot_longer(c(qtr1, qtr2, qtr3, qtr4), names_to="quarter") %>%
  pivot_wider(names_from=otherdisaggregate, names_sep="_", values_from=value) %>% 
  ungroup() %>%
  mutate(period=paste(fiscal_year, quarter)) %>%
   rename("TX_CURR"="NA") %>%
  filter(TX_CURR>0)

#Munge TX_ML Numerator
df_ml <- df %>% 
  filter(indicator=="TX_ML",
         fiscal_year %in% (c( "2021", "2022")),
         standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus",
         otherdisaggregate %in% c("No Contact Outcome - Interruption in Treatment 3+ Months Treatment", 
                                  "No Contact Outcome - Interruption in Treatment <3 Months Treatment", 
                                  "No Contact Outcome - Interruption in Treatment 3-5 Months Treatment", 
                                  "No Contact Outcome - Interruption in Treatment 6+ Months Treatment" 
                                   )) %>% 
  group_by(operatingunit, fiscal_year) %>%
  summarise(across(c(qtr1, qtr2, qtr3, qtr4), sum, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(c(qtr1, qtr2, qtr3, qtr4), names_to="quarter")

# #Munge TX_ML Denominator
#this version did not get fy22q1 correct
# df_mld <- df %>%
#   filter(indicator %in% c("TX_CURR", "TX_NEW"),
#          fiscal_year %in% (c("2021", "2022")),
#          standardizeddisaggregate=="Total Numerator") %>%
#   group_by(operatingunit, indicator, fiscal_year) %>%
#   summarise(across(c(qtr1, qtr2, qtr3, qtr4), sum, na.rm=TRUE)) %>%
#   ungroup()%>%
#   pivot_wider(names_from=indicator, names_sep=".", values_from=c( qtr1, qtr2, qtr3, qtr4)) %>%
#   mutate(q1.iit_d=qtr4.TX_CURR+qtr1.TX_NEW,
#          q2.iit_d=qtr1.TX_CURR+qtr2.TX_NEW,
#          q3.iit_d=qtr2.TX_CURR+qtr3.TX_NEW,
#          q4.iit_d=qtr3.TX_CURR+qtr4.TX_NEW,
#          q1.TX_CURR=qtr1.TX_CURR) %>%
#   select(q1.iit_d, q2.iit_d, q3.iit_d, q4.iit_d, q1.TX_CURR, operatingunit, fiscal_year) %>%
#   pivot_longer(c(q1.iit_d, q2.iit_d, q3.iit_d, q4.iit_d, q1.TX_CURR), names_to="quarter")%>%
#   separate(quarter, c("quarter", "indicator")) %>%
#   mutate(quarter=str_replace(quarter, "q", "qtr"),
#          indicator=str_replace(indicator, "iit", "iit_d")) %>%
#   rename(mld="value")


#this works for q2-q4, but not fy22q1
df_mld_21 <- df %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"),
         fiscal_year %in% (c("2021")),
         standardizeddisaggregate=="Total Numerator") %>% 
  group_by(operatingunit, indicator, fiscal_year) %>% 
  summarise(across(c(qtr1, qtr2, qtr3, qtr4), sum, na.rm=TRUE)) %>%
  ungroup()%>% 
  pivot_wider(names_from=indicator, names_sep=".", values_from=c( qtr1, qtr2, qtr3, qtr4)) %>% 
  mutate(q2.iit_d=qtr1.TX_CURR+qtr2.TX_NEW,
         q3.iit_d=qtr2.TX_CURR+qtr3.TX_NEW,
         q4.iit_d=qtr3.TX_CURR+qtr4.TX_NEW) %>%
  select(q2.iit_d, q3.iit_d, q4.iit_d, operatingunit, fiscal_year) %>% 
  pivot_longer(c(q2.iit_d, q3.iit_d, q4.iit_d, ), names_to="quarter")%>%
  separate(quarter, c("quarter", "indicator")) %>%
  mutate(quarter=str_replace(quarter, "q", "qtr"),
         indicator=str_replace(indicator, "iit", "iit_d")) %>%
  rename(mld="value")

#fy214 tx_curr
df_curr_prev <- df %>% 
  filter(indicator %in% c("TX_CURR"),
         fiscal_year %in% (c("2021")),
         standardizeddisaggregate=="Total Numerator") %>% 
  group_by(operatingunit, indicator, fiscal_year) %>% 
  summarise(across(c(qtr4), sum, na.rm=TRUE)) %>%
  ungroup()%>% 
  pivot_wider(names_from=indicator, names_sep=".", values_from=c(fiscal_year, qtr4)) %>% 
  select(!fiscal_year.TX_CURR)

#fy22q1 alone
df_mld_22 <- df %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"),
         fiscal_year %in% (c("2022")),
         standardizeddisaggregate=="Total Numerator") %>% 
  group_by(operatingunit, indicator, fiscal_year) %>% 
  summarise(across(c(qtr1), sum, na.rm=TRUE)) %>%
  ungroup()%>% 
  pivot_wider(names_from=indicator, names_sep=".", values_from=c(fiscal_year,qtr1)) %>% 
  select(!c(fiscal_year.TX_CURR, fiscal_year.TX_NEW))

df_mld_22_full<-left_join(df_mld_22, df_curr_prev, by=c("operatingunit"="operatingunit")) %>% 
  mutate(q1.iit_d=qtr4.TX_CURR+qtr1.TX_NEW,
         fiscal_year=2022) %>%
  select(q1.iit_d, operatingunit, fiscal_year) %>% 
  pivot_longer(c(q1.iit_d), names_to="quarter")%>%
  separate(quarter, c("quarter", "indicator")) %>%
  mutate(quarter=str_replace(quarter, "q", "qtr"),
         indicator=str_replace(indicator, "iit", "iit_d")) %>%
  rename(mld="value")

df_mld <- rbind(df_mld_21, df_mld_22_full)
glimpse(df_mld)
#Join dfs

#3+ MMD

#1st join IIT
df2 <- df_mld %>% 
  left_join(df_ml, df_mld, by=c("operatingunit"="operatingunit", "quarter"="quarter", "fiscal_year"="fiscal_year")) %>%  
  mutate(val_iit=value/mld) %>%
  select(operatingunit, fiscal_year, quarter, val_iit, indicator)

#contains fy21q1, may want to come here if issues later
df_3mmd<-df1 %>%
  group_by(operatingunit, quarter, fiscal_year) %>%
  summarise(across(c(value), sum, na.rm=TRUE))

df3<-left_join(df2, df_3mmd, by=c("operatingunit"="operatingunit", "quarter"="quarter", "fiscal_year"="fiscal_year")) %>%
  mutate(period=paste(fiscal_year, quarter))

df_final<-left_join(df3, df_tx_curr, by=c("operatingunit"="operatingunit", "quarter"="quarter", "fiscal_year"="fiscal_year", "period"="period")) %>%
  drop_na(value) %>%
  select(-indicator) %>%
  rename(
    "iit"=val_iit,
    "three_mmd"=value)

df_final_dedup<-!duplicated(df_final)
#values
median<-median(df_final$three_mmd)
mean<-mean(df_final$three_mmd)
target_mmd<-.9
target_6mmd<-.5
target_iit<-.02
low_iit<-.003



###################################################################################3
# code from previous qtrs with different colors boxes
# ###create still visuals
# u3mmd_periods<-df_final%>%
#   filter(period %in% c( "2021 qtr2", "2021 qtr3", "2021 qtr4", "2022 qtr1")) %>%
#   mutate(operatingunit=recode(operatingunit,
#                               "Democratic Republic of the Congo" = "DRC",
#                               "Dominican Republic" = "DR",
#                               "Western Hemisphere Region" = "WHR",
#                               "West Africa Region" = "WAR"),
#          median=median(iit)) %>% 
#   #create the axis
#   ggplot(aes(three_mmd, iit, color=operatingunit))+
#   #color the quadrants
#   annotate("rect", xmin = Inf, xmax = median, ymin = Inf, ymax = target_iit, fill= grey20k, alpha=.3)  +
#   annotate("rect", xmin = -Inf, xmax = median, ymin = -Inf, ymax = target_iit , fill= grey20k, alpha=.3) +
#   annotate("rect", xmin = median, xmax = Inf, ymin = target_iit, ymax = -Inf, fill= "#bfddff", alpha=.4) +
#   annotate("rect", xmin = median, xmax = -Inf, ymin = Inf, ymax = target_iit, fill= "#ffb5ba", alpha=.4) +
#   # annotate("rect", xmin = Inf, xmax = median, ymin = Inf, ymax = low_iit, fill= grey20k, alpha=.3)  +
#   # annotate("rect", xmin = -Inf, xmax = median, ymin = -Inf, ymax = low_iit , fill= grey20k, alpha=.3) +
#   # annotate("rect", xmin = median, xmax = Inf, ymin = low_iit, ymax = -Inf, fill= "#ffb5ba", alpha=.4) +
#   # annotate("rect", xmin = median, xmax = -Inf, ymin = Inf, ymax = low_iit, fill= "#ffb5ba", alpha=.4) +
#   #add points
#   geom_point(aes(size=TX_CURR), shape=21, alpha=.6, fill="#002065", color=grey80k, stroke=1)+
#   #label OU names
#   geom_text(aes(label=operatingunit),size=6/.pt, vjust=1.75, color="#595959",family="Source Sans Pro")+
#   #secondary option to label ou names - add lines to farther away pts?
#   # geom_text_repel(aes(label=operatingunit), seed=123, point.size=4, size=8/.pt, color="#595959",family="Source Sans Pro")+
#   #add horizontal line at target iit (.02 = 2%)
#   geom_hline(yintercept=target_iit, color=grey90k, linetype="dotted")+
#   #add vertical line at mmd median (also option to create line at median if desired)
#   geom_vline(xintercept=median, color=grey90k, linetype="dotted")+
#   #geom_vline(xintercept=target_mmd, color=grey60k, linetype="dotted")+
#   #format axis in percent (grey code archive of cody's - percent function not found)
#   scale_x_continuous(labels=scales::percent)+
#   # scale_x_continuous(label=percent)+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(NA, .1))+
#   #format background to remove gridlines
#   si_style_nolines()+
#   facet_wrap(~period)+
#   #prepare theme. remove legend, set fonts (some not shown until title added in)
#   theme(legend.position="none",
#         axis.title = element_text(size=16),
#         plot.title = element_text(size=20),
#         plot.subtitle = element_text(size=24, color="dark red"))+
#   #format label names and add title
#   labs(x = "3+ MMD", y = "IIT",
#        title = "INTERRUPTIONS IN TREATMENT AND 3+ MMD LEVELS")
# u3mmd_periods



###create still visuals -last4 qtrs
u3mmd_periods<-df_final%>%
  filter(period %in% c( "2021 qtr2", "2021 qtr3", "2021 qtr4", "2022 qtr1")) %>%
  mutate(operatingunit=recode(operatingunit,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR",
                              "Western Hemisphere Region" = "WHR",
                              "West Africa Region" = "WAR"),
         median=median(iit)) %>% 
  #create the axis
  ggplot(aes(three_mmd, iit, color=operatingunit))+
  #color the quadrants
  annotate("rect", xmin = Inf, xmax = median, ymin = Inf, ymax = low_iit, fill= "#bfddff", alpha=.3)  +
   annotate("rect", xmin = -Inf, xmax = Inf, ymin = low_iit, ymax = -Inf, fill= "#ffb5ba", alpha=.4) +
  annotate("rect", xmin = median, xmax = -Inf, ymin = Inf, ymax = low_iit, fill= "#ffb5ba", alpha=.1) +
  annotate("rect", xmin = -Inf, xmax = median, ymin = -Inf, ymax = low_iit , fill= "#ffb5ba", alpha=.6) +
  #add points
  geom_point(aes(size=TX_CURR), shape=21, fill= grey20k, alpha=.4, color=grey80k, stroke=1)+
  #label OU names
  #  geom_text(aes(label=operatingunit),size=6/.pt, vjust=1.75, color="#595959",family="Source Sans Pro", check_overlap = TRUE)+
  ggrepel::geom_text_repel( aes(label = operatingunit),size=6/.pt,vjust=.5, color="#595959",family="Source Sans Pro")+
  #secondary option to label ou names - add lines to farther away pts?
  # geom_text_repel(aes(label=operatingunit), seed=123, point.size=4, size=8/.pt, color="#595959",family="Source Sans Pro")+
  #add horizontal line at target iit (.02 = 2%)
  geom_hline(yintercept=low_iit, color=grey90k, linetype="dotted")+
  #add vertical line at mmd median (also option to create line at median if desired)
  geom_vline(xintercept=median, color=grey90k, linetype="dotted")+
  #geom_vline(xintercept=target_mmd, color=grey60k, linetype="dotted")+
  #format axis in percent (grey code archive of cody's - percent function not found)
  scale_x_continuous(labels=scales::percent)+
  # scale_x_continuous(label=percent)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-0.01, .1))+
  #format background to remove gridlines
  si_style_nolines()+
  facet_wrap(~period)+
  #prepare theme. remove legend, set fonts (some not shown until title added in)
  theme(legend.position="none",
        axis.title = element_text(size=16),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=16, color="#bfddff"))+
  #format label names and add title
  labs(x = "3+ MMD", y = "IIT",
       title = "INTERRUPTIONS IN TREATMENT AND 3+ MMD LEVELS",
       caption = "Source: FY22 Q1 MSD" )
u3mmd_periods


# ffb5ba -pink .4 = low iit
# ffb5ba -pink .1= low mmd and iit
#  bfddff - blues = good mmd + iit


###create still visuals - currently quarter only
u3mmd_periods<-df_final%>%
  filter(period %in% c( "2022 qtr1")) %>%
  mutate(operatingunit=recode(operatingunit,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR",
                              "Western Hemisphere Region" = "WHR",
                              "West Africa Region" = "WAR"),
         median=median(iit)) %>% 
  #create the axis
  ggplot(aes(three_mmd, iit, color=operatingunit))+
  #color the quadrants
  annotate("rect", xmin = Inf, xmax = median, ymin = Inf, ymax = low_iit, fill= "#bfddff", alpha=.3)  +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = low_iit, ymax = -Inf, fill= "#ffb5ba", alpha=.4) +
  annotate("rect", xmin = median, xmax = -Inf, ymin = Inf, ymax = low_iit, fill= "#ffb5ba", alpha=.1) +
  annotate("rect", xmin = -Inf, xmax = median, ymin = -Inf, ymax = low_iit , fill= "#ffb5ba", alpha=.6) +
  #add points
  geom_point(aes(size=TX_CURR), shape=21, fill= grey20k, alpha=.4, color=grey80k, stroke=1)+
  #label OU names
   #   geom_text(aes(label=operatingunit),size=6/.pt, vjust=1.75, color="#595959",family="Source Sans Pro", check_overlap = TRUE)+
   ggrepel::geom_text_repel( aes(label = operatingunit),size=6/.pt, color="#595959",family="Source Sans Pro")+
  #secondary option to label ou names - add lines to farther away pts?
  # geom_text_repel(aes(label=operatingunit), seed=123, point.size=4, size=8/.pt, color="#595959",family="Source Sans Pro")+
  #add horizontal line at target iit (.02 = 2%)
  geom_hline(yintercept=low_iit, color=grey90k, linetype="dotted")+
  #add vertical line at mmd median (also option to create line at median if desired)
  geom_vline(xintercept=median, color=grey90k, linetype="dotted")+
  #geom_vline(xintercept=target_mmd, color=grey60k, linetype="dotted")+
  #format axis in percent (grey code archive of cody's - percent function not found)
  scale_x_continuous(labels=scales::percent)+
  # scale_x_continuous(label=percent)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-0.01, NA))+
  #format background to remove gridlines
  si_style_nolines()+
  #prepare theme. remove legend, set fonts (some not shown until title added in)
  theme(legend.position="none",
        axis.title = element_text(size=16),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=16, color="#bfddff"))+
  #format label names and add title
  labs(x = "3+ MMD", y = "IIT",
       title = "INTERRUPTIONS IN TREATMENT AND 3+ MMD LEVELS, FY22Q1",
       caption = "Source: FY22 Q1 MSD" )
u3mmd_periods




###################################################################################3
##not updated q1

#Build out GIF plot

####

u3mmd<-df_final%>%
  filter(period %in% c("2020 qtr3", "2020 qtr4", "2021 qtr1", "2021 qtr2", "2021 qtr3", "2021 qtr4", "2022 qtr1")) %>%
  mutate(operatingunit=recode(operatingunit,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR",
                              "Western Hemisphere Region" = "WHR",
                              "West Africa Region" = "WAR"),
         median=median(iit)) %>% 
  #create the axis
  ggplot(aes(three_mmd, iit, color=operatingunit))+
  #color the quadrants
  annotate("rect", xmin = Inf, xmax = median, ymin = Inf, ymax = target_iit, fill= grey20k, alpha=.3)  +
  annotate("rect", xmin = -Inf, xmax = median, ymin = -Inf, ymax = target_iit , fill= grey20k, alpha=.3) +
  annotate("rect", xmin = median, xmax = Inf, ymin = target_iit, ymax = -Inf, fill= "#bfddff", alpha=.4) +
  annotate("rect", xmin = median, xmax = -Inf, ymin = Inf, ymax = target_iit, fill= "#ffb5ba", alpha=.4) +
  #add points
  geom_point(aes(size=TX_CURR), shape=21, alpha=.6, fill="#002065", color=grey80k, stroke=1)+
  #label OU names
  geom_text(aes(label=operatingunit),size=9/.pt, vjust=1.75, color="#595959",family="Source Sans Pro")+
  #secondary option to label ou names - add lines to farther away pts?
  # geom_text_repel(aes(label=operatingunit), seed=123, point.size=4, size=8/.pt, color="#595959",family="Source Sans Pro")+
  #add horizontal line at target iit (.02 = 2%)
  geom_hline(yintercept=target_iit, color=grey90k, linetype="dotted")+
  #add vertical line at mmd median (also option to create line at median if desired)
  geom_vline(xintercept=median, color=grey90k, linetype="dotted")+
  #geom_vline(xintercept=target_mmd, color=grey60k, linetype="dotted")+
  #format axis in percent (grey code archive of cody's - percent function not found)
  scale_x_continuous(labels=scales::percent)+
  # scale_x_continuous(label=percent)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(NA, .1))+
  #format background to remove gridlines
  si_style_nolines()+
  #prepare theme. remove legend, set fonts (some not shown until title added in)
  theme(legend.position="none",
        axis.title = element_text(size=16),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=24, color="dark red"))+
  #prep for gif, dots moving by period
  transition_states(period, transition_length=2, state_length=1)+
  #format label names and add title
  labs(x = "3+ MMD", y = "IIT",
       title = "INTERRUPTIONS IN TREATMENT AND 3+ MMD LEVELS", subtitle = "{closest_state}")


gif<-animate(u3mmd, renderer=gifski_renderer(), width=1000, height=650, res=92, fps=4)
anim_save("iit_mmd_4fps_q4.gif")
#create slower visuals
gif<-animate(u3mmd, renderer=gifski_renderer(), width=1000, height=650, res=92, fps=2)
anim_save("iit_mmd_2fps_q4.gif")


#create slow vis for only 4 qtrs
u3mmd_4qtr<-df_final%>%
  filter(period %in% c( "2021 qtr1", "2021 qtr2", "2021 qtr3", "2021 qtr4")) %>%
  mutate(operatingunit=recode(operatingunit,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR",
                              "Western Hemisphere Region" = "WHR",
                              "West Africa Region" = "WAR"),
         median=median(iit)) %>% 
  #create the axis
  ggplot(aes(three_mmd, iit, color=operatingunit))+
  #color the quadrants
  annotate("rect", xmin = Inf, xmax = median, ymin = Inf, ymax = target_iit, fill= grey20k, alpha=.3)  +
  annotate("rect", xmin = -Inf, xmax = median, ymin = -Inf, ymax = target_iit , fill= grey20k, alpha=.3) +
  annotate("rect", xmin = median, xmax = Inf, ymin = target_iit, ymax = -Inf, fill= "#bfddff", alpha=.4) +
  annotate("rect", xmin = median, xmax = -Inf, ymin = Inf, ymax = target_iit, fill= "#ffb5ba", alpha=.4) +
  #add points
  geom_point(aes(size=TX_CURR), shape=21, alpha=.6, fill="#002065", color=grey80k, stroke=1)+
  #label OU names
  geom_text(aes(label=operatingunit),size=9/.pt, vjust=1.75, color="#595959",family="Source Sans Pro")+
  #secondary option to label ou names - add lines to farther away pts?
  # geom_text_repel(aes(label=operatingunit), seed=123, point.size=4, size=8/.pt, color="#595959",family="Source Sans Pro")+
  #add horizontal line at target iit (.02 = 2%)
  geom_hline(yintercept=target_iit, color=grey90k, linetype="dotted")+
  #add vertical line at mmd median (also option to create line at median if desired)
  geom_vline(xintercept=median, color=grey90k, linetype="dotted")+
  #geom_vline(xintercept=target_mmd, color=grey60k, linetype="dotted")+
  #format axis in percent (grey code archive of cody's - percent function not found)
  scale_x_continuous(labels=scales::percent)+
  # scale_x_continuous(label=percent)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(NA, .1))+
  #format background to remove gridlines
  si_style_nolines()+
  #prepare theme. remove legend, set fonts (some not shown until title added in)
  theme(legend.position="none",
        axis.title = element_text(size=16),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=24, color="dark red"))+
  #prep for gif, dots moving by period
  transition_states(period, transition_length=.5, state_length=2)+
  #format label names and add title
  labs(x = "3+ MMD", y = "IIT",
       title = "INTERRUPTIONS IN TREATMENT AND 3+ MMD LEVELS", subtitle = "{closest_state}")

gif<-animate(u3mmd_4qtr, renderer=gifski_renderer(), width=1000, height=650, res=92, fps=2)
anim_save("iit_mmd_2fps_fy22q4_4qtrs_.5trans_2length.gif")
gif<-animate(u3mmd_4qtr, renderer=gifski_renderer(), width=1000, height=650, res=92, fps=2)
anim_save("iit_mmd_2fps_fy22q4_4qtrs_1trans_2length.gif")
gif<-animate(u3mmd_4qtr, renderer=gifski_renderer(), width=1000, height=650, res=92, fps=2)
anim_save("iit_mmd_2fps_fy22q4_4qtrs_2trans_1length.gif")
#too slow
# gif<-animate(u3mmd_4qtr, renderer=gifski_renderer(), width=1000, height=650, res=92, fps=1)
# anim_save("iit_mmd_1fps_fy22q4_4qtrs.gif")
