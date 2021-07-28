#Purpose: Create visualization using ggplot comparing performance in MMD and IIT
install.packages("ggrepel")
install.packages("gganimate")
library(gifski)
library(ggrepel)
library(gganimate)
library(glue)

si_palettes$moody_blues %>% show_col()

df_filepath <- "~/Github/MSD/msd_fy21_q2_clean_psnu.txt"
df<- read_msd(df_filepath)

#Munge MMD data for Multiple Quarters
df1 <- df %>% 
  filter(indicator %in% (c("TX_CURR")),
         standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator"),
         fiscal_year %in% (c("2020", "2021")),
         operatingunit!="South Africa") %>% 
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
         fiscal_year %in% (c("2020", "2021")),
         operatingunit!="South Africa") %>%
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
         fiscal_year %in% (c("2020", "2021")),
         standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus",
         otherdisaggregate %in% c("No Contact Outcome - Interruption in Treatment 3+ Months Treatment", 
                                  "No Contact Outcome - Interruption in Treatment <3 Months Treatment")) %>% 
  group_by(operatingunit, fiscal_year) %>%
  summarise(across(c(qtr1, qtr2, qtr3, qtr4), sum, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(c(qtr1, qtr2, qtr3, qtr4), names_to="quarter")

#Munge TX_ML Denominator
df_mld <- df %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"),
         fiscal_year %in% (c("2020", "2021")),
         standardizeddisaggregate=="Total Numerator") %>% 
  group_by(operatingunit, indicator, fiscal_year) %>% 
  summarise(across(c(qtr1, qtr2, qtr3, qtr4), sum, na.rm=TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from=indicator, names_sep=".", values_from=c(qtr1, qtr2, qtr3, qtr4)) %>% 
  mutate(q2.iit_d=qtr1.TX_CURR+qtr2.TX_NEW,
         q3.iit_d=qtr2.TX_CURR+qtr3.TX_NEW,
         q4.iit_d=qtr3.TX_CURR+qtr4.TX_NEW,
         q1.iit_d=qtr1.TX_CURR+qtr1.TX_NEW,) %>%
  select(q1.iit_d, q2.iit_d, q3.iit_d, q4.iit_d, qtr4.TX_CURR, operatingunit, fiscal_year) %>% 
  pivot_longer(c(q1.iit_d, q2.iit_d, q3.iit_d, q4.iit_d, qtr4.TX_CURR), names_to="quarter") %>%
  separate(quarter, c("quarter", "indicator")) %>%
  mutate(quarter=str_replace(quarter, "q", "qtr"),
         indicator=str_replace(indicator, "iit", "iit_d")) %>%
  rename(mld="value")



#Join dfs

#3+ MMD
df2 <- df_mld %>% 
  left_join(df_ml, df_mld, by=c("operatingunit"="operatingunit", "quarter"="quarter", "fiscal_year"="fiscal_year")) %>%  
  mutate(val_iit=value/mld) %>%
  select(operatingunit, fiscal_year, quarter, val_iit, indicator)

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


median<-median(df_final$three_mmd)
target_mmd<-.9
target_6mmd<-.5
target_iit<-.02


#Build out plot

u3mmd<-df_final%>%
  mutate(operatingunit=recode(operatingunit,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR",
                              "Western Hemisphere Region" = "WHR"),
         median=median(iit)) %>% 
  ggplot(aes(three_mmd, iit, color=operatingunit))+
  annotate("rect", xmin = Inf, xmax = median, ymin = Inf, ymax = target_iit, fill= grey20k, alpha=.3)  + 
  annotate("rect", xmin = -Inf, xmax = median, ymin = -Inf, ymax = target_iit , fill= grey20k, alpha=.3) + 
  annotate("rect", xmin = median, xmax = Inf, ymin = target_iit, ymax = -Inf, fill= "#bfddff", alpha=.4) + 
  annotate("rect", xmin = median, xmax = -Inf, ymin = Inf, ymax = target_iit, fill= "#ffb5ba", alpha=.4) + 
  geom_point(aes(size=TX_CURR), shape=21, alpha=.6, fill="#002065", color=grey80k, stroke=1)+
  #geom_text_repel(aes(label=operatingunit), seed=123, point.size=4, size=8/.pt, color="#595959",family="Source Sans Pro")+
  geom_text(aes(label=operatingunit),size=8/.pt, vjust=1.75, color="#595959",family="Source Sans Pro")+
  geom_hline(yintercept=target_iit, color=grey90k, linetype="dotted")+
  geom_vline(xintercept=median, color=grey90k, linetype="dotted")+
  geom_vline(xintercept=target_mmd, color=grey60k, linetype="dotted")+
  scale_x_continuous(label=percent)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(NA, .1))+
  si_style_nolines()+
  theme(legend.position="none")+
  transition_states(period, transition_length=2, state_length=1)+
  labs(x = "3+ MMD", y = "IIT",
       title = "INTERRUPTIONS IN TREATMENT AND 3+ MMD LEVELS", subtitle = "{closest_state}")
  

gif<-animate(u3mmd, renderer=gifski_renderer(), width=1000, height=650, res=92)
anim_save("mygifrepeltest.gif")