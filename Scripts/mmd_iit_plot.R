#Purpose: Create visualization using ggplot comparing performance in MMD and IIT
install.packages("ggrepel")
library(ggrepel)

si_palettes$moody_blues %>% show_col()

df_filepath <- "~/Github/MSD/msd_fy21_q2_clean_psnu.txt"
df<- read_msd(df_filepath)

#Munge MMD data
df1 <- df %>% 
  filter(indicator %in% (c("TX_CURR")),
         standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator"),
         fiscal_year=="2021",
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
  group_by(operatingunit, otherdisaggregate) %>% 
  summarise(across(c(qtr2), sum, na.rm=TRUE)) %>% 
  filter(otherdisaggregate!="<3") %>% 
  spread(otherdisaggregate, qtr2) %>%
  ungroup() %>% 
  mutate(Three_Months_Share=threem/tn,
         Six_Months_Share=sixm/tn) %>% 
  pivot_longer(c(Three_Months_Share, Six_Months_Share), names_to="MMD_Prop") %>% 
  select(operatingunit, MMD_Prop, value) %>% 
  group_by(operatingunit) %>% 
  drop_na(value) %>% 
  mutate(mmd_color=case_when(
    MMD_Prop=="Six_Months_Share" ~si_palettes$moody_blues[3],
    MMD_Prop=="Three_Months_Share" ~si_palettes$moody_blues[5],
    TRUE ~ grey40k)) %>% 
  View()

#Munge TX_ML Numerator
df_ml <- df %>% 
  filter(indicator=="TX_ML",
         fiscal_year=="2021",
         standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus",
         otherdisaggregate %in% c("No Contact Outcome - Interruption in Treatment 3+ Months Treatment", 
                                  "No Contact Outcome - Interruption in Treatment <3 Months Treatment")) %>% 
  group_by(operatingunit) %>% 
  summarise(across(c(qtr2), sum, na.rm=TRUE)) %>%
  ungroup() %>% 
  View()

#Munge TX_ML Denominator
df_mld <- df %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"),
         fiscal_year=="2021",
         standardizeddisaggregate=="Total Numerator") %>% 
  group_by(operatingunit, indicator) %>% 
  summarise(across(c(qtr1, qtr2), sum, na.rm=TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from=indicator, names_sep="_", values_from=c(qtr1, qtr2)) %>% 
  mutate(iit_d=qtr1_TX_CURR+qtr2_TX_NEW) %>% 
  select(operatingunit, iit_d, qtr1_TX_CURR) %>% 
  View()


#Bar chart
df2 <- df_mld %>% 
  left_join(df_ml, df_mld, by="operatingunit") %>%
  mutate(val_iit=qtr2/iit_d,
         iit="iit") %>%
  select(operatingunit, val_iit, iit) %>%
  bind_rows(df1) %>% 
  View()


df2 %>% 
  mutate(MMD_Prop=factor(MMD_Prop, levels=c("Three_Months_Share", "Six_Months_Share"))) %>% 
  ggplot(aes(x=value, y=fct_reorder(operatingunit, value, sum, na.rm=TRUE), group=MMD_Prop, fill=mmd_color))+
  geom_col()+
  geom_point(aes(val_iit))+
  si_style_xgrid()+
  scale_x_continuous(labels = percent, 
                     breaks = c(seq(0, 1, .25))) +
  labs(x = NULL, y = NULL) +
  theme(panel.spacing = unit(0.25, "lines"))+
  scale_fill_identity()

#Scatter

##3 MMD

df_mmd<-df1 %>% 
  group_by(operatingunit) %>%
  summarise(across(c(value), sum, na.rm=TRUE)) %>%
  View()
  
df_iit <- df_mld %>% 
  left_join(df_ml, df_mld, by="operatingunit") %>%
  mutate(iit=qtr2/iit_d) %>%
  select(operatingunit, iit, qtr1_TX_CURR) %>%
  View()

df_mmd_iit <- left_join(df_mmd, df_iit, by="operatingunit") %>% 
  View()
  
median<-median(df_mmd_iit$value)
target_mmd<-.9
target_6mmd<-.5
target_iit<-.02

df_mmd_iit %>%
  mutate(operatingunit=recode(operatingunit,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR",
                              "Western Hemisphere Region" = "WHR"),
         median=median(value)) %>% 
  ggplot(aes(value, iit, color=operatingunit))+
  annotate("rect", xmin = Inf, xmax = median, ymin = Inf, ymax = target_iit, fill= grey20k, alpha=.3)  + 
  annotate("rect", xmin = -Inf, xmax = median, ymin = -Inf, ymax = target_iit , fill= grey20k, alpha=.3) + 
  annotate("rect", xmin = median, xmax = Inf, ymin = target_iit, ymax = -Inf, fill= "#bfddff", alpha=.4) + 
  annotate("rect", xmin = median, xmax = -Inf, ymin = Inf, ymax = target_iit, fill= "#ffb5ba", alpha=.4) + 
  geom_point(aes(size=qtr1_TX_CURR), shape=21, alpha=.6, fill="#002065", color=grey80k, stroke=1)+
  geom_text_repel(aes(label=operatingunit), point.size=4, size=10/.pt, color="#595959",family="Source Sans Pro")+
  geom_hline(yintercept=target_iit, color=grey90k, linetype="dotted")+
  geom_vline(xintercept=median, color=grey90k, linetype="dotted")+
  geom_vline(xintercept=target_mmd, color=grey60k, linetype="dotted")+
  scale_x_continuous(label=percent)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  si_style_nolines()+
  labs(x = "3+ MMD", y = "IIT",
       title = "INTERRUPTIONS IN TREATMENT AND 3+ MMD LEVELS, FY21 Q2") +
  theme(legend.position="none")

ggsave("Images/IIT_3MMD_comparison.pdf", device = cairo_pdf,
       height = 7, width = 8, units = "in")

ggsave("Images/IIT_3MMD_comparison.png",
       height = 7, width = 8, units="in")

#############6 MMD

df_6mmd<-df1 %>%
  filter(MMD_Prop=="Six_Months_Share") %>% 
  select(operatingunit, value) 

df_6mmd_iit <- left_join(df_6mmd, df_iit, by="operatingunit") %>% 
  View()

median6<-median(df_6mmd_iit$value)

df_6mmd_iit %>%
  mutate(operatingunit=recode(operatingunit,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR",
                              "Western Hemisphere Region" = "WHR"),
         median=median(value)) %>% 
  ggplot(aes(value, iit, color=operatingunit))+
  annotate("rect", xmin = Inf, xmax = median6, ymin = Inf, ymax = target_iit, fill= grey20k, alpha=.3)  + 
  annotate("rect", xmin = -Inf, xmax = median6, ymin = -Inf, ymax = target_iit , fill= grey20k, alpha=.3) + 
  annotate("rect", xmin = median6, xmax = Inf, ymin = target_iit, ymax = -Inf, fill= "#bfddff", alpha=.4) + 
  annotate("rect", xmin = median6, xmax = -Inf, ymin = Inf, ymax = target_iit, fill= "#ffb5ba", alpha=.4) + 
  geom_point(aes(size=qtr1_TX_CURR), shape=21, alpha=.6, fill="#002065", color=grey80k, stroke=1)+
  geom_text_repel(aes(label=operatingunit), point.size=4, size=10/.pt, color="#595959",family="Source Sans Pro")+
  geom_hline(yintercept=target_iit, color=grey90k, linetype="dotted")+
  geom_vline(xintercept=median6, color=grey90k, linetype="dotted")+
  geom_vline(xintercept=target_6mmd, color=grey60k, linetype="dotted")+
  scale_x_continuous(label=percent)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  si_style_nolines()+
  labs(x = "6+ MMD", y = "IIT",
       title = "INTERRUPTIONS IN TREATMENT AND 6+ MMD LEVELS, FY21 Q2") +
  theme(legend.position="none")

ggsave("Images/IIT_6MMD_comparison.pdf", device = cairo_pdf,
       height = 7, width = 8, units = "in")

ggsave("Images/IIT_6MMD_comparison.png",
       height = 7, width = 8, units="in")

si_palettes$old_roses %>% show_col()
si_palettes$moody_blues %>% show_col()
si_palettes$denims %>% show_col()
si_palettes$moody_blues[5]
