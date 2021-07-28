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

# ou_color = case_when(
#   duration == "6+ Month MMD" & pct >= 0.5 ~ genoa,
#   duration == "6+ Month MMD" & pct < 0.5 ~ genoa_light,
#   duration == "3+ Month MMD" & pct >= .90 ~ moody_blue,
#   duration == "3+ Month MMD" & pct < .90 ~ moody_blue_light,
#   duration == "<3 Months (non-MMD)" ~ burnt_sienna,
#   TRUE ~ grey40k),


# mmd %>% 
#   filter(!is.na(pct_new)) %>% 
#   ggplot(aes(x = pct, y = ou_order, fill = ou_color)) +
#   geom_col(alpha = 0.85, na.rm = T) +
#   geom_text(aes(label = percent(ou_label, 1)), hjust = 1.05,
#             family = "Source Sans Pro",
#             color = "white", size = 3) +
#   facet_wrap(~mmd_order, scale = "free_y", drop = T) +
#   scale_y_reordered() +
#   si_style_xgrid() +
#   scale_x_continuous(labels = percent, 
#                      breaks = c(seq(0, 1, .2))) +
#   scale_fill_identity() +
#   coord_cartesian(expand = F) +
#   labs(x = NULL, y = NULL) +
#   theme(panel.spacing = unit(0.25, "lines"))



#Plot w annotation 
# plot_annotate <- df %>%
#   ggplot2::ggplot(
#     aes(x = reorder(mech_code, desc(val)),
#         y = val
#     )) +
#   geom_bar(stat = "identity", fill=USAID_mgrey, alpha=.8, width=.4)+
#   xlab("mechanism")+
#   ylab("")+
#   annotate(
#     geom = "curve", x = 3.5, y = 1000, xend = 3, yend = 650, #determine arrow placement on coordinate plane
#     curvature = .3, #control intensity of curve# 
#     arrow = arrow(length = unit(4, "mm")), #determine size of arrow's point
#     color=grey50k
#   ) +
#   annotate(geom = "text", x = 3.55, y = 1000, #determine text placement on coordinate plane
#            label = "Mechanism Ended FY20Q1", #what you want your text to say
#            hjust="left", size=4, color=grey50k, family="Gill Sans MT")+
#   si_style_nolines()




water2 %>%
  filter(country_name=="Uganda", #make variable, wrap into a function, use map or walk
         install_year>1970) %>%
  mutate(
    water_source = replace(water_source, water_source %notin% c("Borehole", "Rainwater Harvesting", "Undefined Shallow Well", "Protected Spring"), "Other"),
    water_source=factor(water_source, levels=c("Borehole", "Rainwater Harvesting", "Protected Spring", "Undefined Shallow Well", "Other")),
    water_color=case_when(
      water_source=="Borehole" ~denim,
      water_source=="Rainwater Harvesting" ~golden_sand,
      water_source=="Protected Spring" ~genoa,
      water_source=="Undefined Shallow Well" ~moody_blue,
      TRUE ~ grey40k)) %>%
  group_by(water_source) %>% 
  mutate(max_value=case_when(count==max(count)~count)) %>% 
  ungroup() %>% 
  ggplot(aes(x=install_year, y=count, group=water_color, color=water_color))+
  geom_line(size=1)+
  geom_area(alpha=.2, aes(fill=water_color))+
  geom_text(aes(label=comma(max_value), color=grey70k, vjust=-.5),na.rm=TRUE)+
  geom_vline(xintercept=2000, color=grey80k, linetype="dotted")+
  geom_vline(xintercept=2010, color=grey80k, linetype="dotted")+
  si_style_ygrid()+
  facet_wrap(~water_source, nrow=2)+
  theme(legend.position="none") +
  scale_y_continuous(limits=c(NA, 2500), label=comma)+
  labs(x = NULL, y = NULL, color = NULL,
       title = "WATER SOURCE MONITORING FREQUENCY - UGANDA", family="Source Sans Pro")+
  scale_color_identity()+
  scale_fill_identity()