library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

merged_dat_smartp_analysis3_with_resid <- readRDS("data/merged_dat_smartp_analysis3_with_resid.rds")%>%
  mutate(exposure_group = factor(exposure_group, c("control", "sm_2y_4y", "sm_3y", "sm_4y"))) %>% 
  select(participant_id, session_id, matches("residuals"), exposure_group) %>% 
  filter(!is.na(exposure_group))
merged_dat_smartp_analysis3_with_resid
ABCD_participants_2grs <- read.table(file = "~/Library/CloudStorage/Box-Box/2. Barzi Lab - Restricted Access/2-ABCD/ABCD data/partition/participants.tsv", header = TRUE)
ABCD_participants_2grs <- ABCD_participants_2grs %>%
  mutate(participant_id = stringr::str_replace_all(participant_id, "NDARINV", ""))

id_gr1 <- ABCD_participants_2grs %>% filter(matched_group == 1) %>% pull(participant_id)
id_gr2 <- ABCD_participants_2grs %>% filter(matched_group == 2) %>% pull(participant_id)

merged_dat_smartp_with_resid_subcohort1 <- merged_dat_smartp_analysis3_with_resid %>% filter(participant_id %in% id_gr1)
merged_dat_smartp_with_resid_subcohort2 <- merged_dat_smartp_analysis3_with_resid %>% filter(participant_id %in% id_gr2)



# Figure 1----
cognition <- c(
  "Flanker Inhibitory Control", # -0.15	-0.23 – -0.07 # -0.13	-0.21 – -0.05
  "Picture Sequence Memory", # -0.27	-0.35 – -0.20 # -0.27	-0.34 – -0.20
  "Picture Vocabulary", # -0.4	-0.46 – -0.33 # -0.37	-0.44 – -0.31
  "Pattern Comparison Speed", # -0.08	-0.17 – 0.00 # -0.18	-0.26 – -0.10
  "Oral Reading Recognition" # -0.24	-0.31 – -0.17 # -0.25	-0.32 – -0.18
)

df_plot <- tibble(
  cognition = rep(cognition, 4),
  cohort = rep(c("Sub-cohort 1","Sub-cohort 1","Sub-cohort 2","Sub-cohort 2"), each = 5),
  exposure = rep(c("Smartphone ownership","Household income < $75,000",
                   "Smartphone ownership","Household income < $75,000"), each = 5),
  
  beta = c(
    0.03, -0.08, -0.13, 0.03, -0.10,
    # -0.29, -0.39, -0.71, -0.15, -0.51,
    -0.15, -0.27, -0.40, -0.08, -0.24,
    -0.02, 0.00, -0.04, 0.05, 0.02,
    # -0.30, -0.39, -0.70, -0.29, -0.55
    -0.13, -0.27, -0.37, -0.18, -0.25
  ),
  
  ci_low = c(
    -0.04, -0.14, -0.19, -0.05, -0.16,
    # -0.37, -0.45, -0.77, -0.22, -0.58,
    -0.23, -0.35, -0.46, -0.17, -0.31,
    -0.09, -0.07, -0.09, -0.02, -0.04,
    # -0.37, -0.45, -0.76, -0.36, -0.61
    -0.21, -0.34, -0.44, -0.26, -0.32
  ),
  
  ci_high = c(
    0.10, -0.01, -0.07, 0.10, -0.04,
    # -0.22, -0.33, -0.65, -0.08, -0.45,
    -0.07, -0.20, -0.33, 0.00, -0.17,
    0.05, 0.06, 0.02, 0.13, 0.08,
    # -0.23, -0.33, -0.64, -0.22, -0.49
    -0.05, -0.20, -0.31, -0.10, -0.18
  )
)

df_plot$cognition <- factor(df_plot$cognition, levels = rev(cognition))

df_plot$exposure <- factor(df_plot$exposure, levels = c("Smartphone ownership", "Household income < $75,000"))

figure1 <- ggplot(df_plot, aes(x = beta, y = cognition, color = cognition)) +
  geom_point(size = 5) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.3, linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 1.5) +
  facet_grid(cohort ~ exposure) +
  scale_x_continuous(labels = function(x) {
    out <- format(x, trim = TRUE, scientific = FALSE)
    out <- sub("\\.0+$", "", out)
    sub("(\\.\\d*[1-9])0+$", "\\1", out)
  }) +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
  labs(x = "Standardized Beta (95% Confidence Interval)", y = NULL) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.text.x  = element_text(size = 20, face = "bold"),
    axis.text.y  = element_text(size = 20, face = "bold"),
    strip.text   = element_text(size = 22, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.spacing = unit(1, "cm")
  )

ggsave(filename = "plots/figure1_final.pdf", plot = figure1, height = 10, width = 15, dpi = 320)

# Figure 2----
cog <- str_wrap(c(
  "Picture Vocabulary",
  "Flanker Inhibitory Control",
  "Pattern Comparison Speed",
  "Oral Reading Recognition",
  "Picture Sequence Memory"), 20)

mod <- c(
  "Acquired a smartphone before Y2",
  "Acquired a smartphone between Y2-Y3",
  "Acquired a smartphone between Y3-Y4"
)

df <- tribble(
  ~cognition, ~model, ~cohort, ~beta, ~lo, ~hi,
  
  cog[1],mod[1],"Sub-cohort 1",-0.29,-0.41,-0.17,
  cog[1],mod[1],"Sub-cohort 2",-0.22,-0.34,-0.10,
  cog[1],mod[2],"Sub-cohort 1",-0.22,-0.35,-0.08,
  cog[1],mod[2],"Sub-cohort 2",-0.17,-0.31,-0.04,
  cog[1],mod[3],"Sub-cohort 1",-0.09,-0.24,0.05,
  cog[1],mod[3],"Sub-cohort 2",-0.05,-0.18,0.09,
  
  cog[2],mod[1],"Sub-cohort 1",0.02,-0.13,0.16,
  cog[2],mod[1],"Sub-cohort 2",-0.10,-0.24,0.03,
  cog[2],mod[2],"Sub-cohort 1",0.02,-0.14,0.18,
  cog[2],mod[2],"Sub-cohort 2",-0.06,-0.22,0.09,
  cog[2],mod[3],"Sub-cohort 1",0.05,-0.12,0.21,
  cog[2],mod[3],"Sub-cohort 2",-0.05,-0.21,0.11,
  
  cog[3],mod[1],"Sub-cohort 1",0.15,-0.01,0.30,
  cog[3],mod[1],"Sub-cohort 2",0.04,-0.11,0.19,
  cog[3],mod[2],"Sub-cohort 1",0.13,-0.03,0.30,
  cog[3],mod[2],"Sub-cohort 2",0.10,-0.07,0.26,
  cog[3],mod[3],"Sub-cohort 1",0.21,0.03,0.38,
  cog[3],mod[3],"Sub-cohort 2",0.01,-0.16,0.18,
  
  cog[4],mod[1],"Sub-cohort 1",-0.13,-0.25,-0.02,
  cog[4],mod[1],"Sub-cohort 2",-0.20,-0.32,-0.07,
  cog[4],mod[2],"Sub-cohort 1",-0.12,-0.25,0.01,
  cog[4],mod[2],"Sub-cohort 2",-0.14,-0.28,0.00,
  cog[4],mod[3],"Sub-cohort 1",-0.07,-0.21,0.07,
  cog[4],mod[3],"Sub-cohort 2",-0.06,-0.21,0.09,
  
  cog[5],mod[1],"Sub-cohort 1",-0.09,-0.21,0.02,
  cog[5],mod[1],"Sub-cohort 2",-0.09,-0.20,0.03,
  cog[5],mod[2],"Sub-cohort 1",-0.05,-0.18,0.07,
  cog[5],mod[2],"Sub-cohort 2",-0.06,-0.19,0.07,
  cog[5],mod[3],"Sub-cohort 1",-0.03,-0.17,0.11,
  cog[5],mod[3],"Sub-cohort 2",-0.04,-0.17,0.09
) %>%
  mutate(
    cognition = factor(cognition, rev(cog)),
    model = factor(model, rev(mod))
  )


figure2 <- ggplot(df,aes(beta,model,color=model))+
  geom_point(size=5)+
  geom_errorbarh(aes(xmin=lo,xmax=hi),height=.25,linewidth=1.4)+
  geom_vline(xintercept=0,lty=2,color="grey40")+
  facet_grid(cognition~cohort)+
  scale_color_manual(values=c("#0072B2","#E69F00","#009E73"))+
  labs(x="Standardized Beta (95% CI)",y=NULL)+
  theme_bw(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 26, face = "bold"),
    axis.text.x  = element_text(size = 20, face = "bold"),
    axis.text.y  = element_text(size = 22),
    strip.text   = element_text(size = 24, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "cm"),   # larger panels
    legend.position = "none",        # remove legend
    plot.margin = margin(20,30,20,30)
  )


ggsave("plots/figure2_final_v2.pdf", figure2, width = 20, height = 22, dpi = 320)
