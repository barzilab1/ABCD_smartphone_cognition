library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)
library(lme4)

# merged_dat_smartp_analysis3_with_resid <- readRDS("data/merged_dat_smartp_analysis3_with_resid.rds")%>%
#   mutate(exposure_group = factor(exposure_group, c("control", "sm_2y_4y", "sm_3y", "sm_4y"))) %>%
#   select(participant_id, session_id, matches("residuals"), exposure_group) %>%
#   filter(!is.na(exposure_group))
# merged_dat_smartp_analysis3_with_resid
ABCD_participants_2grs <- read.table(file = "~/Library/CloudStorage/Box-Box/2. Barzi Lab - Restricted Access/2-ABCD/ABCD data/partition/participants.tsv", header = TRUE)
ABCD_participants_2grs <- ABCD_participants_2grs %>%
  mutate(participant_id = stringr::str_replace_all(participant_id, "NDARINV", ""))

id_gr1 <- ABCD_participants_2grs %>% filter(matched_group == 1) %>% pull(participant_id)
id_gr2 <- ABCD_participants_2grs %>% filter(matched_group == 2) %>% pull(participant_id)

# merged_dat_smartp_with_resid_subcohort1 <- merged_dat_smartp_analysis3_with_resid %>% filter(participant_id %in% id_gr1)
# merged_dat_smartp_with_resid_subcohort2 <- merged_dat_smartp_analysis3_with_resid %>% filter(participant_id %in% id_gr2)



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
  cohort = rep(c("Discovery","Discovery","Replication","Replication"), each = 5),
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

  cog[1],mod[1],"Discovery",-0.29,-0.41,-0.17,
  cog[1],mod[1],"Replication",-0.22,-0.34,-0.10,
  cog[1],mod[2],"Discovery",-0.22,-0.35,-0.08,
  cog[1],mod[2],"Replication",-0.17,-0.31,-0.04,
  cog[1],mod[3],"Discovery",-0.09,-0.24,0.05,
  cog[1],mod[3],"Replication",-0.05,-0.18,0.09,

  cog[2],mod[1],"Discovery",0.02,-0.13,0.16,
  cog[2],mod[1],"Replication",-0.10,-0.24,0.03,
  cog[2],mod[2],"Discovery",0.02,-0.14,0.18,
  cog[2],mod[2],"Replication",-0.06,-0.22,0.09,
  cog[2],mod[3],"Discovery",0.05,-0.12,0.21,
  cog[2],mod[3],"Replication",-0.05,-0.21,0.11,

  cog[3],mod[1],"Discovery",0.15,-0.01,0.30,
  cog[3],mod[1],"Replication",0.04,-0.11,0.19,
  cog[3],mod[2],"Discovery",0.13,-0.03,0.30,
  cog[3],mod[2],"Replication",0.10,-0.07,0.26,
  cog[3],mod[3],"Discovery",0.21,0.03,0.38,
  cog[3],mod[3],"Replication",0.01,-0.16,0.18,

  cog[4],mod[1],"Discovery",-0.13,-0.25,-0.02,
  cog[4],mod[1],"Replication",-0.20,-0.32,-0.07,
  cog[4],mod[2],"Discovery",-0.12,-0.25,0.01,
  cog[4],mod[2],"Replication",-0.14,-0.28,0.00,
  cog[4],mod[3],"Discovery",-0.07,-0.21,0.07,
  cog[4],mod[3],"Replication",-0.06,-0.21,0.09,

  cog[5],mod[1],"Discovery",-0.09,-0.21,0.02,
  cog[5],mod[1],"Replication",-0.09,-0.20,0.03,
  cog[5],mod[2],"Discovery",-0.05,-0.18,0.07,
  cog[5],mod[2],"Replication",-0.06,-0.19,0.07,
  cog[5],mod[3],"Discovery",-0.03,-0.17,0.11,
  cog[5],mod[3],"Replication",-0.04,-0.17,0.09
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


# Revision----
# Variables
list_DVs_cont_nerocog <- c("nc_y_nihtb__picvcb__uncor_score", "nc_y_nihtb__flnkr__uncor_score", "nc_y_nihtb__pttcp__uncor_score",
                           "nc_y_nihtb__readr__uncor_score", "nc_y_nihtb__picsq__uncor_score")
labels <- c("Flanker", "Picture\nSequence", "Picture\nVocabulary", "Pattern\nComparison", "Oral Reading")
covars <- c("sex_br", paste0("nt_p_yst_001___", c(2, 3, 4, 6)),
            "household_income", "parents_high_edu_br")
site <- "site_br"

# Load data
sm_groups <- readRDS("data/sm_groups.rds")
merged_dat_smartp <- readRDS("data/merged_dat_smartp.rds")

# Prepare Y2/Y4 data; retain all four exposure groups
dat <- merged_dat_smartp %>%
    filter(session_id %in% c("ses-02A", "ses-04A")) %>%
    inner_join(sm_groups %>% distinct(participant_id, exposure_group),
               by = "participant_id") %>%
    mutate(sample = case_when(participant_id %in% id_gr1 ~ "Discovery",
                              participant_id %in% id_gr2 ~ "Replication"),
           exposure_group = factor(exposure_group,
                                   levels = c("control", "sm_2y_4y", "sm_3y", "sm_4y"))) %>%
    filter(!is.na(sample))

# Fit adjusted models and predict cognition at Y2/Y4
fit_mod_plot <- function(o, s) {
    d <- dat %>% filter(sample == s) %>%
        mutate(across(all_of(covars), as.factor)) %>%
        drop_na(all_of(c(o, "age_br", covars, site, "family_id_br"))) %>%
        group_by(participant_id) %>%
        filter(n() == 2, n_distinct(session_id) == 2, n_distinct(age_br) == 2) %>%
        ungroup() %>%
        mutate(y = as.numeric(scale(.data[[o]]))) %>% droplevels()

    # Interaction model: nonowner and Y2-owner slopes
    f <- as.formula(paste0(
        "y ~ age_br * exposure_group + ",
        paste(covars, collapse = " + "),
        " + (1|", site, "/family_id_br)"
    ))

    m <- lmer(f, data = d, REML = FALSE)

    # No-interaction model: common age slope across all four groups
    m0 <- lmer(update(f, . ~ . - age_br:exposure_group),
               data = d, REML = FALSE)

    b <- fixef(m)
    bn <- unname(b["age_br"])
    bs <- unname(b["age_br"] + b["age_br:exposure_groupsm_2y_4y"])
    ba <- unname(fixef(m0)["age_br"])

    # Predict adjusted cognitive scores at Y2 and Y4
    base <- d %>% filter(session_id == "ses-02A")
    a2 <- mean(base$age_br)
    a4 <- mean(d$age_br[d$session_id == "ses-04A"])

    pred <- map_dfr(c("Nonowners", "Smartphone owners by Y2", "Overall"), function(g) {
        map_dfr(c(0, 2), function(t) {
            nd <- base %>% mutate(age_br = if (t == 0) a2 else a4)

            if (g != "Overall")
                nd$exposure_group <- factor(
                    if (g == "Nonowners") "control" else "sm_2y_4y",
                    levels = levels(d$exposure_group)
                )

            tibble(
                group = g, year = t,
                cognition = mean(predict(
                    if (g == "Overall") m0 else m,
                    newdata = nd, re.form = NA
                ))
            )
        })
    })

    list(
        pred = pred %>% mutate(sample = s, outcome = o),
        effect = tibble(
            sample = s, outcome = o,
            beta_NSP = bn, beta_SP = bs, beta_age = ba,
            difference = bn - bs,
            relative_pct = 100 * (bn - bs) / ba
        )
    )
}

# Run analyses for all 5 outcomes × 2 samples
res <- crossing(outcome = list_DVs_cont_nerocog,
                sample = c("Discovery", "Replication")) %>%
    mutate(result = map2(outcome, sample, fit_mod_plot))

effects <- bind_rows(map(res$result, "effect"))

pd <- bind_rows(map(res$result, "pred")) %>%
    mutate(
        outcome = factor(outcome, levels = list_DVs_cont_nerocog, labels = labels),
        sample = factor(sample, levels = c("Discovery", "Replication")),
        group = factor(group, levels = c("Nonowners", "Smartphone owners by Y2", "Overall"))
    )

# Plot adjusted trajectories: lines only, no markers or arrows
p <- ggplot(pd, aes(year, cognition, color = group,
                    linetype = group, group = group)) +
    geom_line(linewidth = 1.2) +
    facet_grid(outcome ~ sample, scales = "fixed") +
    scale_x_continuous(breaks = c(0, 2), labels = c("Y2", "Y4"),
                       limits = c(-0.1, 2.4)) +
    scale_color_manual(values = c(
        "Nonowners" = "#159B65",
        "Smartphone owners by Y2" = "#E58B30",
        "Overall" = "#64748B"
    )) +
    scale_linetype_manual(values = c(
        "Nonowners" = "solid",
        "Smartphone owners by Y2" = "solid",
        "Overall" = "dashed"
    )) +
    labs(x = NULL, y = "Adjusted cognitive score (SD)",
         color = NULL, linetype = NULL) +
    theme_bw(base_size = 14) +
    theme(
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text.x  = element_text(size = 20, face = "bold"),
        axis.text.y  = element_text(size = 20, face = "bold"),
        strip.background = element_rect(fill = "grey95", color = NA),
        strip.text   = element_text(size = 22, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 22, face = "bold"),
        legend.key.width = unit(3, "cm")

    )


# Export figure and slope estimates
ggsave(file.path("plots", "adjusted_trajectories.pdf"),
       p, width = 12, height = 15, dpi = 320)

write.csv(effects, "data/adjusted_slopes.csv", row.names = FALSE)
