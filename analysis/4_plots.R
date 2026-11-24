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
list_DVs_cont_nerocog <- c("nc_y_nihtb__flnkr__uncor_score", "nc_y_nihtb__picsq__uncor_score", "nc_y_nihtb__picvcb__uncor_score",
                           "nc_y_nihtb__pttcp__uncor_score", "nc_y_nihtb__readr__uncor_score")
labels <- c("Flanker", "Picture\nSequence", "Picture\nVocabulary", "Pattern\nComparison", "Oral Reading")
covars <- c("sex_br", paste0("nt_p_yst_001___", c(2, 3, 4, 6)), "household_income", "parents_high_edu_br")
covars_strat <- setdiff(covars, "household_income") # Remove income only in stratified models
site <- "site_br"
groups <- c("Nonowners", "Smartphone owners by Y2")

# Load data
sm_groups <- readRDS("data/sm_groups.rds")
merged_dat_smartp <- readRDS("data/merged_dat_smartp.rds")

dat <- merged_dat_smartp %>%
    filter(session_id %in% c("ses-02A", "ses-04A")) %>%
    inner_join(sm_groups %>% distinct(participant_id, exposure_group), by = "participant_id") %>%
    mutate(sample = case_when(participant_id %in% id_gr1 ~ "Discovery",
                              participant_id %in% id_gr2 ~ "Replication"),
           exposure_group = factor(exposure_group, levels = c("control", "sm_2y_4y", "sm_3y", "sm_4y")),
           household_income_75kless = factor(household_income_75kless, levels = c(0, 1),
                                             labels = c("Household income ≥ $75,000", "Household income < $75,000"))) %>%
    filter(!is.na(sample))

# Use baseline Y2 income for stratification
income_y2 <- dat %>% filter(session_id == "ses-02A") %>%
    distinct(participant_id, income_group = household_income_75kless)

dat <- dat %>% select(-household_income_75kless) %>%
    left_join(income_y2, by = "participant_id")

# Same smartphone model, fitted separately by income stratum
fit_mod_plot <- function(o, s, inc) {
    adj <- if (inc == "Overall") covars else covars_strat

    d <- dat %>% filter(sample == s, if (inc == "Overall") TRUE else income_group == inc) %>%
        mutate(across(all_of(adj), as.factor)) %>%
        drop_na(all_of(c(o, "age_br", "exposure_group", adj, site, "family_id_br"))) %>%
        group_by(participant_id) %>%
        filter(n() == 2, n_distinct(session_id) == 2, n_distinct(age_br) == 2) %>%
        ungroup() %>%
        mutate(y = as.numeric(scale(.data[[o]]))) %>% droplevels()

    f <- as.formula(paste0("y ~ age_br * exposure_group + ", paste(adj, collapse = " + "),
                           " + (1|", site, "/family_id_br)"))
    m <- lmer(f, data = d, REML = FALSE)
    m0 <- lmer(update(f, . ~ . - age_br:exposure_group), data = d, REML = FALSE)

    b <- fixef(m)
    beta_NSP <- unname(b["age_br"])
    beta_SP <- unname(b["age_br"] + b["age_br:exposure_groupsm_2y_4y"])
    beta_age <- unname(fixef(m0)["age_br"])

    base <- d %>% filter(session_id == "ses-02A")
    a2 <- mean(base$age_br)
    a4 <- mean(d$age_br[d$session_id == "ses-04A"])

    # Two smartphone groups with model-based 95% CI
    pred <- map_dfr(1:2, function(i) {
        map_dfr(c(0, 2), function(t) {
            nd <- base %>% mutate(age_br = if (t == 0) a2 else a4,
                                  exposure_group = factor(c("control", "sm_2y_4y")[i],
                                                          levels = levels(d$exposure_group)))
            X <- model.matrix(delete.response(terms(lme4::nobars(formula(m)))), nd)
            X <- X[, names(b), drop = FALSE]
            w <- colMeans(X)
            mu <- sum(w * b)
            se <- sqrt(drop(t(w) %*% as.matrix(vcov(m)) %*% w))

            tibble(group = groups[i], year = t, cognition = mu,
                   lower = mu - 1.96 * se, upper = mu + 1.96 * se)
        })
    })

    list(
        pred = pred %>% mutate(income_group = inc, sample = s, outcome = o),
        effect = tibble(income_group = inc, sample = s, outcome = o,
                        n = n_distinct(d$participant_id),
                        beta_NSP = beta_NSP, beta_SP = beta_SP, beta_age = beta_age,
                        difference = beta_NSP - beta_SP,
                        relative_pct = ifelse(abs(beta_age) > 1e-10,
                                              100 * (beta_NSP - beta_SP) / beta_age, NA_real_))
    )
}

# Run overall and both income-stratified analyses
res <- crossing(outcome = list_DVs_cont_nerocog, sample = c("Discovery", "Replication"),
                income_group = c("Overall", "Household income ≥ $75,000", "Household income < $75,000")) %>%
    mutate(result = pmap(list(outcome, sample, income_group), fit_mod_plot))

effects <- bind_rows(map(res$result, "effect"))
pd <- bind_rows(map(res$result, "pred")) %>%
    mutate(outcome = factor(outcome, levels = list_DVs_cont_nerocog, labels = labels),
           sample = factor(sample, levels = c("Discovery", "Replication")),
           income_group = factor(income_group, levels = c("Overall", "Household income ≥ $75,000", "Household income < $75,000")),
           group = factor(group, levels = groups))

# Plot smartphone trajectories by income stratum
plot_trajectories <- function(stratified = FALSE) {
    d <- pd %>% filter(if (stratified) income_group != "Overall" else income_group == "Overall")

    p <- ggplot(d, aes(year, cognition, color = group, group = group)) +
        geom_line(linewidth = 1.2) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.12, linewidth = 0.7) +
        scale_x_continuous(breaks = c(0, 2), labels = c("Y2", "Y4"), limits = c(-0.1, 2.4)) +
        scale_color_manual(values = c("Nonowners" = "#159B65", "Smartphone owners by Y2" = "#E58B30")) +
        labs(x = NULL, y = "Adjusted cognitive score (SD)", color = NULL) +
        theme_bw(base_size = 14) +
        theme(axis.title.y = element_text(size = 22, face = "bold"),
              axis.text.x = element_text(size = 20, face = "bold"),
              axis.text.y = element_text(size = 20, face = "bold"),
              strip.background = element_rect(fill = "grey95", color = NA),
              strip.text = element_text(size = 22, face = "bold"),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(1, "cm"),
              legend.position = "top",
              legend.text = element_text(size = 22, face = "bold"),
              legend.key.width = unit(3, "cm"))

    if (stratified) p + facet_grid(outcome ~ sample + income_group, scales = "fixed")
    else p + facet_grid(outcome ~ sample, scales = "fixed")
}

# Export
dir.create("plots", showWarnings = FALSE, recursive = TRUE)
dir.create("data", showWarnings = FALSE, recursive = TRUE)

p <- plot_trajectories()
p_strat <- plot_trajectories(TRUE)

ggsave("plots/adjusted_trajectories_v2_test.pdf", p, width = 12, height = 15, dpi = 320)
ggsave("plots/income_stratified_trajectories_test.pdf", p_strat, width = 18, height = 15, dpi = 320)

write.csv(filter(effects, income_group == "Overall"), "data/adjusted_slopes_test.csv", row.names = FALSE)
write.csv(filter(pd, income_group == "Overall"), "data/slopes_test.csv", row.names = FALSE)
write.csv(filter(effects, income_group != "Overall"), "data/income_stratified_adjusted_slopes_test.csv", row.names = FALSE)
write.csv(filter(pd, income_group != "Overall"), "data/income_stratified_slopes_test.csv", row.names = FALSE)

print(p)
print(p_strat)
