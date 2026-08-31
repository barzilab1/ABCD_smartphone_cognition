library(purrr)
library(readr)
library(psych)
library(corrplot)
library(Hmisc)
library(dplyr)
library(lavaan)
library(tableone)
library(lme4)
library(sjPlot)
library(naniar)
library(broom.mixed)
library(lmerTest)
library(tibble)

get_model <- function(data, outcome, predictor, covariates = NULL, random_eff = "(1 | site_id_l_br/rel_family_id/src_subject_id)", control_model = FALSE, binary_DV = FALSE) {
  
  mod_formula <- reformulate(c(covariates, predictor, random_eff), response = outcome)
  
  model <- tryCatch({
    if (!binary_DV) {
      if (control_model) {
        # Use lmerControl with specified parameters
        control <- lmerControl(check.nobs.vs.nlev = "ignore",
                               check.nobs.vs.rankZ = "ignore",
                               check.nobs.vs.nRE = "ignore",
                               optimizer = "bobyqa",
                               optCtrl = list(maxfun = 2e5))
        model <- lmer(mod_formula, data = data, control = control)
      } else {
        model <- lmer(mod_formula, data = data)
      }
    } else {
      model <- glmer(mod_formula, data = data, nAGQ = 0, family = "binomial")
    }
    
    # Print the final model formula
    message("Final model formula: ", deparse(mod_formula))
    
    return(model)
  }, warning = function(w) {
    if (grepl("failed to converge", w$message, ignore.case = TRUE)) {
      if(control_model){
        message("Warning: ", w$message, " - also with control_model = TRUE")
        return()
      }
      message("Warning: ", w$message, " - retrying with control_model = TRUE")
      return(get_model(data, outcome, predictor, covariates, random_eff, control_model = TRUE, binary_DV = binary_DV))
    }
  }, error = function(e){ return(NULL)})
  
  return(model)
}


get_est <- function(model, DV, IV, binary_DV = F, conf_int = 0.983) {
  
  output <- data.frame("outcome" = DV, "predictor" = IV, "coef_or_OR" = NA,
                       "low_ci" = NA, "high_ci" = NA)
  
  mixed_eff <- tidy(model, effects = "fixed", conf.int = T, conf.level = conf_int)
  output[, "coef_or_OR"] <- mixed_eff %>% filter(term == IV) %>% pull(estimate) %>% round(., 2)
  output[, "low_ci"] <- mixed_eff %>% filter(term == IV) %>% pull(conf.low) %>% round(., 2)
  output[, "high_ci"] <- mixed_eff %>% filter(term == IV) %>% pull(conf.high) %>% round(., 2)
  
  if(binary_DV) {
    output[, "coef_or_OR"] <- round(exp(output[, "coef_or_OR"]), 2)
    output[, "low_ci"] <- round(exp(output[, "low_ci"]), 2)
    output[, "high_ci"] <- round(exp(output[, "high_ci"]), 2)
  }
  
  return(output)
}

# For the forest
extract_outputs <- function(list_mods, DV, IV, experiment, binary_DV = F) {
  # Extract the estimates/ORs and high/low CI
  if(binary_DV) {effects_output <- map(list_mods, ~get_est(., DV = DV, IV = IV, binary_DV = T))}
  else effects_output <- map(list_mods, ~get_est(., DV = DV, IV = IV))
  
  names_mod <- c("Model 1", "Model 2", "Model 3")
  
  effects_output_df <- effects_output %>% bind_rows() %>% mutate(Model = names_mod) %>% mutate(experiment = experiment)
  
  if(binary_DV) {
    effects_output_df <- effects_output_df %>% mutate(binary_DV = 1)
  } else {effects_output_df <- effects_output_df %>% mutate(binary_DV = 0)}
  
  return(effects_output_df)
}


write_formatted_results_s <- function(
    data,
    output_file,
    outcome_suffix = "_change2to4"
) {
  
  format_p <- function(x) {
    dplyr::case_when(
      is.na(x)  ~ NA_character_,
      x < 0.001 ~ "***",
      x < 0.01  ~ "**",
      x < 0.05  ~ "*",
      TRUE      ~ "NS"
    )
  }
  
  data %>%
    dplyr::mutate(
      Outcome_match = stringr::str_remove_all(Outcome, outcome_suffix)
    ) %>%
    dplyr::mutate(
      # formatted stars
      FDR_p_mod1_format              = format_p(FDR_p_mod1),
      FDR_p_mod2_main_format         = format_p(FDR_p_mod2_main),
      FDR_p_mod2_interaction_format  = format_p(FDR_p_mod2_interaction),
      FDR_p_mod3_format              = format_p(FDR_p_mod3),
      FDR_p_mod4_main_format         = format_p(FDR_p_mod4_main),
      FDR_p_mod4_interaction_format  = format_p(FDR_p_mod4_interaction)
    ) %>%
    dplyr::select(
      Predictor = var,
      Outcome, 
      Beta_mod1, CI_mod1, p_mod1, FDR_p_mod1, FDR_p_mod1_format, 
      Beta_mod2, CI_mod2, p_mod2, FDR_p_mod2_main, FDR_p_mod2_interaction,
      FDR_p_mod2_main_format, FDR_p_mod2_interaction_format, 
      Beta_mod3, CI_mod3, p_mod3, FDR_p_mod3, FDR_p_mod3_format, 
      Beta_mod4, CI_mod4, p_mod4, FDR_p_mod4_main, FDR_p_mod4_interaction,
      FDR_p_mod4_main_format, FDR_p_mod4_interaction_format
    ) %>% 
    writexl::write_xlsx(output_file)
}


run_write_models_addDVbaseline <- function(
    data,
    list_DVs,
    list_covars,
    random_eff,
    list_IVs,
    include_dv2y = FALSE,
    scale_dv2y = FALSE,
    binary_DV = FALSE,
    ext = NULL,
    CI_level = 0.95,
    string_replace_DV = c("_change2to4", "____02A")
) {
  
  stopifnot(is.character(list_covars))
  stopifnot(is.character(list_IVs))
  
  all_models <- purrr::map(list_DVs, function(outcome) {
    
    # ---- DV baseline term (ONLY if requested) ----
    if (include_dv2y) {
      dv_2y <- gsub(
        pattern = string_replace_DV[1],
        replacement = string_replace_DV[2],
        x = outcome
      )
      
      if (scale_dv2y) {
        dv_2y <- paste0("scale(", dv_2y, ")")
      }
    }
    
    models <- purrr::map(list_covars, function(covariate) {
      
      stopifnot(length(covariate) == 1)
      
      purrr::map(list_IVs, function(IV) {
        
        out <- list()
        
      
        # WITHOUT DV baseline
      
        if (!include_dv2y) {
          
          m <- get_model(
            data = data,
            outcome = outcome,
            predictor = IV,
            covariates = covariate,
            random_eff = random_eff,
            control_model = FALSE,
            binary_DV = binary_DV
          )
          
          if (!is.null(m)) {
            name <- paste(
              "IV",
              IV,
              gsub("\\s+", "", covariate),
              random_eff,
              sep = "__"
            )
            out[[name]] <- m
          }
        }
        
      
        # WITH DV baseline
      
        if (include_dv2y) {
          
          covar_plus_dv <- paste(covariate, dv_2y, sep = " + ")
          
          m <- get_model(
            data = data,
            outcome = outcome,
            predictor = IV,
            covariates = covar_plus_dv,
            random_eff = random_eff,
            control_model = FALSE,
            binary_DV = binary_DV
          )
          
          if (!is.null(m)) {
            name <- paste(
              "IV_DV",
              IV,
              gsub("\\s+", "", covariate),
              random_eff,
              sep = "__"
            )
            out[[name]] <- m
          }
        }
        
        out
      })
    })
    
    models <- unlist(models, recursive = TRUE)
    
    if (length(models) == 0) {
      message("No valid models created for DV: ", outcome)
      return(NULL)
    }
    
    file_path <- paste0("results/DV__", outcome)
    if (!is.null(ext)) {
      file_path <- paste0(file_path, "__", ext)
    }
    file_path <- paste0(file_path, ".xls")
    
    tryCatch({
      print(tab_model(models,
                      digits.rsq = 4,
                      digits.p = 5,
                      auto.label = FALSE,
                      collapse.ci = FALSE,
                      file = file_path,
                      show.ci = CI_level))
    }, error = function(e) {
      message("Error in writing models to file: ", e$message)
    })
    
    return(setNames(list(models), outcome))
  })
  
  return(unlist(all_models, recursive = FALSE))
}



read_and_filter_files <- function(
    directory = "results/",
    pattern,
    var_filter,
    num_mod = 2,
    iv_tag = "__IV__"
) {
  
  files <- list.files(path = directory, pattern = pattern, full.names = TRUE)
  cat("Number of files matched:", length(files), "\n")
  
  var_filter_regex <- paste(var_filter, collapse = "|")
  
  all_data <- purrr::map_dfr(files, function(file) {
    
    html_doc <- xml2::read_html(file)
    tables <- rvest::html_table(html_doc, fill = TRUE)
    if (length(tables) == 0) return(NULL)
    
    df <- tables[[1]]
    
    # Fix duplicate column names
    colnames(df) <- make.names(colnames(df), unique = TRUE)
    
    # Extract outcome name
    outcome <- stringr::str_extract(
      basename(file),
      paste0("(?<=DV__)(.*?)(?=", iv_tag, ")")
    ) %>%
      stringr::str_remove_all("scale\\(|\\)")
    
    # ---------------------------
    # Rename columns by model count
    # ---------------------------
    
    if (num_mod == 1) {
      colnames(df)[1:4] <- c(
        "var",
        "Beta_mod1",
        "CI_mod1",
        "p_mod1"
      )
    }
    
    if (num_mod == 2) {
      colnames(df)[1:7] <- c(
        "var",
        "Beta_mod1","CI_mod1","p_mod1",
        "Beta_mod2","CI_mod2","p_mod2"
      )
    }
    
    if (num_mod == 4) {
      colnames(df)[1:13] <- c(
        "var",
        "Beta_mod1","CI_mod1","p_mod1",
        "Beta_mod2","CI_mod2","p_mod2",
        "Beta_mod3","CI_mod3","p_mod3",
        "Beta_mod4","CI_mod4","p_mod4"
      )
    }
    
    df %>%
      dplyr::filter(stringr::str_detect(var, var_filter_regex)) %>%
      dplyr::mutate(Outcome = outcome)
  })
  
  # -----------------------
  # FDR correction
  # -----------------------
  
  if (num_mod == 2) {
    
    p1 <- suppressWarnings(as.numeric(stringr::str_remove(all_data$p_mod1, "<")))
    p2 <- suppressWarnings(as.numeric(stringr::str_remove(all_data$p_mod2, "<")))
    
    is_interaction <- stringr::str_detect(all_data$var, ":")
    is_main <- !is_interaction
    
    all_data <- all_data %>%
      dplyr::mutate(
        FDR_p_mod1 = p.adjust(p1, method = "fdr"),
        FDR_p_mod2_interaction = ifelse(
          is_interaction,
          p.adjust(p2, method = "fdr"),
          NA_real_
        ),
        FDR_p_mod2_main = ifelse(
          is_main,
          p.adjust(p2, method = "fdr"),
          NA_real_
        )
      )
  }
  
  if (num_mod == 4) {
    
    p1 <- suppressWarnings(as.numeric(stringr::str_remove(all_data$p_mod1, "<")))
    p2 <- suppressWarnings(as.numeric(stringr::str_remove(all_data$p_mod2, "<")))
    p3 <- suppressWarnings(as.numeric(stringr::str_remove(all_data$p_mod3, "<")))
    p4 <- suppressWarnings(as.numeric(stringr::str_remove(all_data$p_mod4, "<")))
    
    is_interaction <- stringr::str_detect(all_data$var, ":")
    is_main <- !is_interaction
    
    all_data <- all_data %>%
      dplyr::mutate(
        FDR_p_mod1 = p.adjust(p1, method = "fdr"),
        
        FDR_p_mod2_main = ifelse(
          is_main,
          p.adjust(p2, method = "fdr"),
          NA_real_
        ),
        
        FDR_p_mod2_interaction = ifelse(
          is_interaction,
          p.adjust(p2, method = "fdr"),
          NA_real_
        ),
        
        FDR_p_mod3 = p.adjust(p3, method = "fdr"),
        
        FDR_p_mod4_main = ifelse(
          is_main,
          p.adjust(p4, method = "fdr"),
          NA_real_
        ),
        
        FDR_p_mod4_interaction = ifelse(
          is_interaction,
          p.adjust(p4, method = "fdr"),
          NA_real_
        )
      )
  }
  
  return(all_data)
}



# SPLIT-HALF BOOTSTRAP FUNCTION

extract_multi_beta_p <- function(models, key_predictors, list_IVs) {
  tryCatch({
    
    if (is.null(models) || length(models) == 0) {
      return(tibble(term = key_predictors, beta = NA_real_, p = NA_real_))
    }
    
    dv_name = names(models)[1]
    model_list = models[[dv_name]]
    
    if (is.null(model_list) || length(model_list) == 0) {
      return(tibble(term = key_predictors, beta = NA_real_, p = NA_real_))
    }
    
    model_match = names(model_list)[grepl(list_IVs[1], names(model_list), fixed = TRUE)][1]
    
    if (is.na(model_match)) {
      return(tibble(term = key_predictors, beta = NA_real_, p = NA_real_))
    }
    
    mod = model_list[[model_match]]
    coef_df = broom.mixed::tidy(mod, effects = "fixed")
    
    if (!"p.value" %in% names(coef_df)) coef_df$p.value = NA_real_
    
    res = map_dfr(key_predictors, function(kp) {
      row = coef_df %>% filter(grepl(kp, term)) %>% slice(1)
      
      if (nrow(row) == 0) {
        return(tibble(term = kp, beta = NA_real_, p = NA_real_))
      }
      
      tibble(term = kp,
             beta = as.numeric(row$estimate),
             p = as.numeric(row$p.value))
    })
    
    res
    
  }, error = function(e) tibble(term = key_predictors, beta = NA_real_, p = NA_real_))
}


run_split_half_bootstrap <- function(
    data, list_DVs, list_covars, random_eff, list_IVs,
    include_dv2y = FALSE, binary_DV = FALSE, B = 100,
    key_predictor = NULL, id_var = "participant_id",
    string_replace_DV = c("_change2to4", "____02A")
) {
  
  set.seed(042026)
  cat("Running", B, "bootstraps...\n")
  
  boot_results = map_dfr(1:B, function(b) {
    
    ids = unique(data[[id_var]])
    data_boot = data %>% filter(.data[[id_var]] %in% sample(ids))
    
    ids_boot = sample(unique(data_boot[[id_var]]))
    half = floor(length(ids_boot) / 2)
    
    data_g1 = data_boot %>% filter(.data[[id_var]] %in% ids_boot[1:half])
    data_g2 = data_boot %>% filter(.data[[id_var]] %in% ids_boot[(half + 1):length(ids_boot)])
    
    models_g1 = tryCatch(
      run_write_models_addDVbaseline(
        data = data_g1, list_DVs = list_DVs, list_covars = list_covars,
        random_eff = random_eff, list_IVs = list_IVs,
        include_dv2y = include_dv2y, binary_DV = binary_DV,
        string_replace_DV = string_replace_DV
      ),
      error = function(e) NULL
    )
    
    models_g2 = tryCatch(
      run_write_models_addDVbaseline(
        data = data_g2, list_DVs = list_DVs, list_covars = list_covars,
        random_eff = random_eff, list_IVs = list_IVs,
        include_dv2y = include_dv2y, binary_DV = binary_DV,
        string_replace_DV = string_replace_DV
      ),
      error = function(e) NULL
    )
    
    res = full_join(
      extract_multi_beta_p(models_g1, key_predictor, list_IVs),
      extract_multi_beta_p(models_g2, key_predictor, list_IVs),
      by = "term", suffix = c("_g1", "_g2")
    )
    
    for (col in c("beta_g1", "beta_g2", "p_g1", "p_g2")) {
      if (!col %in% names(res)) res[[col]] = NA_real_
    }
    
    out = res %>%
      mutate(
        sig_g1 = !is.na(p_g1) & p_g1 < 0.05,
        sig_g2 = !is.na(p_g2) & p_g2 < 0.05,
        both_sig = sig_g1 & sig_g2,
        same_sign = !is.na(beta_g1) & !is.na(beta_g2) & sign(beta_g1) == sign(beta_g2),
        same_sign_if_sig = both_sig & same_sign,
        split_id = b
      ) %>%
      select(split_id, term, beta_g1, beta_g2, p_g1, p_g2,
             sig_g1, sig_g2, both_sig, same_sign, same_sign_if_sig)
    
    if (nrow(out) == 0) {
      out = tibble(
        split_id = b, term = key_predictor,
        beta_g1 = NA_real_, beta_g2 = NA_real_,
        p_g1 = NA_real_, p_g2 = NA_real_,
        sig_g1 = FALSE, sig_g2 = FALSE,
        both_sig = FALSE, same_sign = FALSE,
        same_sign_if_sig = FALSE
      )
    }
    
    out
  })
  
  list(
    boot_df = boot_results,
    summary = boot_results %>%
      summarise(
        prop_sig_g1 = mean(sig_g1, na.rm = TRUE),
        prop_sig_g2 = mean(sig_g2, na.rm = TRUE),
        prop_both_sig = mean(both_sig, na.rm = TRUE),
        prop_same_sign = mean(same_sign, na.rm = TRUE),
        prop_same_sign_if_sig = mean(same_sign_if_sig, na.rm = TRUE),
        n = n()
      )
  )
}


run_split_half_bootstrap_allDV <- function(
    data, list_DVs, list_covars, random_eff, list_IVs,
    include_dv2y = FALSE, binary_DV = FALSE, B = 10,
    key_predictor = NULL, id_var = "participant_id",
    string_replace_DV = c("_change2to4", "____02A")
) {
  
  cat("Running ALL DVs...\n")
  
  results_all = map_dfr(list_DVs, function(dv) {
    res = run_split_half_bootstrap(
      data = data, list_DVs = dv, list_covars = list_covars,
      random_eff = random_eff, list_IVs = list_IVs,
      include_dv2y = include_dv2y, binary_DV = binary_DV,
      B = B, key_predictor = key_predictor,
      id_var = id_var, string_replace_DV = string_replace_DV
    )
    res$boot_df %>% mutate(DV = dv)
  })
  
  list(
    boot_df_all = results_all,
    summary = results_all %>%
      group_by(DV, term) %>%
      summarise(
        prop_sig_g1 = mean(sig_g1, na.rm = TRUE),
        prop_sig_g2 = mean(sig_g2, na.rm = TRUE),
        prop_both_sig = mean(both_sig, na.rm = TRUE),
        prop_same_sign = mean(same_sign, na.rm = TRUE),
        prop_same_sign_if_sig = mean(same_sign_if_sig, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
  )
}