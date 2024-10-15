select_features_based_on_proportion <- function(original_data, ranking_list, num_features_to_select) {
  clinical_data <- original_data$clinical
  original_data <- original_data[names(original_data) != "clinical"]
  ranking_list <- ranking_list[names(ranking_list) != "clinical"]

  total_features <- sum(sapply(original_data, ncol)) - length(original_data) # Subtract ID columns

  features_per_dataset <- sapply(original_data, function(x) {
    num <- (ncol(x) - 1) / total_features * num_features_to_select
    if (num > 0.5 & num <= 1) num <- 1 else round(num)
  })

  difference <- num_features_to_select - sum(features_per_dataset)
  while (difference != 0) {
    if (difference > 0) {
      dataset_to_increase <- which.max(features_per_dataset)
      features_per_dataset[dataset_to_increase] <- features_per_dataset[dataset_to_increase] + 1
    } else {
      dataset_to_decrease <- which.max(features_per_dataset)
      features_per_dataset[dataset_to_decrease] <- features_per_dataset[dataset_to_decrease] - 1
    }
    difference <- num_features_to_select - sum(features_per_dataset)
  }

  selected_features <- lapply(names(original_data), function(dataset) {
    if (features_per_dataset[dataset] > 0) {
      selected_feats <- na.omit(ranking_list[[dataset]][1:features_per_dataset[dataset], 1])
      filtered_data <- original_data[[dataset]][, c("ID", pull(selected_feats))]
    } else {
      original_data[[dataset]][1]
    }
  })

  names(selected_features) <- names(original_data)
  selected_features$clinical <- clinical_data

  selected_features %>%
    reduce(full_join, by = "ID")
}

perform_feature_selection <- function(data, sel_method, target, i, separation_mode, nrepeat, directory, n_threads) {
  tryCatch(
    {
      if (sel_method %in% c("auc", "information_gain", "mrmr", "jmim")) {
        ranking <- nested_filtering(
          data = data,
          target = target,
          filter_name = sel_method,
          cutoff_method = "top_n",
          cutoff_treshold = 10,
          return_ranking_list = TRUE,
          n_fold = NULL,
          n_threads = n_threads
        )
        saveRDS(bind_rows(ranking$ranking_list), file.path(directory, paste(sel_method, separation_mode, "iter", nrepeat, "fold", i, "ranking.Rds", sep = "_")))
        return(ranking$ranking_list)
      } else {
        variance <- nested_filtering(
          data = data,
          target = target,
          filter_name = "variance",
          cutoff_method = "percentage",
          cutoff_treshold = 10,
          return_ranking_list = TRUE,
          n_fold = NULL,
          n_threads = n_threads
        )

        var_sel_feat <- variance$filtered_data

        train_data <- var_sel_feat %>%
          reduce(full_join, by = c(target$id_variable))

        data_recipe <- recipes::recipe(train_data) %>%
          recipes::update_role(target$target_variable, new_role = "outcome") %>%
          recipes::update_role(target$id_variable, new_role = "ID") %>%
          recipes::update_role(recipes::has_role(NA), new_role = "predictor")

        if (sel_method == "Lasso") {
          lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
            set_engine("glmnet")

          lasso_wf <- workflow() %>%
            add_recipe(data_recipe) %>%
            add_model(lasso_spec)

          lasso_grid <- grid_regular(penalty(range = c(-3, 0)), levels = 20)
          lasso_tune_results <- tune_grid(
            lasso_wf,
            resamples = vfold_cv(train_data, v = 5),
            grid = lasso_grid
          )
          best_lasso <- select_best(lasso_tune_results, metric = "roc_auc")

          final_lasso_spec <- finalize_model(lasso_spec, best_lasso)

          final_workflow <- workflow() %>%
            add_recipe(data_recipe) %>%
            add_model(final_lasso_spec)

          final_fit <- fit(final_workflow, data = train_data)

          coefs <- broom::tidy(final_fit) %>%
            filter(estimate != 0) %>%
            filter(term != "(Intercept)")

          selected_features <- coefs$term
          saveRDS(coefs, file.path(directory, paste(sel_method, "iter", nrepeat, "fold", i, "ranking.Rds", sep = "_")))
          return(selected_features)
        } else if (sel_method == "RF") {
          rf_model <- rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
            set_engine("ranger", importance = "impurity", num.threads = n_threads) %>%
            set_mode("classification")

          rf_wf <- workflow() %>%
            add_recipe(data_recipe) %>%
            add_model(rf_model)

          rf_tune_results <- tune_grid(
            rf_wf,
            resamples = vfold_cv(train_data, v = 5),
            grid = 20
          )

          best_rf <- select_best(rf_tune_results, metric = "roc_auc")

          final_rf_spec <- finalize_model(rf_model, best_rf)

          final_workflow <- workflow() %>%
            add_recipe(data_recipe) %>%
            add_model(final_rf_spec)

          final_fit <- fit(final_workflow, data = train_data)

          coefs <- final_fit %>%
            extract_fit_parsnip() %>%
            vip::vi()
          saveRDS(coefs, file.path(directory, paste(sel_method, "iter", nrepeat, "fold", i, "ranking.Rds", sep = "_")))
          return(coefs)
        }
      }
    },
    error = function(e) {
      logger::log_error(e$message)
      return(NULL)
    }
  )
}

score_models <- function(train_data, test_data, target, models, n_threads) {
  tryCatch(
    {
      resample <- vfold_cv(train_data, v = 5)

      # Initialize an empty list to hold the models
      model_list <- list()

      data_recipe <- recipes::recipe(train_data) %>%
        recipes::update_role(target$target_variable, new_role = "outcome") %>%
        recipes::update_role(target$id_variable, new_role = "ID") %>%
        recipes::update_role(recipes::has_role(NA), new_role = "predictor")

      # Conditionally add models based on their presence in the models argument
      if ("LR" %in% models) {
        logistic_model <- logistic_reg() %>%
          set_engine("glm") %>%
          set_mode("classification")
        model_list$LR <- logistic_model
      }

      if ("SVM" %in% models) {
        svm_model <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
          set_engine("kernlab") %>%
          set_mode("classification")
        model_list$SVM <- svm_model
      }

      if ("XGB" %in% models) {
        xgb_model <- boost_tree(trees = tune(), tree_depth = tune(), min_n = tune()) %>%
          set_engine("xgboost") %>%
          set_mode("classification")
        model_list$XGB <- xgb_model
      }

      if ("RF" %in% models) {
        rf_model <- rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
          set_engine("ranger", num.threads = n_threads) %>%
          set_mode("classification")
        model_list$RF <- rf_model
      }

      # Create workflows using the dynamically constructed model list
      workflows <- workflow_set(
        preproc = list(recipe = data_recipe),
        models = model_list
      )

      control <- control_grid(
        save_pred = TRUE,
        parallel_over = "everything",
        save_workflow = TRUE,
        allow_par = TRUE
      )

      results <- workflows %>%
        workflow_map(
          seed = 12345,
          resamples = resample,
          metrics = yardstick::metric_set(
            yardstick::mcc,
            yardstick::roc_auc,
            yardstick::accuracy
          ),
          grid = 20,
          control = control
        )

      wf_names <- workflows$wflow_id

      data_split <-
        rsample::make_splits(train_data, test_data)

      results_list <- lapply(wf_names, function(model_name) {
        tryCatch(
          {
            best_results <- results %>%
              extract_workflow_set_result(model_name) %>%
              select_best(metric = "mcc")

            train_metrics <- results %>%
              filter(wflow_id == model_name) %>%
              collect_metrics() %>%
              filter(.config == best_results$.config & wflow_id == model_name) %>%
              transmute(.metric, value = mean, std_err) %>%
              add_column(set = "train", .before = 1)

            train_metrics <- bind_cols(
              train_data %>%
                group_by(!!rlang::sym(target$target_variable)) %>%
                count() %>%
                ungroup() %>%
                summarize(n_train = paste(paste(!!rlang::sym(target$target_variable), n, sep = ": "), collapse = " | ")),
              train_metrics %>%
                mutate(metric = paste(set, .metric, sep = "_")) %>%
                select(-set, -.metric, -std_err) %>%
                spread(metric, value)
            )

            test_results <- results %>%
              extract_workflow(model_name) %>%
              finalize_workflow(best_results) %>%
              last_fit(
                split = data_split,
                metrics = yardstick::metric_set(
                  yardstick::mcc,
                  yardstick::roc_auc,
                  yardstick::accuracy
                )
              )

            test_metrics <- collect_metrics(test_results) %>%
              transmute(.metric, value = .estimate) %>%
              add_column(set = "test", .before = 1)

            bind_cols(
              train_metrics,
              test_data %>%
                group_by(!!rlang::sym(target$target_variable)) %>%
                count() %>%
                ungroup() %>%
                summarize(n_test = paste(paste(!!rlang::sym(target$target_variable), n, sep = ": "), collapse = " | ")),
              test_metrics %>%
                mutate(metric = paste(set, .metric, sep = "_")) %>%
                select(-set, -.metric) %>%
                spread(metric, value)
            ) %>%
              add_column(model = str_extract(model_name, "(?<=recipe_).*"), .before = 1)
          },
          error = function(e) {
            logger::log_error(e$message)
            return(data.frame(model = str_extract(model_name, "(?<=recipe_).*")))
          }
        )
      })

      results_df <- bind_rows(results_list) %>%
        select(model, contains("train"), everything())

      return(results_df)
    },
    error = function(e) {
      logger::log_error(e$message)
      return(NULL)
    }
  )
}

# Function

# Define the run_simulation function
run_simulation <- function(
    experiment_name,
    data,
    target,
    fs_methods = c("auc", "information_gain", "mrmr", "Lasso", "RF"),
    feature_sel_strategies = c("separate", "non-separate"),
    n_features = c(10, 25, 50, 100),
    allow_missing_values = FALSE,
    models = c("LR", "SVM", "XGB", "RF"),
    n_folds = 5,
    nrepeat = 4,
    output_dir = getwd(),
    n_threads = parallel::detectCores() / 2) {
  # Set the directory for the experiment
  directory <- file.path(output_dir, experiment_name)

  # Check if the directory already exists and stop if it does
  stopIfConditionFails(!dir.exists(directory), "Experiment with given name already exists. Please introduce different name")
  # Create directory if it doesn't exist
  dir.create(directory)

  # Create logger
  log_path <- file.path(directory, "simulation_logs.txt")
  logger::log_appender(logger::appender_file(log_path))
  logger::log_threshold(logger::DEBUG)
  logger::log_info("Experiment started")

  # Main simulation loop
  results <- lapply(1:nrepeat, function(rep) {
    logger::log_info(paste("====================== REPETITION", rep))

    tryCatch(
      {
        if (!allow_missing_values) {
          data <- na.omit(data)
        }

        split <- vfold_cv(data[c(target$id_variable, target$target_variable)], v = n_folds, strata = target$target_variable)

        lapply(1:nrow(split), function(i) {
          training_ids <- split$splits[[i]][["in_id"]]
          training_data <- data[training_ids, ]
          testing_data <- anti_join(data, training_data, by = target$id_variable)

          for (method in fs_methods) {
            logger::log_info(paste("==================================== METHOD", method))

            if (method %in% c("Lasso", "RF")) {
              ranking_list <- perform_feature_selection(list(clinical = training_data), method, target, i, "non-separate", rep, directory, n_threads)
              if (method == "Lasso") {
                selected_features <- ranking_list
                data_filtered <- training_data %>%
                  select(target$id_variable, target$target_variable, all_of(selected_features))
                test_data <- select(testing_data, names(data_filtered))
                model_scores <- score_models(data_filtered, target, test_data, n_threads)
                metadata <- data.frame(
                  sel_method = method,
                  num_feat = ncol(data_filtered) - 2,
                  sep_mode = "non-separate",
                  fold = i,
                  n_rep = rep,
                  testing_data %>% summarise(test_n = n()),
                  testing_data %>% group_by(!!rlang::sym(target$target_variable)) %>% count() %>% spread(!!rlang::sym(target$target_variable), n) %>% rename_with(~ paste0("test_", .x))
                )
                model_scores <- bind_cols(metadata, model_scores)
                saveRDS(model_scores, file.path(directory, paste(method, "nrep", rep, "fold", i, "results.Rds", sep = "_")))
              } else if (method == "RF") {
                for (num_features in n_features) {
                  selected_features <- ranking_list[1:num_features, 1] %>% pull()
                  data_filtered <- training_data %>%
                    select(target$id_variable, target$target_variable, all_of(selected_features))
                  test_data <- select(testing_data, names(data_filtered))
                  model_scores <- score_models(data_filtered, target, test_data, n_threads)
                  metadata <- data.frame(
                    sel_method = method,
                    num_feat = num_features,
                    sep_mode = "non-separate",
                    fold = i,
                    n_rep = rep,
                    testing_data %>% summarise(test_n = n()),
                    testing_data %>% group_by(!!rlang::sym(target$target_variable)) %>% count() %>% spread(!!rlang::sym(target$target_variable), n) %>% rename_with(~ paste0("test_", .x))
                  )
                  model_scores <- bind_cols(metadata, model_scores)
                  saveRDS(model_scores, file.path(directory, paste(method, num_features, "feat", "nrep", rep, "fold", i, "results.Rds", sep = "_")))
                }
              }
            } else {
              for (separation_mode in feature_sel_strategies) {
                ranking_list <- if (separation_mode == "separate") {
                  dataset_assignments <- colnames(training_data) %>%
                    str_extract_all("\\[.*?\\]") %>%
                    unlist() %>%
                    str_replace_all("[\\[\\]]", "")

                  split_data <- map(unique(dataset_assignments), function(ds) {
                    selected_columns <- grep(paste0("\\[", ds, "\\]"), colnames(training_data), value = TRUE)
                    training_data %>%
                      select(ID, all_of(selected_columns))
                  })

                  names(split_data) <- unique(dataset_assignments)

                  split_data$clinical <- training_data %>% select(target$id_variable, target$target_variable)
                  perform_feature_selection(split_data, method, target, i, separation_mode, rep, directory, n_threads)
                } else {
                  perform_feature_selection(list(clinical = training_data), method, target, i, separation_mode, rep, directory, n_threads)
                }

                if (!is.null(ranking_list)) {
                  for (num_features in n_features) {
                    if (separation_mode == "separate") {
                      data_filtered <- select_features_based_on_proportion(split_data, ranking_list, num_features)
                    } else {
                      selected_features <- ranking_list %>%
                        bind_rows() %>%
                        arrange(desc(mean_score)) %>%
                        slice_max(order_by = mean_score, n = num_features, with_ties = FALSE) %>%
                        pull(original_name)
                      data_filtered <- training_data %>%
                        select(target$id_variable, target$target_variable, all_of(selected_features))
                    }
                    if (!is.null(data_filtered)) {
                      test_data <- select(testing_data, names(data_filtered))
                      model_scores <- score_models(data_filtered, test_data, target, models, n_threads)
                      metadata <- data.frame(
                        sel_method = method,
                        num_feat = num_features,
                        sep_mode = separation_mode,
                        fold = i,
                        n_rep = rep,
                        testing_data %>% summarise(test_n = n()),
                        testing_data %>% group_by(!!rlang::sym(target$target_variable)) %>% count() %>% spread(!!rlang::sym(target$target_variable), n) %>% rename_with(~ paste0("test_", .x))
                      )
                      model_scores <- bind_cols(metadata, model_scores)
                      saveRDS(model_scores, file.path(directory, paste(method, separation_mode, num_features, "feat", "nrep", rep, "fold", i, "results.Rds", sep = "_")))
                    }
                  }
                }
              }
            }
          }
        })
        gc()
      },
      error = function(e) {
        logger::log_error(e$message)
        return(NULL)
      }
    )
  })

  logger::log_info("Experiment ended")
}
