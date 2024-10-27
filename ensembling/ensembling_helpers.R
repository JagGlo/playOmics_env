
show_plots <- function(data){
  data %>%
    lapply(function(x){
      x$predictions
    }) %>%
    bind_rows() %>%
    rename(`  missing predictions` =  `n_not_classified`, 
           ` adjusted accuracy` = adjusted_acc) %>%
    mutate(lambda = as.factor(lambda)) %>%
    select(n_ensemble, lambda, `  missing predictions`, ` adjusted accuracy`, accuracy) %>%
    # pivot_longer(cols = -c(n_ensemble, equation, lambda, metric_for_model_selection, tested_metric), names_to = "key", values_to = "value") %>%
    pivot_longer(cols = -c(n_ensemble, lambda), names_to = "key", values_to = "value") %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    ggplot(aes(x = n_ensemble, y = value, color = lambda)) +
    geom_line() +
    facet_wrap(~ key, scales = "free_y") +
    labs(x = "number of models voting", y = "") +
    theme_bw() +
    theme(
      # legend.position = "none",
      # axis.text.x= element_text(angle=60, hjust=1),
      text=element_text(size=12,  family="Helvetica", colour = "black"),
      axis.text = element_text(size = 12, colour = "black"),
      plot.title = element_text(size=12, colour = "black"), strip.text.x = element_text(size = 10),
      strip.text.y = element_blank(), panel.grid.minor = element_blank(),
      strip.background.x = element_rect(fill = "white"))
}


# Missingness


calculate_Miss <- function(pred1, pred2) {
  N11 <- sum(!is.na(pred1) & !is.na(pred2), na.rm = T)
  N00 <- sum(is.na(pred1) & is.na(pred2), na.rm = T)
  N10 <- sum(!is.na(pred1) & is.na(pred2), na.rm = T)
  N01 <- sum(is.na(pred1) & !is.na(pred2), na.rm = T)
  Miss_Diss <- (N01 + N10) / (N11 + N10 + N01 + N00)
  Miss_Diss_mod <- (N01) / (N11 + N10 + N01 + N00)
  return(Miss_Diss)
}
# Initialize a matrix to store Q values
# num_classifiers <- ncol(pred_spread_na)
# Miss_matrix_na <- matrix(NA, nrow = num_classifiers, ncol = num_classifiers)
# 
# # Iterate over all unique pairs of classifiers
# for(i in 1:(num_classifiers-1)) {
#   for(j in (i+1):num_classifiers) {
#     Miss_matrix_na[i, j] <- calculate_Miss(pred_spread_na[,i], pred_spread_na[,j])
#     Miss_matrix_na [j, i] <- Miss_matrix_na [i, j]
#   }
# }
# 
# diag(Miss_matrix_na) <- NA
# colnames(Miss_matrix_na) <- colnames(pred_spread_na)
# rownames(Miss_matrix_na) <- colnames(pred_spread_na)
# 
# 

composite_metric <- function(metric_for_model_selection, metric_of_interest, lambda) {
  return(lambda * metric_for_model_selection + (1-lambda) * metric_of_interest)
}

metrics_test_look_at_all <- function(selected_models, data_to_predict, metric_matrix, tested_metric, metric_for_model_selection, lambda, vec_of_ensemble_size) {

  if(lambda != 1){
    # Find the row in 'selected_models' corresponding to the best model
    best_model <- selected_models %>%
      arrange(desc(!!sym(metric_for_model_selection))) %>%
      slice_max(!!sym(metric_for_model_selection), n = 1, with_ties = F)

    # Choose the model that maximizes the composite metric
    chosen_models <- data.frame(
      model_order = 1,
      model_name = best_model$model_name,
      weight = 1
    ) # initial weight

    models_to_remove <- selected_models

    for (i in 2:max(vec_of_ensemble_size)) {
      # remove model from list
      models_to_remove <- models_to_remove %>% filter(model_name != best_model$model_name)

      # Find the correlation values for the best model in 'correlation_matrix'
      relation_with_best_model <-
        metric_matrix %>%
        as.data.frame() %>%
        select(all_of(chosen_models$model_name)) %>%
        rename_with(~ chosen_models$model_name) %>%
        rownames_to_column(var = "model_name") %>%
        gather(models, !!sym(tested_metric), -model_name)

      # if (tested_metric == "miss") {
      #   relation_with_best_model <-
      #     relation_with_best_model %>%
      #     mutate(miss = 1 - miss) # in this metric the highest the better, but this doesn't fit to composition metric
      # }

      data_to_compute <-
        models_to_remove %>%
        select(model_name, !!sym(metric_for_model_selection)) %>%
        left_join(relation_with_best_model, by = "model_name") %>%
        left_join(chosen_models %>% select(model_name, model_order), by = c("models"= "model_name"))

      # Calculate the composite metric for each model
      data_to_compute <-
        data_to_compute %>%
        rename(quality_metric =  !!sym(metric_for_model_selection)) %>%
        group_by(model_name, quality_metric) %>%
        #LWMA approach
        # summarise(mean = sum(!!sym(tested_metric) * model_order) / sum(model_order),  .groups = 'drop') %>%
        #weighted polynomial
        # summarise(mean = sum(!!sym(tested_metric) * (1 / model_order)^2) / sum(model_order),  .groups = 'drop') %>%
        # mean approach
        summarise(mean = mean(!!sym(tested_metric)), .groups = 'drop') %>%
        mutate(mean = (mean- min(mean, na.rm = T)) /(max(mean, na.rm = T)-min(mean, na.rm = T)),
               quality_metric = (quality_metric- min(quality_metric, na.rm = T)) /(max(quality_metric, na.rm = T)-min(quality_metric, na.rm = T))
        ) %>%
        mutate(metric = composite_metric(quality_metric, mean, lambda))
      #   group_by(model_name) %>%
      # summarise(mean = mean(metric))

      best_model_index <- which.max(data_to_compute$metric)
      best_model <- models_to_remove[best_model_index, ]

      # calculate weight based on composite metric
      curr_weight <- data_to_compute$metric[best_model_index]

      chosen_models[i, ] <-
        data.frame(
          model_order = i,
          model_name = best_model$model_name,
          weight = curr_weight
        )
    }
  } else {
    chosen_models <-
      selected_models %>%
      arrange(desc(!!sym(metric_for_model_selection))) %>%
      slice_max(!!sym(metric_for_model_selection), n = max(vec_of_ensemble_size), with_ties = F) %>%
      transmute(
        model_order= 1:max(vec_of_ensemble_size),
        model_name)
  }

  predictions <-
    lapply(vec_of_ensemble_size, function(n){

      predictions_for_p <-
        selected_models %>%
        filter(model_name %in% chosen_models$model_name[1:n]) %>%
        split(., .$model_dir) %>%
        lapply(function(model) {
          predicted <- load_and_predict(model$model_dir, data_to_predict) %>%
            add_column(model_name = model$model_name, .before = 1) %>%
            mutate(ID = 1:nrow(data_to_predict))
        }) %>%
        bind_rows()

      # Aggregate final predictions and add metadata
      final_pred <- predictions_for_p %>%
        group_by(ID) %>%
        count(`.pred_class`) %>%
        filter(!is.na(`.pred_class`)) %>%
        arrange(desc(n)) %>%
        mutate(votes = paste0("votes_for_", `.pred_class`)) %>%
        select(-`.pred_class`) %>%
        spread(votes, n)

      predictions <-
        final_pred %>%
        mutate(final_class = case_when(
          votes_for_ductal > votes_for_lobular ~ "ductal",
          votes_for_ductal < votes_for_lobular ~ "lobular",
          is.na(votes_for_lobular) & !is.na(votes_for_ductal) ~ "ductal",
          !is.na(votes_for_lobular) & is.na(votes_for_ductal) ~ "lobular",
          !is.na(votes_for_ductal) & !is.na(votes_for_lobular) & (votes_for_ductal == votes_for_lobular) ~ "tie"
        )) %>%
        full_join(
          data_to_predict %>%
            mutate(ID = 1:nrow(data_to_predict)) %>%
            select(ID, histological_type),
          by = "ID"
        ) %>%
        mutate(correct = final_class == histological_type,
               correct = if_else(final_class == "tie", "tie", as.character(correct))) %>%
        ungroup() %>%
        count(correct) %>%
        spread(correct, n) %>%
        mutate(
          adjusted_acc = `TRUE` / nrow(data_to_predict),
          accuracy = `TRUE` / nrow(final_pred),
          n_not_classified = nrow(data_to_predict)-nrow(final_pred)
        )

      predictions <-
        bind_cols(
          data.frame(
            n_ensemble = n,
            lambda = lambda,
            metric_for_model_selection = metric_for_model_selection,
            tested_metric = tested_metric
          ),
          predictions
        )
    }) %>%
    bind_rows()

  return(list(
    predictions = predictions,
    models = chosen_models %>%
      add_column(lambda = lambda, .before = 1)
  ))
}

# Corrected misssingness metric

calculate_Miss_step <- function(pred1, pred2) {
  N11 <- sum(!is.na(pred1) & !is.na(pred2), na.rm = T)
  N00 <- sum(is.na(pred1) & is.na(pred2), na.rm = T)
  N10 <- sum(!is.na(pred1) & is.na(pred2), na.rm = T)
  N01 <- sum(is.na(pred1) & !is.na(pred2), na.rm = T)
  Miss_Diss <- (N01 + N10) / (N11 + N10 + N01 + N00)
  Miss_Diss_mod <- (N01) / (N11 + N10 + N01 + N00)
  Miss_Q <- (N11 * N00 - N01 * N10) / (N11*N00 + N01*N10)
  return(Miss_Diss_mod)
}

metrics_test_stepwise <- function(selected_models, data_to_predict, predictions_per_model, tested_metric, metric_for_model_selection, lambda, vec_of_ensemble_size, voting = "majority", target) {

  if(lambda != 1){
    # Find the row in 'selected_models' corresponding to the best model
    best_model <- selected_models %>%
      arrange(desc(!!sym(metric_for_model_selection))) %>%
      slice_max(!!sym(metric_for_model_selection), n = 1, with_ties = F)

    # Choose the model that maximizes the composite metric
    chosen_models <- data.frame(
      model_order = 1,
      model_name = best_model$model_name,
      weight = 1
    ) # initial weight

    models_to_remove <- selected_models

    for (i in 2:max(vec_of_ensemble_size)) {
      # remove model from list
      models_to_remove <- models_to_remove %>% filter(model_name != best_model$model_name)

      all_models_pred <-
        predictions_per_model %>%
        select(all_of(chosen_models$model_name))

      all_models_pred$filled_rows <- apply(all_models_pred, 1, function(row) if (any(!is.na(row))) 1 else NA)

      relation_with_best_model <-
        lapply(models_to_remove$model_name, function(second_model){
          metric <- calculate_Miss_step(all_models_pred$filled_rows, predictions_per_model[[second_model]])
          data.frame(model_name = second_model,
                     metric = metric)

        }) %>%
        bind_rows() %>%
        left_join(
          models_to_remove %>%
            select(model_name, !!sym(metric_for_model_selection)),
          by = "model_name"
        )

      # if (tested_metric == "miss") {
      #   relation_with_best_model <-
      #     relation_with_best_model %>%
      #     mutate(metric = 1 - metric) # in this metric the highest the better, but this doesn't fit to composition metric
      # }

      # Calculate the composite metric for each model
      data_to_compute <-
        relation_with_best_model %>%
        rename(quality_metric =  !!sym(metric_for_model_selection)) %>%
        mutate(metric = if_else(metric == 0, 0, (metric- min(metric, na.rm = T)) /(max(metric, na.rm = T)-min(metric, na.rm = T))),
               quality_metric = (quality_metric- min(quality_metric, na.rm = T)) /(max(quality_metric, na.rm = T)-min(quality_metric, na.rm = T))
        ) %>%
        mutate(composite_metric = composite_metric(quality_metric, metric, lambda))

      best_model_index <- which.max(data_to_compute$composite_metric)
      best_model <- models_to_remove[best_model_index, ]

      # calculate weight based on composite metric
      curr_weight <- data_to_compute$composite_metric[best_model_index]

      if (nrow(best_model)==0) {

        cat("Exiting loop since i reached", i, "\n")
        break
      } else {
        chosen_models[i, ] <-
          data.frame(
            model_order = i,
            model_name = best_model$model_name,
            weight = curr_weight
          )
      }
    }
  } else {
    end_no <- if_else(nrow(selected_models) < max(vec_of_ensemble_size),nrow(selected_models), max(vec_of_ensemble_size))
    chosen_models <-
      selected_models %>%
      arrange(desc(!!sym(metric_for_model_selection))) %>%
      slice_max(!!sym(metric_for_model_selection), n = max(vec_of_ensemble_size), with_ties = F) %>%
      transmute(
        model_order= 1:end_no,
        model_name)
  }

  pred_cols <- paste0(".pred_", target$target_levels)

  predictions <-
    lapply(vec_of_ensemble_size, function(n){
      if(n <= nrow(chosen_models)){
        predictions_for_p <-
          selected_models %>%
          filter(model_name %in% chosen_models$model_name[1:n]) %>%
          split(., .$model_dir) %>%
          lapply(function(model) {
            predicted <- load_and_predict(model$model_dir, data_to_predict) %>%
              add_column(model_name = model$model_name, .before = 1) %>%
              mutate(ID = 1:nrow(data_to_predict))
          }) %>%
          bind_rows()
        if(voting == "majority"){
          # Aggregate final predictions and add metadata
          final_pred <- predictions_for_p %>%
            group_by(ID) %>%
            count(`.pred_class`) %>%
            filter(!is.na(`.pred_class`)) %>%
            mutate(votes = paste0("votes_for_", `.pred_class`)) %>%
            select(-`.pred_class`) %>%
            pivot_wider(
              names_from = votes,
              values_from = n,
              values_fill = list(n = 0)  # Fill missing votes with 0
            )
        } else if(voting == "soft"){
          final_pred <- predictions_for_p %>%
            filter(!is.na(`.pred_class`)) %>%
            group_by(ID) %>%
            summarise(
              across(
                all_of(pred_cols),
                ~ mean(.x, na.rm = TRUE),
                .names = "votes_for_{str_remove(.col, '^.pred_')}"
              )
            )
        }

        predictions <-
          final_pred %>%
          pivot_longer(
            cols = starts_with("votes_for_"),
            names_prefix = "votes_for_",
            names_to = "class",
            values_to = "vote"
          ) %>%
          group_by(ID) %>%
          summarise(
            max_vote = max(vote, na.rm = TRUE),
            n_max = sum(vote == max_vote, na.rm = TRUE),
            classes_with_max = paste(class[vote == max_vote], collapse = ", "),
            .groups = 'drop'
          ) %>%
          mutate(
            final_class = case_when(
              is.infinite(max_vote) ~ NA_character_,
              n_max == 1 ~ classes_with_max,
              n_max > 1 ~ "tie"
            )
          ) %>%
          full_join(
            data_to_predict %>%
              mutate(ID = 1:nrow(data_to_predict)) %>%
              select(ID, !!target$target_variable),
            by = "ID"
          ) %>%
          mutate(correct = final_class == !!sym(target$target_variable),
                 correct = if_else(final_class == "tie", "tie", as.character(correct))) %>%
          ungroup() %>%
          count(correct) %>%
          spread(correct, n) %>%
          mutate(
            adjusted_acc = `TRUE` / nrow(data_to_predict),
            accuracy = `TRUE` / nrow(final_pred),
            n_not_classified = nrow(data_to_predict)-nrow(final_pred)
          )

        predictions <-
          bind_cols(
            data.frame(
              n_ensemble = n,
              lambda = lambda
            ),
            predictions
          )
      }
    }) %>%
    bind_rows()

  return(list(
    predictions = predictions,
    models = chosen_models %>%
      add_column(lambda = lambda, .before = 1)
  ))
}

