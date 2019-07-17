#' Categorical variables encoding using target statistics
#'
#' `step_target_enc` creates a *specification* of a
#'  recipe step that will transform nominal data into its numerical
#'  transformation based on mean or median target statistic.
#'


step_target_enc <- function(
  recipe,
  ..., # селекторы переменных - в список переменных нужно включать таргет
  role = "predictor", # создаем новые переменные, сохраняя исходные
  trained = FALSE, # всегда FALSE
  skip = FALSE, # почти всегда FALSE (TRUE для outcome)
  id = rand_id("mean_target_enc"), # идентификатор шага
  fun = mean,
  prefix = "mean",
  target_var = NULL, # строка с именем таргета для проброса внутрь рецепта
  lookup_table = NULL # для сохранения результатов
) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_target_enc_new(
      terms = terms,
      trained = trained,
      role = role,
      skip = skip,
      id = id,
      fun = fun,
      prefix = prefix,
      target_var = target_var,
      lookup_table = lookup_table
    )
  )
}

## Initializes a new object
step_target_enc_new <-
  function(terms, role, trained, skip, id,
           fun, prefix, target_var, lookup_table) {
    step(
      subclass = "target_enc",
      terms = terms,
      role = role,
      trained = trained,
      skip = skip,
      id = id,
      fun = fun,
      prefix = prefix,
      target_var = target_var,
      lookup_table = lookup_table
    )
  }


# fun = mean or median
target_encoder <- function(data, cols, fun, target) {
  #assert_data_table(data)
  #assert_character(cols)
  #assert_string(target)
  #assert_names(names(data), must.include = c(cols, target))

  res <- gather(select(data, one_of(cols, target)),
                key = "variable",
                value = "value",
                tidyselect::one_of(cols)) %>%
    group_by(variable, value) %>%
    summarise(encoded_vars = fun(!!sym(target)))

  return(res)
}


#data <- as_tibble(mtcars)
#cols <- c("gear", "carb")
#target <- "vs"
#target_encoder(data, cols, fun = median, target)

prep.step_target_enc <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info)
  # Exclude target
  col_names <- setdiff(col_names, x$target_var)
  ## You can add error trapping for non-numeric data here and so on. See the
  ## `check_type` function to do this for basic types.

  lookup_table <- target_encoder(data = training,
                                 cols = col_names,
                                 fun = x$fun,
                                 target = x$target_var)

  step_target_enc_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    skip = x$skip,
    id = x$id,
    fun = x$fun,
    prefix = x$prefix,
    target_var = x$target_var,
    lookup_table = lookup_table
  )
}


f <- function(col_name, new_col_name, data) {
  by_pair <- "value"
  names(by_pair) <- col_name

  rename_pair <- list(sym("encoded_vars"))
  names(rename_pair) <- new_col_name

  left_join(select(data, tidyselect::one_of(col_name)),
            filter(res, variable == col_name),
            by = by_pair) %>%
    select(encoded_vars) %>%
    rename(!!!rename_pair)
}

bake.step_target_enc <- function(object,
                                 new_data,
                                 ...) {
  #assert_data_table(newdata)
  #assert_string(suffix)
  #assert_flag(drop)

  lookup_table <- object$lookup_table
  cols <- intersect(unique(lookup_table$variable),
                    names(new_data))
  new_cols <- paste0(object$prefix, "_", cols)

  if (length(cols) == 0L) {
    return(new_data)
  }

  new_data <- purrr::map2_dfc(cols, new_cols, f, data = new_data)

  return(new_data)
}



dt_train <- as_tibble(mtcars)
cols <- c("gear", "carb")
target <- "vs"
rec_obj <-
  recipe(vs ~ ., data = dt_train) %>%
  step_target_enc(tidyselect::one_of(cols),
                  all_outcomes(),
                  fun = mean,
                  prefix = "mean",
                  target_var = "vs") %>%
  prep(training = dt_train)
bake(rec_obj, dt_train)

