devtools::load_all()

# get lavaan syntax of 1st order model
syntax <- get_lavaan_syntax(
    model = sim_model,
    selected_items   = c('y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
    loadings         = NULL,
    fix_lv_means     = TRUE,
    fix_lv_variances = TRUE,
    fixed_intercept  = 0.0,
    fixed_variance   = 2.0,
    group_reg        = c('y2_1', 'y2_2', 'y3_1', 'y3_2')

)
cat(syntax)


# get sub-dataset from data_list
combined_data <- subset_from_data_list(
    model         = sim_model,
    target_items  = c('y2_1', 'y2_2', 'y3_1', 'y3_2'),
    target_groups = c("A", "BNO", "BUNI")
)


# DIF detection - LASSO
result1 <- lasso_restriction(
    model = sim_model,
    target_items = c('y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
    target_cohorts = c('A', 'BUNI')
)

# DIF detection - MIMIC
result2 <- mimic(
    model = sim_model,
    target_items = c('y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
    target_cohorts = c('A', 'BUNI'),
    anchors = c('y2_1', 'y3_2')
)

# DIF detection - combined
result3 <- dif_analysis(
    model = sim_model,
    target_items = c('y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
    target_cohorts = c('A', 'BUNI')
)
