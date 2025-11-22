devtools::load_all()

# get lavaan syntax of 1st order model
syntax <- lvn_syntax(
    model = sim_model,
    selected_items   = c('y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
    loadings         = NULL,
    fix_lv_means     = TRUE,
    fix_lv_variances = TRUE,
    fixed_intercept  = 0.0,
    fixed_variance   = 1.0,
    group_reg        = c('y2_1', 'y2_2', 'y3_1', 'y3_2')
)
cat(syntax)

# get lavaan syntax of 2st order model
syntax <- lvn_syntax_g(
    model = sim_model,
    selected_items = c('ya1_1', 'ya1_2', 'ya1_3', 'y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
    std_g = TRUE
)
cat(syntax)


# get sub-dataset from data_list
combined_data <- df_from_data_list(
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


# estimated parameters from previous study

# loading of G
pg1 <- 0.801293976
pg2 <- 0.707426222
pg3 <- 0.881140286

# loading of f2
p21 <- 0.987692159
p22 <- 1.941613933
p23 <- 2.946642562

# loading of f3
p31 <- 0.993691913
p32 <- 1.990735719
p33 <- 3.000251725

# var of f2
v21 <- 1.050117860
v22 <- 2.041275660
v23 <- 2.890425029

# var of f3
v31 <- 0.974736061
v32 <- 2.044981500
v33 <- 3.136846141

# intercepts of f2
m21 <- -0.001596929
m22 <- 0.008863728
m23 <- 0.045536891

# intercepts of f3
m31 <- 0.014210936
m32 <- 0.027142766
m33 <- 0.036945344
