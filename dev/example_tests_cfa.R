devtools::load_all()


# 1st order

# multiple groups
fit <- fit1st(sim_model,
              c('y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
              c('A', 'BUNI'))
lavaan::summary(fit$A)

# one group
fit <- fit1st(sim_model,
              c('ya1_1', 'ya1_2', 'ya1_3', 'y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
              c('A'))
lavaan::summary(fit)


# 2nd order

# multiple groups
fit <- fit2nd(sim_model,
              c('y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
              c('A', 'BUNI'))
lavaan::summary(fit$A)

# one group
fit <- fit2nd(sim_model,
              c('ya1_1', 'ya1_2', 'ya1_3', 'y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
              c('A'))
lavaan::summary(fit)


# reporting error
fit <- fit1st(sim_model,
              c('ya1_1', 'ya1_2', 'yb1_1', 'y2_1', 'y2_2', 'y2_3', 'y3_1', 'y3_2', 'y3_3'),
              c('A', 'BUNI'))

