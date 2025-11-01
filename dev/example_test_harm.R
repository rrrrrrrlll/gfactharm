devtools::load_all()

unidif <- list(
    y3_1 = list('BUNI'),
    y3_2 = list('BUNI'),
    y3_3 = list('BUNI')
)

nonunidif <- list(
    y3_1 = list('BNONUNI'),
    y3_2 = list('BNONUNI'),
    y3_3 = list('BNONUNI')
)

dif_list_validate(sim_model, unidif, nonunidif)

unidif_cohorts <- transform_dif_list(unidif)

results <- harmonization(sim_model, unidif, nonunidif)

