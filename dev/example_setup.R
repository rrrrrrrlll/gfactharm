devtools::load_all()

cohorts <- c("A", "BNO", "BUNI", "BNONUNI")

domain_order <- c('fa1', 'fa2', 'f2')

c_d_t_map <- list(
    A = list(
        f1 = c("ya1_1", "ya1_2", "ya1_3"),
        f2 = c("y2_1", "y2_2", "y2_3"),
        f3 = c("y3_1", "y3_2", "y3_3")
    ),
    BNO = list(
        f1 = c("yb1_1", "yb1_2", "yb1_3"),
        f2 = c("y2_1", "y2_2", "y2_3"),
        f3 = c("y3_1", "y3_2", "y3_3")
    ),
    BUNI = list(
        f1 = c("yb1_1", "yb1_2", "yb1_3"),
        f2 = c("y2_1", "y2_2", "y2_3"),
        f3 = c("y3_1", "y3_2", "y3_3")
    ),
    BNONUNI = list(
        f1 = c("yb1_1", "yb1_2", "yb1_3"),
        f2 = c("y2_1", "y2_2", "y2_3"),
        f3 = c("y3_1", "y3_2", "y3_3")
    )
)

A <- read.csv('dev/A.csv')
BNO <- read.csv('dev/BNO.csv')
BUNI <- read.csv('dev/BUNI.csv')
BNONUNI <- read.csv('dev/BNONUNI.csv')

data_list <- list(
    A = A,
    BNO = BNO,
    BUNI = BUNI,
    BNONUNI = BNONUNI
)

sim_model <- specify_model(cohorts                = cohorts,
                           cohort_domain_test_map = c_d_t_map,
                           covariate_lst          = NULL,
                           preferred_domain_order = domain_order,
                           data                   = data_list,
                           group_var              = 'cohort')

