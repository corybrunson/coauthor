## Prior distributions for model coefficients

int_prior_df <- 10
int_prior_loc <- 3

var_prior_dfs <- c(
    time = 2,
    trial = 5,
    review = 5,
    funding = 5,
    breadth = 5,
    prestige = 5,
    viz = 5
)
var_prior_locs <- c(
    time = .1,
    trial = 0,
    review = 0,
    funding = 0,
    breadth = 0,
    prestige = 0,
    viz = 0
)

int_prior <- student_t(df = 10, location = 3)

var_priors <- list(
    
    # Publication date
    time = student_t(df = 2, location = .1),
    
    # Clinical trials
    trial = student_t(df = 5),
    # Interaction with publication date
    "trial:time" = student_t(df = 2),
    
    # Review articles
    review = student_t(df = 5),
    # Interaction with publication date
    "review:time" = student_t(df = 2),
    
    # Grant funding
    funding = student_t(df = 5),
    # Interaction with publication date
    "funding:time" = student_t(df = 2),
    
    # Topical scope
    breadth = student_t(df = 5),
    # Interaction with publication date
    "breadth:time" = student_t(df = 2),
    
    # Journal influence
    prestige = student_t(df = 5),
    # Interaction with publication date
    "prestige:time" = student_t(df = 2),
    
    # Indexing database inclusion
    viz = student_t(df = 5),
    # Interaction with publication date
    "viz:time" = student_t(df = 2)
    
)

cov_priors <- "TODO"
