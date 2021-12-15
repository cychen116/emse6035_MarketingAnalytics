# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
library(conjointTools) 
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here("data", "choiceData.csv"))
head(data)
nrow(data)

# Estimate MNL model
# Transform data to make WTP coherent
data <- data %>% 
    mutate(
        set_price = price/50,
        total_price = price*(quantity/50)
    )
head(data)

# First create some dummy coded variables for categorical variables
data <- dummy_cols(data, c('price','quantity','material','setOptions'))
head(data)

# Save this data for modeling
write_csv(data, here("data", "choiceData_model.csv"))

# Estimate the model
mnl <- logitr(
    data   = data,
    outcome = "choice",
    obsID  = "obsID",
    pars   = c(
        "set_price",
        "quantity" ,
        "material_Bamboo","material_Eco", #Base-Plastic
        "setOptions_fk", "setOptions_fks" #base:f
    ) 
)

# View summary of results
summary(mnl)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl$gradient

# 2nd order condition: Is the hessian negative definite?
# all the eigenvalues are negative, and the hessian is negative definite
eigen(mnl$hessian)$values



# Estimate the dummy model
mnl_dummy <- logitr(
    data   = data,
    outcome = "choice",
    obsID  = "obsID",
    pars   = c(
        "price_25","price_30",            # Base:20
        "quantity_150","quantity_200",    # Base:50
        "material_Bamboo","material_Eco", # Base:Plastic
        "setOptions_fk", "setOptions_fks" # Base:f
    ) 
)

save(
    mnl,
    mnl_dummy,
    file = here("models","model.RData")
)

# MODEL RESULT PLOT - MNL
coefs <- coef(mnl)
covariance <- vcov(mnl)

prob <- data.frame(mnl$probabilities)
std <- se(mnl)
attri <- c(
    "set_price",
    "quantity" ,
    "material_Bamboo","material_Eco", #Base-Plastic
    "setOptions_fk", "setOptions_fks" #base:f
) 

plot_mnl_coefs <- qplot(x = attri,y = coefs)+
    geom_errorbar(aes(x=attri, ymin=coefs-std, ymax=coefs+std), width=0.25)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
    filename = here('figs', 'plot_mnl_coefs.png'), 
    plot = plot_mnl_coefs, 
    width = 12, height = 8
)

# MODEL RESULT PLOT - MNL- Dummy

coefs <- coef(mnl_dummy)
covariance <- vcov(mnl_dummy)

prob <- data.frame(mnl_dummy$probabilities)
std <- se(mnl)
attri <- c(
    "price_25","price_30",#Base:20
    "quantity_150","quantity_200",#Base:50
    "material_Bamboo","material_Eco", #Base:Plastic
    "setOptions_fk", "setOptions_fks" #Base:f
) 

plot_mnl_dummy_coefs <- qplot(x = attri,y = coefs)+
    geom_errorbar(aes(x=attri, ymin=coefs-std, ymax=coefs+std), width=0.25)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_mnl_dummy_coefs 

ggsave(
    filename = here('figs', 'plot_mnl_dummy_coefs.png'), 
    plot = plot_mnl_dummy_coefs , 
    width = 12, height = 8
)