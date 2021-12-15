# You should write code here to analyze your model results, e.g. computing WTP,
# market simulations, sensitivity analyses, etc.

library(logitr)
library(tidyverse)
library(here)
library(janitor)
library(cowplot)
library(maddTools)
library(fastDummies)
library(conjointTools) 
options(dplyr.width = Inf) 
#open model
# --------"---------------------------------------------------------------------
load(here("models", "model.Rdata"))
summary(mnl)

#Compute WTP
coefs <- coef(mnl)
wtp <- coefs / (-1*coefs['set_price'])
#head(wtp)

covariance <- vcov(mnl)
#head(covariance)
draws <- as.data.frame(MASS::mvrnorm(10^5, coefs, covariance))
head(draws)
wtp_draws = -1*(draws[,2:6] / draws[,1])
head(wtp_draws)
wtp_ci <- ci(wtp_draws)
wtp_ci

data <- read_csv(here("data", "choiceData_model.csv"))
names(data)


model_wtp <- logitr(
    data    = data,
    outcome = "choice",
    obsID   = "obsID",
    pars    = c(
        "quantity" ,
        "setOptions_fk", "setOptions_fks" , #base:f
        "material_Bamboo", "material_Eco" #base:material_Plastic 
    ),
    price   = 'set_price', #tell which is price, dont include it in pars
    modelSpace = 'wtp', #preference space model defult 
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
)

summary(model_wtp)                               
model_wtp$gradient                                
eigen(model_wtp$hessian)$values

# Compare computed versus estimated WTP
wtpCompare(mnl, model_wtp, price = 'set_price')

# Save model
save(
    model_wtp,
    file = here("models", "model_wtp.RData")
)

# Method 1: Computed WTP from preference space model:
# Load pref space model
load(here("models", "model.Rdata"))
coefs <- coef(mnl)
covariance <- vcov(mnl)
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
wtp_draws = -1*(coef_draws[,2:6] / coef_draws[,1])
wtp_ci1 <- ci(wtp_draws)
wtp_ci1

# Method 2: Estimate WTP in WTP space model:
load(here("models", "model_wtp.RData")) # Load estimated models
coefs <- coef(model_wtp)
covariance <- vcov(model_wtp)
wtp_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
wtp_ci2 <- ci(wtp_draws)
wtp_ci2 <- wtp_ci2[-1,] # Drop lambda (we won't plot this)
wtp_ci2

# -----------------------------------------------------------------------------
# Plot WTP results from WTP space model

wtp_ci <- wtp_ci2

# Separate coefficient CIs by attribute 
wtp_ci$par <- row.names(wtp_ci)
wtp_quantity  <- wtp_ci %>% filter(par == 'quantity')
wtp_setOptions  <- wtp_ci %>% 
    filter(par == 'setOptions_fk' | par == "setOptions_fks")
wtp_material  <- wtp_ci %>% 
    filter(par== 'material_Bamboo' | par == 'material_Eco')


# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_quantity <- data.frame(level = c(50, 150, 200)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_quantity$mean,
        lower = diff*wtp_quantity$lower,
        upper = diff*wtp_quantity$upper)

df_set_options <- data.frame(level = c("f", "fk", "fks")) %>%
    mutate(
        mean  = c(0, wtp_setOptions$mean),
        lower = c(0, wtp_setOptions$lower),
        upper = c(0, wtp_setOptions$upper))

df_material <- data.frame(level = c("Plastic", "Bamboo", "Eco")) %>%
    mutate(
        mean  = c(0, wtp_material$mean),
        lower = c(0, wtp_material$lower),
        upper = c(0, wtp_material$upper))

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(c(
    df_quantity$lower,
    df_set_options$lower,
    df_material$lower)))
ymax <- ceiling(max(c(
    df_quantity$upper,
    df_set_options$upper,
    df_material$upper)))

# Plot the WTP for each attribute *with 95% CI*
plot_quantity <- df_quantity %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) +
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Quantity (sets per purchase)', y = 'WTP ($)') +
    theme_bw() #continuous plot

plot_material <- df_material %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Material type', y = 'WTP ($)') +
    theme_bw()

plot_set_options <- df_set_options %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Set Options', y = 'WTP ($)') +
    theme_bw()

# Plot all plots in one figure
plot_model_wtp <- plot_grid(
    plot_quantity, plot_set_options, plot_material,
    nrow = 1
)

plot_model_wtp

# Save plots
ggsave(
    filename = here('figs', 'model_wtp.png'),
    plot = plot_model_wtp,
    width = 8, height = 2.3
)

# -----------------------------------------------------------------------------

# Market simulations
    
market_data <- data.frame(
    altID       = c(1, 2, 3), 
    obsID       = c(1, 1, 1),
    set_price   = c(0.16, 0.25, 0.48), # Price based on market data ($)
    quantity    = c(50, 150, 200),   # Quantity in the box
    setOptions_fk = c(0, 1, 0),
    setOptions_fks = c(0, 0, 1),#base:f
    material_Eco    = c(1, 0, 0),
    material_Bamboo = c( 0, 1, 0) #base:plastic
)

market_data <- clean_names(market_data)
head(market_data)
coef(model)

# Use the predict() function to compute the probabilities
sim_model_linear <- predict(
    mnl,
    newdata = market_data, 
    obsID = "obsID", 
    ci = 0.95, 
    returnData = TRUE # This returns your data along with predicted values
)
sim_model_linear

save(
    sim_model_linear,
    file = here("models", "sim_model_linear.RData")
)

# -----------------------------------------------------------------------------
# Multiple simulations using the linear model

# Read in market scenarios

scenarios <- read_csv(here("data", "scenarios.csv"))
scenarios <- scenarios %>% 
    rename(quantity = Quantity, 
           set_price = per_price)

scenarios <- dummy_cols(scenarios, c("setOptions", "material"))

# Use the predict() function to compute the probabilities
sim_model_linear_multi <- predict(
    mnl,
    newdata = scenarios, 
    obsID = "obsID", 
    ci = 0.95,
    returnData = TRUE
)

view(sim_model_linear_multi)

# Save simulations
save(
    sim_model_linear,
    sim_model_linear_multi,
    file = here("data", "sim_model_linear_multi.RData")
)

summary(sim_model_linear_multi)

sim_model_linear %>% 
    mutate(label = c("Eco-friendly", "Bamboo", "Plastic")) %>% 
    ggplot(aes(
        x = label, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Alternative', y = 'Market Shares') +
    theme_bw()

ggsave(
    filename = here('figs', 'market_simulations.png'), 
    width = 4, height = 3.5
)

sim_model_linear_multi %>% 
    ggplot(aes(
        x= as.factor(altID), y= predicted_prob, 
        ymin= predicted_prob_lower, ymax= predicted_prob_upper))+
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width= 0.3)  +
    facet_wrap(~obsID) +
    scale_y_continuous(limits = c(0,1), labels= scales::percent)+
    labs(x= 'Alternatives', y= 'Market shares') +
    #scale_x_discrete(labels = c("Eco", "Bamboo", "Plastic")) +
    theme_bw() #1-Eco, 2-Bamboo, 3-Plastic

ggsave(
    filename = here('figs', 'multi_market_simulations.png'), 
    width = 4, height = 3.5
)

---
    
    ###############################################################################
#Sensitivity analysis
###############################################################################
# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in *price*

load(here("models", "model.RData"))
mnl

# Create a set of alternatives for which to simulate shares
baseline <- data.frame(
    altID       = c(1, 2, 3), 
    obsID       = c(1, 1, 1),
    set_price   = c(20/50, 25/50, 30/50), # Price based on market data ($)
    quantity    = c(50, 150, 200),   # Quantity in the box
    material_Bamboo = c(0,1,0),
    material_Eco = c(0,0,1),
    setOptions_fk = c(0,1,0),#base:plastic
    setOptions_fks = c(0,0,1) 
)

# Define the sensitivity cases
# For this case, let's see how the market share for the Eco 
# (option 2) changes with different Eco prices. That is, I'm holding everything
# the same in every simulation except the price for the Eco

prices <- seq(10/50, 60/50, 0.1) # Define sensitivity price levels, 
#i set the upper value to 10 to check the drop in market share to almost a 0% as price increases
n <- length(prices) # Number of simulations (21)
scenarios_price <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_price$obsID <- rep(seq(n), each = 3) # Reset obsIDs

# Set the price for each scenario
scenarios_price$set_price[which(scenarios_price$altID == 3)] <- prices 


load(here("models", "model.RData"))

# For each case, simulate the market share predictions
sens_price <- predict(
    mnl,
    newdata = scenarios_price, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only Eco alternative
    filter(altID == 3) %>% 
    # Keep only prices and predictions
    select(set_price, starts_with("predicted_")) 

sens_price <- as.data.frame(sens_price) %>%
    dplyr::select(set_price,starts_with("predicted_"))

sens_price

#########

# The probability shifts from essentially 48.5% of the market share at 
# a price of $0.1 per set to 26% at $10 per set

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in multiple attributes

# For these cases, we'll look at how the market share for the Eco-friendly option 
# (option 2) changes with +/- 30% changes in set_price and quantity

# "high" means they result in higher market shares
# "low"  means they result in lower market shares

#scenarios
cases <- tribble(
    ~obsID, ~altID, ~attribute,    ~case,  ~value,
    1,      3,     'set_price',  'high',  30*1.2/50,
    1,      3,     'set_price',  'low',   30*0.8/50,
    1,      1,     'quantity',    'high', 50*1.2,
    1,      1,     'quantity',    'low',  50*0.8
)

cases

baseline
# Define scenarios
n <- 7 # baseline + high & low for each attribute
scenarios_atts <- rep_df(baseline, n) 
scenarios_atts$obsID <- rep(seq(n), each = 3) # Reset obsIDs

# Replace scenarios with case values 
scenarios_atts <- scenarios_atts %>% 
    left_join(cases, by = c("altID", "obsID")) %>% 
    mutate(
        attribute = ifelse(is.na(attribute), "other", attribute),
        case = ifelse(is.na(case), "base", case),
        price = ifelse(attribute == 'set_price', value, set_price),
        quantity = ifelse(attribute == 'quantity', value, quantity)
    )

scenarios_atts


# For each case, simulate the market share predictions
sens_atts <- predict(
    mnl,
    newdata = scenarios_atts, 
    obsID = 'obsID', 
    ci = 0.95, 
    returnData = TRUE) %>%
    # Keep only Eco alternative
    filter(altID == 3) %>% 
    # Keep only attributes and predictions
    select(attribute, case, value, predicted_prob)

sens_atts

# -----------------------------------------------------------------------------
# Save simulations

save(
    sens_price,
    sens_atts,
    file = here("models", "sens_price_model_linear.RData")
)

###################################################################################-----------------------------------------------------------------------------
# Plot sensitivity simulation results

# Load libraries & functions
library(tidyverse)
library(here)
library(maddTools)

# Load simulation results
load(here("models", "sens_price_model_linear.RData"))


########################################################################### 
# Make a line plot of the market sensitivity to price (with uncertainty)

share_price_plot <- 
    sens_price %>% 
    ggplot(aes(
        x = set_price, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_price %>% filter(set_price <= 30/50, set_price >= 20/50), 
        linetype = "solid") +
    expand_limits(x = c(0.2, 0.8), y = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Price ($)', y = 'Market Share %') +
    theme_bw()

share_price_plot


# Save plot
ggsave(
    filename = here('figs', 'share_price_plot.png'), 
    plot = share_price_plot,
    width = 5, height = 4
)

############################################################################ 
# Make a line plot of the revenue sensitivity to price (with uncertainty)

marketSize <- 1000
rev_data <- sens_price %>%
    mutate(
        rev_mean = set_price*marketSize*predicted_prob/100, # Convert to millions
        rev_low  = set_price*marketSize*predicted_prob_lower/100,
        rev_high = set_price*marketSize*predicted_prob_upper/100)

rev_price_plot <- rev_data %>% 
    ggplot(aes(x = set_price, y = rev_mean, ymin = rev_low, ymax = rev_high)) +
    geom_ribbon(alpha = 0.1) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = rev_data %>% filter(set_price <= 30/50, set_price >= 20/50), 
        linetype = "solid") +
    expand_limits(x = c(0.4, 1), y = c(0,7)) +
    labs(x = 'Price ($)', y = 'Revenue ($ Millions)') +
    theme_bw()

rev_price_plot

# Save plot
ggsave(
    filename = here('figs', 'rev_price_plot.png'), 
    plot = rev_price_plot,
    width = 5, height = 4
)

########################################################################## 
# Make a tornado diagram to show market sensitivity to multiple
library(cowplot)

labels <- data.frame( 
    attribute = c('set_price', 'quantity'), 
    label = c('Price ($)', 'Quantity (unit)')
)

tornado_data <- sens_atts %>% 
    filter(case != 'base') %>% 
    # Rename variables for plotting labels
    left_join(labels, by = 'attribute')

tornado_base <- ggtornado(
    data = tornado_data,
    baseline = sens_atts$predicted_prob[1], 
    var = 'label',
    level = 'case',
    value = 'value', 
    result = 'predicted_prob'
) 

# Change the fill colors, adjust labels
tornado_plot <- tornado_base +
    scale_fill_manual(values = c("#67a9cf", "#ef8a62")) + 
    labs(x = 'Market Share', y = 'Attribute')

tornado_plot


# Save plot
ggsave(
    filename = here('figs', 'tornado_plot.png'), 
    plot = tornado_plot,
    width = 5, height = 3
)
