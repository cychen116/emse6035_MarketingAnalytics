# Visualize results of estimated multinomial logit (mnl) models 

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# Load estimated models
load(here("models","model.RData"))

# -----------------------------------------------------------------------------
#MNL
summary(mnl)
coefs <- coef(mnl)
ses<- se(mnl)
coef(summary(mnl))

# -----------------------------------------------------------------------------
# Plot results

# Get the estimated coefficients


# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price <- data.frame(level = c(20, 25, 30)) %>% 
    mutate(
        diff    = level - min(level),
        utility = diff*coefs['set_price'])

df_quantity <- data.frame(level = c(50, 100, 150, 200)) %>% 
    mutate(
        diff    = level - min(level),
        utility = diff*coefs['quantity'])

df_material <- data.frame(
    level = c("Plastic", "Bamboo", "Eco"),
    utility = c(0, coefs['material_Bamboo'], coefs['material_Eco']))

df_setOptions = data.frame(
    level = c("F", "F&K",'F&K&S'),
    utility = c(0, coefs['setOptions_fk'], coefs['setOptions_fks']))

# Get upper and lower bounds (plots should have the same y-axis)
utility <- c(
    df_price$utility, df_quantity$utility, 
    df_material$utility, df_setOptions$utility) 
ymin <- floor(min(utility))
ymax <- ceiling(max(utility))

# Plot the utility for each attribute
plot_price <- df_price %>% 
    ggplot() +
    geom_line(aes(x = level, y = utility)) +
    geom_point(aes(x = level, y = utility))+
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Price ($)', y = 'Utility') +
    theme_bw()

plot_price

plot_quantity <- df_quantity %>% 
    ggplot() +
    geom_line(aes(x = level, y = utility))+
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Quantity (set no.)', y = 'Utility') +
    theme_bw()
plot_quantity

plot_material <- df_material %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Materials', y = 'Utility') +
    theme_bw()
plot_material

plot_setOptions <- df_setOptions %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Set Options', y = 'Utility') +
    theme_bw()

plot_setOptions
# Plot all plots in one figure
plot_mnl <- plot_grid(
    plot_price, plot_quantity, plot_material, plot_setOptions,
    nrow = 1
)

plot_mnl

# Save plots 
ggsave(
    filename = here('figs', 'plot_mnl.png'), 
    plot = plot_mnl, 
    width = 10, height = 2.3
)

# -----------------------------------------------------------------------------
#MNL-Dummy
summary(mnl_dummy)
coefs <- coef(mnl_dummy)

# -----------------------------------------------------------------------------
df_price <- data.frame(
    level = c(20, 25, 30),
    utility = c(0, coefs['price_25'],coefs['price_30']))

df_quantity <- data.frame(
    level = c(50, 150, 200),
    utility = c(0, coefs['quantity_150'],coefs['quantity_200']))

df_material <- data.frame(
    level = c("Plastic", "Bamboo", "Eco"),
    utility = c(0, coefs['material_Bamboo'], coefs['material_Eco']))

df_setOptions = data.frame(
    level = c("F", "F&K",'F&K&S'),
    utility = c(0, coefs['setOptions_fk'], coefs['setOptions_fks']))

# Get upper and lower bounds (plots should have the same y-axis)
utility <- c(
    df_price$utility, df_quantity$utility, 
    df_material$utility, df_setOptions$utility) 
ymin <- floor(min(utility))
ymax <- ceiling(max(utility))

# Plot the utility for each attribute
plot_price <- df_price %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility))+
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Price ($)', y = 'Utility') +
    theme_bw()

plot_price

plot_quantity <- df_quantity %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Quantity (set no.)', y = 'Utility') +
    theme_bw()
plot_quantity

plot_material <- df_material %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Materials', y = 'Utility') +
    theme_bw()
plot_material

plot_setOptions <- df_setOptions %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Set Options', y = 'Utility') +
    theme_bw()

plot_setOptions
# Plot all plots in one figure
plot_mnl_dummy <- plot_grid(
    plot_price, plot_quantity, plot_material, plot_setOptions,
    nrow = 1
)

plot_mnl_dummy

# Save plots 
ggsave(
    filename = here('figs', 'plot_mnl_dummy.png'), 
    plot = plot_mnl_dummy, 
    width = 10, height = 2.3
)