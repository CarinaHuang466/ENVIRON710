#Name: Carina Huang
#Date: 1/14/2026
#title: Lab 1 assignment

#load the libraries
library(tidyverse)
library(palmerpenguins)

#loading the data into a df
penguins_df <- penguins %>% 
  mutate(body_mass_kg = body_mass_g/1000)

#Question 1: Measures of location

#a.mean penguin body mass (in kg)
mass_mean <- mean(penguins_df$body_mass_g, na.rm = TRUE)/1000

#b.median penguin body mass(in kg)
mass_median <- median (penguins_df$body_mass_g, na.rm = TRUE)/1000

#c.percentile
mass_percentile <- quantile(penguins_df$body_mass_g, 
                            probs = seq(0, 1, 0.05),
                            na.rm = TRUE)/1000
view(mass_percentile)
#answer: The 5th and 95th penguin body masses(in kg) is 3.15 to 5.65

#2.a
mass_range<-range(penguins_df$body_mass_g, na.rm = TRUE)/1000

#2.b
mass_sd <- sd(penguins_df$body_mass_kg, na.rm = TRUE)

#2.c
mass_var <- var(penguins_df$body_mass_kg, y=NULL, na.rm = TRUE)


#3.a&b
fig_bodymass <- ggplot(data = penguins_df, 
                       aes(x=island, 
                       y=body_mass_kg,
                       color=species))+
  geom_jitter()+
  labs(x = "Island",
       y = "Penguin Body Mass (kg)",
       color = "Species",
       title = "Penguin Body Mass (kg) by islands and species")

print(fig_bodymass)
                       
#3.c
#For species Adelie, it is distributed across all three islands, and weight about the same in body mass (kg), 
# but you could find Chinstrap penguins mostly only on Dream Island, and Gentoo penguins only on Biscoe Island


#3.d 
#Answer: Most of the raw values of the penguin body masses distributes around 3-4kg, and there's a large number of penguins
#weighting over 4kgs, even over 5kgs. But the reason why the mean is around 4.202 is because most of the Gentoo penguins weight
#above 4kg, so that could bring the value of mean body mass higher. 
