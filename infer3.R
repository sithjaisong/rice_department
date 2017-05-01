# Recall the bootstrap t-confidence interval
p_hat <- mean(one_poll$vote)
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))

# Collect a sample of 30 observations from the population
one_poll <- as.tbl(data.frame(vote = rbinom(n = 30, 1, .6)))

# Resample the data using samples of size 300 (an incorrect strategy!)
one_poll_boot_300 <- one_poll %>%
  rep_sample_n(size = 300, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote))

# Find the endpoints of the the bootstrap t-confidence interval
one_poll_boot_300 %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot)
  )

# Resample the data using samples of size 3 (an incorrect strategy!)
one_poll_boot_3 <- one_poll %>%
  rep_sample_n(size = 3, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote)) 

# Find the endpoints of the the bootstrap t-confidence interval 
one_poll_boot_3 %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))

# Collect 30 observations from a population with true proportion of 0.8
one_poll <- as.tbl(data.frame(vote = rbinom(n = 30, size = 1, prob = 0.8)))

# Compute p-hat of new sample: p_hat
p_hat <- mean(one_poll$vote == 1)

# Resample the 30 observations (with replacement)
one_poll_boot <- one_poll %>%
  rep_sample_n(size = 30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote == 1) )

# Calculate the bootstrap t-confidence interval
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot)
  )
# Calculate a 95% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q025_prop = quantile(prop_yes_boot, p = 0.025),
            q975_prop = quantile(prop_yes_boot, p = 0.975))

# Calculate a 99% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q005_prop = quantile(prop_yes_boot, p = 0.005),
            q995_prop = quantile(prop_yes_boot, p = 0.995))

# Calculate a 90% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q05_prop = quantile(prop_yes_boot, p = 0.05),
            q95_prop = quantile(prop_yes_boot, p = 0.95))