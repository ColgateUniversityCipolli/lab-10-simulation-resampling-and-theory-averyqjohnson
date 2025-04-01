###########################################################################
# HW 10
# Avery Johnson
# MATH 240 - SPRING 2025
###########################################################################

library(tidyverse)
################################################################################
# 1) Basic Simulation
################################################################################

# for sample size of 1004
samp.size <- 1004 # sample size
p_true <- 0.39
simulations <- 10000

sample_counts <- rbinom(simulations, size=samp.size, prob=p_true)
sample_proportions <- sample_counts/samp.size

og.sample <- tibble(proportion=sample_proportions)

sample.1004.plot <- ggplot(data=og.sample)+
  geom_histogram(aes(x=proportion, y=after_stat(density)), color="lightgrey") +
  geom_density(aes(x = proportion), color = "red")+
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab("Sample Proportion")+
  ylab("Density") +
  ggtitle("Sampling Distribution (n=1004)")

middle.95.range.1004 <- quantile(sample_proportions, probs = c(0.025, 0.975))
middle.range.1004 <- middle.95.range.1004[2] - middle.95.range.1004[1]
margin.error.1004 <- middle.range.1004 / 2
  
# for sample size of 2008
samp.size.2 <- 2008 # sample size
p_true <- 0.39
simulations <- 10000

sample_counts.2 <- rbinom(simulations, size=samp.size.2, prob=p_true)
sample_proportions.2 <- sample_counts.2/samp.size.2

og.sample.2 <- tibble(proportion=sample_proportions.2)

sample.2008.plot <- ggplot(data=og.sample.2)+
  geom_histogram(aes(x=proportion, y=after_stat(density)), color="lightgrey") +
  geom_density(aes(x = proportion), color = "red")+
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab("Sample Proportion")+
  ylab("Density") +
  ggtitle("Sampling Distribution (n=2008)")

middle.95.range.2008 <- quantile(sample_proportions.2, probs = c(0.025, 0.975))
middle.range.2008 <- middle.95.range.2008[2] - middle.95.range.2008[1]
margin.error.2008 <- middle.range.2008 / 2

library(patchwork)
sample.plots <- sample.1004.plot + sample.2008.plot

results_table <- tibble(
  `Sample Size` = c(1004, 2008),
  `Lower Bound (95%)` = c(middle.95.range.1004[1], middle.95.range.2008[1]),
  `Upper Bound (95%)` = c(middle.95.range.1004[2], middle.95.range.2008[2]),
  `Margin of Error` = c(margin.error.1004, margin.error.2008)
)
view(results_table)

################################################################################
# 2) Resampling
################################################################################

satisfied.prop <- 0.39
dissatisfied.prop <- 0.59
no.opinion.prop <- 0.02
samp.size <- 1004

# data frame from Gallup survey
gallup.sample <- tibble(id          = 1:samp.size,
                        response = c(rep("Satisfied", round(satisfied.prop * samp.size)), 
                                 rep("Not Satisfied", round(dissatisfied.prop * samp.size)),
                                 rep("No Opinion", round(no.opinion.prop * samp.size)))
)

# resamples
R <- 1000
resamples <- tibble(p.hat = numeric(R))

for (i in 1:R) {
  curr.resample <- sample(gallup.sample$response, size=samp.size, replace=T)
  resamples$p.hat[i] <- mean(curr.resample == "Satisfied")
}

resampling.plot <- ggplot(data=resamples) +
  geom_histogram(aes(x = p.hat, y = after_stat(density)), color = "lightgrey", bins = 30) +
  geom_density(aes(x = p.hat), color="red") +
  geom_hline(yintercept=0) +
  theme_bw() +
  xlab(bquote(hat(p)))+
  ylab("Density") +
  ggtitle(bquote("Resampling Distribution of " ~ hat(p)))

middle_95_range_resample <- quantile(resamples$p.hat, probs = c(0.025, 0.975))
margin_error_resample <- (middle_95_range_resample[2] - middle_95_range_resample[1]) / 2


results_table <- tibble(
  `Sample Size` = c("Simulation (n=1004)", "Simulation (n=2008)", "Resample"),
  `Lower Bound (95%)` = c(middle.95.range.1004[1], middle.95.range.2008[1], middle_95_range_resample[1]),
  `Upper Bound (95%)` = c(middle.95.range.1004[2], middle.95.range.2008[2], middle_95_range_resample[2]),
  `Margin of Error` = c(margin.error.1004, margin.error.2008, margin_error_resample)
)
view(results_table)


################################################################################
# 3) Simulation over n and p
################################################################################

n_values <- seq(100, 3000, by=10)
p_values <- seq(0.01, 0.99, by=0.01)
simulations <- 10000

# initialize an empty tibble
margin_error_results <- tibble(n = numeric(), 
                               p = numeric(), 
                               margin_error = numeric())

for (n in n_values) {
  for (p in p_values) {
    sample_counts <- rbinom(simulations, size=n, prob=p)
    sample_proportions <- sample_counts/n
    middle_95_range <- quantile(sample_proportions, probs = c(0.025, 0.975))
    margin_error <- (middle_95_range[2] - middle_95_range[1]) / 2
    
    margin_error_results<- bind_rows(margin_error_results, 
                                     tibble(n=n, p=p, margin_error=margin_error))
  }
}
view(margin_error_results)

ggplot(data=margin_error_results, aes(x=n, y=p, fill=margin_error)) +
  geom_raster() +
  scale_fill_viridis_c(name="Margin of Error")
  theme_bw() +
  ggtitle("Margin of Error Across Different Sample Sizes and Probabilities") +
  xlab("Sample Size (n)") +
  ylab("Probability (p)")
