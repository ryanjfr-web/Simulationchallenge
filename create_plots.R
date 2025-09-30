# Load required libraries
library(ggplot2)
library(scales)

# Set seed for reproducibility
set.seed(123)

# Simulation parameters
initial_wealth <- 1000
num_simulations <- 100
max_years <- 30

# Function to simulate one game
simulate_game <- function(initial_wealth, years) {
  wealth <- numeric(years + 1)
  wealth[1] <- initial_wealth
  
  for (i in 1:years) {
    coin_flip <- rbinom(1, 1, 0.5)  # 1 for heads, 0 for tails
    if (coin_flip == 1) {
      wealth[i + 1] <- wealth[i] * 1.5  # 50% increase
    } else {
      wealth[i + 1] <- wealth[i] * 0.6  # 40% decrease
    }
  }
  
  return(wealth)
}

# Run 100 simulations
simulation_results <- list()
final_wealths <- numeric(num_simulations)

for (i in 1:num_simulations) {
  wealth_path <- simulate_game(initial_wealth, max_years)
  simulation_results[[i]] <- wealth_path
  final_wealths[i] <- wealth_path[length(wealth_path)]
}

# Create data frame for analysis
simulation_df <- data.frame(
  simulation = rep(1:num_simulations, each = max_years + 1),
  year = rep(0:max_years, num_simulations),
  wealth = unlist(simulation_results)
)

# Plot 1: Single Simulation
set.seed(42)
single_simulation <- simulate_game(initial_wealth, max_years)

single_plot <- ggplot(data.frame(year = 0:max_years, wealth = single_simulation), 
                      aes(x = year, y = wealth)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue", size = 1) +
  geom_hline(yintercept = initial_wealth, color = "red", linetype = "dashed", size = 1) +
  scale_y_continuous(labels = dollar_format(), trans = "log10") +
  scale_x_continuous(breaks = seq(0, max_years, 5)) +
  labs(title = "Single Simulation: Account Balance Over Time",
       subtitle = paste("Final balance: $", format(round(single_simulation[length(single_simulation)], 2), big.mark = ",")),
       x = "Years (Age 25 to 55)",
       y = "Account Balance ($)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "darkblue"),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank())

ggsave("single_simulation_plot.png", single_plot, width = 10, height = 6, dpi = 300)
print("Single simulation plot saved as single_simulation_plot.png")

# Plot 2: Multiple Simulations Trajectories
sample_sims <- sample(1:num_simulations, min(20, num_simulations))
trajectory_df <- simulation_df %>%
  filter(simulation %in% sample_sims)

trajectory_plot <- ggplot(trajectory_df, aes(x = year, y = wealth, group = simulation)) +
  geom_line(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = initial_wealth, color = "red", linetype = "dashed", size = 1) +
  scale_y_continuous(labels = dollar_format(), trans = "log10") +
  labs(title = "Sample Wealth Trajectories (Log Scale)",
       subtitle = "Red line indicates initial investment of $1,000",
       x = "Year",
       y = "Wealth ($)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

ggsave("multiple_simulations_plot.png", trajectory_plot, width = 10, height = 6, dpi = 300)
print("Multiple simulations plot saved as multiple_simulations_plot.png")

# Plot 3: Distribution of Final Wealth
distribution_plot <- ggplot(data.frame(final_wealth = final_wealths), aes(x = final_wealth)) +
  geom_histogram(bins = 25, fill = "steelblue", alpha = 0.7, color = "white", size = 0.3) +
  geom_vline(xintercept = initial_wealth, color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = mean(final_wealths), color = "darkgreen", linetype = "solid", size = 1.2) +
  scale_x_continuous(labels = dollar_format(), trans = "log10") +
  labs(title = "Distribution of Final Account Balances (100 Simulations)",
       subtitle = paste("Mean: $", format(round(mean(final_wealths), 2), big.mark = ","), 
                        "| Median: $", format(round(median(final_wealths), 2), big.mark = ",")),
       x = "Final Account Balance at Age 55 ($)",
       y = "Frequency",
       caption = "Red dashed line: Initial investment ($1,000)\nGreen solid line: Mean final balance") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "darkblue"),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 10, hjust = 0),
        panel.grid.minor = element_blank())

ggsave("distribution_plot.png", distribution_plot, width = 10, height = 6, dpi = 300)
print("Distribution plot saved as distribution_plot.png")

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Mean final balance: $", format(round(mean(final_wealths), 2), big.mark = ","), "\n")
cat("Median final balance: $", format(round(median(final_wealths), 2), big.mark = ","), "\n")
cat("Standard deviation: $", format(round(sd(final_wealths), 2), big.mark = ","), "\n")
cat("Minimum final balance: $", format(round(min(final_wealths), 2), big.mark = ","), "\n")
cat("Maximum final balance: $", format(round(max(final_wealths), 2), big.mark = ","), "\n")
cat("Probability of ending above $1,000: ", round(mean(final_wealths > initial_wealth) * 100, 1), "%\n")
