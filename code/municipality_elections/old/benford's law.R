# Load necessary libraries
library(ggplot2)

# Step 1: Extract the leading digit
leading_digit <- as.numeric(substring(as.character(kommunalwahlen_merge$abs_CDU), 1, 1))
leading_digit <- leading_digit[!leading_digit==0]

# Step 2: Count the occurrences of each digit
leading_digit_counts <- table(leading_digit)

# Convert to a data frame for plotting
digit_data <- as.data.frame(leading_digit_counts)
colnames(digit_data) <- c("Digit", "Count")

# Step 3: Calculate Benford's Law expected probabilities
benford_law <- function(d) {
  return(log10(1 + 1/d))
}

expected_prob <- sapply(1:9, benford_law)
benford_data <- data.frame(Digit = 1:9, Benford_Prob = expected_prob)

# Step 4: Normalize actual counts to probabilities for comparison
digit_data$Observed_Prob <- digit_data$Count / sum(digit_data$Count)

# Plot actual vs. expected Benford distribution
ggplot() +
  geom_bar(data = digit_data, aes(x = as.factor(Digit), y = Observed_Prob), stat = "identity", fill = "blue", alpha = 0.7) +
  geom_line(data = benford_data, aes(x = as.factor(Digit), y = Benford_Prob, group = 1), color = "red", size = 1) +
  geom_point(data = benford_data, aes(x = as.factor(Digit), y = Benford_Prob), color = "red", size = 3) +
  labs(title = "Comparison of Leading Digit Distribution to Benford's Law",
       x = "Leading Digit", y = "Probability") +
  theme_minimal()
