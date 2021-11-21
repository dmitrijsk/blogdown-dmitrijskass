import numpy as np
from sklearn.linear_model import LogisticRegression
# Plotting function in plot_decision_boundary.py.
from plot_decision_boundary import plot_decision_boundary as plot_db

def logistic(z):
    return 1 / (1 + np.exp(-z))

# True theta coefficients.
theta = np.array([4, -2])
# Number of features.
p = len(theta)

# Number of training data points.
n = 200
# Generate feature values from U[0,1].
np.random.seed(1)
X = np.random.rand(n, p)

# Calculate logits.
z = X @ theta.reshape(-1, 1)
# Calculate probabilities.
prob = logistic(z)

# Print the first 5 elements.
print(f"The first five probabilities: {np.round(prob[:5].flatten(), 2)}")

# Generate labels by sampling from Bernoulli(prob)
y = np.random.binomial(1, prob.flatten())

# Print the first 5 elements.
print(f"The first five class labels: {y[:5].flatten()}")

# Check the proportion of classes.
y_counts = np.unique(y, return_counts = True)
print(f"Proportions of classes: {y_counts[1] / n}")

# Train a logistic regression model.
model = LogisticRegression(fit_intercept = False, penalty = "none").fit(X, y)

# Compare the learned parameters to the true ones.
print(f"Learned theta: {np.round(model.coef_, 2)} (true theta was {theta})")

# Plot decision boundaries.
plot_db(X, y, model)
