import numpy as np
from sklearn.linear_model import LogisticRegression
# Plotting function in plot_decision_boundary.py.
from plot_decision_boundary import plot_decision_boundary as plot_db

def logistic(z):
    return 1 / (1 + np.exp(-z))

# True theta coefficients.
theta = np.array([[4], [-2]])
# Number of features.
p = len(theta)

# Number of training data points.
n = 200
# Generate feature values from U[0,1].
np.random.seed(1)
X = np.random.rand(n, p)

# Calculate logits.
z = np.dot(X, theta)
# Calculate probabilities.
prob = logistic(z)

# Print the first 5 elements.
print(f"The first five probabilities: {np.round(prob[:5].flatten(), 2)}")

# ---------------------------------------------------------------------------
# Probabilistic case
# ---------------------------------------------------------------------------

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
plot_db(X, y, 
        model, 
        title = "(a) Bernoulli-distributed class labels",
        filename = "log-reg-decision-boundary-probabilistic.png")


# ---------------------------------------------------------------------------
# Deterministic case
# ---------------------------------------------------------------------------

# Assign classes deterministically.
y_deterministic = np.where(prob.flatten() >= 0.5, 1, 0)
# Train a logistic regression model.
model = LogisticRegression(fit_intercept = False, penalty = "none").fit(X, y_deterministic)
# Training set accuracy is 100%.
acc = np.mean(model.predict(X) == y_deterministic)
print(f"Accuracy on the training set is {acc*100}%")
# Plot decision boundaries.
plot_db(X, y_deterministic, 
        model, 
        title = "(b) Deterministic class labels",
        filename = "log-reg-decision-boundary-deterministic.png")
