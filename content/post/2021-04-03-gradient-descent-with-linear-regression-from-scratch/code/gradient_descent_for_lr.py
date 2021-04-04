"""
Learning rate: 
    Convergence:
        0.001 (slow), 0.005 (fast), 0.01 (jumps).
    Divergence: 0.02
"""

path = "c:/Users/dmitr/Documents/git/blogdown-dmitrijskass/content/post/2021-04-03-gradient-descent-with-linear-regression-from-scratch/images/"

import numpy as np
import matplotlib.pyplot as plt
import viz_fun as viz           # User functions for visualization.


class GradientDescentLinearRegression:
    def __init__(self, X, y, learning_rate=0.01, max_iterations=10000, tolerance=1e-5):
        self.X = X
        self.y = y
        
        self.w = np.zeros(X.shape[1])        # Initialization of params.
        self.w_hist = [self.w]               # History of params.
        self.cost_hist = [self.cost()]       # History of cost.
        
        self.learning_rate = learning_rate   # Learning rate, alpha.
        self.max_iterations = max_iterations # Max iterations.
        self.iterations = 0                  # Actually performed iterartions.
        self.tolerance = tolerance           # Tolerance, epsilon.
    
    def predict(self, X):
        return np.dot(X, self.w.T)
    
    def cost(self):
        y_pred = self.predict(self.X)
        loss = (self.y - y_pred)**2
        return np.mean(loss)

    def grad(self):
        y_pred = self.predict(self.X)
        d_intercept = -2*sum(self.y - y_pred)                        # dJ/d w_0.
        d_x = -2*sum(self.X[:,1:] * (self.y - y_pred).reshape(-1,1)) # dJ/d w_i.
        g = np.append(np.array(d_intercept), d_x)                    # Gradient.
        return g

    def fit(self):        
        w_lstsq = np.linalg.lstsq(self.X, self.y, rcond = None)[0] # OLS solution.
        
        for iter in range(self.max_iterations):
            
            # self.learning_rate = 1 / (iter + 1)                    # Adaptive alpha.
            g = self.grad()                                        # Calculate the gradient.
            self.w = self.w - self.learning_rate * g               # Update parameters.
            self.w_hist.append(self.w)                             # Save to history.
            J = self.cost()                                        # Calculate the cost.
            self.cost_hist.append(J)                               # Save to history.
            
            # Stop if close to the OLS solution.
            if np.linalg.norm(self.w - w_lstsq) < self.tolerance:
                break
        self.iterations = iter


def generate_data(n_predictors = 1, n_observations = 5, location = 1, scale = 3):
    """Generate data for the linear regression"""
    
    # Reproducibility.
    np.random.seed(6)     
    # True parameters, +1 for the intercept.
    w_star = np.random.randn(n_predictors + 1)
    X = np.random.normal(loc = location, 
                         scale = scale, 
                         size = (n_observations, n_predictors))
    # Add a column of ones for an intercept.
    X = np.column_stack((np.ones(n_observations), X))
    noise = np.random.randn(n_observations)
    # Compute output variable.
    y = np.dot(X, w_star.T) + noise
    
    return X, y


X, y = generate_data()
viz.plot_data(X, y)
plt.savefig(path + "1-generated-data.png")


model = GradientDescentLinearRegression(X, y)
model.fit()
print(f"Gradient descent solution in {model.iterations} iterations: {model.w}.")

w_lstsq = np.linalg.lstsq(X, y, rcond = None)[0]
print(f"Least squares solutions: {w_lstsq}.")

if (model.X.shape[1] == 2):

    viz.plot_data_and_fitted_line(model)
    plt.savefig(path + "2-generated-data-and-fitted-line.png")

    viz.plot_cost(model)
    plt.savefig(path + "3-cost-hist.png")
    
    viz.plot_surface(model, path)    
    viz.animate_fitted_line(model, path)
