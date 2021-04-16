"""
Learning rate for the standard gradient descent: 
    Convergence:
        0.001 (slow), 0.005 (fast), 0.01 (jumps).
    Divergence: 0.02
"""

import numpy as np
import matplotlib.pyplot as plt
import viz_fun as viz # My visualization functions.

class GradientDescentLinearRegression:
    """
    Linear Regression with gradient-based optimization.
    Parameters
    ----------
    learning_rate : float
        Learning rate for the gradient descent algorithm.
    max_iterations : int
        Maximum number of iteration for the gradient descent algorithm.
    eps : float
        Tolerance level for the Euclidean norm between the OLS solution
        and the gradient descent solution. The algorithm is stooped when
        the norm becomes less than the tolerance level.
    """
    
    def __init__(self, learning_rate=0.01, max_iterations=100000, eps=1e-6):
        self.learning_rate = learning_rate
        self.max_iterations = max_iterations
        self.eps = eps
        
    def predict(self, X):
        """Returns predictions array of shape [n_samples,1]"""
        return np.dot(X, self.w.T)
    
    def cost(self, X, y):
        """Returns the value of the cost function as a scalar real number"""
        y_pred = self.predict(X)
        loss = (y - y_pred)**2
        return np.mean(loss)

    def grad(self, X, y):
        """Returns the gradient vector"""
        y_pred = self.predict(X)
        d_intercept = -2*sum(y - y_pred)                    # dJ/d w_0.
        d_x = -2*sum(X[:,1:] * (y - y_pred).reshape(-1,1))  # dJ/d w_i.
        g = np.append(np.array(d_intercept), d_x)           # Gradient.
        return g
          
    def fit(self, X, y, adagrad = True):
        """
        Fit linear model with gradient descent.
        
        Parameters
        ----------
        X : numpy array or sparse matrix of shape [n_samples,n_predictors]
            Training data
        y : numpy array of shape [n_samples,1]
            Target values.
        adagrad : boolean
                  If False, uses a standard gradient descent with
                  a constant learning rate. If True, uses AdaGrad.
        
        Returns
        -------
        self : returns an instance of self.
        """
        
        self.w = np.zeros(X.shape[1])                     # Initialization of params.
        if adagrad == True:
            G = np.zeros(X.shape[1])                      # Initialization of cache for AdaGrad.
        w_hist = [self.w]                                 # History of params.
        cost_hist = [self.cost(X, y)]                     # History of cost.      
        
        for iter in range(self.max_iterations):
            
            g = self.grad(X, y)                           # Calculate the gradient.
            if adagrad == False:
                step = self.learning_rate * g             # Calculate standard gradient step.
            else:
                G += g**2                                 # Update cache.
                step = self.learning_rate * \
                    1 / (np.sqrt(G + self.eps)) * g       # Calculate AdaGrad step.
            self.w = self.w - step                        # Update parameters.
            w_hist.append(self.w)                         # Save to history.
            
            J = self.cost(X, y)                           # Calculate the cost.
            cost_hist.append(J)                           # Save to history.
            
            print(f"Iter: {iter}, gradient: {g}, params: {self.w}, cost: {J}")
            
            # Stop if update is small enough.
            if np.linalg.norm(w_hist[-1] - w_hist[-2]) < self.eps:
                break
        
        self.iterations = iter + 1                       # Due to zero-based indexing.
        self.w_hist = w_hist
        self.cost_hist = cost_hist
        
        return self

# Simple alternative to adaptive alpha is self.learning_rate = 1 / (iter + self.eps)
            
def generate_data(n_predictors = 1, n_samples = 5, location = 1, scale = 3):
    """Generate data for the linear regression"""
    
    # Reproducibility.
    np.random.seed(6)     
    # True parameters, +1 for the intercept.
    w_star = np.random.randn(n_predictors + 1)
    X = np.random.normal(loc = location, 
                         scale = scale, 
                         size = (n_samples, n_predictors))
    # Add a column of ones for an intercept.   
    X = np.column_stack((np.ones(n_samples), X))
    noise = np.random.randn(n_samples)
    # Compute output variable.
    y = np.dot(X, w_star.T) + noise
    
    return X, y


if __name__ == "__main__":

    path = "./"

    X, y = generate_data()
    viz.plot_data(X, y)
    plt.savefig(path + "1-generated-data.png")


    model = GradientDescentLinearRegression().fit(X, y)


    print(f"Gradient descent solution in {model.iterations} iterations: {model.w}.")

    w_lstsq = np.linalg.lstsq(X, y, rcond = None)[0]
    print(f"Least squares solutions: {w_lstsq}.")

    if (X.shape[1] == 2):

        viz.plot_data_and_fitted_line(X, y, model)
        plt.savefig(path + "2-generated-data-and-fitted-line.png")

        viz.plot_cost(model)
        plt.savefig(path + "3-cost-hist.png")
        
        viz.plot_surface(X, y, model, path, w_steps = 4)
        viz.animate_fitted_line(X, y, model, path, iterations = 11)



# # Experiment with two mathematically equivalent versions of AdaGrad.
# g = np.array([1, 5])
# eta = 1 # 0.01
# eps = 0 # 1e-6

# # V1: Element-wise.
# G = np.zeros(g.shape)
# G += g**2
# lr = eta / (np.sqrt(G + eps))
# step = lr * g
# print(f"g: {g}, G: {G}, learning rate: {lr}, step: {step}")

# # V2: Matrix.
# G = np.zeros((g.shape[0], g.shape[0]))
# G += np.outer(g,g)
# diag_G = np.diag(G)
# lr = np.diag(eta / np.sqrt(diag_G + eps))
# step = lr @ g
# print(f"g: {g}, G: {G}, learning rate: {lr}, step: {step}")
