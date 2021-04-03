
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm # Color legend for the surface plot.
from celluloid import Camera # Animation.


N_PREDICTORS = 1
N_OBSERVATIONS = 5
N_ITERATIONS = 10000
TOLERANCE = 1e-5
# Convergence: 0.001 (slow), 0.005 (fast), 0.01 (jumps).
# Divergence: 0.02
LEARNING_RATE = 0.001
X_LOC, X_SCALE = 1, 3
np.random.seed(6)


# True weights, +1 for the intercept.
w_star = np.random.randn(N_PREDICTORS + 1)
# N(X_LOC,X_SCALE**2) random X.
X = np.random.normal(loc = X_LOC, scale = X_SCALE, size = (N_OBSERVATIONS, N_PREDICTORS))
# Add a column of ones for an intercept.
X = np.column_stack((np.ones(N_OBSERVATIONS), X))
# N(0,1) random noise.
noise = np.random.randn(N_OBSERVATIONS)
y = np.dot(X, w_star.T) + noise

w_lstsq = np.linalg.lstsq(X, y, rcond = None)[0]
print(f"True parameters: {w_star}\nLeast squares solution: {w_lstsq}")


# Initialization of weights.
w = np.zeros(N_PREDICTORS + 1)

# X
# y
# w


def predict(X, w):
    return np.dot(X, w.T)

def cost(y, X, w):
    y_pred = predict(X, w)
    loss = (y - y_pred)**2
    return np.mean(loss)

def grad(y, X, w):
    y_pred = predict(X, w)
    # Partial derivative w.r.t. an intercept.
    d_intercept = -2*sum(y - y_pred)
    # Partial derivatives w.r.t. X predictors.
    d_x = -2*sum(X[:,1:] * (y - y_pred).reshape(-1,1))
    # Gradient
    g = np.append(np.array(d_intercept), d_x)
    
    return g


cost_lst = [cost(y, X, w)]
w_lst = [w]

for iter in range(N_ITERATIONS):
      
    g = grad(y, X, w)
    
    w = w - LEARNING_RATE * g
    w_lst.append(w)
    
    J = cost(y, X, w)
    cost_lst.append(J)
    
    if np.linalg.norm(w - w_lstsq) < TOLERANCE:
        print(f"Tolerance reached on iteration {iter}.")
        break

print(f"Solution for weights: {w}")



# Plot if the number of predictors, including intercept, is 2.

if (N_PREDICTORS + 1 == 2):

    
    # 2D plot of observations and a fitted line.
    fig, ax = plt.subplots()
    ax.scatter(X[:,1], y, color = "blue", label='Data')
    ax.plot(X[:,1], predict(X, w), color = "red", label='Fitted line')
    plt.legend()
    
    
    # 2D plot of the cost on each iteration.
    fig, ax = plt.subplots()
    ax.plot(cost_lst, color = "blue")
    
    
    # 3D plot of the cost function -------------------------------------------
    
    # Source: https://matplotlib.org/2.0.2/mpl_toolkits/mplot3d/tutorial.html        
    # Plot quality.
    # Source: https://stackoverflow.com/questions/332289/how-do-you-change-the-size-of-figures-drawn-with-matplotlib
    # and https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.figure.html
    fig = plt.figure(figsize = (10, 5), dpi = 300)   
    ax = fig.gca(projection = '3d')
    ax.set_xlabel('Intercept')
    ax.set_ylabel('x')
    ax.set_zlabel('Cost')
    ax.view_init(30, -60+50+30) # Rotate the coordinate system.
    
    # Plot parameters depending on the learning rate.
    if (LEARNING_RATE == 0.02):
        # Divergence.
        x_lo, x_hi = -20, 20
        y_lo, y_hi = -5, 12
        n_steps = 4
        arrow_ratio = 0.001
    else:
        # Convergence.
        x_lo, x_hi = -2, 2
        y_lo, y_hi = -0.05, 0.84*2
        n_steps = len(w_lst)
        arrow_ratio = 0.05

    
    x_space = np.linspace(x_lo, x_hi, 100)
    y_space = np.linspace(y_lo, y_hi, 100)
    x_mesh, y_mesh = np.meshgrid(x_space, y_space)
    z = np.zeros(x_mesh.shape)
    for i in range(len(x_space)):
        for j in range(len(y_space)):
            z[i,j] = cost(y, X, np.array([x_mesh[i,j], y_mesh[i,j]]))
    
    # Plot the surface.
    surf = ax.plot_surface(x_mesh, y_mesh, z,
                           cmap = cm.coolwarm, 
                           linewidth = 0, 
                           antialiased = True, 
                           vmin = 0, 
                           rcount = 300,
                           alpha = 0.6)
    
    # Add a color bar which maps values to colors.
    fig.colorbar(surf, shrink=0.5, aspect=5)

    
    # Add points for cost function value on each iteration -------------------

    # Source: https://matplotlib.org/stable/tutorials/toolkits/mplot3d.html#scatter-plots    
    x_space = np.array(w_lst)[:, 0]
    y_space = np.array(w_lst)[:, 1]
    z = np.array(cost_lst)
        
    ax.scatter(x_space[:n_steps], 
               y_space[:n_steps], 
               z[:n_steps], 
               s = 10,
               color = "red", 
               alpha = 1)
    
    
    x_arrow_end = np.diff(x_space[:n_steps])
    y_arrow_end = np.diff(y_space[:n_steps])
    z_arrow_end = np.diff(z[:n_steps])
    
    
    # Add arrows -------------------------------------------------------------
    
    # Source: https://matplotlib.org/stable/tutorials/toolkits/mplot3d.html#quiver   
    ax.quiver(x_space[:(n_steps-1)],
              y_space[:(n_steps-1)],
              z[:(n_steps-1)],
              x_arrow_end,
              y_arrow_end,
              z_arrow_end,
              arrow_length_ratio = arrow_ratio,
              color = "red", 
              alpha = 1)
    
    plt.show()

    
    # Animation of the fitted line -------------------------------------------
    
    # Source: https://github.com/jwkvam/celluloid
    
    # fig, ax = plt.subplots(figsize=(5, 4), dpi=100)
    # camera = Camera(fig)
    # for i in range(30):
    #     ax.scatter(X[:,1], y, color = "blue")
    #     y_pred = predict(X, w_lst[i])   
    #     t = ax.plot(X[:,1], y_pred, color = "red")
    #     plt.legend(t, [f"Iteration {i}\nLine equation: {w_lst[i][0]:.3f}+{w_lst[i][1]:.3f}x"], loc = 'upper left')
        
    #     camera.snap()   
        
    # animation = camera.animate()
    # animation.save(f"learning_rate_{LEARNING_RATE}.gif", writer = 'imagemagick')

