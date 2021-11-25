import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap

def plot_decision_boundary(X, y, model, title, filename):
    
    # Generate a grid of points and classify them ----------------------------
    
    # Create a grid of points.
    res = 0.01 # Resolution of the decision boundary.
    xs1 = np.arange(0, 1 + res, res)
    xs2 = np.arange(0, 1 + res, res)
    xs1, xs2 = np.meshgrid(xs1, xs2)
    X_grid = np.column_stack((xs1.flatten(), xs2.flatten()))
    # Predict a class for each point of the grid.
    pred_grid = model.predict(X_grid)
    
    # Make a plot ------------------------------------------------------------
    
    fig, ax = plt.subplots(figsize = (4, 4), dpi = 300)
    ax.set(xlim = [0,1], 
           ylim = [0,1], 
           xlabel = '$x_1$', 
           ylabel = '$x_2$', 
           title = title)
    # Plot data points.
    scatter = ax.scatter(X[:,0], X[:,1], c = y, cmap='bwr') # Blue-white-red colormap.
    ax.legend(*scatter.legend_elements(), loc = 'upper left')
    # Plot class regions.
    cmap = LinearSegmentedColormap.from_list('cmap', ['skyblue', 'lightsalmon'])
    ax.imshow(pred_grid.reshape(xs1.shape[0],-1),
               cmap = cmap,
               origin = 'lower',
               extent = [0,1,0,1],
               aspect = 'auto', alpha = 0.6)
    fig.savefig(filename)
    