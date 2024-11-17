# MInfer

**MInfer** is an R package designed for analyzing metabolomics data. It provides tools for data preparation, covariance matrix generation, Jacobian matrix computation, and visualization of metabolite interaction networks.

---

## Installation

You can install **MInfer** directly from GitHub using the following commands:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install MInfer from GitHub
devtools::install_github("cellbiomaths/MInfer")

# Once installed, load the package:
library(MInfer)
```
Example Usage
Below is a complete example demonstrating how to use MInfer.

Step 1: Load Example Data
The package includes example data for testing:

```r
# Load example data
data(example_data)
```

Step 2: Prepare Data
Prepare the data by selecting specific conditions (e.g., 6C and 16C):

```r
# Prepare data for two conditions
data_6C <- prepare_data(met_input, 6)
data_16C <- prepare_data(met_input, 16)
```

Step 3: Generate Covariance Matrices
Generate covariance matrices for the prepared data. The parameter num_tp specifies the number of time points.

```r
# Generate covariance matrices
cov_6C <- generate_covariance(data_6C, num_tp = 1)
cov_16C <- generate_covariance(data_16C, num_tp = 1)
```

Step 4: Calculate Jacobian Matrices
Compute the Jacobian matrices to analyze metabolite interactions.

```r
# Calculate Jacobian matrices
jacobian_6C <- calculate_jacobian(cov_6C[[1]], interactions_fin, icount = 15)
jacobian_16C <- calculate_jacobian(cov_16C[[1]], interactions_fin, icount = 15)
```

Step 5: Visualize Results
Visualize the Jacobian matrices using a heatmap or 3D plot:

```r
# Heatmap visualization
visualize_heatmap(jacobian_6C$J, title = "Jacobian Matrix - 6C")
# 3D visualization
visualize_3d(jacobian_16C$J)
```
