# MInfer / R

**MInfer** is an R package designed for analyzing metabolomics data. It provides tools for data preparation, covariance matrix generation, Jacobian matrix computation, and visualization of metabolite interaction networks.

MInfer represents a novel computational framework that effectively facilitates the transition from MetaboAnalyst to Jacobian analysis, enhancing the exploration of metabolomic networks. This integration allows researchers to harness the strengths of both platforms, enabling a comprehensive analysis that captures the dynamic interactions between metabolites. 

---

## Installation

You can install **MInfer** directly from GitHub using the following commands:

```r
# Install devtools if not already installed
# install.packages("devtools")

# Install MInfer from GitHub
devtools::install_github("https://github.com/cellbiomaths/MInfer")

# Once installed, load the package:
library(MInfer)
```

Below is a example demonstrating how to use MInfer:
### Example for metabolite interaction network (MIN)

1. Define Input Metabolite IDs for Testing
The first step is to define a list of metabolite IDs (KEGG IG) that you wish to analyze.
These IDs should be present in the rows and columns of the dataset.

```r
# Define metabolite IDs
input_ids <- c('C00042', 'C00149', 'C00036')
# Run the minfer function to analyze the data
results <- minfer(input_ids)
print(results)
```
### Example for calculation of Jacobian Matrices
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
