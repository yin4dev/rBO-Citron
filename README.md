
---

# rBO-Citron v0.2.0
A Shiny dashboard application for interactive **data management** and **Bayesian optimization**.

*(This app is part of the **WakaCitrus-Informatics** series. For other apps in the series, please visit [my GitHub repositories](https://github.com/yin4dev?tab=repositories).)*

Welcome to **rBO-Citron v0.2**! This open‐source project is designed for managing datasets (via local SQLite or CSV uploads) and performing Bayesian optimization using surrogate models with advanced features. The application now features flexible surrogate model selection, robust one-hot encoding for categorical variables, advanced constraint management, enhanced interactive visualizations, and flexible prediction modes. Contributions, improvements, and customizations are welcome—feel free to fork and submit pull requests.

---

## Table of Contents
1. [Overview](#overview)
2. [Updated Points](#updated-points)
3. [Key Features](#key-features)
4. [Dependencies](#dependencies)
5. [Installation and Setup](#installation-and-setup)
   - [Required Tools](#required-tools)
6. [Usage](#usage)
   - [Running the Application](#running-the-application)
   - [Feature Management](#feature-management)
7. [Configuration](#configuration)
8. [License](#license)
9. [Special Thanks](#special-thanks)

---

## Overview
**rBO-Citron v0.2** is a Shiny-based dashboard application built in R that provides a user-friendly interface for both data management and Bayesian optimization. Users can load, edit, and explore datasets from local SQLite databases or CSV files. After preparing their data, users can execute Bayesian optimization with selectable surrogate models and acquisition functions—all with interactive visualizations and prediction capabilities.

*Release Date: 2025-03-14*

---

## Updated Points
- **Surrogate Model Selection**: **Gaussian Process** (default) or **Random Forest**
- **One-Hot Encoding** for Categorical Variables
- Several UI improvements


---

## Key Features
- **Data Management**:  
  - Load data from local SQLite databases, uploaded SQLite files, or CSV files.
  - Edit data interactively with features such as inline editing, adding/deleting rows and columns, and dynamic searching, sorting, and filtering.
  - Save changes directly to the database or export data as CSV/SQLite files.

- **Bayesian Optimization**:  
  - **Surrogate Model Selection**: Choose your surrogate model—either **Gaussian Process** (default) or **Random Forest**—to best suit your optimization needs.
  - **One-Hot Encoding for Categorical Variables**: The app automatically performs one-hot transformations on categorical explanatory variables so they are handled appropriately during model training and prediction.
  - Configure advanced optimization settings including various acquisition functions (Expected Improvement, Probability of Improvement, Upper Confidence Bound) and hyperparameters.
  - Manage optimization constraints interactively with a dedicated constraint editor.
  
- **Interactive Visualizations**:  
  - Explore correlations with dynamically generated correlation plots.
  - Visualize prediction landscapes, contour plots, and acquisition function evaluations.
  
- **Prediction Module**:  
  - **Manual Input**: Enter custom values for immediate predictions.
  - **CSV Batch Predictions**: Upload CSV files for automated, batch predictions with downloadable results.

---

## Dependencies
- **Programming Language/Framework**:  
  - R, Shiny

- **Key Libraries/Packages**:  
  - `shiny`, `shinydashboard`, `shinyBS`, `DT`, `data.table`, `GPfit`, `ggplot2`, `shinythemes`, `shinyWidgets`, `ggcorrplot`, `shinyjs`, `rhandsontable`, `RSQLite`, `DBI`, `randomForest`

- **System Tools**:  
  - [R](https://www.r-project.org/)  
  - [RStudio](https://www.rstudio.com/)

---

## Installation and Setup

### Required Tools
1. **Clone or Download the Repository**  
   Clone this repository or download the source files to your local machine.

2. **Install External Tools/Services**  
   Ensure that you have R installed along with your preferred IDE (e.g., RStudio). No additional external services are required for running the Shiny app.

3. **Install Required Packages**  
   Install the necessary R packages by running:
   
   ```r
   install.packages(c("shiny", "shinydashboard", "shinyBS", "DT", "data.table", "GPfit", 
                      "ggplot2", "shinythemes", "shinyWidgets", "ggcorrplot", "shinyjs", 
                      "rhandsontable", "RSQLite", "DBI", "randomForest"))
   ```

---

## Usage

### Running the Application
Launch the application using the following command in R or RStudio:

```r
library(shiny)
runApp("path_to_your_app_directory")
```

Replace `"path_to_your_app_directory"` with the actual path where the app files are located.

### Feature Management
- **Data Management**:  
  - Select your data source (Local SQLite, Uploaded SQLite, or CSV).
  - Interactively search, sort, filter, and edit your dataset.
  - Save your changes directly to the database or export data as CSV/SQLite.

- **Bayesian Optimization**:  
  - Choose your objective and explanatory variables.
  - Define and apply optimization constraints using the interactive constraint editor.
  - Adjust hyperparameters and select your preferred acquisition function.
  - Run the optimization to obtain candidate solutions and explore the results with dynamic plots.

- **Prediction Module**:  
  - **Manual Mode**: Input custom values using the interactive table and view immediate predictions.
  - **CSV Upload Mode**: Upload a CSV file to perform batch predictions and download the resulting output.

---

## Configuration
The app settings (e.g., optimization target, number of candidates, surrogate model type, hyperparameters) can be adjusted directly within the interface. Further customization can be done by modifying the source code as needed.

---

## License

This project is licensed under the [GNU General Public License v3.0 (GPL-3.0)](https://www.gnu.org/licenses/gpl-3.0.html).

Copyright (C) 2025 Hongrong Yin

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

---

## Special Thanks
Special thanks to my loving Aimi Yago for her continuous support, inspiration, and contributions to this project's success! 🎉

---
