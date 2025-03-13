
# rBO-Citron
A Shiny dashboard application for interactive **data management** and **Bayesian optimization**.

(This app is part of the **WakaCitrus-Informatics** series. For other apps in the **WakaCitrus-Informatics** series, please visit:　https://github.com/yin4dev?tab=repositories)

Welcome to **rBO-Citron**! This open-source project is designed for managing datasets (via local SQLite or CSV uploads) and performing Bayesian optimization using Gaussian Process models. Contributions, improvements, and customizations are welcome—feel free to fork and submit pull requests.

---

## Table of Contents
1. [Overview](#overview)
2. [Key Features](#key-features)
3. [Dependencies](#dependencies)
4. [Installation and Setup](#installation-and-setup)
   - [Required Tools](#required-tools)
5. [Usage](#usage)
   - [Running the Application](#running-the-application)
   - [Feature Management](#feature-management)
6. [Configuration](#configuration)
7. [License](#license)
8. [Special Thanks](#special-thanks)

---

## Overview
**rBO-Citron** is a Shiny-based dashboard application built in R that provides a user-friendly interface for data management and Bayesian optimization. The app allows users to load, edit, and explore datasets from SQLite databases or CSV files, and then perform Bayesian optimization using Gaussian Process models with interactive visualizations and prediction capabilities.

---

## Key Features
- **Data Management**: Load and edit data from local SQLite databases, uploaded SQLite files, or CSV files. Features include searching, sorting, filtering, and direct in-app editing with options to add or delete rows and columns.
- **Bayesian Optimization**: Execute optimization using various acquisition functions (Expected Improvement, Probability of Improvement, Upper Confidence Bound) and generate candidate solutions based on Gaussian Process models.
- **Interactive Visualizations**: Explore correlations, predictions, and acquisition function landscapes through dynamic plots.
- **Prediction Module**: Supports both manual input and batch predictions via CSV upload, providing immediate visual and tabular feedback.

---

## Dependencies
- **Programming Language/Framework**: R, Shiny
- **Libraries/Packages**:  
  `shiny`, `shinydashboard`, `shinyBS`, `DT`, `data.table`, `GPfit`, `ggplot2`, `shinythemes`, `shinyWidgets`, `ggcorrplot`, `shinyjs`,` rhandsontable`, `RSQLite`, `DBI`
- **System Tools**:  
  - [R](https://www.r-project.org/)
  - [RStudio](https://www.rstudio.com/)
  
---

## Installation and Setup

### Required Tools
1. **Clone or Download the Repository**  
   Clone this repository or download the source files

2. **Install External Tools/Services**  
   Make sure you have R installed along with your preferred IDE (e.g., RStudio). No additional external services are required for running the Shiny app.

3. **Install Required Packages**  
   Install the necessary R packages by running:
   
   ```r
   install.packages(c("shiny", "shinydashboard", "shinyBS", "DT", "data.table", "GPfit", 
                      "ggplot2", "shinythemes", "shinyWidgets", "ggcorrplot", "shinyjs", 
                      "rhandsontable", "RSQLite", "DBI"))
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
  - Use the interactive table to search, sort, filter, and edit data.
  - Save changes directly to the database or export data as CSV/SQLite.
- **Bayesian Optimization**:  
  - Choose the objective and explanatory variables.
  - Set constraints and adjust hyperparameters.
  - Run the optimization to view candidate solutions and visualizations.
- **Prediction Module**:  
  - Use manual input to test specific values or upload a CSV for batch predictions.
  - Review prediction results directly in the app or download them as CSV.

---

## License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0).

Copyright (C) 2025 Hongrong Yin

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

---

## Acknowledgments

Special thanks to my loving Aimi Yago for her continuous support, inspiration, and contributions to this project's success! 🎉
