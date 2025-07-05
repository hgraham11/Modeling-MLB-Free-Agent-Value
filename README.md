# MLB Free Agent Contract Prediction (2025–2026)

This project forecasts MLB free agent contract terms using machine learning and statistical modeling in R. It’s divided into two parts:

- **Part 1:** Linear regression models trained on clustered player groups (./2026%20MLB%20Free%20Agent%20Predictions%20Pt.%201-3.pdf)
- **Part 2:** Random forest models for enhanced predictive performance

## Objective

Predict:
- Average Annual Value (AAV)
- Contract Length (Years)

Using:
- Player performance data from 2022–2025
- K-means clustering
- Principal Component Analysis (PCA)
- Linear Regression & Random Forest models

## Methods

- **Clustering:** Players grouped by similarity (e.g., “Very Good Hitters”, “Durable Starters”)
- **PCA:** Dimensionality reduction for visualizing and modeling
- **Modeling:** Regression and Random Forests trained separately on hitters and pitchers

## Files

- `2026 MLB Free Agent Predictions Pt. 1-3.pdf`: Report covering clustering, PCA, and linear regression
- `2026 MLB Free Agent Predictions Pt. 2-2.pdf`: Report using Random Forest models
- `BUS 465 Final Project Presentation.pdf`: Slide deck summarizing project
- `FULLCOMPLETEPROJECT.RData`: Full R workspace with all models and data objects

## Tools Used

- **R** (v4.4.2)
- Packages: `dplyr`, `ggplot2`, `randomForest`, `cluster`, `factoextra`, `caret`

## Future Improvements

- Test ensemble models like XGBoost
- Integrate injury history, team fit, and market context
- Improve accuracy on elite player predictions

## Contact

Project by **Hunter Graham**
[LinkedIn](https://www.linkedin.com/in/huntergraham1/) 
