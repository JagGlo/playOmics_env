# playOmics Environment

This repository contains the necessary setup for running the [playOmics](https://github.com/JagGlo/playOmics) environment using Docker, including an Rmd script for testing purposes. It leverages Docker Compose for easy setup and execution.

## Prerequisites

Before you begin, ensure you have the following installed on your system:

- [Git](https://git-scm.com/downloads)
- [Docker Desktop](https://www.docker.com/products/docker-desktop)

## Getting Started

Follow these steps to get the environment up and running:

### 1. Clone the Repository

Clone this repository to your local machine:

```bash
git clone https://github.com/JagGlo/playOmics_env.git
cd playOmics_env
```

### 2. Docker Compose

Use Docker Compose to build and start the containers. This will set up the playOmics environment and any associated services.

```bash
docker-compose up -d
```

This command builds the necessary Docker images and starts the containers in detached mode.

### 3. Accessing RStudio

Once the container is running, RStudio Server can be accessed through your web browser.

- Open your web browser and navigate to `http://localhost:8888`.
- Log in using the RStudio credentials provided in the Docker Compose configuration (default: username `rstudio` and password `rstudio`).

### 4. Running Tests and Analysis

To verify the setup and functionality of the environment, you have two options in RStudio: running a test script through an R Markdown file or executing an analysis script. Follow the steps below for each method:

**Running the Test Scripts:**

1. Open the R Markdown file named `playOmics-example.Rmd` or `playOmics-THCA-example.Rmd` located in the project directory.
2. Run the script by knitting the document to ensure that the environment is correctly set up and functioning.

**Executing the Analysis Script:**

1. Locate the `run_experiment.R` script within the project directory.
2. Source the script in RStudio using the command `source("run_experiment.R")` to run the analysis.

These steps allow you to test the environment with an example workflow and execute a more comprehensive analysis, ensuring the playOmics setup meets your needs.

### 5. Feature Selection Simulations

The "Feature Selection Simulations" directory contains all the necessary components for running feature selection simulations within the playOmics environment. This includes scripts and functions for simulating and visualizing the performance of different feature selection strategies.

To run the feature selection simulations, navigate to the feature selection simulations folder.
You'll find three primary files:

1. `FS_simulation_functions.R`: Contains the core functions for performing feature selection simulations.
2. `FS_simulations.Rmd`: An R Markdown file where you can run the feature selection simulations. It provides a detailed workflow for running the simulations and generating results.
3. `FS_simulations.html`: The resulting HTML file with visualized outcomes from the simulations.
Run the simulations by opening and knitting the FS_simulations.Rmd file in RStudio. This document will walk you through various feature selection techniques, helping you choose the best methods for your analysis.

6. Ensemble Learning

The "Ensembling" directory is designed for performing ensemble learning, where multiple models' predictions are combined to maximize the overall prediction performance. Ensemble methods help reduce model variance and increase the accuracy of predictions by leveraging the strengths of different classifiers.

To perform ensemble learning, follow these steps:

1. Go to the ensembling folder in your project directory.
2. There are two key files here:

- `ensembling_helpers.R`: Contains helper functions for ensemble learning, such as methods for combining model predictions and calculating performance metrics.
- `ensembling_notebook.Rmd`: The R Markdown file where the ensemble learning process is laid out. This notebook will guide you through the steps of combining model predictions, running stepwise metrics calculations, and evaluating performance.

At the end of this ensemble learning pipeline, an ensemble model is created that selects a minimal set of models while maximizing prediction accuracy.

### 7. Comparing playOmics with Other Algorithms

To facilitate a comprehensive comparison of playOmics against other algorithms, with results that are showcased within our publication, a dedicated script is available. Follow the instructions below to access and utilize this comparison script:

1. Navigate to the `comparison` folder located within the project directory.
2. Here, you will find the script named `comparison_with_other_algos.Rmd`.
3. Open this R Markdown file in RStudio.
4. Knit the document to execute the comparisons as presented in the publication.

This script provides a detailed analysis and comparison, allowing you to see how playOmics performs relative to other algorithms in the field.

## Troubleshooting

If you encounter issues, consider the following:

- Ensure Docker Desktop is running correctly on your machine.
- Check if the port 8888 is already in use on your host machine. If so, modify the port mapping in `docker-compose.yml`.
- For Docker-related issues, refer to the [Docker documentation](https://docs.docker.com/).
