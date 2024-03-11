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

### 4. Running the Test Script

In RStudio, you can run the provided R Markdown file named `playOmics-example.Rmd` script to test the environment. Follow these steps:

1. Open the `playOmics-example.Rmd` script located in the project directory.
2. Run the script to ensure that the environment is correctly set up and functioning.

## Troubleshooting

If you encounter issues, consider the following:

- Ensure Docker Desktop is running correctly on your machine.
- Check if the port 8888 is already in use on your host machine. If so, modify the port mapping in `docker-compose.yml`.
- For Docker-related issues, refer to the [Docker documentation](https://docs.docker.com/).
