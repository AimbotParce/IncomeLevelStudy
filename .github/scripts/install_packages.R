required_packages <- c(
    "crayon", "car", "FactoMineR", "chemometrics", "corrplot", "readr",
    "RColorBrewer", "PerformanceAnalytics", "mice", "dplyr", "rmarkdown"
)

install_if_missing <- function(packages) {
    for (pkg in packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            install.packages(pkg, repos = "https://cloud.r-project.org/")
        }
    }
}

install_if_missing(required_packages)
