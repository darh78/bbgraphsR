# bbgraphsR (Development Version)

This is a work-in-progress R package for scraping, processing, analyzing and visualizing MLB data — including tools for pulling birthplace stats by country from Baseball Reference.

---

## 🚀 Getting Started (for Collaborators)

To contribute or test the package locally, follow these steps:

---

### 🔧 1. Clone the repository into a new RStudio project

1. In RStudio, go to `File > New Project > Version Control > Git`.
2. Enter the repository URL:
https://github.com/darh78/bbgraphsR.git
3. Choose a local folder where the project will be saved.
4. Click **Create Project** — this will open a new RStudio session with the project.

---

### 📦 2. Install development dependencies

You’ll need `devtools`, which makes it easy to load the package without installing it:

```r
install.packages("devtools") 
```

### ▶️ 3. Load the package using devtools::load_all()

From within the project directory (or inside RStudio):

```r
devtools::load_all()
```

This simulates loading the package as if it were installed, without actually installing it. You can now call any exported functions.

### 🧪 Example usage

```r
# Load the package code
devtools::load_all()

# Run a function
scrape_country_birth_players("Dominican Republic")
```

### 📌 Notes
	•	This package is not yet available via CRAN or installable via library().
	•	Once stable, installation instructions will be updated.

### 📬 Contact

Maintainer: Daniel Hernández

GitHub: @darh78
