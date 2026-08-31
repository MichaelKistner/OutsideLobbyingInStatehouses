# Outside Lobbying in Statehouses

Data, code, and other materials for *Outside Lobbying in Statehouses*.

---

## Project Structure

This project is split across **two locations** to keep large data files out of Git and avoid sync conflicts.

### GitHub Repository (version-controlled)

```
OutsideLobbyingInStatehouses/
├── Code/                          # R scripts (all analysis code)
│   └── Comparing Illinois Slip and CHORUS Data.R
├── Figures and Tables/            # Generated plots and tables
├── Initial Data/                  # ← gitignored, see Dropbox below
├── Processed Data/                # ← gitignored, see Dropbox below
├── .gitignore
├── Outside Lobbying in Statehouses.Rproj
├── README.md                      # ← this file
├── renv.lock                      # Package versions (once renv is set up)
└── renv/
    └── activate.R                 # renv bootstrap script
```

### Shared Dropbox Folder (not in Git)

```
Outside Lobbying in Statehouses (Dropbox)/
├── Initial Data/
│   ├── CHORUS/
│   │   ├── bills.parquet
│   │   ├── block_assignments.parquet
│   │   ├── clients.parquet
│   │   ├── positions.parquet
│   │   ├── CHORUS CODEBOOK.pdf
│   │   └── CHORUS README.pdf
│   └── Witness Slips/
│       ├── Slip Data (Original Download).csv
│       ├── Witness Slips Dataframe (Cleaned).Rda
│       ├── Bill Dataframe.Rda
│       ├── Ideal Points Dataframe.Rda
│       ├── Witness Dataframe.Rda
│       ├── Witness Slips CODEBOOK.docx
│       └── Witness Slips README.txt
├── Processed Data/
│   ├── Sample of Arizona Position Data.csv
│   └── Sample of Illinois Position Data.csv
├── Literature/
│   └── (reference PDFs)
└── Meetings and Feedback/
    └── (meeting notes)
```

---

## Getting Started

### 1. Clone the repository

```bash
git clone https://github.com/MichaelKistner/OutsideLobbyingInStatehouses.git
```

### 2. Set up Dropbox data access

The data files are shared via a Dropbox folder called **Outside Lobbying in Statehouses**. Make sure this folder is synced to your machine (not "online only").

Copy or symlink the data into the cloned repo so your code can find it:

**Option A — Symlink (recommended, no duplication):**

```bash
# macOS / Linux
cd OutsideLobbyingInStatehouses
ln -s "/path/to/Dropbox/Outside Lobbying in Statehouses/Initial Data" "Initial Data"
ln -s "/path/to/Dropbox/Outside Lobbying in Statehouses/Processed Data" "Processed Data"

# Windows (run as Administrator)
cd OutsideLobbyingInStatehouses
mklink /D "Initial Data" "C:\Users\YOU\Dropbox\Outside Lobbying in Statehouses\Initial Data"
mklink /D "Processed Data" "C:\Users\YOU\Dropbox\Outside Lobbying in Statehouses\Processed Data"
```

**Option B — Copy the folders directly** into the repo root. They are gitignored, so they won't be committed.

### 3. Install R packages with renv

On first setup:

```r
# renv will bootstrap itself when you open the .Rproj
# Then restore the locked package versions:
renv::restore()
```

When you add a new package:

```r
install.packages("new_package")
renv::snapshot()    # updates renv.lock — commit this change
```

### 4. Open the RStudio project

Always open the project via `Outside Lobbying in Statehouses.Rproj`. This sets the working directory to the repo root so that all file paths work consistently across machines.

In your R scripts, reference data files with paths relative to the project root:

```r
library(arrow)

# These paths work for everyone as long as step 2 is done
positions <- read_parquet("Initial Data/CHORUS/positions.parquet")
slips     <- load("Initial Data/Witness Slips/Witness Slips Dataframe (Cleaned).Rda")
```

---

## Collaboration Workflow

### Branches

Create a branch for each piece of work rather than committing directly to `main`:

```bash
git checkout -b mk/group-mobilization-plot
# ... make changes, commit ...
git push origin mk/group-mobilization-plot
```

Then open a pull request on GitHub for review before merging.

### Naming convention for branches

Use your initials and a short description: `mk/descriptive-name`, `bs/model-comparison`, etc.

### What goes where

| Content | Location | Why |
|---|---|---|
| R scripts | GitHub | Version control + code review |
| `.Rproj`, `.gitignore`, `README` | GitHub | Project config |
| `renv.lock` | GitHub | Reproducible package versions |
| Raw / processed data | Dropbox | Too large for Git |
| Literature PDFs | Dropbox | Binary files, no diffing benefit |
| Meeting notes | Dropbox | Reference material |
| Generated figures | GitHub (optional) | Convenient for PR review |

---

## Data Sources

### Illinois Witness Slips

Public testimony records from the Illinois General Assembly. See `Initial Data/Witness Slips/Witness Slips README.txt` and the accompanying codebook for variable definitions and download details.

### CHORUS (Comprehensive Organized Reporting of Unified Statehouse data)

State-level lobbying position data. See `Initial Data/CHORUS/CHORUS README.pdf` and the codebook for schema documentation.

---

## Contributors

- Michael Kistner
- Geoff Lorenz
- Boris Shor
