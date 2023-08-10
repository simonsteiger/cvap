# Clinicians' Visualisation and Analysis Platform (CVAP)

The CVAP is an R Shiny app to make selected statistics and visualisations from the [Swedish Rheumatology Quality Register](www.srq.nu) accessible to clinicians and the wider public.

# Running the app locally
- First, install dependencies with `renv::restore()`
- Adjust local directory to read SRQ data in `app/logic/data/PATH.R`
- Run `rhino::app()`

# Maintenance

This goal of this section is to allow the reader to make simple changes without breaking the app. Let's begin by having a look at how the code is organised.

## The `app` folder

All the relevant code is in the `/app` directory.

```
└── app
    ├── js                  # Directory for JS code, not in use
    │
    ├── logic               # Directory for non-reactive functions
    │   ├── aux_server          # Functions used in servers
    │   │   ├── output              # Plots, HTML, ...
    │   │   ├── misc                # Observers, errors
    │   │   └── wrangle             # Data cleaning
    │   ├── aux_ui              # Functions used in UIs
    │   │   ├── container           # Cards, layouts, ...
    │   │   └── input               # Input widgets
    │   ├── data                # Data wrangling and storage
    │   │   ├── gadm                # Geojson data for map
    │   │   ├── srq                 # Wrangling scripts for SRQ data
    │   │   └── test                # Made-up data for tests
    │   ├── srqlib              # Imported functions from srqlib
    │   └── swissknife          # Imported functions from swissknife
    │
    ├── static              # Directory for static elements (logos, min.css...)
    │   ├── css                 # Compressed css, never modify
    │   └── js                  # Compressed js, not in use
    │
    ├── styles              # Directory for SASS code
    │
    ├── view                # Directory for Shiny modules
    │   ├── output              # Modules to create output elements
    │   ├── page                # Modules to create individual VAPs
    │   └── wrangle             # Modules to wrangle data interactively
    └── main.R              # Call subpage modules
```

## The `tests` folder

This folder contains automated tests to make sure that all functions run as intended. Always run these tests after making a change. They currently do not catch _every_ error, but still cover important parts of the code. If a test fails, adress the error before publishing a new version of the app.