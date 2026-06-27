# SPEC26_Unmixing
The codes for SPEC School 2026 Spectral Unmixing Group's analysis. We had been working on https://github.com/S-Ng/SPEC26_Unmixing repo and this is the final version of the code. 

Authors: Simon Ng (@S-NG), Ryoko Araki (@RY4GIT), Kara Lamoreux, Gwen Kirschke, Kelsey Huelsman 

## 1. Preparing hyperspectral data
### 1.1. NEON AOP hyperspectral imagery 
- Run `GEE_NEON_imagery2CSV.js` on Google Earth Engine code to pull NEON hyperspectral imagery into a CSV
  - `{dataset_name}_hyp_bands_only.csv` is the example input with each pixel as a row and all the bands as columns
  - `{dataset_name}_hyp_csv_{version}.csv` contains lat/lon information for the spectra information

- Use QGIS to sample "pure-spectra" sampled from NEON AOP imagery. Run `spectra_qgis_curate.py` and `spectra_qgis_plot.py` to curate the data 
  - Sampled spectra are located in the SPEC_School_2026 Google drive under `Team_Projects` > `Spectral_Unmixing` > `qgis_spectra_pt.csv` and `shade_pt_table.csv`

### 1.2. Field spectrometer hyperspectral data

- Run `spectra_field_organize.R` to format raw `.sig` files into CSV datatable format
  - Raw `.sig` files are stored in the SPEC_School_2026 Google drive under `Field_Data` > `Spectra_Unmixing` directory

- Run `spectra_field_join.py` to combine the field spec data with field annotation notes 
  - Field validation notes are located in the SPEC_School_2026 Google drive under `Team_Projects` > `Spectral_Unmixing` > `transect_validation_data`

- Run `specgtra_field_and_shades.py` to combine the field spec data, field annotations, and shade pixels sampled from NEON AOP data using QGIS (in Step 1.1)

## 2. Spectral unmixing
- Execute `NEON_multipixel_unmixing.jl` in Julia
  - It uses SpectralUnmixing julia library: https://github.com/emit-sds/SpectralUnmixing.jl/tree/dev
  - `Manifest.toml` and `Project.toml` define the Julia environment
  - `NEON_multipixel_unmixing_ra.jl` is Ryoko's version (includes some codes to concat lat/lon data automatically)
  - This code uses `neon_aop_refl,` the header file that converts FieldSpec 1nm spectral resolution to whatever NEON AOP has

## 3. Post-processing
- Run `CSV_to_raster_251203.ipynb` or `CSV_to_raster_251203.py`

