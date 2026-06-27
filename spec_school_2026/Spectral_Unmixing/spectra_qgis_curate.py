# %% 
import os
import pandas as pd
import numpy as np
import re

# %%
data_dir = "G:/Shared drives/Ryoko and Hilary/SPECschool/Spectra_csv"
spectra_file = "qgis_spectra_pt.csv"

# %%
df_spectra = pd.read_csv(os.path.join(data_dir, spectra_file))
df_spectra.head()

# %% Read in wavelength and bands correspondence
wl_file = "neon_aop_refl.hdr"
with open(os.path.join(data_dir, wl_file), 'r') as file:
    data = file.read()

# Find the wavelength array using regex
wavelength_match = re.search(r"wavelength\s*=\s*{([^}]*)}", data, re.IGNORECASE|re.DOTALL)
if wavelength_match:
    wavelengths_str = wavelength_match.group(1)
    wavelengths = [float(w.strip()) for w in wavelengths_str.split(",") if w.strip()]
else:
    wavelengths = []

print(len(wavelengths), "wavelengths:", wavelengths[:3], "...")
# %%
# %% for shade spectra df, rename SAMPLE_{band_n} columns to wavelength (nm); band 1 -> wavelengths[0]
band_rename = {
    col: wavelengths[int(col.split("_")[1]) - 1]
    for col in df_spectra.columns
    if re.fullmatch(r"band_\d+", col)
}
df_spectra.rename(columns=band_rename, inplace=True)

df_spectra.head()
# %% Add column items for shade spectra df
sub_class_dict = {
    "s": "shade",
    "c": "canopy",
    "u": "understory",
    "d": "dead_tree",
}
df_spectra["sub_class"] = df_spectra["Class"].map(sub_class_dict)
df_spectra["ID"] = np.arange(1, 1 + len(df_spectra))
df_spectra["file_ID"] = df_spectra["ID"]
df_spectra["sample_name"] = df_spectra["ID"].apply(lambda i: f"qgispt_{i}")
df_spectra.head()

# %%
print(len(df_spectra))
# %% Order meta columns first
first_cols = ['sample_name', 'ID', 'file_ID', 'sub_class']
# meta_cols = [c for c in df_spectra.columns if c not in wl_cols]
other_cols = [c for c in df_spectra.columns if c not in first_cols]
df_spectra = df_spectra[first_cols + other_cols]
df_spectra.head()

# %% Save the dataframe
out_file = "qgis_spectra_pt_formatted.csv"
df_spectra.to_csv(os.path.join(data_dir, out_file), index=False)
print(f"Saved {len(df_spectra)} rows")
# %%