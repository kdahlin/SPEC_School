# %% 
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.colors import Normalize, to_rgb

# %%
data_dir = r"G:/Shared drives/Ryoko and Hilary/SPECschool/Spectra_csv"
spectra_file = "qgis_spectra_pt_formatted.csv"

# %% Read in spectra data
df = pd.read_csv(os.path.join(data_dir, spectra_file))
# %%
df.head()

# %%

# %% Plot spectra by subclass
wl_cols = [c for c in df.columns if str(c).replace(".", "").isdigit()]
wavelengths = np.array([float(c) for c in wl_cols])

fig, axes = plt.subplots(1, 3, figsize=(13, 4), sharey=True)
sub_classes = ["understory", "dead_tree", "canopy"]
title_label = ["Understory", "Dead Tree", "Canopy"]
# Define base colors for each sub_class for legends
sub_class_colors = {"understory": "#1f77b4", "dead_tree": "#ff7f0e", "canopy": "#2ca02c"}

for i, (ax, sub_class) in enumerate(zip(axes, sub_classes)):
    subset = df[df["sub_class"] == sub_class].copy()
    
    if not subset.empty:

        for idx, row in subset.iterrows():
            color = sub_class_colors[sub_class]
            y = row[wl_cols].to_numpy() / 10000
            mask = y >= 0
            line, = ax.plot(wavelengths, np.where(mask, y, np.nan),
                            alpha=0.5, linewidth=1, color=color)

    ax.set_title(rf"{title_label[i]} ($n$={len(subset)})", fontsize=14)
    ax.set_xlabel("Wavelength (nm)", fontsize=14)
    if i == 0:
        ax.set_ylabel("Reflectance", fontsize=14)
    else:
        ax.set_ylabel("")

fig.tight_layout()
ax.set_ylim(-0.005, 1.1)
plt.savefig(os.path.join(data_dir, "qgis_spectra_pt.png"), dpi=300)
plt.show()
# %%# %% Plot the shade spectra (dark gray gradations)
fig, ax = plt.subplots(figsize=(4, 2.5))
df_shade = df[df["sub_class"] == "shade"].copy()
# Mask values less than 0

for _, row in df_shade.iterrows():
    _y = row[wl_cols].to_numpy() / 10000
    mask = _y >= 0
    y = np.where(mask, _y, np.nan)  # Replace masked values with np.nan
    line, = ax.plot(wavelengths, y,
                    alpha=0.5, linewidth=1, color="darkgray")

ax.set_xlabel("Wavelength (nm)", fontsize=14)
ax.set_ylabel("Reflectance", fontsize=14)
ax.set_ylim(-0.005, 1.1)
fig.tight_layout()
plt.savefig(os.path.join(data_dir, "qgis_spectra_pt_shade.png"), dpi=300)
plt.show()

# %%
