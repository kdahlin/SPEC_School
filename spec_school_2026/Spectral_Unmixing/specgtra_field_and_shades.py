# %% 
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
from matplotlib import cm
from matplotlib.colors import Normalize
import re
# %% File paths
data_dir = "G:/Shared drives/Ryoko and Hilary/SPECschool/Spectra_csv"

# %% Read in field spectra data
data_file = "MLBS26_leafspectra_with_attrs_cleaner.csv"
df = pd.read_csv(os.path.join(data_dir, data_file))
df.head()

# %% Read in shade spectra
shades_file = "shade_pt_table.csv"
df_shades = pd.read_csv(os.path.join(data_dir, shades_file))
df_shades.head()

# %% Wavelength and bands correspondence
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

# %% for shade spectra df, rename SAMPLE_{band_n} columns to wavelength (nm); band 1 -> wavelengths[0]
band_rename = {
    col: wavelengths[int(col.split("_")[1]) - 1]
    for col in df_shades.columns
    if re.fullmatch(r"SAMPLE_\d+", col)
}
df_shades.rename(columns=band_rename, inplace=True)

if "SAMPLE_ID" in df_shades.columns:
    df_shades.rename(columns={"SAMPLE_ID": "ID"}, inplace=True)

df_shades.head()
# %% Add column items for shade spectra df
df_shades["cleaner_groupings"] = "yes"
df_shades["sub_class"] = "shade"
df_shades["ID"] = np.arange(76, 76 + len(df_shades))
df_shades["file_ID"] = df_shades["ID"]
df_shades["sample_name"] = df_shades["ID"].apply(lambda i: f"shade_{i}")

# %% Interpolate shade NEON AOP spectra onto field spectra wavelength grid
def spectral_cols(frame):
    return [c for c in frame.columns if str(c).replace(".", "").isdigit()]

wl_cols_df = spectral_cols(df)
x2 = np.array([float(c) for c in wl_cols_df])

wl_cols_shades = spectral_cols(df_shades)
x1 = np.array([float(c) for c in wl_cols_shades])
sort_x1 = np.argsort(x1)
x1_sorted = x1[sort_x1]
shade_wl_sorted = [wl_cols_shades[i] for i in sort_x1]

_y_interp = np.array([
    np.interp(x2, x1_sorted, row[shade_wl_sorted].to_numpy(dtype=float))
    for _, row in df_shades.iterrows()
])

# Convert the reflectance
y_interp = _y_interp/10000
# %%
# Get the metadata columns for the shade spectra df
meta_cols_shades = [c for c in df_shades.columns if c not in wl_cols_shades]
# %%
# Create a new dataframe with the interpolated spectra
df_shades_interp = pd.DataFrame(y_interp, columns=wl_cols_df)
print(wl_cols_df)

# %% Check the first row of the field spectra df
df.head().iloc[0][wl_cols_df]
# %%
# Add the metadata columns to the interpolated spectra dataframe    
for col in meta_cols_shades:
    df_shades_interp[col] = df_shades[col].values

# %% Combine the field spectra and interpolated shade spectra dataframes
df_combined = pd.concat(
    [
        df[meta_cols_shades + wl_cols_df],
        df_shades_interp[meta_cols_shades + wl_cols_df]
    ],
    ignore_index=True,
    axis=0
)

# %% Save the combined dataframe
out_file = "MLBS26_leafspectra_with_attrs_cleaner_and_shades.csv"
df_combined.to_csv(os.path.join(data_dir, out_file), index=False)
print(f"Saved {len(df_combined)} rows ({len(df)} leaf + {len(df_shades)} shade)")

# %%
# # %% Plot before/after interpolation
# n_plot = min(3, len(df_shades))
# fig, axes = plt.subplots(2, n_plot, figsize=(4 * n_plot, 6), sharex="col", squeeze=False)

# for i in range(n_plot):
#     y_before = df_shades.iloc[i][shade_wl_sorted].to_numpy(dtype=float)
#     y_after = y_interp[i]

#     axes[0, i].plot(x1_sorted, y_before, linewidth=1)
#     axes[0, i].set_title(f"Before (shade row {i})")
#     axes[0, i].set_ylabel("Value")

#     axes[1, i].plot(x2, y_after, linewidth=1, color="C1")
#     # add scatter
#     axes[1, i].scatter(x1_sorted, y_before, color="gray", alpha=0.5, s=10)
#     axes[1, i].set_title(f"After (shade row {i})")
#     axes[1, i].set_xlabel("Wavelength (nm)")
#     axes[1, i].set_ylabel("Value")

# fig.suptitle("Shade spectra: NEON AOP bands vs interpolated to lab grid")
# fig.tight_layout()
# plt.savefig(os.path.join(data_dir, "shade_interp_check.png"), dpi=150)
# plt.show()

# %% Plot the shade spectra (dark gray gradations)
wavelengths_plot = np.array([float(c) for c in wl_cols_df])

gray_cmap = cm.Greys
gray_min, gray_max = 0.25, 0.80

def identity_label(row):
    sid = row.get("sample_identity")
    if pd.notna(sid) and str(sid).strip():
        return str(sid)
    return str(row["sample_name"])

fig, ax = plt.subplots(figsize=(4, 2.5))
df_to_plot = df_shades_interp.copy()
# Mask values less than 0

legend_handles = []
for _, row in df_to_plot.iterrows():
    # color = gray_cmap
    mask = row[wl_cols_df].to_numpy(dtype=float) >= 0
    line, = ax.plot(
        wavelengths_plot[mask],
        row[wl_cols_df].to_numpy(dtype=float)[mask],
        alpha=0.5,
        linewidth=1,
        color="darkgray",
        # label=pid if pid not in plotted_ids else None,
    )
    # legend_handles.append(line)

# if legend_handles:
    # ax.legend(handles=legend_handles, loc="upper right", fontsize=10, bbox_to_anchor=(1, 1))

# ax.set_title(rf"Leaf and shade spectra ($n$={len(df_to_plot)})", fontsize=14)
ax.set_xlabel("Wavelength (nm)", fontsize=14)
ax.set_ylabel("Reflectance", fontsize=14)
ax.set_ylim(-0.005, 1.1)
fig.tight_layout()
plt.savefig(os.path.join(data_dir, "MLBS26_shadespectra.png"), dpi=300)
plt.show()

# %%

