# %%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
from matplotlib import cm
from matplotlib.colors import Normalize, to_rgb

# %% File paths
spectra_dir = "G:/Shared drives/Ryoko and Hilary/SPECschool/Spectra_csv"
spectra_file = "MLBS26_leafspectra.csv"
spectra_attrs = "spectra_attributes.csv"

# %% Read in spectra data
df_spectra = pd.read_csv(os.path.join(spectra_dir, spectra_file))
df_spectra.head()

# %% Clean (Use the unnamed column for the ID)
df_spectra.drop(columns=['ID'], inplace=True)
df_spectra["ID"] = df_spectra["Unnamed: 0"]
df_spectra.drop(columns=['Unnamed: 0', 'number'], inplace=True)

# Order columns
first_cols = ['sample_name', 'ID']
other_cols = [col for col in df_spectra.columns if col not in first_cols]
df_spectra = df_spectra[first_cols + other_cols]

df_spectra.head()
# %% Download attributes from GDrive and read in
df_attrs = pd.read_csv(os.path.join(spectra_dir, spectra_attrs))
df_attrs.head()

# %% Join attributes to spectra
df_joined = df_spectra.merge(df_attrs, right_on="file_ID", left_on="ID", how="inner")
first_cols = ['sample_name', 'ID', 'spectrometer_identity', 'file_ID', 'broad_class', 'sub_class', 'sample_identity', 'notes', 'quality', 'cleaner_groupings']
other_cols = [col for col in df_spectra.columns if col not in first_cols]
df_joined = df_joined[first_cols + other_cols]

# Check
print(len(df_spectra), len(df_joined))
df_joined.head()

# %% Save joined data
df_joined.to_csv(os.path.join(spectra_dir, 'MLBS26_leafspectra_with_attrs.csv'), index=False)

# %% Filter for good quality and entry with subclass of understory, dead tree, and canopy
df_joined_good = df_joined[df_joined["quality"].isin(["good","weird"])] # Include weird for now
df_joined_good = df_joined_good[df_joined_good["sub_class"].isin(["understory","dead_tree","canopy"])]

# Check
print(len(df_joined))
print(len(df_joined_good))

# Save filtered data
df_joined_good.to_csv(os.path.join(spectra_dir, 'MLBS26_leafspectra_with_attrs_good.csv'), index=False)


# %% Filter for good quality (without moss in dead tree) and entry with subclass of understory, dead tree, and canopy
df_joined_cleaner = df_joined[df_joined["cleaner_groupings"].isin(["yes"])]
df_joined_cleaner = df_joined_cleaner[df_joined_cleaner["sub_class"].isin(["understory","dead_tree","canopy"])]

# Check
print(len(df_joined))
print(len(df_joined_cleaner))

# Save filtered data
df_joined_cleaner.to_csv(os.path.join(spectra_dir, 'MLBS26_leafspectra_with_attrs_cleaner.csv'), index=False)

# %% Plot spectra by subclass
wl_cols = [c for c in df_spectra.columns if str(c).replace(".", "").isdigit()]
wavelengths = np.array([float(c) for c in wl_cols])

fig, axes = plt.subplots(1, 3, figsize=(13, 4), sharey=True)
sub_classes = ["understory", "dead_tree", "canopy"]
title_label = ["Understory", "Dead Tree", "Canopy"]
# Define base colors for each sub_class for legends
sub_class_colors = {"understory": "#1f77b4", "dead_tree": "#ff7f0e", "canopy": "#2ca02c"}

def adjust_color_lightness(c, amount=0.2, max_lighten=0.3):
    """Lighten color but don't go to full white; amount < 1, max_lighten <= 1."""
    # Convert to RGB
    base = np.array(to_rgb(c))
    white = np.array([1, 1, 1])
    return tuple(base + (white - base) * min(amount, max_lighten))

df_to_plot = df_joined_cleaner.copy()
for i, (ax, sub_class) in enumerate(zip(axes, sub_classes)):
    subset = df_to_plot[df_to_plot["sub_class"] == sub_class].copy()
    
    legend_handles = []
    if not subset.empty:
        colormaps = {
            "understory": cm.Blues if len(subset) > 1 else cm.Blues,
            "dead_tree": cm.Oranges if len(subset) > 1 else cm.Oranges,
            "canopy": cm.Greens if len(subset) > 1 else cm.Greens,
        }
        cmap = colormaps.get(sub_class, cm.viridis)
        subset = subset.sort_values("sample_identity")
        sample_identities = subset["sample_identity"].astype(str).unique()
        
        # Special handling for 'dead_tree' with exactly 2 classes to keep both colors reasonable
        if sub_class == "dead_tree" and len(sample_identities) == 2:
            # Manually pick two visually distinct saturations
            color_vals = [0.35, 0.75]  # less extreme than default (0,1)
            sample_identity_to_color_val = {sid: color_vals[i] for i, sid in enumerate(sample_identities)}
            get_color_val = lambda sid: sample_identity_to_color_val[sid]
        else:
            norm = Normalize(vmin=0, vmax=len(sample_identities)-1 if len(sample_identities)>1 else 1)
            sample_identity_to_color_val = {sid: i for i, sid in enumerate(sample_identities)}
            get_color_val = lambda sid: norm(sample_identity_to_color_val[sid])
        
        plotted_identities = set()

        for idx, row in subset.iterrows():
            sid = str(row["sample_identity"])
            color_val = get_color_val(sid)
            color = cmap(color_val)
            line, = ax.plot(wavelengths, row[wl_cols].to_numpy(),
                            alpha=0.7, linewidth=1, color=color,
                            label=sid if sid not in plotted_identities else None)
            if sid not in plotted_identities:
                legend_handles.append(line)
                plotted_identities.add(sid)
            # file_id = str(row.get("file_ID", ""))
            # end_x = wavelengths[-1]
            # end_y = row[wl_cols].to_numpy()[-1]
            # # Instead of just +0.2, use a blend toward white but limit maximum lightening
            # if isinstance(color, str):
            #     text_color = adjust_color_lightness(color, amount=0.3, max_lighten=0.35)
            # else:
            #     base = np.array(color[:3])
            #     white = np.array([1, 1, 1])
            #     text_color = tuple(base + (white - base) * 0.3)
            # ax.text(end_x + 10, end_y, file_id, fontsize=7, color=text_color, va='center', alpha=0.85)
        
        if legend_handles:
            ax.legend(handles=legend_handles, loc="upper right", fontsize=10, bbox_to_anchor=(1, 1))
    else:
        pass

    ax.set_title(rf"{title_label[i]} ($n$={len(subset)})", fontsize=14)
    ax.set_xlabel("Wavelength (nm)", fontsize=14)
    if i == 0:
        ax.set_ylabel("Reflectance", fontsize=14)
    else:
        ax.set_ylabel("")

    # subset.to_csv(os.path.join(spectra_dir, f"MLBS26_leafspectra_by_subclass_{sub_class}_cleaner.csv"), index=False)

# fig.suptitle("MLBS26 Leaf Spectra by sub_class")
fig.tight_layout()
plt.savefig(os.path.join(spectra_dir, "MLBS26_leafspectra_by_subclass_cleaner.png"), dpi=300)
plt.show()

# %% Find the sample where the relfectance average is larger than 1.0
# df_spectra[df_spectra[wl_cols].mean(axis=1) > 1]
# %%
