import numpy as np
import rioxarray
import rasterio
import pandas as pd
from pyproj import Proj, Transformer
import os

wk_dir = r"G:\Shared drives\Ryoko and Hilary\SPECschool\Spectra_csv"
git_dir = r"C:\Users\flipl\dev\SPEC26_Unmixing"
coord_ref = 'EPSG:32617' # UTM 17
rds = rioxarray.open_rasterio(os.path.join(wk_dir, "2023_MLBS_canopy_fraction_cleaner_and_shade.tif"), crs = coord_ref)
hyp_pxls = pd.read_csv(os.path.join(wk_dir, "qgis_spectra_pt_formatted_unmixed.csv"))
fractions = rds[0]
fractions.attrs['long_name'] = 'fraction' # change long_name attribute so it's just one band
fractions.plot(add_colorbar=False)
#["understory_fraction", "dead_tree_fraction", "canopy_fraction", "shade_fraction"]
fraction = "shade_fraction" ## NOTE change this


fraction_names = ["understory_fraction", "dead_tree_fraction", "canopy_fraction", "shade_fraction"]
for fraction in fraction_names:
    print("Processing", fraction)

    fractions_p = fractions.rio.reproject(coord_ref).astype(np.float32)
    fractions_p.values[:] = np.NaN # set all data to 0

    # Rewrite pixels to be desired value from csv file, matching longitude and latitude
    p = Proj(coord_ref)
    for i in range(len(hyp_pxls)):
        #print(i, end = ' ')
        long,lat = p(hyp_pxls['longitude'][i],hyp_pxls['latitude'][i]) # get lat long from pixel in grid format (I think these lat long are actually grid coordinates)
        fractions_p_sel = fractions_p.sel(x = long, y = lat, method = 'nearest') # find closest pixel in existing raster
        fractions_p.loc[dict(x = fractions_p_sel.x.values, y = fractions_p_sel.y.values)] = hyp_pxls[fraction][i] # rewrite value of pixel with value from spreadsheet
    fractions_p.rio.to_raster(os.path.join(wk_dir, "qgis_spectra_pt_" + fraction + ".tif"))
