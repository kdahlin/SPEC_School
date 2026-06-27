using SpectralUnmixing, CSV, Plots, Tables, Statistics

# Load endmember library
endmember_library = SpectralLibrary("C:/Users/ngsim/Documents_SN/UCLA Grad School/SpecSchool/unmixing_files/MLBS26_leafspectra_with_attrs_cleaner_and_shades.csv", "sub_class", 10, 0, ["understory","dead_tree","canopy", "shade"]);
load_data!(endmember_library);
filter_by_class!(endmember_library);

#println("4. endmember library is type")
#println(typeof(endmember_library2))

refl_file_wl = read_envi_wavelengths("C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/neon_aop_refl.hdr"); # input my own edited NEON AOP info header
interpolate_library_to_new_wavelengths!(endmember_library, refl_file_wl);

# fill the ignored regions with NaNs to improve visualization of the endmember spectra.
remove_wavelength_region_inplace!(endmember_library, true);


# Reduce endmember_library to good bands
unmixing_library= deepcopy(endmember_library)
# remove the ignored regions entirely instead of filling with NaNs to improve computational efficiency.
remove_wavelength_region_inplace!(unmixing_library, false);


# Set unmixing critieria for E(MC)2 as Francisco explained in his email
n_mc = 25;
mode = "sma";
num_endmembers=[30];
normalization= "brightness";
optimization="bvls"; # not sure what bvls is, but it seems to be default 251126

max_combinations=100;
combination_type="class-even";

class_idx = prepare_combinations(unmixing_library, combination_type);
options = prepare_options(unmixing_library, combination_type, num_endmembers, class_idx);

# Unmix all pixels in images


images = ["C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T1D/B3T1D_2019_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T1D/B3T1D_2021_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T1D/B3T1D_2022_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T1D/B3T1D_2025_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T2D/B3T2D_2019_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T2D/B3T2D_2021_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T2D/B3T2D_2022_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T2D/B3T2D_2025_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T3D/B3T3D_2019_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T3D/B3T3D_2021_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T3D/B3T3D_2022_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T3D/B3T3D_2025_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T4D/B3T4D_2019_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T4D/B3T4D_2021_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T4D/B3T4D_2022_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T4D/B3T4D_2025_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3CD/B3CD_2019_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3CD/B3CD_2021_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3CD/B3CD_2022_hyp_bands_only.csv",
 "C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3CD/B3CD_2025_hyp_bands_only.csv"]

images = ["C:/Users/ngsim/Documents_SN/UCLA Grad School/SpecSchool/unmixing_files/2023_MLBS_hyp_v3_bands_only.csv"]

for image in images
    print(image)
    #neon_pixels = CSV.read("C:/Users/ngsim/Documents_SN/UCLA Grad School/Geog 214/NEON AOP Spectral Unmixing/NEAT_Block3_Clip_test.csv", Tables.matrix, header=true, types = Float64); # read desired NEON pixel in
    #neon_pixels = CSV.read("C:/Users/ngsim/Documents_SN/UCLA Grad School/NEON_AOP_Spectral_Unmixing/B3T4D/B3T4D_2025_hyp_bands.csv", Tables.matrix, header=true, types = Float64); # read desired NEON pixel in
    #print(image)
    neon_pixels = CSV.read(image, Tables.matrix, header=true, types = Float64); # read desired NEON pixel in
    neon_pixels_gb = neon_pixels[:,endmember_library.good_bands]; # get rid of bad bands in NEON pixel

    num_pixels = length(neon_pixels_gb[:,1]);
    unmixed_pixels = zeros(num_pixels, 5) # columns: brightness, soil, npv, pv
    #print(num_pixels)

    for i in 1:num_pixels
         # printed tracker
        pixel = neon_pixels_gb[i,:] # select single pixel
        neon_pixel_gb = reshape(pixel, (1, size(pixel)[1])); # reshape neon pixel into 1xBands matrix
        
        # Scale neon pixel data. This seems to be critical to get proper unmixing
        #neon_pixel_gb = scale_data(neon_pixel_gb, endmember_library.wavelengths[endmember_library.good_bands], normalization)
        neon_pixel_gb = neon_pixel_gb/10000 # according to https://www.neonscience.org/resources/learning-hub/tutorials/neon-refl-h5-py, seems neon is scaled up by 10000 for storage space, so this makes sense to re-scale down

        # Execute unmixing
        mr, mv, cfr, cfv = unmix_pixel(unmixing_library, neon_pixel_gb, nothing,
                                    class_idx, 	options, mode, n_mc,
                                    num_endmembers, normalization, optimization,
                                    max_combinations, combination_type);
        # outputs are one element longer because brightness is appended

        unmixed_pixels[i,:] = mr; # store fractions and brightness
        print(i, " ")
    end

    address_length = length(image)
    savename = string(image[1:address_length-19],"_v3_fractions_cleaner_and_shade.csv")
    CSV.write(savename, Tables.table(unmixed_pixels), writeheader=false)
    print("saved as ",savename)
end
