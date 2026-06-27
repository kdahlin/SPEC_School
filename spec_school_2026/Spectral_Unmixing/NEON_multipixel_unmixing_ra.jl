using SpectralUnmixing, CSV, Plots, Tables, Statistics, DataFrames
using Base.Threads
using LinearAlgebra: BLAS

BLAS.set_num_threads(1)  # avoid nested parallelism with @threads

# Load endmember library
data_dir = "G:/Shared drives/Ryoko and Hilary/SPECschool/Spectra_csv"
spectra_file = "qgis_spectra_pt_formatted.csv"
refl_file = "neon_aop_refl.hdr"
output_file = "qgis_spectra_pt_formatted_unmixed.csv"
classes = ["understory","dead_tree","canopy", "shade"]
unmixed_classes = ["understory_fraction", "dead_tree_fraction", "canopy_fraction", "shade_fraction", "brightness"]
endmember_library = SpectralLibrary(joinpath(data_dir, spectra_file), "sub_class", 10, 0, classes);
load_data!(endmember_library);
filter_by_class!(endmember_library);

#println("4. endmember library is type")
#println(typeof(endmember_library2))

refl_file_wl = read_envi_wavelengths(joinpath(data_dir, refl_file)); # input my own edited NEON AOP info header
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
images = ["C:/Users/flipl/dev/SPEC26_Unmixing/2023_MLBS_hyp_v3_bands_only.csv"]

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
    savename = joinpath(data_dir, output_file)
    # savename = string(image[1:address_length-19],"_v3_fractions_cleaner_and_shade.csv")
    # Because unmixed_pixels is (num_pixels, 5), split into brightness + 4 classes for header
    out_table = Tables.table(unmixed_pixels, header=unmixed_classes)

    # Read "longitude" and "latitude" from "2023_MLBS_hyp_csv_v3.csv", and horizontally concatenate with out_table
    coords = CSV.File("C:/Users/flipl/dev/SPEC26_Unmixing/2023_MLBS_hyp_csv_v3.csv") |> Tables.columntable
    lon_lat_table = (; longitude = coords.longitude, latitude = coords.latitude)
    # convert unmixed_pixels to a NamedTuple or DataFrame with the header 'unmixed_classes' if needed
    using DataFrames
    unmixed_df = DataFrame(unmixed_pixels, Symbol.(unmixed_classes))
    coords_df = DataFrame(lon_lat_table)
    out_table = hcat(unmixed_df, coords_df)

    CSV.write(savename, out_table, writeheader=true)
    print("saved as ",savename)
end
