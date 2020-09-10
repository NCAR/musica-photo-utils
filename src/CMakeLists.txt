################################################################################
# musica-photo-utils library

set(PHOTO_UTILS_SRC phot_kind_mod.F90
		    numer_mod.F90
		    wavelength_grid.F90
                    input_file.F90
		    input_slice.F90
                    environ_conditions_mod.F90)

add_library(musica-photo-utils-lib ${PHOTO_UTILS_SRC})

target_link_libraries(musica-photo-utils-lib ${NETCDF_LIBS})

set_target_properties(musica-photo-utils-lib PROPERTIES OUTPUT_NAME musica-photo-utils)

################################################################################
