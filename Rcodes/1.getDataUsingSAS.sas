libname datain 'Q:/RDW_Data2/Work_Areas/gys_development/gys_db';
libname dataout 'F:/GitHub/tree_growth-climate_change-relationships/data';


data dataout.tree_for_yong;
set datain.gys_tree (keep = tree_id species_code plot_id out_of_plot_ind tree_stem_map_bearing tree_stem_map_slope_distance tree_stem_map_horiz_distance
natural_or_planted samp_id plot_no tree_no);
run;

data dataout.tree_meas_for_yong;
set datain.gys_tree_measurement (keep = samp_id plot_no tree_no meas_no crown_class_code height_source_code ht_diameter_curve_use_code ht_measurement_status_code tree_id diam_at_13m
sub_plot_tree_ind meas_ht);
run;

data dataout.plot;
set datain.gys_plot;
run;

data dataout.sample;
set datain.gys_sample;
run;

data dataout.sample_measurement;
set datain.gys_sample_measurement_withtreat;
run;

data dataout.subplot;
set datain.gys_subplot;
run;




