libname dataint 'Q:/RDW_Data2/Work_Areas/gys_development/psp_compiler_2017/sasds/main/tree';
libname datain 'Q:/RDW_Data2/Work_Areas/gys_development/gys_db';
libname dataout 'D:/GitHub/tree_growth-climate_change-relationships/data/sasfiles';


data dataout.tree_for_yong;
set datain.gys_tree (keep = tree_id species_code plot_id out_of_plot_ind tree_stem_map_bearing tree_stem_map_slope_distance tree_stem_map_horiz_distance
natural_or_planted samp_id plot_no tree_no);
run;

data dataout.agetree_for_yong;
set dataint.psp_tree_all (keep = samp_id plot_no tree_no meas_no meas_no_orig bore_ht age_boreht age_bh age_tot);
run;

data dataout.tree_meas_for_yong;
set datain.psp_tree_all (keep = samp_id plot_no tree_no meas_no meas_no_orig bore_ht age_boreht age_bh age_tot);
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

data dataout.tree_meas_for_yong;
set datain.Gys_tree_measurement_withdam (keep = samp_id plot_no tree_no meas_no CROWN_CLASS_CODE HT_MEASUREMENT_STATUS_CODE TREE_CLASS_CODE DIAM_AT_13M
DEAD_STANDING_OR_DOWN DIAM_AT_137M SUB_PLOT_TREE_IND MEAS_HT BORING_AGE BORING_HT AGE_CORRECTION TOTAL_AGE NEW_AGE TREE_TAG_OK_IND AGE_CORE_TAKEN_IND
damage_agent_type_code1 damage_agent_type_code2);
run;
