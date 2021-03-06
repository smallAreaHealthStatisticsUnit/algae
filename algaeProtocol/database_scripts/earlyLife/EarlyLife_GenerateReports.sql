/**
 * ================================================================================================= 
 * MODULE COMM_EARLY_REP: Backup and Generate Early Life Reports
 * =================================================================================================
 * Description
 * -----------
 * This module contains routines that are meant to help finish the analyses.  It covers two 
 * important tasks:
 * (1) backs up staging tables
 * (2) generates result tables and writes them to CSV files.
 *
 * Some of the staging tables can be expensive to create.  For example, in the early life analysis,
 * the staging_exp_data can contain millions of rows and take a long time to load.  If the late 
 * analysis is run after the early analysis, it will drop and reconsititute the staging tables, but
 * will often include different data.
 *
 * The reports provide a way to filter what tables should be written to CSV files.  It is not really
 * meant to act as a security measure, but it does represent the place where cohort projects can decide
 * whether they are comfortable with extracting results.
 *
 * This is also where variables are renamed for how they will be referenced in publications.  Here,
 * we prefix most of them with "algae_"
 *
 * Main Functions
 * --------------
 * (1) early_life_create_result_tables_and_backup
 * (2) early_life_create_reports 
 *
 * Assumptions
 * ----------- 
 * These two methods should be called at the very end of the analysis and the backup routine should
 * be called before reports are generated.
 *
 * ------------------------------------------------------------------------------------------------
 * Copyright 2017 Imperial College London, developed by the Small Area
 * Health Statistics Unit in collaboration with the Avon Longitudinal Study of Parents
 * and Children (ALSPAC).
 * 
 * The code was originally authored by Kevin Garwood and reviewed by Olly Butters
 * and Iain Bickerstaffe.
 *
 * This file is part of the ALGAE (Algorithms for Assessing Early life exposures) project.
 * ALGAE is free software: you can redistribute it and/or modify it under the terms of the GNU 
 * Lesser General Public License as published by the Free Software Foundation, either version 3 
 * of the License, or (at your option) any later version.
 *
 * This code is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the code.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Kevin Garwood
 * ================================================================================================= 
 */


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION early_life_create_result_tables_and_backup
 * -----------------------------------------------------
 * Description
 * -----------
 * Backs up the staging tables and creates result tables.  Early and late life analyses often share
 * commonly named tables.  In this method, a copies of tables are made to capture the output of
 * one analysis or the other.  This is also the routine in the early life analysis where variables
 * are renamed for the official variable names that will be cited by other studies that use ALGAE
 * variables.  
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION early_life_create_result_tables_and_backup()
	RETURNS void AS 
$$
DECLARE
	comments TEXT;
BEGIN


	DROP TABLE IF EXISTS bak_early_staging_exp_data;
	CREATE TABLE bak_early_staging_exp_data AS
	SELECT
		*
	FROM
		staging_exp_data;
	
	
	DROP TABLE IF EXISTS bak_early_staging_addr_history_data;
	CREATE TABLE bak_early_staging_addr_history_data AS
	SELECT
		*
	FROM
		staging_addr_history_data;
	
	DROP TABLE IF EXISTS bak_early_staging_geocode_data;
	CREATE TABLE bak_early_staging_geocode_data AS
	SELECT
		*
	FROM
		staging_geocode_data;
	

	DROP TABLE IF EXISTS bak_early_staging_study_member_data;
	CREATE TABLE bak_early_staging_study_member_data AS
	SELECT
		*
	FROM
		staging_study_member_data;

	
	
	-- -----------------------------------------------
	-- Generate life stage data file
	-- -----------------------------------------------	
	DROP TABLE IF EXISTS results_early_life_stages;
	CREATE TABLE results_early_life_stages AS
	SELECT
		person_id AS algae1100_person_id,
		ith_life_stage AS algae1101_ith_life_stage,
		life_stage AS algae1102_life_stage,
		life_stage_year AS algae1103_life_stage_year,
		start_date AS algae1104_start_date,
		end_date AS algae1105_end_date,
		life_stage_duration AS algae1106_life_stage_duration
	FROM
		fin_general_life_stage_data
	ORDER BY
		person_id,
		ith_life_stage;
	

	-- ------------------------------------
	-- Generate cleaned address period file
	-- ------------------------------------	
	
	DROP TABLE IF EXISTS results_early_cleaned_addr_periods;
	CREATE TABLE results_early_cleaned_addr_periods AS
	SELECT
		original_row_number AS algae2100_original_row_number,
		person_id AS algae2101_person_id,
		ith_residence AS algae2102_ith_residence,
		geocode AS algae2103_geocode,
		date_state AS algae2104_date_state,
		start_date AS algae2105_start_date,
		end_date AS algae2106_end_date,
		duration AS algae2107_duration,
		ith_residence_type AS algae2108_ith_residence_type,
		has_valid_geocode AS algae2109_has_valid_geocode,
		has_name_exposures AS algae2110_has_name_exposures,		
		has_nox_rd_exposures AS algae2111_has_nox_rd_exposures,
		has_pm10_gr_exposures AS algae2112_has_pm10_gr_exposures,
		has_pm10_rd_exposures AS algae2113_has_pm10_rd_exposures,
		has_pm10_tot_exposures AS algae2114_has_pm10_tot_exposures,		
		maximum_life_stage_overlap AS algae2115_max_life_stage_overlap,
		is_fixed_invalid_geocode AS algae2116_is_fixed_inv_geocode,
		fit_extent AS algae2117_fit_extent,
		adjusted_start_date AS algae2118_adj_start_date,
		adjusted_end_date AS algae2119_adj_end_date,
		days_changed AS algae2120_days_changed,
		fit_type AS algae2121_fit_type,
		start_date_delta1 AS algae2122_start_date_delta1,
		start_date_delta2 AS algae2123_start_date_delta2,
		end_date_delta1 AS algae2124_end_date_delta1,
		end_date_delta2 AS algae2125_end_date_delta2,
		previous_geocode AS algae2126_previous_geocode,
		next_geocode AS algae2127_next_geocode,
		fin_adjusted_start_date AS algae2128_fin_adj_start_date,
		imputed_first_start AS algae2129_imputed_first_start,
		fin_adjusted_end_date AS algae2130_fin_adj_end_date,
		imputed_last_end AS algae2131_imputed_last_end,
		start_date_days_from_conception AS algae2132_start_date_days_from_concep,
		is_within_exposure_time_frame AS algae2133_is_within_exp
	FROM
		fin_cleaned_addr_periods
	ORDER BY
		person_id,
		ith_residence;
	

	-- ------------------------------------
	-- Generate exposure data files
	-- ------------------------------------	

	DROP TABLE IF EXISTS results_early_mob_cln_exp;
	CREATE TABLE results_early_mob_cln_exp AS
	SELECT
		person_id AS algae3100_person_id,
		life_stage AS algae3101_life_stage,		
		life_stage_duration AS algae3102_life_stage_duration,
		name_invalid_address_days AS algae3103_name_inv_addr_days,
		name_oob_days AS algae3104_name_oob_days,
		name_poor_address_days AS algae3105_name_poor_addr_days,
		name_missing_exp_days AS algae3106_name_missing_exp_days,
		name_good_address_days AS algae3107_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3108_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3109_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3110_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3111_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3112_nox_rd_good_addr_days,
		
		pm10_rd_invalid_address_days AS algae3113_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3114_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3115_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3116_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3117_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3118_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3119_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3120_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3121_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3122_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3123_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3124_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3125_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3126_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3127_pm10_tot_good_addr_days,

		name_sum AS algae3128_name_sum,
		name_err_sum AS algae3129_name_err_sum,
		name_avg AS algae3130_name_avg,
		name_err_avg AS algae3131_name_err_avg,
		name_med AS algae3132_name_med,
		name_err_med AS algae3133_name_err_med,
		nox_rd_sum AS algae3134_nox_rd_sum,
		nox_rd_err_sum AS algae3135_nox_rd_err_sum,
		nox_rd_avg AS algae3136_nox_rd_avg,
		nox_rd_err_avg AS algae3137_nox_rd_err_avg,
		nox_rd_med AS algae3138_nox_rd_med,
		nox_rd_err_med AS algae3139_nox_rd_err_med,
		pm10_gr_sum AS algae3140_pm10_gr_sum,
		pm10_gr_err_sum AS algae3141_pm10_gr_err_sum,
		pm10_gr_avg AS algae3142_pm10_gr_avg,
		pm10_gr_err_avg AS algae3143_pm10_gr_err_avg,
		pm10_gr_med AS algae3144_pm10_gr_med,
		pm10_gr_err_med AS algae3145_pm10_gr_err_med,
		pm10_rd_sum AS algae3146_pm10_rd_sum,
		pm10_rd_err_sum AS algae3147_pm10_rd_err_sum,		
		pm10_rd_avg AS algae3148_pm10_rd_avg,
		pm10_rd_err_avg AS algae3149_pm10_rd_err_avg,
		pm10_rd_med AS algae3150_pm10_rd_med,
		pm10_rd_err_med AS algae3151_pm10_rd_err_med,
		pm10_tot_sum AS algae3152_pm10_tot_sum,
		pm10_tot_err_sum AS algae3153_pm10_tot_err_sum,		
		pm10_tot_avg AS algae3154_pm10_tot_avg,
		pm10_tot_err_avg AS algae3155_pm10_tot_err_avg,
		pm10_tot_med AS algae3156_pm10_tot_med,
		pm10_tot_err_med AS algae3157_pm10_tot_err_med
	FROM
		fin_mob_cln_exp
	ORDER BY
		person_id,
		ith_life_stage;
			
	DROP TABLE IF EXISTS results_early_mob_uncln_exp;
	CREATE TABLE results_early_mob_uncln_exp AS
	SELECT
		person_id AS algae3200_person_id,
		life_stage AS algae3201_life_stage,
		life_stage_duration AS algae3202_life_stage_duration,

		name_invalid_address_days AS algae3203_name_inv_addr_days,
		name_oob_days AS algae3204_name_oob_days,
		name_poor_address_days AS algae3205_name_poor_addr_days,
		name_missing_exp_days AS algae3206_name_missing_exp_days,
		name_good_address_days AS algae3207_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3208_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3209_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3210_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3211_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3212_nox_rd_good_addr_days,
		
		pm10_rd_invalid_address_days AS algae3213_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3214_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3215_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3216_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3217_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3218_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3219_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3220_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3221_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3222_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3223_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3224_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3225_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3226_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3227_pm10_tot_good_addr_days,			

		name_sum AS algae3228_name_sum,
		name_avg AS algae3229_name_avg,
		name_med AS algae3230_name_med,
		nox_rd_sum AS algae3231_nox_rd_sum,
		nox_rd_avg AS algae3232_nox_rd_avg,
		nox_rd_med AS algae3233_nox_rd_med,
		pm10_gr_sum AS algae3234_pm10_gr_sum,
		pm10_gr_avg AS algae3235_pm10_gr_avg,
		pm10_gr_med AS algae3236_pm10_gr_med,
		pm10_rd_sum AS algae3237_pm10_rd_sum,
		pm10_rd_avg AS algae3238_pm10_rd_avg,
		pm10_rd_med AS algae3239_pm10_rd_med,
		pm10_tot_sum AS algae3240_pm10_tot_sum,
		pm10_tot_avg AS algae3241_pm10_tot_avg,
		pm10_tot_med AS algae3242_pm10_tot_med
	FROM
		fin_mob_uncln_exp
	ORDER BY
		person_id,
		ith_life_stage;	
	
	DROP TABLE IF EXISTS results_early_stg_mob_exp;
	CREATE TABLE results_early_stg_mob_exp AS
	SELECT
		person_id AS algae3300_person_id,
		life_stage AS algae3301_life_stage,
		life_stage_duration AS algae3302_life_stage_duration,

		name_invalid_address_days AS algae3303_name_inv_addr_days,
		name_oob_days AS algae3304_name_oob_days,
		name_poor_address_days AS algae3305_name_poor_addr_days,
		name_missing_exp_days AS algae3306_name_missing_exp_days,
		name_good_address_days AS algae3307_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3308_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3309_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3310_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3311_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3312_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3313_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3314_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3315_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3316_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3317_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3318_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3319_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3320_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3321_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3322_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3323_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3324_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3325_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3326_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3327_pm10_tot_good_addr_days,		

		name_sum AS algae3328_name_sum,
		name_avg AS algae3329_name_avg,
		name_med AS algae3330_name_med,
		nox_rd_sum AS algae3331_nox_rd_sum,
		nox_rd_avg AS algae3332_nox_rd_avg,
		nox_rd_med AS algae3333_nox_rd_med,
		pm10_gr_sum AS algae3334_pm10_gr_sum,
		pm10_gr_avg AS algae3335_pm10_gr_avg,
		pm10_gr_med AS algae3336_pm10_gr_med,
		pm10_rd_sum AS algae3337_pm10_rd_sum,
		pm10_rd_avg AS algae3338_pm10_rd_avg,
		pm10_rd_med AS algae3339_pm10_rd_med,
		pm10_tot_sum AS algae3340_pm10_tot_sum,
		pm10_tot_avg AS algae3341_pm10_tot_avg,
		pm10_tot_med AS algae3342_pm10_tot_med
	FROM
		fin_stg_mob_exp
	ORDER BY
		person_id,
		ith_life_stage;		

	DROP TABLE IF EXISTS results_early_no_mob_birth_addr_exp;
	CREATE TABLE results_early_no_mob_birth_addr_exp AS
	SELECT
		person_id AS algae3400_person_id,
		life_stage AS algae3401_life_stage,
		life_stage_duration AS algae3402_life_stage_duration,		
		name_invalid_address_days AS algae3403_name_inv_addr_days,
		name_oob_days AS algae3404_name_oob_days,
		name_poor_address_days AS algae3405_name_poor_addr_days,
		name_missing_exp_days AS algae3406_name_missing_exp_days,
		name_good_address_days AS algae3407_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3408_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3409_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3410_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3411_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3412_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3413_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3414_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3415_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3416_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3417_pm10_rd_good_addr_days,
		
		pm10_gr_invalid_address_days AS algae3418_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3419_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3420_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3421_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3422_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3423_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3424_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3425_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3426_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3427_pm10_tot_good_addr_days,		

		name_sum AS algae3428_name_sum,
		name_avg AS algae3429_name_avg,
		name_med AS algae3430_name_med,
		nox_rd_sum AS algae3431_nox_rd_sum,
		nox_rd_avg AS algae3432_nox_rd_avg,
		nox_rd_med AS algae3433_nox_rd_med,
		pm10_gr_sum AS algae3434_pm10_gr_sum,
		pm10_gr_avg AS algae3435_pm10_gr_avg,
		pm10_gr_med AS algae3436_pm10_gr_med,
		pm10_rd_sum AS algae3437_pm10_rd_sum,
		pm10_rd_avg AS algae3438_pm10_rd_avg,
		pm10_rd_med AS algae3439_pm10_rd_med,
		pm10_tot_sum AS algae3440_pm10_tot_sum,
		pm10_tot_avg AS algae3441_pm10_tot_avg,
		pm10_tot_med AS algae3442_pm10_tot_med
	FROM
		fin_no_mob_birth_addr_exp
	ORDER BY
		person_id,
		ith_life_stage;		

	-- ---------------------------------------
	-- Generate exposure data difference files
	-- ---------------------------------------

	DROP TABLE IF EXISTS results_early_mob_cln_vs_unimp;
	CREATE TABLE results_early_mob_cln_vs_unimp AS
	SELECT
		person_id AS algae4100_person_id,
		life_stage AS algae4101_life_stage,
		name_sum_percent_err AS algae4102_name_sum_percent_err,			
		name_avg_percent_err AS algae4103_name_avg_percent_err,
		name_med_percent_err AS algae4104_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4105_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4106_nox_rd_avg_percent_err,			
		nox_rd_med_percent_err AS algae4107_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4108_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4109_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4110_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4111_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4112_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4113_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4114_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4115_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4116_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_unimp
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_mob_cln_vs_no_mob;
	CREATE TABLE results_early_mob_cln_vs_no_mob AS
	SELECT
		person_id AS algae4200_person_id,
		life_stage AS algae4201_life_stage,
		name_sum_percent_err AS algae4202_name_sum_percent_err,			
		name_avg_percent_err AS algae4203_name_avg_percent_err,
		name_med_percent_err AS algae4204_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4205_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4206_nox_rd_avg_percent_err,			
		nox_rd_med_percent_err AS algae4207_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4208_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4209_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4210_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4211_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4212_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4213_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4214_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4215_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4216_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_no_mob
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_uncln_vs_no_mob;
	CREATE TABLE results_early_uncln_vs_no_mob AS
	SELECT
		person_id AS algae4300_person_id,
		life_stage AS algae4301_life_stage,
		name_sum_percent_err AS algae4302_name_sum_percent_err,			
		name_avg_percent_err AS algae4303_name_avg_percent_err,
		name_med_percent_err AS algae4304_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4305_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4306_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4307_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4308_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4309_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4310_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4311_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4312_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4313_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4314_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4315_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4316_pm10_tot_med_diff
	FROM
		fin_uncln_vs_no_mob
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_mob_cln_vs_stg_mob_birth;
	CREATE TABLE results_early_mob_cln_vs_stg_mob_birth AS
	SELECT
		person_id AS algae4400_person_id,
		life_stage AS algae4401_life_stage,
		name_sum_percent_err AS algae4402_name_sum_percent_err,			
		name_avg_percent_err AS algae4403_name_avg_percent_err,
		name_med_percent_err AS algae4404_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4405_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4406_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4407_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4408_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4409_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4410_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4411_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4412_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4413_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4414_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4415_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4416_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_stg_mob_birth
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_mob_uncln_vs_stg_mob_birth;
	CREATE TABLE results_early_mob_uncln_vs_stg_mob_birth AS
	SELECT
		person_id AS algae4500_person_id,
		life_stage AS algae4501_life_stage,
		name_sum_percent_err AS algae4502_name_sum_percent_err,			
		name_avg_percent_err AS algae4503_name_avg_percent_err,
		name_med_percent_err AS algae4504_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4505_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4506_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4507_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4508_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4509_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4510_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4511_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4512_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4513_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4514_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4515_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4516_pm10_tot_med_diff
	FROM
		fin_mob_uncln_vs_stg_mob_birth
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_stg_mob_vs_no_mob_birth;
	CREATE TABLE results_early_stg_mob_vs_no_mob_birth AS
	SELECT
		person_id AS algae4600_person_id,
		life_stage AS algae4601_life_stage,
		name_sum_percent_err AS algae4602_name_sum_percent_err,			
		name_avg_percent_err AS algae4603_name_avg_percent_err,
		name_med_percent_err AS algae4604_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4605_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4606_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4607_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4608_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4609_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4610_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4611_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4612_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4613_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4614_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4615_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4616_pm10_tot_med_diff
	FROM
		fin_stg_mob_vs_no_mob_birth
	ORDER BY
		person_id,
		ith_life_stage;

	-- ---------------------------------------
	-- Generate covariate results
	-- ---------------------------------------

	-- Generating covariates
	DROP TABLE IF EXISTS results_early_life_stages_cov;	
	CREATE TABLE results_early_life_stages_cov AS
	SELECT
		person_id AS algae5100_person_id,
		life_stage AS algae5101_life_stage,
		ed91 AS algae5102_ed91,
		oa2001 AS algae5103_oa2001,
		coa2011 AS algae5104_coa2011
	FROM
		fin_life_stages_cov
	ORDER BY
		person_id,
		ith_life_stage;
	
	DROP TABLE IF EXISTS results_early_moves_cov;
	CREATE TABLE results_early_moves_cov AS
	SELECT
		person_id AS algae5200_person_id,
		start_date_days_from_conception AS algae5201_mv_to_concept_days,
		ed91 AS algae5202_ed91,
		oa2001 AS algae5203_oa2001,
		coa2011 AS algae5204_coa2011
	FROM
		fin_moves_cov
	ORDER BY
		person_id;
	

	-- ---------------------------------------
	-- Generate sensitivity results
	-- ---------------------------------------
	
	-- Generating sensitivity variables
	DROP TABLE IF EXISTS results_early_sens_variables;
	CREATE TABLE results_early_sens_variables AS 
	SELECT
		person_id AS algae6100_person_id,
		at_1st_addr_conception AS algae6101_at_1st_addr_concept,
		absent_during_exp_period AS algae6102_absent_in_exp,
		gestation_age_at_birth AS algae6103_gest_age,	
		is_gestation_age_imputed AS algae6104_is_gest_age_imp,
		total_addr_periods AS algae6105_total_addr,
		fixed_geocodes AS algae6106_fixed_geocodes,
		over_laps AS algae6107_over_laps,
		gaps AS algae6108_gaps,
		gap_and_overlap_same_period AS algae6109_gap_over_lap,
		deletions AS algae6110_deletions,
		imp_blank_start_dates AS algae6111_cln_blank_start_date,
		imp_blank_end_dates AS algae6112_cln_blank_end_date,
		imp_blank_both_dates AS algae6113_cln_blank_both_dates,
		imp_last_dates AS algae6114_cln_last_dates,
		days_changed AS algae6115_days_changed,
		total_contention_days AS algae6116_contention_days,
		no_exposure_data_days AS algae6117_no_exp_data_days
	FROM
		fin_sens_variables
	ORDER BY
		person_id;


	DROP TABLE IF EXISTS results_early_stage_sens_variables;
	CREATE TABLE results_early_stage_sens_variables AS
	SELECT
		person_id AS algae6200_person_id,
		life_stage AS algae6201_life_stage,
		contention_days AS algae6202_contention_days,
		distinct_addresses AS algae6203_distinct_addr,
		moves AS algae6204_moves
	FROM
		fin_life_stage_sensitivity_variables;
	

END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION early_life_create_result_tables_and_backup
 * -----------------------------------------------------
 * Description
 * -----------
 * Dumps result tables for the early life analysis to CSV files.
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION early_life_create_reports(output_directory TEXT)
	RETURNS void AS 
$$
DECLARE

	results_early_life_stages_csv_file TEXT;
	results_early_mob_cln_exp_csv_file TEXT;
	results_early_mob_uncln_exp_csv_file TEXT;
	results_early_stg_mob_exp_csv_file TEXT;
	results_early_no_mob_birth_addr_exp_csv_file TEXT;
	results_early_life_stages_cov_csv_file TEXT;
	results_early_moves_cov_csv_file TEXT;
	results_early_sens_variables_csv_file TEXT;
	results_early_stage_sens_variables_csv_file TEXT;
	
	results_early_mob_cln_vs_uncln_csv_file TEXT;
	results_early_mob_cln_vs_stg_mob_csv_file TEXT;
	results_early_mob_cln_vs_stg_mob_birth_csv_file TEXT;

	results_early_mob_uncln_vs_stg_mob_birth_csv_file TEXT;
	results_early_mob_uncln_vs_stg_mob_csv_file TEXT;

	
	results_early_stg_mob_vs_no_mob_birth_csv_file TEXT;
	results_early_cleaned_addr_periods_csv_file TEXT;
	results_early_move_date_from_conception_csv_file TEXT;
	date_phrase TEXT;
	
BEGIN

	date_phrase :=
		(SELECT to_char(current_timestamp, 'YYYY-Mon-DD-HH24-MI'));


	-- -----------------------------------------------
	-- Generate life stage data file
	-- -----------------------------------------------

	results_early_life_stages_csv_file :=
		output_directory || 
		'\life_stage_data' ||
		'\results_early_life_stages_' || date_phrase || '.csv';
		
	RAISE NOTICE 'early life stages file==%==', results_early_life_stages_csv_file;
	EXECUTE format ('
	COPY results_early_life_stages
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_life_stages_csv_file);	


	-- ------------------------------------
	-- Generate cleaned address period file
	-- ------------------------------------
	results_early_cleaned_addr_periods_csv_file :=
		output_directory || 
		'\cleaned_address_history\res_early_cleaned_addr' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_cleaned_addr_periods
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_cleaned_addr_periods_csv_file);


	-- -----------------------------------------------
	-- Generate exposure result files
	-- -----------------------------------------------

	-- Generate result file for early mobility exposures with imputing
	results_early_mob_cln_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_clean\res_early_mob_cln_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_cln_exp_csv_file);	

	-- Generate result file for early mobility exposures without imputing
	results_early_mob_uncln_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_unclean\res_early_mob_uncln_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_uncln_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_uncln_exp_csv_file);

	-- Generate result file for early exposures with no mobility
	-- Uses address at start of each life stage to represent entire
	-- life stage
	results_early_stg_mob_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_life_stage\res_early_stg_mob_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_stg_mob_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_stg_mob_exp_csv_file);

	-- Generate result file for early exposures with no mobility
	-- Uses address at birth to cover entire early life period
	results_early_no_mob_birth_addr_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\no_mobility_birth_address\res_early_no_mob_birth_addr_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_no_mob_birth_addr_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_no_mob_birth_addr_exp_csv_file);

	-- -----------------------------------------------
	-- Generate exposure difference files
	-- -----------------------------------------------

	results_early_mob_cln_vs_uncln_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_cln_vs_uncln_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_unimp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_cln_vs_uncln_csv_file);

	results_early_mob_cln_vs_stg_mob_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_cln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_cln_vs_stg_mob_csv_file);

	results_early_mob_uncln_vs_stg_mob_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_uncln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_uncln_vs_stg_mob_csv_file);



	results_early_mob_cln_vs_stg_mob_birth_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_cln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_cln_vs_stg_mob_birth_csv_file);

	results_early_mob_uncln_vs_stg_mob_birth_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_uncln_vs_no_mob_birth_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_stg_mob_birth
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_uncln_vs_stg_mob_birth_csv_file);


	results_early_stg_mob_vs_no_mob_birth_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_stg_mob_vs_stg_mob_birth_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_stg_mob_vs_no_mob_birth
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_stg_mob_vs_no_mob_birth_csv_file);














	-- -----------------------------------------------
	-- Generate covariate files
	-- -----------------------------------------------

	results_early_life_stages_cov_csv_file :=
		output_directory || 
		'\covariates\res_early_cov_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_life_stages_cov
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_life_stages_cov_csv_file);

	results_early_moves_cov_csv_file :=
		output_directory || 
		'\covariates\res_early_moves_cov_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_moves_cov
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_moves_cov_csv_file);

	-- -----------------------------------------------
	-- Generate sensitivity variable files
	-- -----------------------------------------------

	results_early_sens_variables_csv_file :=
		output_directory || 
		'\sensitivity_variables\res_early_sens_variables_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_sens_variables
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_sens_variables_csv_file);


	results_early_stage_sens_variables_csv_file :=
		output_directory || 
		'\sensitivity_variables\res_early_stage_sens_variables_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_stage_sens_variables
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_stage_sens_variables_csv_file);


END;
$$   LANGUAGE plpgsql;




