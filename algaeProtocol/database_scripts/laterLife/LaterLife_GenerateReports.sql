/**
 * ================================================================================================= 
 * MODULE COMM_EARLY_REP: Backup and Generate Late Life Reports
 * =================================================================================================
 * Description
 * -----------
 * This module contains routines that are meant to help finish the analyses.  It covers two 
 * important tasks:
 * (1) backs up staging tables
 * (2) generates result tables and writes them to CSV files.
 *
 * Some of the staging tables can be expensive to create.  For example, in the late life analysis,
 * the staging_exp_data can contain millions of rows and take a long time to load.  If the late 
 * analysis is run after the late analysis, it will drop and reconsititute the staging tables, but
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
 * (1) later_life_create_result_tables_and_backup
 * (2) later_life_create_reports 
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
 * This file is part of the ALGAE (Algorithms for Assessing Late life exposures) project.
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
 * FUNCTION later_life_create_result_tables_and_backup
 * -----------------------------------------------------
 * Description
 * -----------
 * Backs up the staging tables and creates result tables.  Late and late life analyses often share
 * commonly named tables.  In this method, a copies of tables are made to capture the output of
 * one analysis or the other.  This is also the routine in the late life analysis where variables
 * are renamed for the official variable names that will be cited by other studies that use ALGAE
 * variables.  
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION later_life_create_result_tables_and_backup()
	RETURNS void AS 
$$
DECLARE
	comments TEXT;
BEGIN

/*
	DROP TABLE IF EXISTS bak_later_staging_exp_data;
	CREATE TABLE bak_later_staging_exp_data AS
	SELECT
		*
	FROM
		staging_exp_data;
	
	
	DROP TABLE IF EXISTS bak_later_staging_addr_history_data;
	CREATE TABLE bak_later_staging_addr_history_data AS
	SELECT
		*
	FROM
		staging_addr_history_data;
	
	DROP TABLE IF EXISTS bak_later_staging_geocode_data;
	CREATE TABLE bak_later_staging_geocode_data AS
	SELECT
		*
	FROM
		staging_geocode_data;
	

	DROP TABLE IF EXISTS bak_later_staging_study_member_data;
	CREATE TABLE bak_later_staging_study_member_data AS
	SELECT
		*
	FROM
		staging_study_member_data;
*/
	
	
	-- -----------------------------------------------
	-- Generate life stage data file
	-- -----------------------------------------------	
	DROP TABLE IF EXISTS results_later_life_stages;
	CREATE TABLE results_later_life_stages AS
	SELECT
		person_id AS algae1200_person_id,
		ith_life_stage AS algae1201_ith_life_stage,
		life_stage AS algae1202_life_stage,
		life_stage_year AS algae1203_life_stage_year,
		start_date AS algae1204_start_date,
		end_date AS algae1205_end_date,
		life_stage_duration AS algae1206_life_stage_duration
	FROM
		fin_general_life_stage_data
	ORDER BY
		person_id,
		ith_life_stage;
	

	-- ------------------------------------
	-- Generate cleaned address period file
	-- ------------------------------------	
	
	DROP TABLE IF EXISTS results_later_cleaned_addr_periods;
	CREATE TABLE results_later_cleaned_addr_periods AS
	SELECT
		original_row_number AS algae2200_original_row_number,
		person_id AS algae2201_person_id,
		ith_residence AS algae2202_ith_residence,
		geocode AS algae2203_geocode,
		date_state AS algae2204_date_state,
		start_date AS algae2205_start_date,
		end_date AS algae2206_end_date,
		duration AS algae2207_duration,
		ith_residence_type AS algae2208_ith_residence_type,
		has_valid_geocode AS algae2209_has_valid_geocode,
		has_name_exposures AS algae2210_name_exposures,
		has_nox_rd_exposures AS algae2211_nox_rd_exposures,
		has_pm10_gr_exposures AS algae2212_pm10_gr_exposures,
		has_pm10_rd_exposures AS algae2213_pm10_rd_exposures,
		has_pm10_tot_exposures AS algae2214_pm10_tot_exposures,		
		maximum_life_stage_overlap AS algae2215_max_life_stage_overlap,
		is_fixed_invalid_geocode AS algae2216_is_fixed_inv_geocode,
		fit_extent AS algae2217_fit_extent,
		adjusted_start_date AS algae2218_adj_start_date,
		adjusted_end_date AS algae2219_adj_end_date,
		days_changed AS algae2220_days_changed,
		fit_type AS algae2221_fit_type,
		start_date_delta1 AS algae2222_start_date_delta1,
		start_date_delta2 AS algae2223_start_date_delta2,
		end_date_delta1 AS algae2224_end_date_delta1,
		end_date_delta2 AS algae2225_end_date_delta2,
		previous_geocode AS algae2226_previous_geocode,
		next_geocode AS algae2227_next_geocode,
		fin_adjusted_start_date AS algae2228_fin_adj_start_date,
		imputed_first_start AS algae2229_imputed_first_start,
		fin_adjusted_end_date AS algae2230_fin_adj_end_date,
		imputed_last_end AS algae2231_imputed_last_end,
		start_date_days_from_conception AS algae2232_start_date_days_from_concep,
		is_within_exposure_time_frame AS algae2233_is_within_exp
	FROM
		fin_cleaned_addr_periods
	ORDER BY
		person_id,
		ith_residence;

	-- ------------------------------------
	-- Generate exposure data files
	-- ------------------------------------	

	DROP TABLE IF EXISTS results_later_mob_cln_exp;
	CREATE TABLE results_later_mob_cln_exp AS
	SELECT
		person_id AS algae3500_person_id,
		life_stage AS algae3501_life_stage,	
		life_stage_duration AS algae3502_life_stage_duration,
		name_invalid_address_days AS algae3503_name_inv_addr_days,
		name_oob_days AS algae3504_name_oob_days,
		name_poor_address_days AS algae3505_name_poor_addr_days,
		name_missing_exp_days AS algae3506_name_missing_exp_days,
		name_good_address_days AS algae3507_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3508_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3509_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3510_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3511_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3512_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3513_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3514_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3515_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3516_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3517_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3518_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3519_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3520_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3521_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3522_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3523_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3524_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3525_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3526_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3527_pm10_tot_good_addr_days,	

		name_sum AS algae3528_name_sum,
		name_err_sum AS algae3529_name_err_sum,
		name_avg AS algae3530_name_avg,
		name_err_avg AS algae3531_name_err_avg,
		name_med AS algae3532_name_med,
		name_err_med AS algae3533_name_err_med,
		nox_rd_sum AS algae3534_nox_rd_sum,
		nox_rd_err_sum AS algae3535_nox_rd_err_sum,
		nox_rd_avg AS algae3536_nox_rd_avg,
		nox_rd_err_avg AS algae3537_nox_rd_err_avg,
		nox_rd_med AS algae3538_nox_rd_med,
		nox_rd_err_med AS algae3539_nox_rd_err_med,
		pm10_gr_sum AS algae3540_pm10_gr_sum,
		pm10_gr_err_sum AS algae3541_pm10_gr_err_sum,
		pm10_gr_avg AS algae3542_pm10_gr_avg,
		pm10_gr_err_avg AS algae3543_pm10_gr_err_avg,
		pm10_gr_med AS algae3544_pm10_gr_med,
		pm10_gr_err_med AS algae3545_pm10_gr_err_med,
		pm10_rd_sum AS algae3546_pm10_rd_sum,
		pm10_rd_err_sum AS algae3547_pm10_rd_err_sum,		
		pm10_rd_avg AS algae3548_pm10_rd_avg,
		pm10_rd_err_avg AS algae3549_pm10_rd_err_avg,
		pm10_rd_med AS algae3550_pm10_rd_med,
		pm10_rd_err_med AS algae3551_pm10_rd_err_med,
		pm10_tot_sum AS algae3552_pm10_tot_sum,
		pm10_tot_err_sum AS algae3553_pm10_tot_err_sum,		
		pm10_tot_avg AS algae3554_pm10_tot_avg,
		pm10_tot_err_avg AS algae3555_pm10_tot_err_avg,
		pm10_tot_med AS algae3556_pm10_tot_med,
		pm10_tot_err_med AS algae3557_pm10_tot_err_med
	FROM
		fin_mob_cln_exp
	ORDER BY
		person_id,
		ith_life_stage;
			
	DROP TABLE IF EXISTS results_later_mob_uncln_exp;
	CREATE TABLE results_later_mob_uncln_exp AS
	SELECT
		person_id AS algae3600_person_id,
		life_stage AS algae3601_life_stage,
		life_stage_duration AS algae3602_life_stage_duration,
		
		name_invalid_address_days AS algae3603_name_inv_addr_days,
		name_oob_days AS algae3604_name_oob_days,
		name_poor_address_days AS algae3605_name_poor_addr_days,
		name_missing_exp_days AS algae3606_name_missing_exp_days,
		name_good_address_days AS algae3607_name_good_addr_days,
		
		nox_rd_invalid_address_days AS algae3608_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3609_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3610_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3611_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3612_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3613_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3614_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3615_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3616_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3617_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3618_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3619_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3620_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3621_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3622_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3623_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3624_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3625_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3626_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3627_pm10_tot_good_addr_days,

		name_sum AS algae3628_name_sum,
		name_avg AS algae3629_name_avg,
		name_med AS algae3630_name_med,
		nox_rd_sum AS algae3631_nox_rd_sum,
		nox_rd_avg AS algae3632_nox_rd_avg,
		nox_rd_med AS algae3633_nox_rd_med,
		pm10_gr_sum AS algae3634_pm10_gr_sum,
		pm10_gr_avg AS algae3635_pm10_gr_avg,
		pm10_gr_med AS algae3636_pm10_gr_med,
		pm10_rd_sum AS algae3637_pm10_rd_sum,
		pm10_rd_avg AS algae3638_pm10_rd_avg,
		pm10_rd_med AS algae3639_pm10_rd_med,
		pm10_tot_sum AS algae3640_pm10_tot_sum,
		pm10_tot_avg AS algae3641_pm10_tot_avg,
		pm10_tot_med AS algae3642_pm10_tot_med
	FROM
		fin_mob_uncln_exp
	ORDER BY
		person_id,
		ith_life_stage;	
	
	DROP TABLE IF EXISTS results_later_stg_mob_exp;
	CREATE TABLE results_later_stg_mob_exp AS
	SELECT
		person_id AS algae3700_person_id,
		life_stage AS algae3701_life_stage,
		life_stage_duration AS algae3702_life_stage_duration,
		
		name_invalid_address_days AS algae3703_name_inv_addr_days,
		name_oob_days AS algae3704_name_oob_days,
		name_poor_address_days AS algae3705_name_poor_addr_days,
		name_missing_exp_days AS algae3706_name_missing_exp_days,
		name_good_address_days AS algae3707_name_good_addr_days,
		
		nox_rd_invalid_address_days AS algae3708_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3709_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3710_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3711_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3712_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3713_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3714_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3715_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3716_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3717_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3718_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3719_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3720_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3721_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3722_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3723_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3724_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3725_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3726_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3727_pm10_tot_good_addr_days,

		name_sum AS algae3728_name_sum,
		name_avg AS algae3729_name_avg,
		name_med AS algae3730_name_med,
		nox_rd_sum AS algae3731_nox_rd_sum,
		nox_rd_avg AS algae3732_nox_rd_avg,
		nox_rd_med AS algae3733_nox_rd_med,
		pm10_gr_sum AS algae3734_pm10_gr_sum,
		pm10_gr_avg AS algae3735_pm10_gr_avg,
		pm10_gr_med AS algae3736_pm10_gr_med,
		pm10_rd_sum AS algae3737_pm10_rd_sum,
		pm10_rd_avg AS algae3738_pm10_rd_avg,
		pm10_rd_med AS algae3739_pm10_rd_med,
		pm10_tot_sum AS algae3740_pm10_tot_sum,
		pm10_tot_avg AS algae3741_pm10_tot_avg,
		pm10_tot_med AS algae3742_pm10_tot_med
	FROM
		fin_stg_mob_exp
	ORDER BY
		person_id,
		ith_life_stage;		

	-- ---------------------------------------
	-- Generate exposure data difference files
	-- ---------------------------------------

	DROP TABLE IF EXISTS results_later_mob_cln_vs_unimp;
	CREATE TABLE results_later_mob_cln_vs_unimp AS
	SELECT
		person_id AS algae4700_person_id,
		life_stage AS algae4701_life_stage,		
		name_sum_percent_err AS algae4702_name_sum_percent_err,			
		name_avg_percent_err AS algae4703_name_avg_percent_err,
		name_med_percent_err AS algae4704_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4705_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4706_nox_rd_avg_percent_err,			
		nox_rd_med_percent_err AS algae4707_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4708_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4709_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4710_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4711_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4712_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4713_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4714_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4715_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4716_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_unimp
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_later_mob_cln_vs_no_mob;
	CREATE TABLE results_later_mob_cln_vs_no_mob AS
	SELECT
		person_id AS algae4800_person_id,
		life_stage AS algae4801_life_stage,
		name_sum_percent_err AS algae4802_name_sum_percent_err,			
		name_avg_percent_err AS algae4803_name_avg_percent_err,
		name_med_percent_err AS algae4804_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4805_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4806_nox_rd_avg_percent_err,			
		nox_rd_med_percent_err AS algae4807_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4808_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4809_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4810_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4811_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4812_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4813_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4814_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4815_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4816_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_no_mob
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_later_uncln_vs_no_mob;
	CREATE TABLE results_later_uncln_vs_no_mob AS
	SELECT
		person_id AS algae4900_person_id,
		life_stage AS algae4901_life_stage,
		name_sum_percent_err AS algae4902_name_sum_percent_err,			
		name_avg_percent_err AS algae4903_name_avg_percent_err,
		name_med_percent_err AS algae4904_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4905_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4906_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4907_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4908_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4909_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4910_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4911_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4912_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4913_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4914_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4915_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4916_pm10_tot_med_diff
	FROM
		fin_uncln_vs_no_mob
	ORDER BY
		person_id,
		ith_life_stage;

	-- ---------------------------------------
	-- Generate covariate results
	-- ---------------------------------------

	-- Generating covariates
	DROP TABLE IF EXISTS results_later_life_stages_cov;	
	CREATE TABLE results_later_life_stages_cov AS
	SELECT
		person_id AS algae5300_person_id,
		life_stage AS algae5301_life_stage,
		ed91 AS algae5302_ed91,
		oa2001 AS algae5303_oa2001,
		coa2011 AS algae5304_coa2011
	FROM
		fin_life_stages_cov
	ORDER BY
		person_id,
		ith_life_stage;
	
	DROP TABLE IF EXISTS results_later_moves_cov;
	CREATE TABLE results_later_moves_cov AS
	SELECT
		person_id AS algae5400_person_id,
		start_date_days_from_conception AS algae5401_mv_to_concept_days,
		ed91 AS algae5402_ed91,
		oa2001 AS algae5403_oa2001,
		coa2011 AS algae5404_coa2011
	FROM
		fin_moves_cov
	ORDER BY
		person_id;
	

	-- ---------------------------------------
	-- Generate sensitivity results
	-- ---------------------------------------

	DROP TABLE IF EXISTS results_later_sens_variables;
	CREATE TABLE results_later_sens_variables AS 
	SELECT
		person_id AS algae6300_person_id,
		at_1st_addr_conception AS algae6301_at_1st_addr_concept,
		absent_during_exp_period AS algae6302_absent_in_exp,
		gestation_age_at_birth AS algae6303_gest_age,	
		is_gestation_age_imputed AS algae6304_is_gest_age_imp,
		total_addr_periods AS algae6305_total_addr,
		fixed_geocodes AS algae6306_fixed_geocodes,
		over_laps AS algae6307_over_laps,
		gaps AS algae6308_gaps,
		gap_and_overlap_same_period AS algae6309_gap_over_lap,
		deletions AS algae6310_deletions,
		imp_blank_start_dates AS algae6311_cln_blank_start_date,
		imp_blank_end_dates AS algae6312_cln_blank_end_date,
		imp_blank_both_dates AS algae6313_cln_blank_both_dates,
		imp_last_dates AS algae6314_cln_last_dates,
		days_changed AS algae6315_days_changed,
		total_contention_days AS algae6316_contention_days,
		no_exposure_data_days AS algae6317_no_exp_data_days
	FROM
		fin_sens_variables
	ORDER BY
		person_id;

	DROP TABLE IF EXISTS results_later_stage_sens_variables;
	CREATE TABLE results_later_stage_sens_variables AS
	SELECT
		person_id AS algae6400_person_id,
		life_stage AS algae6401_life_stage,
		contention_days AS algae6402_contention_days,
		distinct_addresses AS algae6403_distinct_addr,
		moves AS algae6404_moves
	FROM
		fin_life_stage_sensitivity_variables;
	

END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION later_life_create_result_tables_and_backup
 * -----------------------------------------------------
 * Description
 * -----------
 * Dumps result tables for the late life analysis to CSV files.
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION later_life_create_reports(output_directory TEXT)
	RETURNS void AS 
$$
DECLARE

	results_later_life_stages_csv_file TEXT;
	results_later_mob_cln_exp_csv_file TEXT;
	results_later_mob_uncln_exp_csv_file TEXT;
	results_later_stg_mob_exp_csv_file TEXT;
	results_later_no_mob_birth_addr_exp_csv_file TEXT;
	results_later_life_stages_cov_csv_file TEXT;
	results_later_moves_cov_csv_file TEXT;
	results_later_sens_variables_csv_file TEXT;
	results_later_stage_sens_variables_csv_file TEXT;
	
	results_later_mob_cln_vs_uncln_csv_file TEXT;
	results_later_mob_cln_vs_stg_mob_csv_file TEXT;
	results_later_mob_uncln_vs_stg_mob_csv_file TEXT;	
	results_later_cleaned_addr_periods_csv_file TEXT;
	results_later_move_date_from_conception_csv_file TEXT;
	date_phrase TEXT;
	
BEGIN

	date_phrase :=
		(SELECT to_char(current_timestamp, 'YYYY-Mon-DD-HH24-MI'));


	-- -----------------------------------------------
	-- Generate life stage data file
	-- -----------------------------------------------

	results_later_life_stages_csv_file :=
		output_directory || 
		'\life_stage_data' ||
		'\results_later_life_stages_' || date_phrase || '.csv';
		
	RAISE NOTICE 'late life stages file==%==', results_later_life_stages_csv_file;
	EXECUTE format ('
	COPY results_later_life_stages
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_life_stages_csv_file);	


	-- ------------------------------------
	-- Generate cleaned address period file
	-- ------------------------------------
	results_later_cleaned_addr_periods_csv_file :=
		output_directory || 
		'\cleaned_address_history\res_later_cleaned_addr' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_cleaned_addr_periods
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_cleaned_addr_periods_csv_file);


	-- -----------------------------------------------
	-- Generate exposure result files
	-- -----------------------------------------------

	-- Generate result file for late mobility exposures with imputing
	results_later_mob_cln_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_clean\res_later_mob_cln_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_cln_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_cln_exp_csv_file);	

	-- Generate result file for late mobility exposures without imputing
	results_later_mob_uncln_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_unclean\res_later_mob_uncln_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_uncln_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_uncln_exp_csv_file);

	-- Generate result file for late exposures with no mobility
	-- Uses address at start of each life stage to represent entire
	-- life stage
	results_later_stg_mob_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_life_stage\res_later_stg_mob_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_stg_mob_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_stg_mob_exp_csv_file);

	-- Generate result file for late exposures with no mobility
	-- Uses address at birth to cover entire late life period
	/*
	results_later_no_mob_birth_addr_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\no_mobility\res_later_no_mob_birth_addr_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_stg_mob_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_stg_mob_exp_csv_file);
	*/

	-- -----------------------------------------------
	-- Generate exposure difference files
	-- -----------------------------------------------

	results_later_mob_cln_vs_uncln_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_later_mob_cln_vs_uncln_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_cln_vs_unimp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_cln_vs_uncln_csv_file);

	results_later_mob_cln_vs_stg_mob_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_later_mob_cln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_cln_vs_stg_mob_csv_file);

	results_later_mob_uncln_vs_stg_mob_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_later_mob_uncln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_uncln_vs_stg_mob_csv_file);


	-- -----------------------------------------------
	-- Generate covariate files
	-- -----------------------------------------------

	results_later_life_stages_cov_csv_file :=
		output_directory || 
		'\covariates\res_later_cov_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_life_stages_cov
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_life_stages_cov_csv_file);

	results_later_moves_cov_csv_file :=
		output_directory || 
		'\covariates\res_later_moves_cov_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_moves_cov
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_moves_cov_csv_file);

	-- -----------------------------------------------
	-- Generate sensitivity variable files
	-- -----------------------------------------------

	results_later_sens_variables_csv_file :=
		output_directory || 
		'\sensitivity_variables\res_later_sens_variables_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_sens_variables
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_sens_variables_csv_file);


	results_later_stage_sens_variables_csv_file :=
		output_directory || 
		'\sensitivity_variables\res_later_stage_sens_variables_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_stage_sens_variables
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_stage_sens_variables_csv_file);


END;
$$   LANGUAGE plpgsql;




