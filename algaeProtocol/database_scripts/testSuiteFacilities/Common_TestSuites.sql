/**
 * ================================================================================================= 
 * MODULE TEST_SUITES: Test Suites
 * =================================================================================================
 * Description
 * -----------
 * This module contains code for running an automated test suite, which tests various areas of 
 * functionality in the ALGAE protocol.
 * Assumptions
 * -----------
 * Directories containing test data should already be installed before the test suite is run.
 *
 * Main Function
 * -------------
 *    run_test_suite
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
 * FUNCTION test_preprocessing
 * -----------------------------
 * Description
 * -----------
 * Verifies that variables found in the fin_addr_periods table and variables found in the
 * sensitivity variables table reflect correct behaviour.
 * ------------------------------------------------------------------------------------------------ 
 */

CREATE OR REPLACE FUNCTION test_preprocessing(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	expected_results_directory TEXT;

	expected_geocode_flags_file TEXT;
	expected_study_member_flags_file TEXT;

BEGIN

	test_suite_directory 
		:= test_directory || 
		'\preprocessing_data\early_life\input_data';
	PERFORM load_original_test_data(test_suite_directory);

	PERFORM comm_preprocess_staging_tables();
	PERFORM comm_perform_prelim_system_checks();

	-- we are only interested in looking at the staging tables to make sure
	-- that pre-processing has been done correctly.

	expected_results_directory
		:= test_directory ||
			'\preprocessing_data\early_life\expected_results';

	expected_geocode_flags_file 
		:= expected_results_directory || 
		'\expected_geocode_flags.csv';

	DROP TABLE IF EXISTS test_preprocessing_geocodes;
	CREATE TABLE test_preprocessing_geocodes (
		geocode TEXT,
		comments TEXT,
		has_valid_geocode TEXT
	);
	
	EXECUTE format ('
	COPY test_preprocessing_geocodes (	
		geocode,
		comments,
		has_valid_geocode) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_geocode_flags_file);

	
	expected_study_member_flags_file 
		:= expected_results_directory || 
		'\expected_study_member_flags.csv';

	DROP TABLE IF EXISTS test_preprocessing_study_member_data;
	CREATE TABLE test_preprocessing_study_member_data (
		person_id TEXT,
		comments TEXT,
		absent_during_exp_period TEXT,
		at_1st_addr_conception TEXT
	);
	
	EXECUTE format ('
	COPY test_preprocessing_study_member_data (	
		person_id,
		comments,
		absent_during_exp_period,
		at_1st_addr_conception) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_study_member_flags_file);

	DROP TABLE IF EXISTS test_preprocessing_expected1;
	CREATE TABLE test_preprocessing_expected1 AS 
	SELECT
		a.geocode,
		a.comments,
		CASE
			WHEN a.has_valid_geocode = b.has_valid_geocode THEN
				TRUE
			ELSE
				FALSE
		END is_has_valid_geocode_correct
	FROM
		test_preprocessing_geocodes a,
		staging_geocode_data b
	WHERE
		a.geocode = b.geocode
	ORDER BY
		a.geocode;

	INSERT INTO tmp_all_test_case_results 
	SELECT
		geocode AS test_case_name,
		'Preprocessing: Geocode Flags' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_has_valid_geocode_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'	
		END AS pass_or_fail
	FROM
		test_preprocessing_expected1
	ORDER BY
		geocode;

	DROP TABLE IF EXISTS test_preprocessing_expected2;
	CREATE TABLE test_preprocessing_expected2 AS 
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN a.absent_during_exp_period = b.absent_during_exp_period THEN
				TRUE
			ELSE
				FALSE
		END AS is_absent_during_exp_period_correct,
		CASE
			WHEN a.at_1st_addr_conception = b.at_1st_addr_conception THEN
				TRUE
			ELSE
				FALSE
		END AS is_at_1st_addr_conception_correct		
	FROM
		test_preprocessing_study_member_data a,
		staging_study_member_data b
	WHERE
		a.person_id = b.person_id
	ORDER BY
		a.person_id;
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Preprocessing: Study Member Flags' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_absent_during_exp_period_correct = FALSE OR
				is_at_1st_addr_conception_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'	
		END AS pass_or_fail
	FROM
		test_preprocessing_expected2
	ORDER BY
		person_id;

	-- Cleanup
	DROP TABLE test_preprocessing_geocodes;
	DROP TABLE test_preprocessing_study_member_data;
	DROP TABLE test_preprocessing_expected1;
	DROP TABLE test_preprocessing_expected2;	

END;

$$   LANGUAGE plpgsql;
--SELECT "test_preprocessing"('C:\algae_protocol\test_environment');

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION test_addr_periods
 * -----------------------------
 * Description
 * -----------
 * Verifies that variables found in the fin_addr_periods table and variables found in the
 * sensitivity variables table reflect correct behaviour.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION test_address_histories(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	expected_results_directory TEXT;
	expected_results_file TEXT;
	expected_sens_results_file TEXT;

	expected_stage_sens_results_file TEXT;
	
	number_of_mismatches INT;

BEGIN
	test_suite_directory := test_directory || '\address_history_data\early_life\input_data';
	output_directory := test_directory || '\address_history_data\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	expected_results_directory := test_directory || '\address_history_data\early_life\expected_results';
	expected_results_file := expected_results_directory || '\expected_address_period_results.csv';
	expected_sens_results_file 
		:= expected_results_directory || 
			'\expected_address_periods_sens_results.csv';

	expected_stage_sens_results_file
		:= expected_results_directory || 
			'\expected_address_periods_stage_sens_results.csv';


	DROP TABLE IF EXISTS test_addr_period_fields_expected1;
	CREATE TABLE test_addr_period_fields_expected1 (
		person_id TEXT,
		ith_residence INT,
		comments TEXT, -- comment field, often used in testing
		date_state TEXT,
		start_date DATE,
		end_date TEXT,
		fin_adjusted_start_date DATE,
		fin_adjusted_end_date TEXT,
		fit_type TEXT,
		days_changed INT,
		geocode TEXT,
		start_date_contender TEXT,
		start_date_delta1 DATE,
		start_date_delta2 DATE,
		end_date_contender TEXT,
		end_date_delta1 DATE,
		end_date_delta2 DATE,
		imputed_first_start TEXT,
		imputed_last_end TEXT
	);

	EXECUTE format ('
	COPY test_addr_period_fields_expected1 (	
		person_id,
		ith_residence,
		comments, -- comment field, often used in testing
		date_state,
		start_date,
		end_date,
		fin_adjusted_start_date,
		fin_adjusted_end_date,
		fit_type,
		days_changed,
		geocode,
		start_date_contender,
		start_date_delta1,
		start_date_delta2,
		end_date_contender,
		end_date_delta1,
		end_date_delta2,
		imputed_first_start,
		imputed_last_end) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_results_file);

	DROP TABLE IF EXISTS test_addr_period_fields_expected2;
	CREATE TABLE test_addr_period_fields_expected2 AS 
	SELECT
		person_id,
		ith_residence,
		comments,
		date_state,
		start_date,
		CASE
			WHEN end_date = 'now' THEN
				NOW() -- this is a way making sure that expected value changes as time goes on
			ELSE
				end_date::date
		END AS end_date,
		fin_adjusted_start_date,
		CASE
			WHEN fin_adjusted_end_date = 'now' THEN
				NOW()::date 
			ELSE
				fin_adjusted_end_date::date
		END fin_adjusted_end_date,
		fit_type,
		days_changed,
		geocode,
		start_date_contender,
		start_date_delta1,
		start_date_delta2,
		end_date_contender,
		end_date_delta1,
		end_date_delta2,
		imputed_first_start,
		imputed_last_end		
	FROM
		test_addr_period_fields_expected1;

	DROP TABLE IF EXISTS results_test_addr_periods1;
	CREATE TABLE results_test_addr_periods1 AS
	SELECT
		a.person_id,
		a.comments,
		a.ith_residence,
		CASE
			WHEN COALESCE(a.date_state::text, 'null') = COALESCE(b.date_state::text, 'null') THEN
				TRUE
		ELSE
			FALSE
		END AS is_date_state_correct,	       
		CASE
			WHEN COALESCE(a.fin_adjusted_start_date::text, 'null') = COALESCE(b.fin_adjusted_start_date::text, 'null') THEN
		TRUE
		ELSE
			FALSE
		END AS is_fin_adj_start_date_correct,
		CASE
			WHEN COALESCE(a.fin_adjusted_end_date::text, 'null') = COALESCE(b.fin_adjusted_end_date::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_fin_adj_end_date_correct,
		CASE
			WHEN COALESCE(a.start_date_delta1::text, 'null') = COALESCE(b.start_date_delta1::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_start_date_delta1_correct,
		CASE
			WHEN COALESCE(a.start_date_delta2::text, 'null') = COALESCE(b.start_date_delta2::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_start_date_delta2_correct,
		CASE
			WHEN COALESCE(a.end_date_delta1::text, 'null') = COALESCE(b.end_date_delta1::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_end_date_delta1_correct,
		CASE
			WHEN COALESCE(a.end_date_delta2::text, 'null') = COALESCE(b.end_date_delta2::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_end_date_delta2_correct,
		CASE
			WHEN COALESCE(a.start_date_contender::text, 'null') = COALESCE(b.start_date_contender::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_start_date_contender_correct,
		CASE
			WHEN COALESCE(a.end_date_contender::text, 'null') = COALESCE(b.end_date_contender::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_end_date_contender_correct,
		CASE
			WHEN COALESCE(a.fit_type::text, 'null') = COALESCE(b.fit_type::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_fit_type_correct,
		CASE
			WHEN COALESCE(a.days_changed::text, 'null') = COALESCE(b.days_changed::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_days_changed_correct,
		CASE
			WHEN COALESCE(a.imputed_first_start::text, 'null') = COALESCE(b.imputed_first_start::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imputed_first_start_correct,
		CASE
			WHEN COALESCE(a.imputed_last_end::text, 'null') = COALESCE(b.imputed_last_end::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imputed_last_end_correct
	FROM
		test_addr_period_fields_expected2 a,
		fin_cleaned_addr_periods b
	WHERE
		a.person_id = b.person_id AND
		a.ith_residence = b.ith_residence
	ORDER BY
		a.person_id,
		a.ith_residence;
		
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Address History Data' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_date_state_correct = FALSE OR 
				is_fin_adj_start_date_correct = FALSE OR 
				is_fin_adj_end_date_correct = FALSE OR 
				is_start_date_delta1_correct = FALSE OR 
				is_start_date_delta2_correct = FALSE OR 
				is_end_date_delta1_correct = FALSE OR 
				is_end_date_delta2_correct = FALSE OR 
				is_start_date_contender_correct = FALSE OR 
				is_end_date_contender_correct = FALSE OR 
				is_fit_type_correct = FALSE OR 
				is_days_changed_correct = FALSE OR 
				is_imputed_first_start_correct = FALSE OR 
				is_imputed_last_end_correct = FALSE THEN

				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_addr_periods1
	ORDER BY
		person_id;

	

	DROP TABLE IF EXISTS test_early_addr_periods_sens_expected;
	CREATE TABLE test_early_addr_periods_sens_expected (
		person_id TEXT,
		comments TEXT,
		over_laps INT,
		gaps INT,
		gap_and_overlap_same_period INT,
		deletions INT,
		imp_blank_start_dates INT,
		imp_blank_end_dates INT,
		imp_blank_both_dates INT,
		imp_last_dates INT,
		days_changed INT,
		total_addr_periods INT,
		total_contention_days INT
	);

	EXECUTE format ('
	COPY test_early_addr_periods_sens_expected (	
		person_id,
		comments,
		over_laps,
		gaps,
		gap_and_overlap_same_period,
		deletions,
		imp_blank_start_dates,
		imp_blank_end_dates,
		imp_blank_both_dates,
		imp_last_dates,
		days_changed,
		total_addr_periods,
		total_contention_days)
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_sens_results_file);

	DROP TABLE IF EXISTS results_test_addr_periods2;
	CREATE TABLE results_test_addr_periods2 AS
	WITH expected_sensitivity_variables AS
		(SELECT
			person_id,
			comments,
			over_laps,
			gaps,
			gap_and_overlap_same_period,
			deletions,
			imp_blank_start_dates,
			imp_blank_end_dates,
			imp_blank_both_dates,
			imp_last_dates,
			days_changed,
			total_addr_periods,
			total_contention_days
		 FROM
		 	test_early_addr_periods_sens_expected),
	actual_sensitivity_variables AS
		(SELECT
			person_id,
			over_laps,
			gaps,
			gap_and_overlap_same_period,
			deletions,
			imp_blank_start_dates,
			imp_blank_end_dates,
			imp_blank_both_dates,
			imp_last_dates,
			days_changed,
			total_addr_periods,
			total_contention_days
		 FROM
		 	fin_sens_variables)	
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN COALESCE(a.over_laps::text, 'null') = COALESCE(b.over_laps::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_over_laps_correct,
		CASE
			WHEN COALESCE(a.gaps::text, 'null') = COALESCE(b.gaps::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_gaps_correct,
		CASE
			WHEN COALESCE(a.gap_and_overlap_same_period::text, 'null') = COALESCE(b.gap_and_overlap_same_period::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_gap_and_overlap_same_period_correct,
		CASE
			WHEN COALESCE(a.deletions::text, 'null') = COALESCE(b.deletions::text, 'null') THEN
				TRUE
			ELSE
				FALSE				
		END AS is_deletions_correct,
		CASE
			WHEN COALESCE(a.imp_blank_start_dates::text, 'null') = COALESCE(b.imp_blank_start_dates::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imp_blank_start_dates_correct,
		CASE
			WHEN COALESCE(a.imp_blank_end_dates::text, 'null') = COALESCE(b.imp_blank_end_dates::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imp_blank_end_dates_correct,			
		CASE
			WHEN COALESCE(a.imp_blank_both_dates::text, 'null') = COALESCE(b.imp_blank_both_dates::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imp_blank_both_dates_correct,
		CASE
			WHEN COALESCE(a.imp_last_dates::text, 'null') = COALESCE(b.imp_last_dates::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imp_last_dates_correct,
		CASE
			WHEN COALESCE(a.days_changed::text, 'null') = COALESCE(b.days_changed::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_days_changed_correct,
		CASE
			WHEN COALESCE(a.total_addr_periods::text, 'null') = COALESCE(b.total_addr_periods::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_total_addr_periods_correct,
		CASE
			WHEN COALESCE(a.total_contention_days::text, 'null') = COALESCE(b.total_contention_days::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_total_contention_days_correct				
	FROM
		 expected_sensitivity_variables a,
		 actual_sensitivity_variables b
	WHERE
		a.person_id = b.person_id;

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Address Cleaning Sensitivity Variables' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_total_addr_periods_correct = FALSE OR
				is_over_laps_correct = FALSE OR 
				is_gaps_correct = FALSE OR 
				is_gap_and_overlap_same_period_correct = FALSE OR 
				is_deletions_correct = FALSE OR 
				is_imp_blank_start_dates_correct = FALSE OR 
				is_imp_blank_start_dates_correct = FALSE OR 
				is_imp_blank_end_dates_correct = FALSE OR 
				is_imp_blank_both_dates_correct = FALSE OR 
				is_imp_last_dates_correct = FALSE OR 
				is_days_changed_correct = FALSE OR 
				is_total_addr_periods_correct = FALSE OR
				is_total_contention_days_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
		FROM
			results_test_addr_periods2
		ORDER BY
			person_id;
	
	
	DROP TABLE IF EXISTS test_early_addr_periods_stage_sens_expected;
	CREATE TABLE test_early_addr_periods_stage_sens_expected (
		person_id TEXT,
		comments TEXT,
		life_stage TEXT,
		contention_days INT,
		distinct_addresses INT,
		moves INT
	);

	EXECUTE format ('
	COPY test_early_addr_periods_stage_sens_expected (	
		person_id,
		comments,
		life_stage,
		contention_days,
		distinct_addresses,
		moves)
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_stage_sens_results_file);
	
	DROP TABLE IF EXISTS results_test_addr_periods3;
	CREATE TABLE results_test_addr_periods3 AS
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN COALESCE(a.contention_days::text, 'null') = COALESCE(b.contention_days::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_contention_days_correct,
		CASE
			WHEN COALESCE(a.distinct_addresses::text, 'null') = COALESCE(b.distinct_addresses::text, null) THEN
				TRUE
			ELSE
				FALSE
		END AS is_distinct_addresses_correct,
		CASE
			WHEN COALESCE(a.moves::text, 'null') = COALESCE(b.moves::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_moves_correct
	FROM
		test_early_addr_periods_stage_sens_expected a,
		fin_life_stage_sensitivity_variables b
	WHERE
		a.person_id = b.person_id AND
		a.life_stage = b.life_stage
	ORDER BY
		a.person_id;

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Address Cleaning Life Stage Sensitivity Variables' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_contention_days_correct = FALSE OR 
				is_distinct_addresses_correct = FALSE OR 
				is_moves_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
		FROM
			results_test_addr_periods3
		ORDER BY
			person_id;
	
	-- Cleanup
	--DROP TABLE test_addr_period_fields_expected1;
	--DROP TABLE test_addr_period_fields_expected2;
	--DROP TABLE test_early_addr_periods_sens_expected;	
	--DROP TABLE results_test_addr_periods1;
	--DROP TABLE results_test_addr_periods2;
	--DROP TABLE results_test_addr_periods3;
	
END;

$$   LANGUAGE plpgsql;
--SELECT "test_addr_periods"('C:\algae_protocol\test_environment');

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION test_geocode_data
 * --------------------------
 * Description
 * -----------
 * Verifies that variables found in the fin_addr_periods table and variables found in the
 * sensitivity variables table reflect correct behaviour.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION test_geocode_data(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	expected_results_directory TEXT;
	expected_results_file TEXT;
	expected_sens_results_file TEXT;
	number_of_mismatches INT;

BEGIN
	test_suite_directory := 
		test_directory || '\geocode_data\early_life\input_data';
	output_directory :=
		test_directory || '\geocode_data\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	expected_results_directory := test_directory || '\geocode_data\early_life\expected_results';
	expected_results_file := expected_results_directory || '\expected_results.csv';
	expected_sens_results_file := expected_results_directory || '\expected_sensitivity_variable_results.csv';

	DROP TABLE IF EXISTS test_early_bad_geocodes_expected;
	CREATE TABLE test_early_bad_geocodes_expected (
		person_id TEXT,
		ith_residence INT,
		comments TEXT, -- comment field, often used in testing
		geocode TEXT, --(x,y) coordinates. Typically "x-y" will be the actual geocode value
		start_date DATE,
		end_date DATE,
		early_is_fixed_invalid_geocode TEXT,
		ith_residence_type TEXT,
		fin_adjusted_start_date DATE,
		fin_adjusted_end_date DATE
	);

	EXECUTE format ('
	COPY test_early_bad_geocodes_expected (	
		person_id,
		ith_residence,
		comments,
		geocode,
		start_date,
		end_date,
		early_is_fixed_invalid_geocode,
		ith_residence_type,
		fin_adjusted_start_date,
		fin_adjusted_end_date) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_results_file);

		--has_valid_geocode,
		--has_exposures,
		--is_out_of_bounds,


	WITH expected_actual_comparisons AS
		(SELECT
			a.person_id,
			a.comments,
			a.ith_residence,
			CASE
				WHEN COALESCE(a.start_date::text, 'null') = COALESCE(b.start_date::text, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_start_date_correct,
			CASE
				WHEN COALESCE(a.end_date::text, 'null') = COALESCE(b.end_date::text, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_end_date_correct,
			CASE
				WHEN COALESCE(a.early_is_fixed_invalid_geocode, 'null') = COALESCE(b.is_fixed_invalid_geocode, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_early_is_fixed_bad_geocode_correct,
			CASE
				WHEN COALESCE(a.ith_residence_type, 'null') = COALESCE(b.ith_residence_type, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_ith_residence_type_correct,
			CASE
				WHEN COALESCE(a.fin_adjusted_start_date::text, 'null') = COALESCE(b.fin_adjusted_start_date::text, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_fin_adjusted_start_date_correct,
			CASE
				WHEN COALESCE(a.fin_adjusted_end_date::text, 'null') = COALESCE(b.fin_adjusted_end_date::text, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_fin_adjusted_end_date_correct	
	FROM
		test_early_bad_geocodes_expected a,
		fin_cleaned_addr_periods b
	WHERE
		a.person_id = b.person_id AND
		a.ith_residence = b.ith_residence
	ORDER BY
		a.person_id,
		a.ith_residence) 
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Geocode Data' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_start_date_correct = FALSE OR
				is_end_date_correct = FALSE OR
				is_early_is_fixed_bad_geocode_correct = FALSE OR
				is_ith_residence_type_correct = FALSE OR
				is_fin_adjusted_start_date_correct = FALSE OR
				is_fin_adjusted_end_date_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		expected_actual_comparisons
	ORDER BY
		person_id;



	DROP TABLE IF EXISTS test_early_bad_geocodes_sens_expected;
	CREATE TABLE test_early_bad_geocodes_sens_expected (
		person_id TEXT,
		comments TEXT, -- comment field, often used in testing
		total_addr_periods INT,
		invalid_geocodes INT,
		fixed_geocodes INT
	);

	EXECUTE format ('
	COPY test_early_bad_geocodes_sens_expected (	
		person_id,
		comments,
		total_addr_periods,
		invalid_geocodes,
		fixed_geocodes)
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_sens_results_file);

	DROP TABLE IF EXISTS results_test_bad_geocodes1;
	CREATE TABLE results_test_bad_geocodes1 AS
	WITH expected_sensitivity_variables AS
		(SELECT
			person_id,
			comments,
			total_addr_periods,
			invalid_geocodes,
			fixed_geocodes
		 FROM
		 	test_early_bad_geocodes_sens_expected),
	actual_sensitivity_variables AS
		(SELECT
			person_id,
			total_addr_periods,
			invalid_geocodes,
			fixed_geocodes
		 FROM
		 	fin_sens_variables)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN a.total_addr_periods = b.total_addr_periods THEN
				TRUE
			ELSE
				FALSE
		END AS is_total_addr_periods_correct,
		CASE
			WHEN a.invalid_geocodes = b.invalid_geocodes THEN
				TRUE
			ELSE
				FALSE
		END AS is_invalid_geocodes_correct,
		CASE
			WHEN a.fixed_geocodes = b.fixed_geocodes THEN
				TRUE
			ELSE
				FALSE				
		END AS is_fixed_geocodes_correct
	 FROM
	 	expected_sensitivity_variables a,
	 	actual_sensitivity_variables b
	 WHERE
	 	a.person_id = b.person_id;
		 			 	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Geocode Data' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_total_addr_periods_correct = FALSE OR
				is_invalid_geocodes_correct = FALSE OR
				is_fixed_geocodes_correct = FALSE THEN 
			'FAIL'
		ELSE
			'PASS'
		END AS pass_or_fail
	FROM
		results_test_bad_geocodes1
	ORDER BY
		person_id;
	
	-- Cleanup
	--DROP TABLE test_early_bad_geocodes_expected;	
	--DROP TABLE test_early_bad_geocodes_sens_expected;
	--DROP TABLE results_test_bad_geocodes1;
	
END;

$$   LANGUAGE plpgsql;

--SELECT "test_bad_geocodes"('C:\algae_protocol\test_environment');

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION test_study_member_data
 * -------------------------------
 * Description
 * -----------
 * Verifies that variables related to life stage calculates behave correctly.
 *
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION test_study_member_data(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	
	expected_results_directory TEXT;
	expected_life_stage_results_file TEXT;
	expected_life_stage_sens_results_file TEXT;
	
	expected_study_member_flags_file TEXT;	
	
	
	number_of_mismatches INT;

BEGIN

	test_suite_directory := 
		test_directory || '\study_member_data\early_life\input_data';
	output_directory :=
		test_directory || '\study_member_data\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	output_directory := test_directory || '\study_member_data\early_life\results';
	expected_results_directory := test_directory || '\study_member_data\early_life\expected_results';

	-- =================================================
	-- Test sensitivity variables
	-- =================================================	

	expected_life_stage_sens_results_file :=
		expected_results_directory || '\expected_life_stage_sens_results.csv';

	DROP TABLE IF EXISTS test_expected_life_stage_sens_results;
	CREATE TABLE test_expected_life_stage_sens_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   gestation_age_at_birth INT,
	   is_gestation_age_imputed TEXT,
	   absent_during_exp_period TEXT,
	   at_1st_addr_conception TEXT
	);

	EXECUTE format ('
	COPY test_expected_life_stage_sens_results (	
		person_id,
		comments, -- comment field, often used in testing
		gestation_age_at_birth,
		is_gestation_age_imputed,
		absent_during_exp_period,
		at_1st_addr_conception) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_life_stage_sens_results_file);

	-- now compare expected vs actual results	
	DROP TABLE IF EXISTS results_test_life_stages1;
	CREATE TABLE results_test_life_stages1 AS	
	WITH expected_sensitivity_results AS
		(SELECT
			person_id,
			comments,
			gestation_age_at_birth,
			is_gestation_age_imputed,
			absent_during_exp_period,
			at_1st_addr_conception
		 FROM
		 	test_expected_life_stage_sens_results),
	actual_sensitivity_results AS
		(SELECT
			person_id,
			gestation_age_at_birth,
			is_gestation_age_imputed,
			absent_during_exp_period,
			at_1st_addr_conception
		 FROM
		 	fin_sens_variables)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN a.gestation_age_at_birth = b.gestation_age_at_birth THEN
				TRUE
			ELSE
				FALSE
		END AS is_gestation_age_at_birth_correct,
		CASE
			WHEN a.is_gestation_age_imputed = b.is_gestation_age_imputed THEN
				TRUE
			ELSE
				FALSE
		END AS is_gestation_age_flag_correct,
		CASE
			WHEN a.absent_during_exp_period = b.absent_during_exp_period THEN
				TRUE
			ELSE
				FALSE
		END AS is_absent_during_exp_period_correct,
		CASE
			WHEN a.at_1st_addr_conception = b.at_1st_addr_conception THEN
				TRUE
			ELSE
				FALSE
		END AS is_at_1st_addr_conception_correct			
	 FROM
	 	expected_sensitivity_results a,
	 	actual_sensitivity_results b
	 WHERE
	 	a.person_id = b.person_id;
	 		 	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Study Member Data' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_gestation_age_at_birth_correct = FALSE OR
				is_gestation_age_flag_correct = FALSE OR 	      	 
			 	is_at_1st_addr_conception_correct = FALSE OR
			 	is_absent_during_exp_period_correct = FALSE OR 
			 	is_at_1st_addr_conception_correct = FALSE THEN 
			 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_life_stages1
	ORDER BY
		person_id;

	-- =================================================
	-- Test life stage variables
	-- =================================================	
	expected_life_stage_results_file :=
		expected_results_directory || '\expected_life_stage_results.csv';

	DROP TABLE IF EXISTS test_expected_life_stage_results;
	CREATE TABLE test_expected_life_stage_results (
		person_id TEXT,
		comments TEXT, -- comment field, often used in testing
		life_stage TEXT,
		start_date DATE,
		end_date DATE,
		life_stage_duration INT);

	EXECUTE format ('
	COPY test_expected_life_stage_results (	
		person_id,
		comments,
		life_stage,
		start_date,
		end_date,
		life_stage_duration) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_life_stage_results_file);

	DROP TABLE IF EXISTS results_test_life_stages2;
	CREATE TABLE results_test_life_stages2 AS
	WITH expected_life_stage_results AS
		(SELECT
			person_id,
			comments,
			life_stage,
			start_date,
			end_date,
			life_stage_duration
		 FROM
		 	test_expected_life_stage_results),
	actual_life_stage_results AS
		(SELECT
			person_id,
			life_stage,
			start_date,
			end_date,
			life_stage_duration
		 FROM
		 	fin_general_life_stage_data)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN COALESCE(a.start_date::text, 'null') = COALESCE(b.start_date::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_start_date_correct,
		CASE
			WHEN COALESCE(a.end_date::text, 'null') = COALESCE(b.end_date::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_end_date_correct,
		CASE
			WHEN COALESCE(a.life_stage_duration::text, 'null') = COALESCE(b.life_stage_duration::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_life_stage_duration_correct
	FROM
		expected_life_stage_results a,
		actual_life_stage_results b
	WHERE
		a.person_id = b.person_id AND
		a.life_stage = b.life_stage
	ORDER BY
		a.person_id,
		a.life_stage;
				
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Study Member Data' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_start_date_correct = FALSE OR
				is_end_date_correct = FALSE OR
				is_life_stage_duration_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'	
		END AS pass_or_fail
	FROM
		results_test_life_stages2
	ORDER BY
		person_id;


	expected_study_member_flags_file :=
		expected_results_directory || '\expected_study_member_flags.csv';

	DROP TABLE IF EXISTS test_expected_study_member_flag_results;
	CREATE TABLE test_expected_study_member_flag_results (
		person_id TEXT,
		comments TEXT, -- comment field, often used in testing
		absent_during_exp_period TEXT,
		at_1st_addr_conception TEXT);

	EXECUTE format ('
	COPY test_expected_study_member_flag_results (	
		person_id,
		comments,
		absent_during_exp_period,
		at_1st_addr_conception) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_study_member_flags_file);

	DROP TABLE IF EXISTS results_test_life_stages3;
	CREATE TABLE results_test_life_stages3 AS
	WITH expected_life_stage_results AS
		(SELECT
			person_id,
			comments,
			absent_during_exp_period,
			at_1st_addr_conception
		 FROM
		 	test_expected_study_member_flag_results),
	actual_life_stage_results AS
		(SELECT
			person_id,
			absent_during_exp_period,
			at_1st_addr_conception
		 FROM
		 	fin_sens_variables)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN COALESCE(a.absent_during_exp_period::text, 'null') = COALESCE(b.absent_during_exp_period::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_absent_during_exp_period_correct,
		CASE
			WHEN COALESCE(a.at_1st_addr_conception::text, 'null') = COALESCE(b.at_1st_addr_conception::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_at_1st_addr_conception_correct		
	FROM
		expected_life_stage_results a,
		actual_life_stage_results b
	WHERE
		a.person_id = b.person_id
	ORDER BY
		a.person_id;

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Study Member Data' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_absent_during_exp_period_correct = FALSE OR
				is_at_1st_addr_conception_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'	
		END AS pass_or_fail
	FROM
		results_test_life_stages3
	ORDER BY
		person_id;


	-- Cleanup
	DROP TABLE test_expected_life_stage_sens_results;
	DROP TABLE test_expected_life_stage_results;
	DROP TABLE test_expected_study_member_flag_results;
	-- DROP TABLE results_test_life_stages1;
	-- DROP TABLE results_test_life_stages2;
	-- DROP TABLE results_test_life_stages3;

END;

$$   LANGUAGE plpgsql;
--SELECT "test_life_stages"('C:\algae_protocol\test_environment');





CREATE OR REPLACE FUNCTION test_early_dropped_exposures(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;	
	expected_results_directory TEXT;

	early_cln_exp_dq_results_file TEXT;	
	early_uncln_exp_dq_results_file TEXT;
	early_stg_exp_dq_results_file TEXT;
	early_birth_addr_exp_dq_results_file TEXT;

	number_of_mismatches INT;

BEGIN

	test_suite_directory := 
		test_directory || '\dropped_exposures\early_life\input_data';
	output_directory :=
		test_directory || '\dropped_exposures\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	-- =================================================
	-- Test sensitivity variables
	-- =================================================	
	
	expected_results_directory 
		:= test_directory || '\dropped_exposures\early_life\expected_results';


	early_cln_exp_dq_results_file :=
		expected_results_directory || '\early_cln_exp_dq_results.csv';

	DROP TABLE IF EXISTS test_early_cln_exp_dq_results;
	CREATE TABLE test_early_cln_exp_dq_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   inv_addr_days INT,
	   oob_addr_days INT,
	   poor_addr_days INT,
	   missing_days INT,
	   good_addr_days INT
	);

	EXECUTE format ('
	COPY test_early_cln_exp_dq_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		inv_addr_days,
		oob_addr_days,
		poor_addr_days,
		missing_days,
		good_addr_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_cln_exp_dq_results_file);


	early_uncln_exp_dq_results_file :=
		expected_results_directory || '\early_uncln_exp_dq_results.csv';

	DROP TABLE IF EXISTS test_early_uncln_exp_dq_results;
	CREATE TABLE test_early_uncln_exp_dq_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   inv_addr_days INT,
	   oob_addr_days INT,
	   poor_addr_days INT,
	   missing_days INT,
	   good_addr_days INT
	);

	EXECUTE format ('
	COPY test_early_uncln_exp_dq_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		inv_addr_days,
		oob_addr_days,
		poor_addr_days,
		missing_days,
		good_addr_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_uncln_exp_dq_results_file);

	early_stg_exp_dq_results_file :=
		expected_results_directory || '\early_stg_exp_dq_results.csv';

	DROP TABLE IF EXISTS test_early_stg_exp_dq_results;
	CREATE TABLE test_early_stg_exp_dq_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   inv_addr_days INT,
	   oob_addr_days INT,
	   poor_addr_days INT,
	   missing_days INT,
	   good_addr_days INT
	);

	EXECUTE format ('
	COPY test_early_stg_exp_dq_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		inv_addr_days,
		oob_addr_days,
		poor_addr_days,
		missing_days,
		good_addr_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_stg_exp_dq_results_file);


	early_birth_addr_exp_dq_results_file :=
		expected_results_directory || '\early_birth_addr_exp_dq_results.csv';

	DROP TABLE IF EXISTS test_early_birth_addr_exp_dq_results;
	CREATE TABLE test_early_birth_addr_exp_dq_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   inv_addr_days INT,
	   oob_addr_days INT,
	   poor_addr_days INT,
	   missing_days INT,
	   good_addr_days INT
	);

	EXECUTE format ('
	COPY test_early_birth_addr_exp_dq_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		inv_addr_days,
		oob_addr_days,
		poor_addr_days,
		missing_days,
		good_addr_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_birth_addr_exp_dq_results_file);

	DROP TABLE IF EXISTS results_early_uncln_exp_dq_results;
	CREATE TABLE results_early_uncln_exp_dq_results AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			inv_addr_days AS pm10_tot_invalid_address_days,
			oob_addr_days AS pm10_tot_oob_days,
			poor_addr_days AS pm10_tot_poor_address_days,
			missing_days AS pm10_tot_missing_exp_days,
			good_addr_days AS pm10_tot_good_address_days
		 FROM
		 	test_early_uncln_exp_dq_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_invalid_address_days,
			pm10_tot_oob_days,
			pm10_tot_poor_address_days,
			pm10_tot_missing_exp_days,
			pm10_tot_good_address_days
		 FROM
		 	fin_mob_uncln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,		
		CASE
			WHEN a.pm10_tot_invalid_address_days = b.pm10_tot_invalid_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_invalid_count_correct,
		CASE
			WHEN a.pm10_tot_oob_days = b.pm10_tot_oob_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_oob_count_correct,
		CASE
			WHEN a.pm10_tot_poor_address_days = b.pm10_tot_poor_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_poor_count_correct,
		CASE
			WHEN a.pm10_tot_missing_exp_days = b.pm10_tot_missing_exp_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_missing_count_correct,
		CASE
			WHEN a.pm10_tot_good_address_days = b.pm10_tot_good_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_good_count_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 	
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Dropped Exposures: Uncleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_invalid_count_correct = FALSE OR
				is_oob_count_correct = FALSE OR
				is_poor_count_correct = FALSE OR
				is_missing_count_correct = FALSE OR
				is_good_count_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_early_uncln_exp_dq_results
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_early_stg_exp_dq_results;
	CREATE TABLE results_early_stg_exp_dq_results AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			inv_addr_days AS pm10_tot_invalid_address_days,
			oob_addr_days AS pm10_tot_oob_days,
			poor_addr_days AS pm10_tot_poor_address_days,
			missing_days AS pm10_tot_missing_exp_days,
			good_addr_days AS pm10_tot_good_address_days
		 FROM
		 	test_early_stg_exp_dq_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_invalid_address_days,
			pm10_tot_oob_days,
			pm10_tot_poor_address_days,
			pm10_tot_missing_exp_days,
			pm10_tot_good_address_days
		 FROM
		 	fin_stg_mob_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,		
		CASE
			WHEN a.pm10_tot_invalid_address_days = b.pm10_tot_invalid_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_invalid_count_correct,
		CASE
			WHEN a.pm10_tot_oob_days = b.pm10_tot_oob_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_oob_count_correct,
		CASE
			WHEN a.pm10_tot_poor_address_days = b.pm10_tot_poor_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_poor_count_correct,
		CASE
			WHEN a.pm10_tot_missing_exp_days = b.pm10_tot_missing_exp_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_missing_count_correct,
		CASE
			WHEN a.pm10_tot_good_address_days = b.pm10_tot_good_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_good_count_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 	
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Dropped Exposures: Life Stage' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_invalid_count_correct = FALSE OR
				is_oob_count_correct = FALSE OR
				is_poor_count_correct = FALSE OR
				is_missing_count_correct = FALSE OR
				is_good_count_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_early_stg_exp_dq_results
	ORDER BY
		person_id,
		ith_life_stage;




	DROP TABLE IF EXISTS results_early_birth_addr_exp_dq_results;
	CREATE TABLE results_early_birth_addr_exp_dq_results AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			inv_addr_days AS pm10_tot_invalid_address_days,
			oob_addr_days AS pm10_tot_oob_days,
			poor_addr_days AS pm10_tot_poor_address_days,
			missing_days AS pm10_tot_missing_exp_days,
			good_addr_days AS pm10_tot_good_address_days
		 FROM
		 	test_early_birth_addr_exp_dq_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_invalid_address_days,
			pm10_tot_oob_days,
			pm10_tot_poor_address_days,
			pm10_tot_missing_exp_days,
			pm10_tot_good_address_days
		 FROM
		 	fin_no_mob_birth_addr_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,		
		CASE
			WHEN a.pm10_tot_invalid_address_days = b.pm10_tot_invalid_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_invalid_count_correct,
		CASE
			WHEN a.pm10_tot_oob_days = b.pm10_tot_oob_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_oob_count_correct,
		CASE
			WHEN a.pm10_tot_poor_address_days = b.pm10_tot_poor_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_poor_count_correct,
		CASE
			WHEN a.pm10_tot_missing_exp_days = b.pm10_tot_missing_exp_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_missing_count_correct,
		CASE
			WHEN a.pm10_tot_good_address_days = b.pm10_tot_good_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_good_count_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 	
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Dropped Exposures: Birth Address' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_invalid_count_correct = FALSE OR
				is_oob_count_correct = FALSE OR
				is_poor_count_correct = FALSE OR
				is_missing_count_correct = FALSE OR
				is_good_count_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_early_birth_addr_exp_dq_results
	ORDER BY
		person_id,
		ith_life_stage;








	
END;

$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test_early_exposure_data(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	
	expected_results_directory TEXT;

	early_cln_expected_results_file TEXT;
	early_uncln_expected_results_file TEXT;
	early_stg_mob_expected_results_file TEXT;
	early_no_mob_birth_addr_expected_results_file TEXT;
	early_exp_sens_expected_results_file TEXT;
	
	number_of_mismatches INT;

BEGIN

	test_suite_directory := 
		test_directory || '\exposure_data\early_life\input_data';
	output_directory :=
		test_directory || '\exposure_data\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	-- =================================================
	-- Test sensitivity variables
	-- =================================================	

	expected_results_directory 
		:= test_directory || '\exposure_data\early_life\expected_results';

	early_cln_expected_results_file :=
		expected_results_directory || '\early_cln_expected_results.csv';

	DROP TABLE IF EXISTS test_early_cln_exp_expected_results;
	CREATE TABLE test_early_cln_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION,
	   pm10_tot_err_sum DOUBLE PRECISION,
	   pm10_tot_err_med DOUBLE PRECISION,
	   pm10_tot_err_avg DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_early_cln_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med,
		pm10_tot_err_sum,
		pm10_tot_err_med,
		pm10_tot_err_avg) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_cln_expected_results_file);


	early_uncln_expected_results_file :=
		expected_results_directory || '\early_uncln_expected_results.csv';

	DROP TABLE IF EXISTS test_early_uncln_exp_expected_results;
	CREATE TABLE test_early_uncln_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_early_uncln_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_uncln_expected_results_file);

	early_stg_mob_expected_results_file :=
		expected_results_directory || '\early_stg_mob_expected_results.csv';

	DROP TABLE IF EXISTS test_early_stg_mob_exp_expected_results;
	CREATE TABLE test_early_stg_mob_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_early_stg_mob_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_stg_mob_expected_results_file);

	early_no_mob_birth_addr_expected_results_file :=
		expected_results_directory || '\early_no_mob_birth_addr_expected_results.csv';

	DROP TABLE IF EXISTS test_early_stg_mob_birth_exp_expected_results;
	CREATE TABLE test_early_stg_mob_birth_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_early_stg_mob_birth_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_no_mob_birth_addr_expected_results_file);



	early_exp_sens_expected_results_file :=
		expected_results_directory || '\early_exp_sens_expected_results.csv';

	DROP TABLE IF EXISTS test_early_exp_sens_expected_results;
	CREATE TABLE test_early_exp_sens_expected_results (
	   person_id TEXT,
	   comments TEXT,
	   no_exposure_data_days INT   
	);

	EXECUTE format ('
	COPY test_early_exp_sens_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		no_exposure_data_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_exp_sens_expected_results_file);

	DROP TABLE IF EXISTS results_test_early_cln_exp;
	CREATE TABLE results_test_early_cln_exp AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med,
			pm10_tot_err_sum,
			pm10_tot_err_med,
			pm10_tot_err_avg
		 FROM
		 	test_early_cln_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med,
			pm10_tot_err_sum,
			pm10_tot_err_med,
			pm10_tot_err_avg
		 FROM
		 	fin_mob_cln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_sum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Early Exposures: Cleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE OR
				is_pm10_tot_err_sum_correct = FALSE  OR
				is_pm10_tot_err_avg_correct = FALSE OR
				is_pm10_tot_err_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_cln_exp
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_test_early_uncln_exp;
	CREATE TABLE results_test_early_uncln_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_early_uncln_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_mob_uncln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Early Exposures: Uncleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_uncln_exp
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_test_early_stg_mob_exp;
	CREATE TABLE results_test_early_stg_mob_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_early_stg_mob_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_stg_mob_exp)
	SELECT
		a.person_id,
		a.ith_life_stage,
		a.life_stage,
		a.comments,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Early Exposures: Fixed Life Stage Mobility' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_stg_mob_exp
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_test_early_stg_mob_birth_exp;
	CREATE TABLE results_test_early_stg_mob_birth_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_early_stg_mob_birth_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_no_mob_birth_addr_exp)
	SELECT
		a.person_id,
		a.ith_life_stage,
		a.life_stage,
		a.comments,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Early Exposures: Fixed Mobility Birth Address' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_stg_mob_birth_exp
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_test_early_exp_sens;
	CREATE TABLE results_test_early_exp_sens AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			no_exposure_data_days
		 FROM
		 	test_early_exp_sens_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			no_exposure_data_days
		 FROM
		 	fin_sens_variables)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN a.no_exposure_data_days = b.no_exposure_data_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_no_exposure_data_days_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id
	ORDER BY
		a.person_id;
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Exposure Sensitivity Variables' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_no_exposure_data_days_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_exp_sens
	ORDER BY
		person_id;

	-- Cleanup
	DROP TABLE test_early_cln_exp_expected_results;
	DROP TABLE test_early_uncln_exp_expected_results;
	DROP TABLE test_early_stg_mob_exp_expected_results;
	DROP TABLE test_early_stg_mob_birth_exp_expected_results;
	DROP TABLE test_early_exp_sens_expected_results;

	/*
	DROP TABLE results_test_early_cln_exp;
	DROP TABLE results_test_early_uncln_exp;
	DROP TABLE results_test_early_stg_mob_exp;
	DROP TABLE results_test_early_stg_mob_birth_exp;
	DROP TABLE results_test_early_exp_sens;
	*/
END;

$$   LANGUAGE plpgsql;





CREATE OR REPLACE FUNCTION test_later_exposure_data(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	
	expected_results_directory TEXT;

	early_cln_expected_results_file TEXT;
	early_uncln_expected_results_file TEXT;
	early_stg_mob_expected_results_file TEXT;
	
	number_of_mismatches INT;

BEGIN

	test_suite_directory := 
		test_directory || '\exposure_data\later_life\input_data';
	output_directory :=
		test_directory || '\exposure_data\later_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_later_analysis(output_directory);

	-- =================================================
	-- Test sensitivity variables
	-- =================================================	
	expected_results_directory 
		:= test_directory || '\exposure_data\later_life\expected_results';

	early_cln_expected_results_file :=
		expected_results_directory || '\later_cln_expected_results.csv';

	DROP TABLE IF EXISTS test_later_cln_exp_expected_results;
	CREATE TABLE test_later_cln_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION,
	   pm10_tot_err_sum DOUBLE PRECISION,
	   pm10_tot_err_med DOUBLE PRECISION,
	   pm10_tot_err_avg DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_later_cln_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med,
		pm10_tot_err_sum,
		pm10_tot_err_med,
		pm10_tot_err_avg) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_cln_expected_results_file);


	early_uncln_expected_results_file :=
		expected_results_directory || '\later_uncln_expected_results.csv';

	DROP TABLE IF EXISTS test_later_uncln_exp_expected_results;
	CREATE TABLE test_later_uncln_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_later_uncln_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_uncln_expected_results_file);

	early_stg_mob_expected_results_file :=
		expected_results_directory || '\later_stg_mob_expected_results.csv';

	DROP TABLE IF EXISTS test_later_stg_mob_exp_expected_results;
	CREATE TABLE test_later_stg_mob_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_later_stg_mob_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_stg_mob_expected_results_file);


	DROP TABLE IF EXISTS results_test_later_cln_exp;
	CREATE TABLE results_test_later_cln_exp AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med,
			pm10_tot_err_sum,
			pm10_tot_err_med,
			pm10_tot_err_avg
		 FROM
		 	test_later_cln_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med,
			pm10_tot_err_sum,
			pm10_tot_err_med,
			pm10_tot_err_avg
		 FROM
		 	fin_mob_cln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_sum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 



	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Late Exposures: Cleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE OR
				is_pm10_tot_err_sum_correct = FALSE  OR
				is_pm10_tot_err_avg_correct = FALSE OR
				is_pm10_tot_err_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_later_cln_exp
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_test_later_uncln_exp;
	CREATE TABLE results_test_later_uncln_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_later_uncln_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_mob_uncln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Later Exposures: Uncleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_later_uncln_exp
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_test_later_stg_mob_exp;
	CREATE TABLE results_test_later_stg_mob_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_later_stg_mob_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_stg_mob_exp)
	SELECT
		a.person_id,
		a.ith_life_stage,
		a.life_stage,
		a.comments,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Later Exposures: Fixed Life Stage Mobility' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_later_stg_mob_exp
	ORDER BY
		person_id,
		ith_life_stage;

END;

$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION run_test_suite
 * -------------------------
 * Description
 * -----------
 * Runs test suite comprising test case methods that cover different areas of ALGAE functionality
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION run_test_suite(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	date_phrase TEXT;
	results_test_suite_results_csv_file TEXT;
	
BEGIN

	DROP TABLE IF EXISTS tmp_all_test_case_results;
	CREATE TABLE tmp_all_test_case_results (
		test_case_name TEXT,
		test_area TEXT,
		test_case_description TEXT,
		pass_or_fail TEXT);

	RAISE NOTICE 'Test directory ==%==', test_directory;
	
	PERFORM test_preprocessing(test_directory);
	PERFORM test_study_member_data(test_directory);
	PERFORM test_geocode_data(test_directory);
	PERFORM test_address_histories(test_directory);
	PERFORM test_early_dropped_exposures(test_directory);      
	PERFORM test_early_exposure_data(test_directory);
	PERFORM test_later_exposure_data(test_directory);
	
	DROP TABLE IF EXISTS results_test_suites;
	CREATE TABLE results_test_suites AS
	SELECT
		test_case_name,
		test_area,
		test_case_description,
		pass_or_fail
	FROM
		 tmp_all_test_case_results
	ORDER BY
		test_area,
		test_case_name,
		test_case_description;

	DROP TABLE tmp_all_test_case_results;
	
	
	-- Write test results to a csv file	
	date_phrase :=
		(SELECT to_char(current_timestamp, 'YYYY-Mon-DD-HH24-MI'));
	results_test_suite_results_csv_file :=
		test_directory || 
		'\res_test_suite_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_test_suites
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_test_suite_results_csv_file);	
	
END;
$$   LANGUAGE plpgsql;

--SELECT "run_test_suite"('C:\algae_protocol\test_environment');

