/*
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
 * FUNCTION run_early_study
 * ------------------------
 * Description
 * -----------
 * The routine for running an early life study.  Note that the key difference between this method
 * and run_later_study is the table fin_daily_exposures table is created.  In the early life analysis
 * it involves just making a copy of the staging table.  In the late life analysis, it means
 * converting annual exposures into a set of daily exposures
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION run_later_analysis(output_directory TEXT)
	RETURNS void AS 
$$
DECLARE

BEGIN


	PERFORM comm_preprocess_staging_tables();
	PERFORM comm_perform_prelim_system_checks();
	
	DROP TABLE IF EXISTS fin_daily_exposures;
	CREATE TABLE fin_daily_exposures AS
	WITH days_for_years1 AS
		(SELECT
			geocode,
			date_of_year AS start_date,
			(date_of_year + INTERVAL '1 year' - INTERVAL '1 day')::date AS end_date
		 FROM
		 	staging_exp_data),				
	days_for_years2 AS
		(SELECT
			geocode,
			(generate_series(start_date, end_date, '1 day'::interval))::date AS date_of_year
		 FROM
		 	days_for_years1)
	SELECT
		geocode,
		generate_series(
			date_of_year,
			(date_of_year + INTERVAL '1 year' - INTERVAL '1 day')::date,
			'1 day'::interval) AS date_of_year,
		name,
		nox_rd,
		pm10_gr,
		pm10_rd,
		pm10_tot
	FROM
		staging_exp_data
	ORDER BY
		geocode,
		date_of_year;	
			
	ALTER TABLE fin_daily_exposures ADD PRIMARY KEY (geocode, date_of_year);
		
	
	PERFORM late_calc_life_stages();
	PERFORM comm_set_study_member_sensitivity_data();
	PERFORM comm_process_addr_histories();
	PERFORM comm_set_geocode_sensitivity_data();
	PERFORM comm_set_address_history_sensitivity_data();
	
	PERFORM common_calc_exposures();
		
	PERFORM comm_set_exp_sensitivity_data();
	PERFORM comm_set_stage_sensitivity_data();

	PERFORM comm_determine_life_stage_cov();
	PERFORM comm_determine_moves_cov();
	PERFORM later_life_create_result_tables_and_backup();
	PERFORM later_life_create_reports(output_directory);
	PERFORM comm_cleanup_sensitivity_variable_data();

	--PERFORM comm_cleanup();
	
END;

$$   LANGUAGE plpgsql;