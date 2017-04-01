/**
 * ================================================================================================= 
 * MODULE COMM_PRELIM_CHECKS: Preliminary System Checks
 * =================================================================================================
 * Description
 * -----------
 * This module contains code which is used to verify the integrity of staging tables before they 
 * are used by the rest of the program. Most of the code tries to add new properties to staging
 * tables to ensure that field values are not null.
 *
 * Assumptions
 * -----------
 * The code here assumes that the method common_preprocess_staging_tables() has already been
 * executed.  When common_perform_prelim_system_checks(), we assume that staging tables
 * have been created from their respective original data tables.  For example, a cohort group
 * may create a table original_study_member_data, but the ALGAE code will create a table called
 * staging_study_member_data, which attempts to address issues of formatting data and streamlining
 * different ways to express variables (eg: changing 'Yes', 'yes', 'true' to a standard 'Y'.
 *
 * Main Function
 * -------------
 *    comm_perform_prelim_system_checks
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
 * MAIN FUNCTION common_perform_prelim_system_checks
 * ------------------------------------------------------
 * Description
 * -----------
 * Ensures that certain fields in the staging tables are guaranteed not to have null values.
 * If any of the ALTER TABLE statements fail, the protocol will abort.  The method exists in order
 * to help minimise sources of bugs from influencing results in later steps.
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION comm_perform_prelim_system_checks()
	RETURNS void AS 
$$
DECLARE

BEGIN	

	--check if basic tables have been created
	perform check_table_exists('staging_study_member_data');	
	perform check_table_exists('staging_addr_history_data');	
	perform check_table_exists('staging_geocode_data');	
	perform check_table_exists('staging_exp_data');	

	--apply database constraints to identify problems in the data

    --Apply constraints to study member data
	ALTER TABLE staging_study_member_data ADD PRIMARY KEY(person_id);	
	ALTER TABLE staging_study_member_data ALTER COLUMN birth_date SET NOT NULL;
	--note that the field 'estimated_gestation_age' could be blank.	
	ALTER TABLE staging_study_member_data ALTER COLUMN absent_during_exp_period SET NOT NULL;
	ALTER TABLE staging_study_member_data ALTER COLUMN at_1st_addr_conception SET NOT NULL;
	
	--Apply constraints to address history data
	ALTER TABLE staging_addr_history_data ALTER COLUMN person_id SET NOT NULL;

    --Apply constraints to geocode and covariate data
	ALTER TABLE staging_geocode_data ALTER COLUMN geocode SET NOT NULL;
	ALTER TABLE staging_geocode_data ALTER COLUMN has_valid_geocode SET NOT NULL;

   	DROP INDEX IF EXISTS ind_staging_geocode_data;
	CREATE INDEX  ind_staging_geocode_data ON staging_geocode_data(geocode);
	
	--Apply constraints to exposure data
	ALTER TABLE staging_exp_data ALTER COLUMN geocode SET NOT NULL;
	ALTER TABLE staging_exp_data ALTER COLUMN date_of_year SET NOT NULL;
   	ALTER TABLE staging_exp_data ADD PRIMARY KEY (geocode, date_of_year);
   

   	
   
   
END;
$$   LANGUAGE plpgsql;	

--SELECT "common_perform_prelim_system_checks"();
