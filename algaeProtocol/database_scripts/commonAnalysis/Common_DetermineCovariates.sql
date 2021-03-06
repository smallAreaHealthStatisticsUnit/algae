/**
 * ================================================================================================= 
 * MODULE COMM_COV_LIFE_STAGE: Determine Spatial Coordinates
 * =================================================================================================
 * Description
 * -----------
 * This module contains code that tries to find geographic covariate data for the locations that
 * study members occupied at the start of each life stage. 
 *
 * This module contains code that is used to help epidemiologists and exposure scientists associate
 * study members with social deprivation scores based on how people move.  Moving is often regarded
 * as a type of stress to an individual's life.  The code here gathers geographical covariate data 
 * for the start date of every move.  It also captures the number of days between the start date of
 * a move and the conception date.  
 *
 * The table it produces has these fields:
 * person_id	start_days_from_conception	ed91	oa2001	coa2011
 * 
 * 
 * Assumptions
 * -----------
 * The routines in this module assume that the address periods have already been completely 
 * processed.
 *
 * Main Function
 * -------------
 *    comm_determine_life_stage_cov()
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
 * MAIN FUNCTION comm_determine_life_stage_cov
 * --------------------------------------------------
 * Description
 * -----------
 * Obtains the geocode associated with the start date of each life stage, then obtains the
 * geographic covariates that are associated with that geocode.
 *
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION comm_determine_life_stage_cov()
	RETURNS void AS 
$$
DECLARE

BEGIN

	--By the time we have created fin_cleaned_addr_periods, the address
	--periods should be temporally configuous.  Therefore, the start date of 
	--each life stage should be associated with exactly one address period.
	
	DROP TABLE IF EXISTS fin_life_stages_cov;
	CREATE TABLE fin_life_stages_cov AS
	WITH locations_on_life_stage_start_dates AS 
		(SELECT
			a.person_id,
			a.ith_life_stage,
			a.life_stage,
			b.geocode
		 FROM
		 	fin_general_life_stage_data a,
		 	fin_cleaned_addr_periods b
		 WHERE
		 	a.person_id = b.person_id AND
		 	a.start_date BETWEEN b.fin_adjusted_start_date AND b.fin_adjusted_end_date AND
		 	b.fit_type != 'D' AND --ignore deleted address periods
		 	b.is_fixed_invalid_geocode = 'N') --ignore periods with bad geocodes we "fixed"
	SELECT	
		c.person_id,
		c.ith_life_stage,
		c.life_stage,
		c.geocode,
		e.ed91,
		e.oa2001,
		e.coa2011
	FROM
		locations_on_life_stage_start_dates c,
		staging_geocode_data e
	WHERE
		c.geocode = e.geocode 
	ORDER BY
		c.person_id,
		c.ith_life_stage;

	ALTER TABLE fin_life_stages_cov ADD PRIMARY KEY (person_id, ith_life_stage);

END;
$$   LANGUAGE plpgsql;
--SELECT "comm_determine_life_stage_cov"();


/*
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION comm_determine_moves_cov
 * ------------------------------------------
 * Description
 * -----------
 * Establishes the geographical covariates for each move (2nd address period and onwards within the 
 * exposure time frame).  The sequence of moves is established by the number of days between when 
 * someone moved and that person's conception date.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_determine_moves_cov()
	RETURNS void AS 
$$
DECLARE

BEGIN

	--Note: we use the ith_residence_type because ith_residence of 2 could
	--become the first viable address if it subsumes the one before it.
	DROP TABLE IF EXISTS fin_moves_cov;
	CREATE TABLE fin_moves_cov AS
	SELECT
		a.person_id,
		a.start_date_days_from_conception,
		b.geocode,
		b.ed91,
		b.oa2001,
		b.coa2011
	 FROM
	 	fin_cleaned_addr_periods a,
		staging_geocode_data b
	WHERE
		a.geocode = b.geocode AND 
		a.is_within_exposure_time_frame = 'Y' AND -- make sure period is relevant to exp time frame
		a.fit_type != 'D' AND -- ignore deleted periods
		a.is_fixed_invalid_geocode = 'N' AND --ignore periods that have been 'fixed' and omitted
		(a.ith_residence_type = 'middle' OR
		 a.ith_residence_type = 'last') -- moves begin with the start date of addr 2
	 ORDER BY 
	 	a.person_id;
	 	
	ALTER TABLE fin_moves_cov ADD PRIMARY KEY (person_id, start_date_days_from_conception);
	 	

END;
$$   LANGUAGE plpgsql;
--SELECT "comm_determine_moves_cov"();



