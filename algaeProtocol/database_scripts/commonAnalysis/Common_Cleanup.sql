/*
 * This is a module to help clean up some of the temporary tables that the protocol
 * produces.  When you're debugging the code or tracing end results back to the
 * original data sets, you may want to not call the comm_cleanup method.
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

CREATE OR REPLACE FUNCTION comm_cleanup()
	RETURNS void AS 
$$
DECLARE


BEGIN 

	DROP TABLE IF EXISTS fin_basic_life_stage_data;
	DROP TABLE IF EXISTS fin_cleaned_addr_periods;
	DROP TABLE IF EXISTS fin_daily_exposures;
	DROP TABLE IF EXISTS fin_general_life_stage_data;
	DROP TABLE IF EXISTS fin_life_stages_cov;
	DROP TABLE IF EXISTS fin_life_stage_sensitivity_variables;
	DROP TABLE IF EXISTS fin_uncln_vs_no_mob;
	DROP TABLE IF EXISTS fin_mob_uncln_exp;
	DROP TABLE IF EXISTS fin_mob_cln_exp;
	DROP TABLE IF EXISTS fin_mob_cln_vs_no_mob;
	DROP TABLE IF EXISTS fin_mob_cln_vs_unimp;
	DROP TABLE IF EXISTS fin_stg_mob_exp;
	DROP TABLE IF EXISTS fin_moves_cov;
	DROP TABLE IF EXISTS fin_sens_variables;

END;
$$   LANGUAGE plpgsql;


