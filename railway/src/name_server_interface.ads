----------------------------------------------------------------------------------
--  Copyright 2013                                								--
--  Moreno Ambrosin                         									--
--  Railway_Simulation 1.0                                       				--
--  Concurrent and Distributed Systems class project  							--
--  Master Degree in Computer Science                 							--
--  Academic year 2012/2013                              						--
--  Dept. of Pure and Applied Mathematics             							--
--  University of Padua, Italy                        							--
--                                                    							--
--  This file is part of Railway_Simulation project.							--
--																				--
--  Railway_Simulation is free software: you can redistribute it and/or modify	--
--  it under the terms of the GNU General Public License as published by		--
--  the Free Software Foundation, either version 3 of the License, or			--
--  (at your option) any later version.											--
--																				--
--  Railway_Simulation is distributed in the hope that it will be useful,		--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>. --
----------------------------------------------------------------------------------
with Ada.Strings.Hash;

with Ada.Containers;use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;

package Name_Server_Interface is

	package String_String_Maps is new Ada.Containers.Indefinite_Hashed_Maps(
		Key_Type 		=> String,
		Element_Type 	=> String,
		Hash			=> Ada.Strings.Hash,
		Equivalent_Keys => "="
	);

	-- # String-String Map used as a cache to maintain last addresses
	Last_Addresses : String_String_Maps.Map;

	Name_Server_Exception : exception;

	-- #
	-- # Remote Procedure used to be bounded to a specific address by the Name Server.
	-- #
	procedure Bind (
		Name_Server : access String;
		Node_Name 	: access String;
		Address		: access String);


	-- #
	-- # Remote Procedure used to be added to Name Server.
	-- #
	procedure Resolve (
		Name_Server : in	 String;
		Node_Name 	: in	 String;
		Callback	: access procedure(Response : String));


end Name_Server_Interface;