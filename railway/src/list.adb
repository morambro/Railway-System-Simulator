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
--  Railway_Simulation is distributed in the hope that it will be useful,			--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>.		--
----------------------------------------------------------------------------------

package body List is

	procedure Add(
		This	: in	out List;
		Elem 	: in 		Element)
	is
		NewNode : access Node := new Node'(Content => Elem,Next => null);
	begin
		if This.Head = null then
			This.Head := NewNode;
			This.Tail := NewNode;
		else
			This.Tail.Next := NewNode;
		end if;

	end Add;

	function Get(
		This	: in	out List
		Remove	: in 		Boolean) return Element is
	begin
		if This.Head /= null then
			return This.Head.
    	end if;
    end Get;

end List;
