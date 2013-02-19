--  Copyright Maciej Sobczak 2008-2012.
--  This file is part of YAMI4.
--
--  YAMI4 is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  YAMI4 is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

package Dashboard.Configuration is

   --
   --  Initializes all configuration options from the config file
   --  that is named in the first command-line parameter or
   --  from the yami4dashboard.cfg file if no command-line argument is given.
   --
   procedure Init (Success : out Boolean);

   --
   --  Returns the port number.
   --
   function Port return Positive;
   
   --
   --  Returns the service location for the given index,
   --  empty if not defined.
   --
   function Names_Location (Index : in Positive) return String;
   function Broker_Location (Index : in Positive) return String;
   function Queues_Location (Index : in Positive) return String;
   
   --
   --  Returns the timeout value for remote calls.
   --
   function Remote_Timeout return Duration;
   
   --
   --  Initial log level.
   --
   function Log_Enabled (Module : in Dashboard_Module) return Boolean;

end Dashboard.Configuration;
