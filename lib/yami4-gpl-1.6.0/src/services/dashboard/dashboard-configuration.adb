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

with Log;

with IAL.Properties;
with IAL.Utils;

with Ada.Command_Line;
with Ada.IO_Exceptions;

package body Dashboard.Configuration is

   Default_Config_File_Name : constant String := "yami4dashboard.cfg";

   procedure Init (Success : out Boolean) is

      procedure Init_From_File (Config_File_Name : in String) is
      begin
         IAL.Properties.Load_Properties (Config_File_Name);
         Success := True;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Put (Main, "Cannot read the configuration file: " &
                       Config_File_Name);
      end Init_From_File;

   begin
      Success := False;
      if Ada.Command_Line.Argument_Count /= 0 then
         Init_From_File (Ada.Command_Line.Argument (1));
      else
         Init_From_File (Default_Config_File_Name);
      end if;
   end Init;

   function Port return Positive is
   begin
      return Positive'Value (IAL.Properties.Get ("port", "8080"));
   end Port;

   function Names_Location (Index : in Positive) return String is
      Field_Name : constant String :=
        "names." & IAL.Utils.Natural_Image (Index) & ".location";
   begin
      return IAL.Properties.Get (Field_Name);
   end Names_Location;

   function Broker_Location (Index : in Positive) return String is
      Field_Name : constant String :=
        "broker." & IAL.Utils.Natural_Image (Index) & ".location";
   begin
      return IAL.Properties.Get (Field_Name);
   end Broker_Location;
   
   function Queues_Location (Index : in Positive) return String is
      Field_Name : constant String :=
        "queues." & IAL.Utils.Natural_Image (Index) & ".location";
   begin
      return IAL.Properties.Get (Field_Name);
   end Queues_Location;
   
   function Remote_Timeout return Duration is
   begin
      return Duration'Value (IAL.Properties.Get ("remote.timeout", "3.0"));
   end Remote_Timeout;
   
   function Log_Enabled (Module : in Dashboard_Module) return Boolean is
   begin
      return Boolean'value
        (IAL.Properties.Get
           ("log." & Dashboard_Module'Image (Module), "false"));
   end Log_Enabled;

end Dashboard.Configuration;
