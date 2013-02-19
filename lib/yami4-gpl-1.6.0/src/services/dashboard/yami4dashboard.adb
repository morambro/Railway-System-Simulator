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

with Dashboard.Actions;
with Dashboard.Configuration;
with Dashboard.Handlers;
with Log;

with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server;
with AWS.Status;

with Ada.Text_IO;

procedure YAMI4Dashboard is
   
   Success : Boolean;
   Port : Positive;
   WS : AWS.Server.HTTP;
   
begin
   Dashboard.Configuration.Init (Success);
   Dashboard.Actions.Init (Dashboard.Configuration.Remote_Timeout);
   
   if Success then
   
      for Module in Dashboard.Dashboard_Module'Range loop
         Log.Enable (Module,
                     Dashboard.Configuration.Log_Enabled (Module));
      end loop;

      Log.Put (Dashboard.Main, "initialized configuration settings");
      
      Port := Dashboard.Configuration.Port;

      AWS.Server.Start (Web_Server => WS,
                        Name => "YAMI4 dashboard server",
                        Callback => Dashboard.Handlers.Main_Handler'Access,
                        Port => Port,
                        Upload_Directory => "./files");
   
      Log.Put (Dashboard.Main,
               "dashboard server started on port" & Positive'Image (Port) &
                 ", waiting for requests");
   
      loop
         delay 10.0;
      end loop;
   else
      Ada.Text_IO.Put_Line ("configuration not initialized properly");
   end if;
   
end YAMI4Dashboard;
